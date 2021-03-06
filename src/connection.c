#include <Rinternals.h>
#include <R_ext/Connections.h>
#include <Rdefines.h>

#include <string.h>
#include <errno.h>
#include <unistd.h>

#if defined(Win32)
#  include <winsock2.h>
#  include <io.h>
#else
#  include <sys/socket.h>
#  include <sys/un.h>
#  include <sys/select.h>
#endif

#include "connection.h"

/* shared */

struct skt {
    short fd, active_fd;
    int timeout;
    /* server only */
    fd_set active_fds;
    int backlog;
};

struct skt * _skt(short fd, int timeout, int backlog)
{
    struct skt *p = Calloc(1, struct skt);

    p->fd = p->active_fd = fd;
    p->timeout = timeout;
    /* server only */
    FD_ZERO(&p->active_fds);
    p->backlog = backlog;

    return p;
}

void _skt_free(struct skt *skt)
{
    Free(skt);
}

size_t skt_recv(void *buf, size_t size, size_t nitems, int fd)
{
    size_t n;

    do {
        n = recv(fd, buf, size * nitems, MSG_WAITALL);
    } while (n == -1 && errno == EINTR);     /* interrupt before receipt */

    if (n < 0)
        Rf_error("socketeer 'read' error:\n  %s", strerror(errno));

    return n;
}

size_t skt_send(const void *buf, size_t size, size_t nitems, int fd)
{
    size_t n;

    do {
        n = send(fd, buf, size * nitems, 0);
    } while (n < 0 && errno == EINTR);

    if (n < 0)
        Rf_error("socketeer 'write' error:\n  %s", strerror(errno));

    return n;
}

/* server */

Rboolean socketeer_local_open_server(Rconnection ptr)
{
    struct sockaddr_un hints;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct sockaddr_un));
    hints.sun_family = AF_UNIX;
    strncpy(hints.sun_path, ptr->description, sizeof(hints.sun_path) - 1);

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd == -1)
        Rf_error("could not create local socket server:\n  %s", strerror(fd));

    errcode = bind(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1)
        Rf_error("could not bind to local socket:\n  %s", strerror(errcode));

    struct skt *srv = (struct skt *) ptr->private;
    srv->fd = fd;
    if (listen(srv->fd, srv->backlog) < 0)
        Rf_error("could not 'listen' on socket:\n  %s", strerror(errno));

    ptr->isopen = TRUE;
    return TRUE;
}

/* client */

Rboolean socketeer_local_open_client(Rconnection ptr)
{
    struct sockaddr_un hints;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct sockaddr_un));
    hints.sun_family = AF_UNIX;
    strncpy(hints.sun_path, ptr->description, sizeof(hints.sun_path) - 1);

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd == -1) {
        Rf_warning("could not open local socket client:\n  %s", strerror(fd));
        return FALSE;
    }

    errcode = connect(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1) {
        Rf_warning("could not connect open local socket:\n  %s",
                   strerror(errcode));
        return FALSE;
    }

    struct skt *client = (struct skt *) ptr->private;
    client->fd = client->active_fd = fd;
    ptr->isopen = TRUE;
    ptr->blocking = FALSE;

    return TRUE;
}

/* connection */

size_t socketeer_read(void *buf, size_t size, size_t n, Rconnection ptr)
{
    struct skt *skt = (struct skt *) ptr->private;
    return skt_recv(buf, size, n, skt->active_fd);
}

size_t socketeer_write(const void *buf, size_t size, size_t n, Rconnection ptr)
{
    struct skt *skt = (struct skt *) ptr->private;
    return skt_send(buf, size, n, skt->active_fd);
}

void socketeer_close(Rconnection ptr)
{
    struct skt *skt = (struct skt *) ptr->private;
    for (int fd = 0; fd < FD_SETSIZE; ++fd)
        if (FD_ISSET(fd, &skt->active_fds))
            close(fd);
    if (skt->fd != 0) {
        close(skt->fd);
        skt->fd = 0;
    }
    ptr->isopen = FALSE;
}

void socketeer_destroy(Rconnection ptr)
{
    _skt_free(ptr->private);
    ptr->private = NULL;
}

SEXP _connection_local(const char *path, const char *mode, const char *class,
                       Rconnection *ptr)
{
    SEXP con = R_new_custom_connection(path, mode, class, ptr);
    (*ptr)->text = FALSE;
    (*ptr)->close = socketeer_close;
    (*ptr)->destroy = socketeer_destroy;
    (*ptr)->read = socketeer_read;
    (*ptr)->write = socketeer_write;

    return con;
}

SEXP connection_local_client(SEXP path, SEXP mode, SEXP timeout)
{
    Rconnection ptr = NULL;
    SEXP con = _connection_local(
        CHAR(STRING_ELT(path, 0)), CHAR(STRING_ELT(mode, 0)), "local_client",
        &ptr);
    ptr->open = socketeer_local_open_client;
    ptr->private = (void *) _skt(0, Rf_asInteger(timeout), 0);

    return con;
}

SEXP connection_local_server(SEXP path, SEXP mode, SEXP timeout, SEXP backlog)
{
    Rconnection ptr = NULL;
    SEXP con = _connection_local(
        CHAR(STRING_ELT(path, 0)), CHAR(STRING_ELT(mode, 0)), "local_server",
        &ptr);
    ptr->open = socketeer_local_open_server;
    ptr->private =
        (void *) _skt(0, Rf_asInteger(timeout), Rf_asInteger(backlog));

    return con;
}

SEXP connection_server_selectfd(SEXP con, SEXP mode)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *srv = (struct skt *) ptr->private;

    fd_set fds = srv->active_fds;
    struct timeval tv;
    int n = 0;

    tv.tv_sec = srv->timeout;
    tv.tv_usec = 0;

    if (CHAR(Rf_asChar(mode))[0] == 'r')
        n = select(FD_SETSIZE, &fds, NULL, NULL, &tv);
    else
        n = select(FD_SETSIZE, NULL, &fds, NULL, &tv);
    if (n < 0)
        Rf_error("'selectfd' failed:\n  %s", strerror(errno));

    int i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &fds) && FD_ISSET(i, &srv->active_fds))
            i_rec += 1;

    SEXP res = PROTECT(Rf_allocVector(INTSXP, i_rec));
    int *clients = INTEGER(res);

    i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &fds) && FD_ISSET(i, &srv->active_fds))
            clients[i_rec++] = i;

    UNPROTECT(1);
    return res;
}

SEXP connection_server_accept(SEXP con)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *srv = (struct skt *) ptr->private;

    struct sockaddr_un sockaddr;
    socklen_t len = sizeof(struct sockaddr_un);
    int client_fd;

    client_fd = accept(srv->fd, (struct sockaddr *) &sockaddr, &len);
    if (client_fd < 0)
        Rf_error("could not 'accept' on socket:\n  %s", strerror(errno));
    FD_SET(client_fd, &srv->active_fds);

    return Rf_ScalarInteger(client_fd);
}

SEXP connection_server_activefds(SEXP con)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *srv = (struct skt *) ptr->private;

    int n = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &srv->active_fds))
            n += 1;

    SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
    int *clients = INTEGER(res);

    n = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &srv->active_fds))
            clients[n++] = i;

    UNPROTECT(1);
    return res;
}

SEXP connection_server_set_activefd(SEXP con, SEXP fd)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *skt = (struct skt *) ptr->private;
    skt->active_fd = Rf_asInteger(fd);

    return con;
}
