#include <Rinternals.h>
#include <R_ext/Connections.h>
#include <Rdefines.h>

#include <string.h>
// #ifdef HAVE_ERRNO_H
#include <errno.h>
// #endif
# include <unistd.h>

#if defined(Win32)
#  include <winsock2.h>
#  include <io.h>
#else
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  include <sys/socket.h>
#  include <sys/un.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
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
        Rf_error("could not create local socket server:\n  %s",
                 gai_strerror(fd));

    errcode = bind(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1)
        Rf_error("could not bind to local socket:\n  %s",
                 gai_strerror(errcode));

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
        Rf_warning("could not open local socket client:\n  %s",
                   gai_strerror(fd));
        return FALSE;
    }

    errcode = connect(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1) {
        Rf_warning("could not connect open local socket:\n  %s",
                   gai_strerror(errcode));
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
}

void socketeer_destroy(Rconnection ptr)
{
    _skt_free(ptr->private);
    ptr->private = NULL;
}

SEXP _local_client(const char *class, const char *path, const char *mode,
                   const int fd, Rboolean isopen, int timeout)
{
    Rconnection ptr = NULL;
    SEXP con = R_new_custom_connection(path, mode, class, &ptr);
    PROTECT(con);

    ptr->text = FALSE;
    ptr->private = (void *) _skt(fd, timeout, 0);
    ptr->open = socketeer_local_open_client;
    ptr->close = socketeer_close;
    ptr->destroy = socketeer_destroy;
    ptr->read = socketeer_read;
    ptr->write = socketeer_write;
    ptr->isopen = isopen;

    UNPROTECT(1);
    return con;
}

SEXP connection_local_client_fd(SEXP con)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *client = (struct skt *) ptr->private;
    return Rf_ScalarInteger(client->active_fd);
}

SEXP connection_local_client(SEXP path, SEXP mode, SEXP timeout)
{
    return _local_client(
        "local_client", CHAR(STRING_ELT(path, 0)), CHAR(STRING_ELT(mode, 0)),
        0, FALSE, Rf_asInteger(timeout));
}

SEXP connection_local_server(SEXP path, SEXP mode, SEXP timeout, SEXP backlog)
{
    Rconnection ptr = NULL;
    SEXP con = R_new_custom_connection(
        CHAR(STRING_ELT(path, 0)), CHAR(STRING_ELT(mode, 0)), "local_server",
        &ptr);
    PROTECT(con);

    ptr->text = FALSE;
    ptr->private =
        (void *) _skt(0, Rf_asInteger(timeout), Rf_asInteger(backlog));
    ptr->open = socketeer_local_open_server;
    ptr->close = socketeer_close;
    ptr->destroy = socketeer_destroy;
    ptr->read = socketeer_read;
    ptr->write = socketeer_write;

    UNPROTECT(1);
    return con;
}

SEXP connection_server_selectfd(SEXP con, SEXP timeout)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *srv = (struct skt *) ptr->private;

    fd_set fds = srv->active_fds;
    struct timeval tv;
    int n = 0;

    tv.tv_sec = Rf_asInteger(timeout);
    tv.tv_usec = 0;

    SEXP res = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(res, 0, Rf_allocVector(LGLSXP, 2));
    int *server = LOGICAL(VECTOR_ELT(res,0));

    /* read fds */
    if ((n = select(FD_SETSIZE, &fds, NULL, NULL, &tv)) < 0)
        Rf_error("'selectfd' failed:\n  %s", strerror(errno));

    int i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &fds) && i != srv->fd)
            i_rec += 1;

    SET_VECTOR_ELT(res, 1, Rf_allocVector(INTSXP, i_rec));
    int *clients = INTEGER(VECTOR_ELT(res, 1));

    server[0] = FD_ISSET(srv->fd, &fds);
    i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (i != srv->fd && FD_ISSET(i, &fds))
            clients[i_rec++] = i;

    /* write */
    fds = srv->active_fds;
    if ((n = select(FD_SETSIZE, NULL, &fds, NULL, &tv)) < 0)
        Rf_error("'selectfd' failed:\n  %s", strerror(errno));

    i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &fds) && i != srv->fd)
            i_rec += 1;

    SET_VECTOR_ELT(res, 2, Rf_allocVector(INTSXP, i_rec));
    clients = INTEGER(VECTOR_ELT(res, 2));

    server[1] = FD_ISSET(srv->fd, &fds);
    i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (i != srv->fd && FD_ISSET(i, &fds))
            clients[i_rec++] = i;

    UNPROTECT(1);
    return res;
}

SEXP connection_server_accept(SEXP con)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *srv = (struct skt *) ptr->private;

    struct sockaddr_in sockaddr;
    socklen_t len = sizeof(struct sockaddr_in);
    int client_fd;

    client_fd = accept(srv->fd, (struct sockaddr *) &sockaddr, &len);
    if (client_fd < 0)
        Rf_error("could not 'accept' on socket:\n  %s", strerror(errno));
    FD_SET(client_fd, &srv->active_fds);

    return Rf_ScalarInteger(client_fd);
}

SEXP connection_server_set_activefd(SEXP con, SEXP fd)
{
    Rconnection ptr = R_GetConnection(con);
    struct skt *skt = (struct skt *) ptr->private;
    skt->active_fd = Rf_asInteger(fd);

    return con;
}
