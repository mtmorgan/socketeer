#include "sockets.h"
#include <R_ext/RS.h>

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
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#endif

static SEXP SOCKETS_CLIENT_TAG = NULL;
static SEXP SOCKETS_SERVER_TAG = NULL;
static SEXP SOCKETS_CLIENTOF_TAG = NULL;
static int BUF_SIZE = 32767;

struct client {
    short fd;
};

SEXP sockets_init()
{
    SOCKETS_CLIENT_TAG = install("sockets_client");
    SOCKETS_SERVER_TAG = install("sockets_server");
    SOCKETS_CLIENTOF_TAG = install("sockets_clientof");
    return R_NilValue;
}

/* utilities */

void _is_raw(SEXP sraw)
{
    if (TYPEOF(sraw) != RAWSXP)
        Rf_error("'raw' must be a raw() vector");
}

void _is_integer_scalar_non_negative(SEXP x, const char *name)
{
    if ((TYPEOF(x) != INTSXP) || (LENGTH(x) != 1) ||
        (ISNA(INTEGER(x)[0])) || INTEGER(x)[0] < 0)
        Rf_error("'%s' must be non-negative integer(1)", name);
}

/* client */

Rboolean _is_client(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETS_CLIENT_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("not a 'client' instance");
    return test;
}

struct client * _client_ptr(SEXP sclient, Rboolean fail)
{
    struct client *p = R_ExternalPtrAddr(sclient);
    if ((NULL == p) && fail)
        Rf_error("'client' is not valid (closed?)");
    return p;
}

struct client *_client_close(SEXP sclient)
{
    struct client *p = _client_ptr(sclient, TRUE);
    close(p->fd);
    p-> fd = 0;
    return p;
}

void _client_finalizer(SEXP sclient)
{
    if (!_is_client(sclient, FALSE))
        return;

    struct client *p = _client_close(sclient);
    if (NULL == p)
        return;

    Free(p);
    R_SetExternalPtrAddr(sclient, NULL);
}

SEXP client(SEXP shostname, SEXP sport)
{
    const char *hostname = CHAR(Rf_asChar(shostname));
    const int port = Rf_asInteger(sport);
    SEXP sport1 = PROTECT(Rf_asChar(sport));
    const char *service = CHAR(sport1); /* port-as-character */
    struct addrinfo hints, *addr, *a;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;       /* Allow IPv4 only */
    hints.ai_socktype = SOCK_STREAM; /* stream socket */
    hints.ai_flags = AI_NUMERICSERV; /* Numeric service */
    hints.ai_protocol = 0;          /* Any protocol */

    errcode = getaddrinfo(hostname, service, &hints, &addr);
    if (errcode != 0) {
        UNPROTECT(1);
        Rf_error("could not get address for %s:%s:\n  %s",
                 hostname, service, gai_strerror(errcode));
    }

    for (a = addr; a != NULL; a = a->ai_next) {
        fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
        if (fd == -1)
            continue;

        if (connect(fd, a->ai_addr, a->ai_addrlen) != -1) /* CONNECT */
            break;

        close(fd);
    }

    if (a == NULL)              /* No address succeeded */
        Rf_error("could not connect to address %s:%s", hostname, service);
    freeaddrinfo(addr);
    UNPROTECT(1);

    /* R external pointer */
    struct client *p = Calloc(1, struct client);
    p->fd = fd;
    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_CLIENT_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _client_finalizer, TRUE);

    UNPROTECT(1);
    return sext;
}

SEXP client_recv(SEXP sclient)
{
    (void) _is_client(sclient, TRUE);

    return R_NilValue;
}

SEXP client_send(SEXP sclient, SEXP sraw)
{
    (void) _is_client(sclient, TRUE);
    (void) _is_raw(sraw);

    return R_NilValue;
}

SEXP client_close(SEXP sclient)
{
    (void) _is_client(sclient, TRUE);

    return R_NilValue;
}

/* clientof */

struct clientof {
    short fd;
    struct sockaddr_in *addr;
};

static Rboolean _is_clientof(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETS_CLIENTOF_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("not a 'clientof' instance");
    return test;
}

struct clientof *_clientof_ptr(SEXP sclient, Rboolean fail)
{
    struct clientof * p = R_ExternalPtrAddr(sclient);
    if ((NULL == p) && fail)
        Rf_error("'clientof' is not valid (closed?)");
    return p;
}

struct clientof *_clientof_close(SEXP sclientof)
{
    struct clientof *p = _clientof_ptr(sclientof, TRUE);
    close(p->fd);
    p-> fd = 0;
    return p;
}

void _clientof_finalizer(SEXP sclientof)
{
    if (!_is_clientof(sclientof, FALSE))
        return;

    struct clientof *p = _clientof_close(sclientof);
    if (NULL == p)
        return;

    Free(p->addr);
    Free(p);
    R_SetExternalPtrAddr(sclientof, NULL);
}

struct clientof *_clientof(short fd, struct sockaddr_in *addr)
{
    struct clientof *p = Calloc(1, struct clientof);
    p->fd = fd;
    p->addr = addr;
    return p;
}

/* server */

struct server {
    short fd;
    fd_set active_fds;
};

static Rboolean _is_server(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETS_SERVER_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("not a 'server' instance");
    return test;
}

struct server * _server_ptr(SEXP sserver, Rboolean fail)
{
    struct server * p = R_ExternalPtrAddr(sserver);
    if ((NULL == p) && fail)
        Rf_error("'server' is not valid (closed?)");
    return p;
}

static struct server * _server_close(SEXP sext)
{
    if (!_is_server(sext, FALSE))
        return NULL;

    struct server *p = _server_ptr(sext, FALSE);
    if (NULL == p)
        return NULL;

    if ((0 != p->fd) && (close(p->fd) != 0))
        Rf_error("could not close 'server':\n  %s");

    p->fd = 0;
    return p;
}

static void _server_finalizer(SEXP sext)
{
    if (!_is_server(sext, FALSE))
        return;

    struct server *p = _server_close(sext);
    if (NULL == p)
        return;

    Free(p);
    R_SetExternalPtrAddr(sext, NULL);
}

SEXP server(SEXP shostname, SEXP sport)
{
    const char *hostname = CHAR(Rf_asChar(shostname));
    const int port = Rf_asInteger(sport);
    SEXP sport1 = PROTECT(Rf_asChar(sport));
    const char *service = CHAR(sport1); /* port-as-character */
    struct addrinfo hints, *addr, *a;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;       /* Allow IPv4 */
    hints.ai_socktype = SOCK_STREAM; /* Stream socket */
    hints.ai_flags = AI_PASSIVE;     /* For wildcard IP address */
    hints.ai_protocol = 0;           /* Any protocol */

    errcode = getaddrinfo(hostname, service, &hints, &addr);
    if (errcode != 0) {
        UNPROTECT(1);
        Rf_error("could not get address for %s:%s:\n  %s",
                 hostname, service, gai_strerror(errcode));
    }

    for (a = addr; a != NULL; a = a->ai_next) {
        fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
        if (fd == -1)
            continue;

        if (bind(fd, a->ai_addr, a->ai_addrlen) != -1) /* BIND */
            break;

        close(fd);
    }

    if (a == NULL)              /* No address succeeded */
        Rf_error("could not connect to address %s:%s", hostname, service);
    freeaddrinfo(addr);
    UNPROTECT(1);

    /* R external pointer */
    struct server *p = Calloc(1, struct server);
    p->fd = fd;
    FD_ZERO(&p->active_fds);
    FD_SET(p->fd, &p->active_fds);

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_SERVER_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _server_finalizer, TRUE);
    UNPROTECT(1);
    return sext;
}

SEXP server_listen(SEXP sext, SEXP sbacklog)
{
    (void) _is_server(sext, TRUE);
    (void) _is_integer_scalar_non_negative(sbacklog, "backlog");
    struct server *p = _server_ptr(sext, TRUE);

    if (listen(p->fd, Rf_asInteger(sbacklog)) < 0)
        Rf_error("could not 'listen' on socket:\n  %s", strerror(errno));

    return R_NilValue;
}

SEXP server_select(SEXP sserver, SEXP stimeout)
{
    (void) _is_integer_scalar_non_negative(stimeout, "timeout");
    (void) _is_server(sserver, TRUE);
    if (!LOGICAL(sockets_is_open(sserver))[0])
        Rf_error("'server' socket is not open");

    struct server *p = _server_ptr(sserver, TRUE);
    fd_set read_fds = p->active_fds;
    struct timeval tv;
    int n = 0;

    tv.tv_sec = Rf_asInteger(stimeout);
    tv.tv_usec = 0;
    if ((n = select(FD_SETSIZE, &read_fds, NULL, NULL, &tv)) < 0)
        Rf_error("'select' failed:\n  %s", strerror(errno));

    int i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &read_fds) && i != p->fd)
            i_rec += 1;

    SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, Rf_ScalarLogical(FD_ISSET(p->fd, &read_fds)));
    SET_VECTOR_ELT(res, 1, Rf_allocVector(INTSXP, i_rec));
    int *clients = INTEGER(VECTOR_ELT(res, 1));

    i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (i != p->fd && FD_ISSET(i, &read_fds))
            clients[i_rec++] = i;

    UNPROTECT(1);
    return res;
}

SEXP server_accept(SEXP sserver)
{
    (void) _is_server(sserver, TRUE);
    struct server *p = _server_ptr(sserver, TRUE);

    struct sockaddr_in *client = Calloc(1, struct sockaddr_in);
    socklen_t len = sizeof(struct sockaddr_in);
    int client_fd;

    client_fd = accept(p->fd, (struct sockaddr *) client, &len);
    if (client_fd < 0) {
        Free(client);
        Rf_error("could not 'accept' on socket:\n  %s", strerror(errno));
    }
    FD_SET(client_fd, &p->active_fds);

    struct clientof *c = _clientof(client_fd, client);
    SEXP sclientof = PROTECT(R_MakeExternalPtr(c, SOCKETS_CLIENTOF_TAG, NULL));
    R_RegisterCFinalizerEx(sclientof, _clientof_finalizer, TRUE);

    UNPROTECT(1);
    return sclientof;
}

SEXP server_recvfrom(SEXP sserver, SEXP sclientof)
{
    (void) _is_server(sserver, TRUE);
    (void) _is_clientof(sclientof, TRUE);

    return R_NilValue;
}

SEXP server_sendto(SEXP sserver, SEXP sraw, SEXP sclientof)
{
    (void) _is_server(sserver, TRUE);
    (void) _is_clientof(sclientof, TRUE);
    _is_raw(sraw);

    return R_NilValue;
}

SEXP server_close_clientof(SEXP sserver, SEXP sclientof)
{
    (void) _is_server(sserver, TRUE);
    (void) _is_clientof(sclientof, TRUE);

    struct server *p = _server_ptr(sserver, TRUE);
    struct clientof *cf = _clientof_ptr(sclientof, TRUE);

    FD_CLR(cf->fd, &p->active_fds);
    _clientof_close(sclientof);

    return sserver;
}

SEXP server_close(SEXP sserver)
{
    (void) _is_server(sserver, TRUE);
    (void) _server_close(sserver);
    return Rf_ScalarLogical(TRUE);
}

/* clientof */

SEXP clientof_recv(SEXP sclientof)
{
    (void) _is_clientof(sclientof, TRUE);
    struct clientof *p = _clientof_ptr(sclientof, TRUE);
    char receive_buf[BUF_SIZE];
    ssize_t n = 0;

    while (n == 0) {
        if ((n = recv(p->fd, receive_buf, BUF_SIZE - 1, 0)) < 0)
            Rf_error("clientof 'recv' error:\n  %s", strerror(errno));
    }

    SEXP ret = PROTECT(Rf_allocVector(RAWSXP, n));
    memcpy(RAW(ret), receive_buf, n);
    UNPROTECT(1);
    return ret;
}

SEXP clientof_send(SEXP sclientof, SEXP sraw)
{
    (void) _is_clientof(sclientof, TRUE);
    _is_raw(sraw);
    struct clientof *p = _clientof_ptr(sclientof, TRUE);
    ssize_t n;

    if ((n = send(p->fd, RAW(sraw), LENGTH(sraw), 0)) < 0)
        Rf_error("clientof 'send':\n  %s", strerror(errno));

    return Rf_ScalarInteger(n);
}

/* socket */

Rboolean _is_sockets(SEXP ssockets, Rboolean fail)
{
    Rboolean test = _is_client(ssockets, FALSE) |
        _is_server(ssockets, FALSE) |
        _is_clientof(ssockets, FALSE);
    if (fail && !test)
        Rf_error("not a 'sockets' subclass");
    return test;
}

SEXP sockets_fd(SEXP ssockets)
{
    (void) _is_sockets(ssockets, TRUE);
    int fd = 0;

    if (!LOGICAL(sockets_is_open(ssockets))[0])
        Rf_error("socket is not open");

    if (_is_client(ssockets, FALSE)) {
        struct client *p = _client_ptr(ssockets, FALSE);
        fd = p->fd;
    } else if (_is_server(ssockets, FALSE)) {
        struct server *p = _server_ptr(ssockets, FALSE);
        fd = p->fd;
    } else if (_is_clientof(ssockets, FALSE)) {
        struct clientof *p = _clientof_ptr(ssockets, FALSE);
        fd = p->fd;
    }

    return Rf_ScalarInteger(fd);
}

SEXP sockets_is_open(SEXP ssockets)
{
    (void) _is_sockets(ssockets, TRUE);
    Rboolean test;

    if (_is_client(ssockets, FALSE)) {
        struct client *p = _client_ptr(ssockets, FALSE);
        test = (NULL != p) && (0 != p->fd);
    } else if (_is_server(ssockets, FALSE)) {
        struct server *p = _server_ptr(ssockets, FALSE);
        test = (NULL != p) && (0 != p->fd);
    } else if (_is_clientof(ssockets, FALSE)) {
        struct clientof *p = _clientof_ptr(ssockets, FALSE);
        test = (NULL != p) && (0 != p->fd);
    }

    return Rf_ScalarLogical(test);
}
