#include "socketeer.h"
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
#  include <sys/un.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#endif

static SEXP SOCKETEER_CLIENT_TAG = NULL;
static SEXP SOCKETEER_SERVER_TAG = NULL;

SEXP socketeer_init()
{
    SOCKETEER_CLIENT_TAG = install("socketeer_client");
    SOCKETEER_SERVER_TAG = install("socketeer_server");
    return R_NilValue;
}

/* buffer */

struct buffer {
    int block_size, used_size;
    char *block;
    struct buffer *next;
};

struct buffer *_buffer(int block_size)
{
    struct buffer *b = Calloc(1, struct buffer);
    b->block_size = block_size;
    b->used_size = 0;
    b->block = Calloc(b->block_size, char);
    b->next = NULL;
    return b;
}

struct buffer *_buffer_grow(struct buffer *b)
{
    b->next = _buffer(b->block_size);
    return b->next;
}

void _buffer_free(struct buffer *b)
{
    struct buffer *next;
    while (NULL != b) {
        next = b->next;
        Free(b->block);
        Free(b);
        b = next;
    }
}

SEXP _buffer_as_raw(struct buffer *b)
{
    ssize_t n = 0;
    struct buffer *curr = b;
    while (NULL != curr) {
        n += curr->used_size;
        curr = curr->next;
    }

    SEXP res = PROTECT(Rf_allocVector(RAWSXP, n));
    unsigned char *p = RAW(res);
    n = 0;
    curr = b;
    while (NULL != curr) {
        memcpy(p + n, curr->block, curr->used_size);
        n += curr->used_size;
        curr = curr->next;
    }

    UNPROTECT(1);
    return res;
}

/* utilities */

void _is_raw(SEXP x, const char *name)
{
    if (TYPEOF(x) != RAWSXP)
        Rf_error("'%s' must be raw()", name);
}

void _is_character_scalar(SEXP x, const char *name)
{
    if ((TYPEOF(x) != STRSXP) || (Rf_length(x) != 1) ||
        (Rf_length(Rf_asChar(x)) == 0))
        Rf_error("'%s' must be character(1) with nzchar()", name);
}

void _is_logical_scalar(SEXP x, const char *name)
{
    if ((TYPEOF(x) != LGLSXP) || (Rf_length(x) != 1) || (ISNA(LOGICAL(x)[0])))
        Rf_error("'%s' must be logical(1) and not NA", name);
}

void _is_integer_scalar_non_negative(SEXP x, const char *name)
{
    if ((TYPEOF(x) != INTSXP) || (Rf_length(x) != 1) ||
        (ISNA(INTEGER(x)[0])) || INTEGER(x)[0] < 0)
        Rf_error("'%s' must be non-negative integer(1)", name);
}

SEXP _send(int fd, SEXP sraw, const char *name)
{
    (void) _is_raw(sraw, "raw");
    ssize_t n;

    if ((n = send(fd, RAW(sraw), LENGTH(sraw), 0)) < 0)
        Rf_error("%s 'send':\n  %s", name, strerror(errno));

    return Rf_ScalarInteger(n);
}

SEXP _recv(int fd, int block_size)
{
    struct buffer
        *buffer_head = _buffer(block_size),
        *b = buffer_head;
    ssize_t n = 0;
    int flags = 0;

    for (;;) {
        do
            n = recv(fd, b->block, b->block_size, flags);
        while (n == EINTR);     /* interrupt before receipt */

        if (n == 0) {           /* terminated gracefully */
            break;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
            if (n > 0) b->used_size += n;
            break;            /* blocking */
        } else if (n < 0) {     /* error */
            _buffer_free(buffer_head);
            Rf_error("'recv' error:\n  %s", strerror(errno));
        }

        /* n == block_size, maybe more data? */
        flags |= MSG_DONTWAIT;
        b->used_size = n;
        b = _buffer_grow(b);
    }

    SEXP ret = PROTECT(_buffer_as_raw(buffer_head));
    _buffer_free(buffer_head);

    UNPROTECT(1);
    return ret;
}

/* client */

struct client {
    short fd;
};

struct client *_client(short fd)
{
    struct client *p = Calloc(1, struct client);
    p->fd = fd;
    return p;
}

Rboolean _is_client(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETEER_CLIENT_TAG == R_ExternalPtrTag(sext));
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

Rboolean _client_open(const char *hostname, const char *service,
                      struct client **client_ptr)
{
    struct addrinfo hints, *addr, *a;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;       /* Allow IPv4 only */
    hints.ai_socktype = SOCK_STREAM; /* stream socket */
    hints.ai_flags = AI_NUMERICSERV; /* Numeric service */
    hints.ai_protocol = 0;           /* Any protocol */

    errcode = getaddrinfo(hostname, service, &hints, &addr);
    if (errcode != 0) {
        Rf_warning("could not get address for %s:%s:\n  %s",
                   hostname, service, gai_strerror(errcode));
        return FALSE;
    }

    for (a = addr; a != NULL; a = a->ai_next) {
        fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
        if (fd == -1)
            continue;

        if (connect(fd, a->ai_addr, a->ai_addrlen) != -1) /* CONNECT */
            break;

        close(fd);
    }

    freeaddrinfo(addr);

    if (a == NULL) {            /* No address succeeded */
        Rf_warning("could not connect to address %s:%s", hostname, service);
        return FALSE;
    }

    *client_ptr = _client(fd);

    return TRUE;
}

void _client_close(struct client *client)
{
    if (NULL == client)
        return;

    if ((0 != client->fd) && (close(client->fd) != 0))
        Rf_error("could not close 'client':\n  %s");

    client-> fd = 0;
}

void _client_finalizer(SEXP sclient)
{
    if (!_is_client(sclient, FALSE))
        return;

    struct client *client = _client_ptr(sclient, FALSE);
    if (NULL == client)
        return;
    _client_close(client);

    Free(client);
    R_SetExternalPtrAddr(sclient, NULL);
}

SEXP client(SEXP shostname, SEXP sport)
{
    _is_character_scalar(shostname, "hostname");
    _is_integer_scalar_non_negative(sport, "port");

    const char *hostname = CHAR(Rf_asChar(shostname));
    SEXP sport1 = PROTECT(Rf_asChar(sport));
    const char *service = CHAR(sport1); /* port-as-character */
    struct client *client;
    SEXP ret = NULL;

    Rboolean ok = _client_open(hostname, service, &client);
    if (!ok) {
        UNPROTECT(1);
        Rf_error("socketeer 'client' failed to open");
    }

    ret = PROTECT(R_MakeExternalPtr(client, SOCKETEER_CLIENT_TAG, NULL));
    R_RegisterCFinalizerEx(ret, _client_finalizer, TRUE);
    UNPROTECT(2);

    return ret;
}

SEXP client_recv(SEXP sclient, SEXP sbuffer_block_size)
{
    (void) _is_client(sclient, TRUE);
    _is_integer_scalar_non_negative(sbuffer_block_size, "buffer_block_size");
    struct client *p = _client_ptr(sclient, TRUE);
    return _recv(p->fd, Rf_asInteger(sbuffer_block_size));
}

SEXP client_send(SEXP sclient, SEXP sraw)
{
    (void) _is_client(sclient, TRUE);
    struct client *p = _client_ptr(sclient, TRUE);
    return _send(p->fd, sraw, "client");
}

SEXP client_close(SEXP sclient)
{
    (void) _is_client(sclient, TRUE);
    _client_close(_client_ptr(sclient, FALSE));
    return R_NilValue;
}

/* server */

struct server {
    short fd;
    fd_set active_fds;
};

struct server *_server(short fd)
{
    struct server *p = Calloc(1, struct server);
    p->fd = fd;
    FD_ZERO(&p->active_fds);
    FD_SET(p->fd, &p->active_fds);
    return p;
}

static Rboolean _is_server(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETEER_SERVER_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("not a 'server' instance");
    return test;
}

struct server *_server_ptr(SEXP sserver, Rboolean fail)
{
    struct server * p = R_ExternalPtrAddr(sserver);
    if ((NULL == p) && fail)
        Rf_error("'server' is not valid (closed?)");
    return p;
}

static struct server *_server_close(SEXP sserver)
{
    struct server *p = _server_ptr(sserver, FALSE);
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
    (void) _is_character_scalar(shostname, "hostname");
    (void) _is_integer_scalar_non_negative(sport, "port");

    const char *hostname = CHAR(Rf_asChar(shostname));
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
    struct server *p = _server(fd);

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETEER_SERVER_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _server_finalizer, TRUE);
    UNPROTECT(1);

    return sext;
}

SEXP server_listen(SEXP sext, SEXP sbacklog)
{
    (void) _is_server(sext, TRUE);
    _is_integer_scalar_non_negative(sbacklog, "backlog");
    struct server *p = _server_ptr(sext, TRUE);

    if (listen(p->fd, Rf_asInteger(sbacklog)) < 0)
        Rf_error("could not 'listen' on socket:\n  %s", strerror(errno));

    return R_NilValue;
}

SEXP server_selectfd(SEXP sserver, SEXP stimeout)
{
    (void) _is_server(sserver, TRUE);
    (void) _is_integer_scalar_non_negative(stimeout, "timeout");
    if (!LOGICAL(socketeer_is_open(sserver))[0])
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

    struct sockaddr_in client;
    socklen_t len = sizeof(struct sockaddr_in);
    int client_fd;

    client_fd = accept(p->fd, (struct sockaddr *) &client, &len);
    if (client_fd < 0)
        Rf_error("could not 'accept' on socket:\n  %s", strerror(errno));
    FD_SET(client_fd, &p->active_fds);

    struct client *c = _client(client_fd);
    SEXP sclient = PROTECT(R_MakeExternalPtr(c, SOCKETEER_CLIENT_TAG, NULL));
    R_RegisterCFinalizerEx(sclient, _client_finalizer, TRUE);

    UNPROTECT(1);
    return sclient;
}

SEXP server_close_client(SEXP sserver, SEXP sclient)
{
    (void) _is_server(sserver, TRUE);
    (void) _is_client(sclient, TRUE);

    struct server *p = _server_ptr(sserver, TRUE);
    struct client *cf = _client_ptr(sclient, TRUE);

    FD_CLR(cf->fd, &p->active_fds);
    _client_close(cf);

    return sserver;
}

SEXP server_close(SEXP sserver)
{
    (void) _is_server(sserver, TRUE);
    (void) _server_close(sserver);
    return Rf_ScalarLogical(TRUE);
}

/* socket */

Rboolean _is_socketeer(SEXP ssocketeer, Rboolean fail)
{
    Rboolean test =
        _is_client(ssocketeer, FALSE) || _is_server(ssocketeer, FALSE);
    if (fail && !test)
        Rf_error("not a 'socketeer' subclass");
    return test;
}

SEXP socketeer_fd(SEXP ssocketeer)
{
    (void) _is_socketeer(ssocketeer, TRUE);
    int fd = 0;

    if (!LOGICAL(socketeer_is_open(ssocketeer))[0])
        Rf_error("socket is not open");

    if (_is_client(ssocketeer, FALSE)) {
        struct client *p = _client_ptr(ssocketeer, FALSE);
        fd = p->fd;
    } else if (_is_server(ssocketeer, FALSE)) {
        struct server *p = _server_ptr(ssocketeer, FALSE);
        fd = p->fd;
    } else if (_is_client(ssocketeer, FALSE)) {
        struct client *p = _client_ptr(ssocketeer, FALSE);
        fd = p->fd;
    }

    return Rf_ScalarInteger(fd);
}

SEXP socketeer_is_open(SEXP ssocketeer)
{
    (void) _is_socketeer(ssocketeer, TRUE);
    Rboolean test = FALSE;

    if (_is_client(ssocketeer, FALSE)) {
        struct client *p = _client_ptr(ssocketeer, FALSE);
        test = (NULL != p) && (0 != p->fd);
    } else if (_is_server(ssocketeer, FALSE)) {
        struct server *p = _server_ptr(ssocketeer, FALSE);
        test = (NULL != p) && (0 != p->fd);
    } else if (_is_client(ssocketeer, FALSE)) {
        struct client *p = _client_ptr(ssocketeer, FALSE);
        test = (NULL != p) && (0 != p->fd);
    }

    return Rf_ScalarLogical(test);
}

/*

  server/client_local

 */

Rboolean _client_local_open(const char *pathname, struct client **client_ptr)
{
    struct sockaddr_un hints;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct sockaddr_un));
    hints.sun_family = AF_UNIX;
    strncpy(hints.sun_path, pathname, sizeof(hints.sun_path) - 1);

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd == -1) {
        Rf_warning("could not create local socket client:\n  %s",
                   gai_strerror(fd));
        return FALSE;
    }

    errcode = connect(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1) {
        Rf_warning("could not connect to local socket:\n  %s",
                   gai_strerror(errcode));
        return FALSE;
    }

    *client_ptr = _client(fd);

    return TRUE;
}

SEXP client_local(SEXP spath)
{
    _is_character_scalar(spath, "path");

    const char *path = CHAR(Rf_asChar(spath));
    struct client *client;
    SEXP ret = NULL;

    Rboolean ok = _client_local_open(path, &client);
    if (!ok) {
        UNPROTECT(1);
        Rf_error("socketeer 'client' failed to open");
    }

    ret = PROTECT(R_MakeExternalPtr(client, SOCKETEER_CLIENT_TAG, NULL));
    R_RegisterCFinalizerEx(ret, _client_finalizer, TRUE);
    UNPROTECT(1);

    return ret;
}

SEXP server_local(SEXP spath)
{
    (void) _is_character_scalar(spath, "path");

    const char *path = CHAR(Rf_asChar(spath));
    struct sockaddr_un hints;
    int errcode = 0, fd = 0;

    memset(&hints, 0, sizeof(struct sockaddr_un));
    hints.sun_family = AF_UNIX;
    strncpy(hints.sun_path, path, sizeof(hints.sun_path) - 1);

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd == -1)
        Rf_error("could not create local socket server:\n  %s",
                 gai_strerror(fd));

    errcode = bind(fd, (const struct sockaddr *) &hints, sizeof(hints));
    if (errcode == -1)
        Rf_error("could not bind to local socket:\n  %s",
                 gai_strerror(errcode));

    struct server *p = _server(fd);

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETEER_SERVER_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _server_finalizer, TRUE);
    UNPROTECT(1);

    return sext;
}
