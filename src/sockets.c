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

static SEXP SOCKETS_TAG = NULL;
static int BACKLOG = 0;
static int BUF_SIZE = 32767;

struct sockets_ptr {
    char *hostname;
    int port;
    short fd;
    fd_set active_fds;          /* server only */
};

SEXP sockets_init(SEXP sbacklog)
{
    SOCKETS_TAG = install("sockets");
    BACKLOG = INTEGER(sbacklog)[0];
    return R_NilValue;
}

/* utilities */

void _check_integer_scalar_non_negative(SEXP x, const char *name)
{
    if ((TYPEOF(x) != INTSXP) || (LENGTH(x) != 1) ||
        (ISNA(INTEGER(x)[0])) || INTEGER(x)[0] < 0)
        Rf_error("'%s' must be non-negative integer(1)", name);
}


/* socket */

static Rboolean _check_sockets(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETS_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("'socket' must be a sockets instance");
    return test;
}

struct sockets_ptr * _sockets_ptr(SEXP sext, Rboolean fail)
{
    struct sockets_ptr * p = R_ExternalPtrAddr(sext);
    if ((NULL == p) && fail)
        Rf_error("'socket' is not valid (closed?)");

    return p;
}

static struct sockets_ptr * _sockets_close(SEXP sext)
{
    if (!_check_sockets(sext, FALSE))
        return NULL;

    struct sockets_ptr *p = _sockets_ptr(sext, FALSE);
    if (NULL == p)
        return NULL;

    if ((0 != p->fd) && (close(p->fd) != 0))
        Rf_error("could not close 'server' socket %s:%d:\n  %s",
                 p->hostname, p->port, strerror(errno));

    p->fd = 0;
    return p;
}

static void _sockets_finalizer(SEXP sext)
{
    if (!_check_sockets(sext, FALSE))
        return NULL;

    struct sockets_ptr *p = _sockets_close(sext);
    if (NULL == p)
        return;

    Free(p->hostname);
    Free(p);
    R_SetExternalPtrAddr(sext, NULL);
}

SEXP is_sockets(SEXP sext)
{
    return Rf_ScalarLogical(_check_sockets(sext, FALSE));
}

SEXP is_open(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, FALSE);
    return Rf_ScalarLogical((NULL != p) && (0 != p->fd));
}

SEXP socket_hostname(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);
    return Rf_ScalarString(mkChar(p->hostname));
}

SEXP socket_port(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);
    return Rf_ScalarInteger(p->port);
}

/* client */

SEXP client_connect(SEXP shost, SEXP sport)
{
    const char *hostname = CHAR(Rf_asChar(shost));
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
    struct sockets_ptr *p = Calloc(1, struct sockets_ptr);
    p->hostname = Calloc(strlen(hostname) + 1, char);
    memcpy(p->hostname, hostname, strlen(hostname));
    p->port = port;
    p->fd = fd;

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _sockets_finalizer, TRUE);

    UNPROTECT(1);
    return sext;
}

SEXP client_disconnect()
{
    return R_NilValue;
}

/* server */

SEXP server_bind(SEXP shost, SEXP sport)
{
    const char *hostname = CHAR(Rf_asChar(shost));
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

    if (listen(fd, BACKLOG) < 0)
        Rf_error("could not 'listen' on socket %s:%d:\n  %s",
                 hostname, port, strerror(errno));

    /* R external pointer */
    struct sockets_ptr *p = Calloc(1, struct sockets_ptr);
    p->hostname = Calloc(strlen(hostname) + 1, char);
    memcpy(p->hostname, hostname, strlen(hostname));
    p->port = port;
    p->fd = fd;
    FD_ZERO(&p->active_fds);
    FD_SET(p->fd, &p->active_fds);

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _sockets_finalizer, TRUE);

    UNPROTECT(1);
    return sext;
}

SEXP server_listen(SEXP sext, SEXP backlog)
{
    (void) _check_integer_scalar_non_negative(backlog);
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);

    if (listen(p->fd, backlog) < 0)
        Rf_error("could not 'listen' on socket %s:%d:\n  %s",
                 hostname, port, strerror(errno));
}

SEXP server_select(SEXP sext, SEXP stimeout)
{
    (void) _check_integer_scalar_non_negative(stimeout, "timeout");
    (void) _check_sockets(sext, TRUE);
    if (!LOGICAL(is_open(sext))[0])
        Rf_error("'server' socket is not open");

    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);
    fd_set read_fds = p->active_fds;
    struct timeval tv;
    int n = 0;

    tv.tv_sec = INTEGER(stimeout)[0];
    tv.tv_usec = 0;
    if ((n = select(FD_SETSIZE, &read_fds, NULL, NULL, &tv)) < 0)
        Rf_error("'select' failed on %s:%d:\n  %s",
                 p->hostname, p->port, strerror(errno));
    if (n == 0)                 /* timeout */
        return Rf_allocVector(VECSXP, 0);

    struct sockaddr_in client;
    socklen_t len = sizeof(struct sockaddr_in);
    int client_fd;

    int i_rec = 0;
    for (int i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET(i, &read_fds) && i != p->fd)
            i_rec += 1;
    SEXP res = PROTECT(Rf_allocVector(VECSXP, i_rec));
    i_rec = 0;

    char recv_buf[BUF_SIZE];

    for (int i = 0; i < FD_SETSIZE; ++i) {

        if (!FD_ISSET(i, &read_fds))
            continue;

        if (i == p->fd) {       /* accept */
            client_fd = accept(i, (struct sockaddr *) &client, &len);
            if (client_fd < 0)
                Rf_error("could not 'accept' on socket %s:%d:\n  %s",
                         p->hostname, p->port, strerror(errno));
            FD_SET(client_fd, &p->active_fds);
        } else {                /* recv */
            ssize_t n = read(i, recv_buf, BUF_SIZE - 1);

            if (n <= 0) {       /* recv close or error */
                if (close(i) != 0)
                    Rf_warning("'select' failed to close client:\n  %s",
                               strerror(errno));
                FD_CLR(i, &p->active_fds);

            } else {            /* recv message */
                recv_buf[n] = '\0';
                SET_VECTOR_ELT(res, i_rec, Rf_allocVector(RAWSXP, n));
                memcpy(RAW(VECTOR_ELT(res, i_rec)), recv_buf, n);
                i_rec += 1;
            }
        }
    }

    UNPROTECT(1);
    return res;
}

SEXP server_send(SEXP sext, SEXP stext)
{
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);
    int n = 0;

    for (int i = 0; i < LENGTH(stext); ++i) {
        const char *elt = CHAR(STRING_ELT(stext, i));
        const int elt_n = strlen(elt);
        if (send(p->fd, elt, elt_n, 0) < 0)
            Rf_error("server 'send' element %d error:\n  %s", strerror(errno));
        n += elt_n;
    }

    return Rf_ScalarInteger(n);
}

SEXP server_recv(SEXP sext, SEXP scheck_user_interrupt)
{
    (void) _check_integer_scalar_non_negative(scheck_user_interrupt,
                                       "check_user_interrupt");
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = _sockets_ptr(sext, TRUE);
    char receive_buf[BUF_SIZE];
    ssize_t n = 0;

    struct timeval tv;
    tv.tv_sec = INTEGER(scheck_user_interrupt)[0];
    tv.tv_usec = 0;

    while (n == 0) {
        if ((n = recv(p->fd, receive_buf, BUF_SIZE - 1, 0)) < 0)
            Rf_error("server 'recv' error:\n  %s", strerror(errno));
        receive_buf[n] = '\0';
    }

    return Rf_ScalarString(mkChar(receive_buf));
}

SEXP server_close(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    (void) _sockets_close(sext);
    return Rf_ScalarLogical(TRUE);
}
