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


static SEXP SOCKETS_SERVER_TAG = NULL;
static int BACKLOG = 0;

struct sockets_ptr {
    char *hostname[2];          /* server, client */
    int port[2];
    short fd[2];
};

SEXP sockets_init(SEXP sbacklog)
{
    SOCKETS_SERVER_TAG = install("sockets_server");
    BACKLOG = INTEGER(sbacklog)[0];
    return R_NilValue;
}

SEXP client_connect()
{
    return R_NilValue;
}

SEXP client_disconnect()
{
    return R_NilValue;
}

/* socket */

static Rboolean _check_sockets(SEXP sext, Rboolean fail)
{
    Rboolean test = (EXTPTRSXP == TYPEOF(sext)) &&
        (SOCKETS_SERVER_TAG == R_ExternalPtrTag(sext));
    if (fail && !test)
        Rf_error("'socket' must be a sockets instance");
    return test;
}

static struct sockets_ptr * _sockets_close(SEXP sext)
{
    if (NULL == R_ExternalPtrAddr(sext))
        return NULL;

    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    if (NULL == p)
        return NULL;

    if ((0 != p->fd[0]) && (close(p->fd[0]) != 0)) {
        if (0 !=  p->fd[1])
            (void) close(p->fd[1]);
        Rf_error("could not close 'server' socket:\n  %s", strerror(errno));
    }

    if ((0 != p->fd[1]) && (close(p->fd[1]) != 0))
        Rf_error("could not close 'client' socket:\n  %s", strerror(errno));

    p->fd[0] = p->fd[1] = 0;
    return p;
}

static void _sockets_finalizer(SEXP sext)
{
    struct sockets_ptr *p = _sockets_close(sext);
    if (NULL == p)
        return;

    for (int i = 0; i < 2; ++i)
        Free(p->hostname[i]);
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
    SEXP ret = PROTECT(Rf_allocVector(LGLSXP, 2));
    if (NULL != R_ExternalPtrAddr(sext)) {
        struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
        for (int i = 0; i < 2; ++i)
            LOGICAL(ret)[i] = (0 != p->fd[i]);
    }
    UNPROTECT(1);
    return ret;
}

SEXP socket_hostname(SEXP sext)
{
    _check_sockets(sext, TRUE);
    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    SEXP ret = PROTECT(Rf_allocVector(STRSXP, 2));
    for (int i = 0; i < 2; ++i)
        if (NULL != p->hostname[i])
            SET_STRING_ELT(ret, i, mkChar(p->hostname[i]));
        else SET_STRING_ELT(ret, i, NA_STRING);
    UNPROTECT(1);
    return ret;
}

SEXP socket_port(SEXP sext)
{
    _check_sockets(sext, TRUE);
    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, 2));
    for (int i = 0; i < 2; ++i)
        if (0 != p->port[i])
            INTEGER(ret)[i] = p->port[i];
        else INTEGER(ret)[i] = NA_INTEGER;
    UNPROTECT(1);
    return ret;
}

/* server */

SEXP server_bind(SEXP shost, SEXP sport)
{
    const char *hostname = CHAR(STRING_ELT(shost, 0));
    const short port = INTEGER(sport)[0];

    struct hostent *hp = gethostbyname(hostname);

    if (NULL == hp)
        Rf_error("invalid hostname '%s'", hostname);

    struct sockaddr_in server;
    socklen_t len = sizeof(struct sockaddr_in);
    int server_fd;

    /* set up the address information */
    server.sin_family = AF_INET;
    memcpy((char *) &server.sin_addr, hp->h_addr_list[0], hp->h_length);
    server.sin_port = htons(port);

    /* open a socket */
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        Rf_error("could not create socket:\n  %s", strerror(errno));

    /* bind the socket */
    if (bind(server_fd, (struct sockaddr *) &server, len) < 0)
        Rf_error("could not bind socket:\n  %s", strerror(errno));

    /* R external pointer */
    struct sockets_ptr *p = Calloc(1, struct sockets_ptr);
    p->hostname[0] = Calloc(strlen(hostname) + 1, char);
    memcpy(p->hostname[0], hostname, strlen(hostname));
    p->port[0] = port;
    p->fd[0] = server_fd;

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_SERVER_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _sockets_finalizer, TRUE);

    UNPROTECT(1);
    return sext;
}

SEXP server_accept(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    
    if (listen(p->fd[0], BACKLOG) < 0)
        Rf_error("could not 'listen' on socket %s:%d:\n  %s",
                 p->hostname[0], p->port[0], strerror(errno));
    
    struct sockaddr_in client;
    socklen_t len = sizeof(struct sockaddr_in);
    int client_fd;

    client_fd = accept(p->fd[0], (struct sockaddr *) &client, &len);
    if (client_fd < 0)
        Rf_error("could not 'accept' on socket %s:%d:\n  %s",
                 p->hostname[0], p->port[0], strerror(errno));

    const char *client_hostname = inet_ntoa(client.sin_addr);
    p->hostname[1] = Calloc(strlen(client_hostname) + 1, char);
    memcpy(p->hostname[1], client_hostname, strlen(client_hostname));
    p->port[1] = ntohs(client.sin_port);
    p->fd[1] = client_fd;

    return sext;
}

SEXP server_close(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    (void) _sockets_close(sext);
    return Rf_ScalarLogical(TRUE);
}
