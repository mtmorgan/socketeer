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

struct sockets_ptr {
    char *hostname;
    int port;
    short fd;
};

SEXP sockets_init()
{
    SOCKETS_SERVER_TAG = install("sockets_server");
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


    if ((0 != p->fd) && (close(p->fd) != 0))
        Rf_error("could not close socket:\n  %s", strerror(errno));
    p->fd = 0;
    return p;
}

static void _sockets_finalizer(SEXP sext)
{
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
    Rboolean test = (NULL != R_ExternalPtrAddr(sext)) &&
        (0 != ((struct sockets_ptr *) R_ExternalPtrAddr(sext))->fd);
    return Rf_ScalarLogical(test);
}

SEXP socket_hostname(SEXP sext)
{
    _check_sockets(sext, TRUE);
    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    return Rf_ScalarString(mkChar(p->hostname));
}

SEXP socket_port(SEXP sext)
{
    _check_sockets(sext, TRUE);
    struct sockets_ptr *p = (struct sockets_ptr *) R_ExternalPtrAddr(sext);
    return Rf_ScalarInteger(p->port);
}

/* server */

SEXP server_bind(SEXP shost, SEXP sport)
{
    const char *hostname = CHAR(STRING_ELT(shost, 0));
    const short port = INTEGER(sport)[0];

    struct hostent *hp = gethostbyname(hostname);

    if (NULL == hp)
        Rf_error("invalid hostname '%s'", hostname);

    struct sockaddr_in saddr_in;
    socklen_t saddr_len = sizeof(struct sockaddr_in); // length of address
    int server_sock;            // socket file descriptor

    /* set up the address information */
    saddr_in.sin_family = AF_INET;
    memcpy((char *) &saddr_in.sin_addr, hp->h_addr_list[0], hp->h_length);
    saddr_in.sin_port = htons(port);

    /* open a socket */
    if ((server_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        Rf_error("could not create socket:\n  %s", strerror(errno));

    /* bind the socket */
    if (bind(server_sock, (struct sockaddr *) &saddr_in, saddr_len) < 0)
        Rf_error("could not bind socket:\n  %s", strerror(errno));

    /* R external pointer */
    struct sockets_ptr *p = Calloc(1, struct sockets_ptr);
    p->hostname = Calloc(strlen(hostname) + 1, char);
    memcpy(p->hostname, hostname, strlen(hostname));
    p->port = port;
    p->fd = server_sock;

    SEXP sext = PROTECT(R_MakeExternalPtr(p, SOCKETS_SERVER_TAG, NULL));
    R_RegisterCFinalizerEx(sext, _sockets_finalizer, TRUE);

    UNPROTECT(1);
    return sext;
}

SEXP server_close(SEXP sext)
{
    (void) _check_sockets(sext, TRUE);
    (void) _sockets_close(sext);
    return Rf_ScalarLogical(TRUE);
}
