#ifndef SOCKET_H
#define SOCKET_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

    SEXP sockets_init(SEXP sbacklog);
    SEXP is_sockets(SEXP sext);
    SEXP is_open(SEXP sext);
    SEXP socket_hostname(SEXP sext);
    SEXP socket_port(SEXP sext);

    SEXP client_connect(SEXP shost, SEXP sport);
    SEXP client_disconnect();

    SEXP server_bind(SEXP shost, SEXP sport);
    SEXP server_select(SEXP sext, SEXP stimeout);
    SEXP server_send(SEXP sext, SEXP sraw);
    SEXP server_close(SEXP sext);

#ifdef __cplusplus
}
#endif


#endif
