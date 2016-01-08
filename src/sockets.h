#ifndef SOCKET_H
#define SOCKET_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

    SEXP sockets_init();
    SEXP sockets_fd();

    SEXP sockets_is_open(SEXP ssocket);

    SEXP client(SEXP shostname, SEXP sport); /* socket(), connect() */
    SEXP client_recv(SEXP sclient);
    SEXP client_send(SEXP sclient, SEXP sraw);
    SEXP client_close(SEXP sclient);

    SEXP server(SEXP shostname, SEXP sport); /* socket(), bind() */
    SEXP server_listen(SEXP sserver, SEXP backlog);
    SEXP server_select(SEXP sserver, SEXP stimeout);
    SEXP server_accept(SEXP sserver);
    SEXP clientof_recv(SEXP sserver);
    SEXP server_recvfrom(SEXP sserver, SEXP sclientof);
    SEXP clientof_send(SEXP sserver, SEXP sraw);
    SEXP server_sendto(SEXP sserver, SEXP sraw, SEXP sclientof);
    SEXP server_close_clientof(SEXP sserver, SEXP sclientof);
    SEXP server_close(SEXP sserver);

#ifdef __cplusplus
}
#endif


#endif
