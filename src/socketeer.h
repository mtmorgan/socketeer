#ifndef SOCKET_H
#define SOCKET_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

    SEXP socketeer_init();
    SEXP socketeer_fd();
    SEXP socketeer_is_open(SEXP ssocket);

    /* client */
    SEXP client(SEXP shostname, SEXP sport); /* socket(), connect() */
    SEXP client_recv(SEXP sclient, SEXP sbuffer_block_size);
    SEXP client_send(SEXP sclient, SEXP sraw);
    SEXP client_close(SEXP sclient);

    /* server */
    SEXP server(SEXP shostname, SEXP sport); /* socket(), bind() */
    SEXP server_listen(SEXP sserver, SEXP backlog);
    SEXP server_selectfd(SEXP sserver, SEXP stimeout);
    SEXP server_accept(SEXP sserver);
    SEXP server_close_client(SEXP sserver, SEXP sclient);
    SEXP server_close(SEXP sserver);

    /* UDP server-side clients */
    /* SEXP server_recvfrom(SEXP sserver); */
    /* SEXP server_sendto(SEXP sserver, SEXP sraw, SEXP sclient); */

#ifdef __cplusplus
}
#endif


#endif
