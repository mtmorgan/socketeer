#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "sockets.h"

static const R_CallMethodDef callMethods[] = {
    {".sockets_init", (DL_FUNC) &sockets_init, 0},
    {".sockets_fd", (DL_FUNC) &sockets_fd, 1},
    {".sockets_is_open", (DL_FUNC) &sockets_is_open, 1},

    {".client", (DL_FUNC) &client, 2},
    {".client_recv", (DL_FUNC) &client_recv, 1},
    {".client_send", (DL_FUNC) &client_send, 2},
    {".client_close", (DL_FUNC) &client_close, 1},

    {".server", (DL_FUNC) &server, 2},
    {".server_listen", (DL_FUNC) &server_listen, 2},
    {".server_select", (DL_FUNC) &server_select, 2},
    {".server_accept", (DL_FUNC) &server_accept, 1},
    {".clientof_recv", (DL_FUNC) &clientof_recv, 1},
    {".server_recvfrom", (DL_FUNC) &server_recvfrom, 2},
    {".clientof_send", (DL_FUNC) &clientof_send, 2},
    {".server_sendto", (DL_FUNC) &server_sendto, 3},
    {".server_close_clientof", (DL_FUNC) &server_close_clientof, 2},
    {".server_close", (DL_FUNC) &server_close, 1},

    {NULL, NULL, 0}
};

void R_init_sockets(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

void R_unload_sockets(DllInfo *info)
{
    (void) info;
}
