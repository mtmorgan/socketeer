#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "socketeer.h"

static const R_CallMethodDef callMethods[] = {
    {".socketeer_init", (DL_FUNC) &socketeer_init, 0},
    {".socketeer_fd", (DL_FUNC) &socketeer_fd, 1},
    {".socketeer_is_open", (DL_FUNC) &socketeer_is_open, 1},

    /* client */
    {".client", (DL_FUNC) &client, 2},
    {".client_recv", (DL_FUNC) &client_recv, 2},
    {".client_send", (DL_FUNC) &client_send, 2},
    {".client_close", (DL_FUNC) &client_close, 1},

    /* server */
    {".server", (DL_FUNC) &server, 2},
    {".server_listen", (DL_FUNC) &server_listen, 2},
    {".server_selectfd", (DL_FUNC) &server_selectfd, 2},
    {".server_accept", (DL_FUNC) &server_accept, 1},
    {".server_close_client", (DL_FUNC) &server_close_client, 2},
    {".server_close", (DL_FUNC) &server_close, 1},

    /* UDP server-side clients */
    /* {".server_recvfrom", (DL_FUNC) &server_recvfrom, 2}, */
    /* {".server_sendto", (DL_FUNC) &server_sendto, 3}, */
    {NULL, NULL, 0}
};

void R_init_socketeer(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

void R_unload_socketeer(DllInfo *info)
{
    (void) info;
}
