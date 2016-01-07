#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "sockets.h"

static const R_CallMethodDef callMethods[] = {
    {".sockets_init", (DL_FUNC) &sockets_init, 1},

    {".is_sockets", (DL_FUNC) &is_sockets, 1},
    {".is_open", (DL_FUNC) &is_open, 1},
    {".socket_hostname", (DL_FUNC) &socket_hostname, 1},
    {".socket_port", (DL_FUNC) &socket_port, 1},

    {".client_connect", (DL_FUNC) &client_connect, 2},
    {".client_disconnect", (DL_FUNC) &client_disconnect, 0},

    {".server_bind", (DL_FUNC) &server_bind, 2},
    {".server_select", (DL_FUNC) &server_select, 2},
    {".server_send", (DL_FUNC) &server_send, 2},
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
