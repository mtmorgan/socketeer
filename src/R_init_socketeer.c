#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "connection.h"

static const R_CallMethodDef callMethods[] = {
    /* connections */
    {".connection_local_server", (DL_FUNC) &connection_local_server, 4},
    {".connection_server_selectfd", (DL_FUNC) &connection_server_selectfd, 2},
    {".connection_server_accept", (DL_FUNC) &connection_server_accept, 1},
    {".connection_server_set_activefd", (DL_FUNC) &connection_server_set_activefd, 2},

    {".connection_local_client", (DL_FUNC) &connection_local_client, 3},
    {".connection_local_client_fd", (DL_FUNC) &connection_local_client_fd, 1},

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
