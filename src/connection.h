#ifndef CONNECTION
#define CONNECTION

#include <Rinternals.h>

SEXP connection_local_server(SEXP path, SEXP mode, SEXP timeout, SEXP backlog);
SEXP connection_server_selectfd(SEXP con, SEXP timeout);
SEXP connection_server_accept(SEXP con);
SEXP connection_server_set_activefd(SEXP con, SEXP fd);

SEXP connection_local_client(SEXP path, SEXP mode, SEXP timeout);
SEXP connection_local_client_fd(SEXP con);

#endif
