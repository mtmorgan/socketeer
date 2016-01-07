.onLoad <- function(libname, pkgname) {
    .Call(.sockets_init, BACKLOG=5L)
}
