.onLoad <- function(libname, pkgname) {
    .Call(.sockets_init, 5L)
}
