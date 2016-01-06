print.sockets <- function(x, ...)
{
    cat(class(x)[length(class(x)) - 1L], " socket on ",
        socket_hostname(x), ":", socket_port(x),
        ". is_open: ", is_open(x), "\n",
        sep="")
}

is_socket <- function(socket)
    .Call(.is_sockets, socket)

is_open <- function(socket)
    .Call(.is_open, socket)

socket_hostname <- function(socket)
    .Call(.socket_hostname, socket)

socket_port <- function(socket)
    .Call(.socket_port, socket)

client_connect <- function(...)
    .Call(.client_connect, ...)

client_disconnect <- function(...)
    .Call(.client_disconnect, ...)

server_bind <- function(host, port)
{
    stopifnot(is(host, "character"), length(host) == 1L, nzchar(host))
    port <- as.integer(port)
    stopifnot(length(port) == 1L, !is.na(port), port > 0L)
    sock <- .Call(.server_bind, host, port)
    class(sock) <- c("server", "sockets")
    sock
}

server_close <- function(server)
    invisible(.Call(.server_close, server))
