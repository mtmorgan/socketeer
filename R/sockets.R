print.sockets <- function(x, ...)
{
    hostnames <- socket_hostname(x)
    ports <- socket_port(x)
    open <- is_open(x)
    cat("socket ", socket_hostname(x), ":", socket_port(x),
        ". is_open: ", is_open(x),
        "\n", sep="")
}

is_socket <- function(socket)
    .Call(.is_sockets, socket)

is_open <- function(socket)
    .Call(.is_open, socket)

socket_hostname <- function(socket)
    .Call(.socket_hostname, socket)

socket_port <- function(socket)
    .Call(.socket_port, socket)

client_connect <- function(host="localhost", port)
{
    stopifnot(is(host, "character"), length(host) == 1L, nzchar(host))
    port <- as.integer(port)
    stopifnot(length(port) == 1L, !is.na(port), port > 0L)
    sock <- .Call(.client_connect, host, port)
    class(sock) <- c("client", "sockets")
    sock
}

client_disconnect <- function(...)
    .Call(.client_disconnect, ...)

server_bind <- function(host="localhost", port)
{
    stopifnot(is(host, "character"), length(host) == 1L, nzchar(host))
    port <- as.integer(port)
    stopifnot(length(port) == 1L, !is.na(port), port > 0L)
    sock <- .Call(.server_bind, host, port)
    class(sock) <- c("server", "sockets")
    sock
}

server_select <- function(server, timeout = 30)
    ## timeout in seconds
    .Call(.server_select, server, as.integer(timeout))

server_send <- function(server, any)
    ## return value: number of characters sent
    invisible(.Call(.server_send, server, any))

server_close <- function(server)
    invisible(.Call(.server_close, server))
