print.sockets <- function(x, ...)
{
    hostnames <- socket_hostname(x)
    ports <- socket_port(x)
    open <- is_open(x)
    cat("socket",
        "\n  server: ",
        if (open[1]) {
            sprintf("%s:%d", hostnames[1], ports[1])
        } else "NA",
        "\n  client: ",
        if (open[2]) {
            sprintf("%s:%d", hostnames[2], ports[2])
        } else "NA",
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

server_accept <- function(server)
    .Call(.server_accept, server)

server_close <- function(server)
    invisible(.Call(.server_close, server))
