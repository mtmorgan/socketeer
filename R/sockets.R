##
## utilities
##

print.sockets <- function(x, ...)
{
    cat(class(x)[1], " ", x$hostname, ":", x$port, " is_open: ", is_open(x),
        "\n", sep="")
}

._sockets_fd <- function(socket)
    .Call(.sockets_fd, socket)

hostname <- function(socket)
    socket$hostname

port <- function(socket)
    socket$port

is_open <- function(socket)
    .Call(.sockets_is_open, socket$socket)

##
## client
##

client <- function(hostname="localhost", port)
{
    stopifnot(is(hostname, "character"), length(hostname) == 1L,
              nzchar(hostname))
    port <- as.integer(port)
    stopifnot(length(port) == 1L, !is.na(port), port > 0L)
    client <- .Call(.client, hostname, port)
    structure(list(socket=client, hostname=hostname, port=port),
              class=c("client", "sockets"))
}

client_recv <- function(client)
    .Call(.client_recv, client$socket)

client_send <- function(client, raw)
    .Call(.client_send, client$socket, raw)

client_close <- function(client)
    .Call(.client_close, client$socket)

##
## server
##

server <- function(hostname="localhost", port)
{
    stopifnot(is(hostname, "character"), length(hostname) == 1L,
              nzchar(hostname))
    port <- as.integer(port)
    stopifnot(length(port) == 1L, !is.na(port), port > 0L)
    server <- .Call(.server, hostname, port)
    structure(list(socket=server, hostname=hostname, port=port),
              class=c("server", "sockets"))
}

server_listen <- function(server, backlog=5L)
    invisible(.Call(.server_listen, server$socket, backlog))

server_select <- function(server, timeout = 30)
{
    ## timeout in seconds
    .Call(.server_select, server$socket, as.integer(timeout))
}

server_accept <- function(server)
{
    clientof <- .Call(.server_accept, server$socket)
    structure(list(socket=clientof, fd=._sockets_fd(clientof),
                   hostname=hostname(server), port=port(server)),
              class=c("clientof", "client", "sockets"))
}

clientof_recv <- function(clientof)
    .Call(.clientof_recv, clientof$socket)

clientof_send <- function(clientof, raw)
    ## return value: number of characters sent
    invisible(.Call(.clientof_send, clientof$socket, raw))

server_recvfrom <- function(server, clientof)
    .Call(.server_recvfrom, server$socket, clientof$socket)

server_sendto <- function(server, raw, clientof)
    invisible(.Call(.server_sendto, server$socket, raw, clientof$socket))

server_close_clientof <- function(server, clientof)
    invisible(.Call(.server_close_clientof, server$socket, clientof$socket))

server_close <- function(server)
    invisible(.Call(.server_close, server$socket))
