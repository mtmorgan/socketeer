##
## utilities
##

print.socketeer <- function(x, ...)
{
    cat(class(x)[1], " ", x$hostname, ":", x$port, " is_open: ", is_open(x),
        "\n", sep="")
}

._socketeer_fd <- function(socket)
    .Call(.socketeer_fd, socket)

socketeer_fd <- function(socket)
    ._socketeer_fd(socket$socket)

hostname <- function(socket)
    socket$hostname

port <- function(socket)
    socket$port

is_open <- function(socket)
    .Call(.socketeer_is_open, socket$socket)

##
## generics
##

send <- function(x, ...)
    UseMethod("send")

recv <- function(x, ...)
    UseMethod("recv")

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
              class=c("client", "socketeer"))
}

recv.client <- function(client, buffer_block_size=32768L)
    .Call(.client_recv, client$socket, as.integer(buffer_block_size))

send.client <- function(client, raw)
    invisible(.Call(.client_send, client$socket, raw))

close.client <- function(con)
{
    if (is_open(con))
        con <- .Call(.client_close, con$socket)
    invisible(con)
}

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
              class=c("server", "socketeer"))
}

listen <- function(server, backlog=5L)
    invisible(.Call(.server_listen, server$socket, backlog))

selectfd <- function(server, timeout = 30)
{
    ## timeout in seconds
    .Call(.server_selectfd, server$socket, as.integer(timeout))
}

accept <- function(server)
{
    client <- .Call(.server_accept, server$socket)
    structure(list(socket=client, fd=._socketeer_fd(client),
                   hostname=hostname(server), port=port(server)),
              class=c("clientof", "client", "socketeer"))
}

recv.clientof <- function(clientof, buffer_block_size=32768L)
    .Call(.client_recv, clientof$socket, as.integer(buffer_block_size))

send.clientof <- function(clientof, raw)
    ## return value: number of characters sent
    invisible(.Call(.client_send, clientof$socket, raw))

close.clientof <- function(con, server)
{
    if (is_open(con))
        con <- .Call(.server_close_client, server$socket, con$socket)
    invisible(con)
}

close.server <- function(con)
    invisible(.Call(.server_close, con$socket))
