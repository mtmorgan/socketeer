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

recv.client <- function(client)
    .Call(.client_recv, client$socket)

send.client <- function(client, raw)
    invisible(.Call(.client_send, client$socket, raw))

close.client <- function(con)
    invisible(.Call(.client_close, con$socket))

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
    clientof <- .Call(.server_accept, server$socket)
    structure(list(socket=clientof, fd=._socketeer_fd(clientof),
                   hostname=hostname(server), port=port(server)),
              class=c("clientof", "client", "socketeer"))
}

recv.clientof <- function(clientof)
    .Call(.clientof_recv, clientof$socket)

send.clientof <- function(clientof, raw)
    ## return value: number of characters sent
    invisible(.Call(.clientof_send, clientof$socket, raw))

recvfrom <- function(server, clientof)
    .Call(.server_recvfrom, server$socket, clientof$socket)

sendto <- function(server, raw, clientof)
    invisible(.Call(.server_sendto, server$socket, raw, clientof$socket))

close.clientof <- function(con, server)
    invisible(.Call(.server_close_clientof, server$socket, con$socket))

close.server <- function(con)
    invisible(.Call(.server_close, con$socket))
