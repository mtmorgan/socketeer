##
## utilities
##

#' @useDynLib socketeer, .registration=TRUE

#' @rdname socketeer
#'
#' @title Internet and local socket connections
#'
#' @importFrom methods is
#'
#' @description The `client()` / `server()` pair is used to create
#'     socket connections on one or more machines. This requires a
#'     valid hostname or IP address and open port for
#'     communication. The program flow is to create the `server()` and
#'     indicate that the server is ready to `listen()` for new
#'     clients. One or more clients are created using `client()`,
#'     which includes the client attempting to connect to the
#'     server. The server uses `selectfd()` and `accept()` to complete
#'     the client connection. Server and client are then able to
#'     `send()` and `recv()` messages. `serialize()` is used on
#'     sending, `unserialize()` on receipt.
#'
#' @param hostname character(1) internet IP or host name, or "localhost".
#'
#' @param port integer(1) port to connect. Port must be open.
#'
#' @return `client()`: a client object.
#'
#' @export
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

#' @rdname socketeer
#'
#' @return `server()`: a server object.
#'
#' @export
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

##
## client/server_local
##

#' @rdname socketeer
#'
#' @description The `client_local()` / `server_local()` pair is used
#'     to create socket connections on a single machine. Communication
#'     is via Unix files rather than host and port.
#'
#' @param path character(1) file path to be used for local socket
#'     connections.
#'
#' @return `client_local()`: a client_local object.
#'
#' @export
client_local <- function(path = tempfile(fileext=".socketeer"))
{
    stopifnot(is(path, "character"), length(path) == 1L, nzchar(path))
    client <- .Call(.client_local, path)
    structure(
        list(socket=client, path = path),
        class=c("local", "client", "socketeer")
    )
}

#' @rdname socketeer
#'
#' @return `server_local()`: a server_local object.
#'
#' @export
server_local <- function(path = tempfile(fileext = ".socketeer"))
{
    stopifnot(is(path, "character"), length(path) == 1L, nzchar(path))
    server <- .Call(.server_local, path)
    structure(
        list(socket=server, path = path),
        class=c("local", "server", "socketeer")
    )
}

#' @rdname socketeer
#'
#' @param server an object of class `server`.
#'
#' @param backlog integer(1) (<128) the maximum number of pending
#'     client requests allowed.
#'
#' @return `listen()`: NULL, invisibly.
#'
#' @export
listen <- function(server, backlog=5L)
    invisible(.Call(.server_listen, server$socket, backlog))

#' @rdname socketeer
#'
#' @param timeout integer(1) number of seconds to block waiting for
#'     incoming connections.
#'
#' @return `selectfd()`: a list of length 2. The first element is a
#'     vector of logical values indicating descriptors ready for
#'     reading. The second element is a vector of file descriptors.
#'
#' @export
selectfd <- function(server, timeout = 30)
{
    ## timeout in seconds
    .Call(.server_selectfd, server$socket, as.integer(timeout))
}

#' @rdname socketeer
#'
#' @return `accept()` returns an object of class `clientof` that can
#'     be used to `send()` and `recv()` messages with the client.
#'
#' @export
accept <- function(server)
{
    client <- .Call(.server_accept, server$socket)
    structure(list(socket=client, fd=._socketeer_fd(client),
                   hostname=hostname(server), port=port(server)),
              class=c("clientof", "client", "socketeer"))
}

#' @rdname socketeer
#'
#' @return `is_open()`: logical(1) indication that the port is open
#'     (TRUE) or not FALSE)
#'
#' @export
is_open <- function(socket)
    .Call(.socketeer_is_open, socket$socket)

#' @rdname socketeer
#'
#' @param msg an R object to be serialized and transmitted between
#'     server and client
#'
#' @return `send()`: the number of bytes transmitted, invisibly.
#'
#' @export
send <- function(socket, msg)
    invisible(.Call(.client_send, socket$socket, serialize(msg, NULL)))

#' @rdname socketeer
#'
#' @param buffer_block_size integer(1) size of buffer used during
#'     receipt.
#'
#' @return `recv()`: the unserialized message. `recv()` is a blocking call.
#'
#' @export
recv <- function(socket, buffer_block_size=32768L)
{
    msg <- .Call(.client_recv, socket$socket, as.integer(buffer_block_size))
    unserialize(msg)
}

._socketeer_fd <- function(socket)
    .Call(.socketeer_fd, socket)

hostname <- function(socket)
    socket$hostname

port <- function(socket)
    socket$port

#' @rdname socketeer
#'
#' @export
close.client <- function(socket)
{
    if (is_open(socket))
        socket <- .Call(.client_close, socket$socket)
    invisible(socket)
}

#' @rdname socketeer
#'
#' @export
close.clientof <- function(socket, server)
{
    if (is_open(socket))
        socket <- .Call(.server_close_client, server$socket, socket$socket)
    invisible(socket)
}

#' @rdname socketeer
#'
#' @export
close.server <- function(socket)
{
    if (is_open(socket))
        socket <- .Call(.server_close, socket$socket)
    invisible(socket)
}

#' @rdname socketeer
#' @export
print.socketeer <- function(x, ...)
{
    cat(class(x)[1], " ", x$hostname, ":", x$port, " is_open: ", is_open(x),
        "\n", sep="")
}

#' @rdname socketeer
#' @export
print.local <- function(x, ...)
{
    cat(
        class(x)[1], " is_open: ", is_open(x), "\n",
        sep = ""
    )
}
