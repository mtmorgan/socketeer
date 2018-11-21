#' @useDynLib socketeer, .registration=TRUE

connection_local_client <-
    function(path, mode = "w+b", timeout = 30L)
{
    timeout <- as.integer(timeout)
    stopifnot(
        is_scalar_character(path),
        is_scalar_character(mode),
        is_scalar_integer(timeout)
    )
    .Call(.connection_local_client, path, mode, timeout);
}

connection_local_server <-
    function(path, mode = "w+b", timeout = 30L, backlog = 5L)
{
    timeout <- as.integer(timeout)
    backlog <- as.integer(backlog)
    stopifnot(
        is_scalar_character(path),
        is_scalar_character(mode),
        is_scalar_integer(timeout),
        is_scalar_integer(backlog),
        backlog > 0L && backlog < 129L
    )
    .Call(.connection_local_server, path, mode, timeout, backlog)
}

connection_server_accept <-
    function(srv)
{
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_accept, srv)
}

connection_server_selectfd <-
    function(srv, mode = c("r", "w"))
{
    mode <- match.arg(mode)
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_selectfd, srv, mode)
}

connection_server_set_activefd <-
    function(srv, fd)
{
    stopifnot(
        is(srv, "local_server"),
        is_scalar_integer(fd)
    )
    srv <- .Call(.connection_server_set_activefd, srv, fd)
    invisible(srv)
}

#' @export 
local_cluster <-
    function(n, timeout, client)
{
    n <- as.integer(n)
    timeout <- as.integer(timeout)
    stopifnot(
        is_scalar_integer(n),
        n > 0L && n < 1000L,
        is_scalar_integer(timeout),
        timeout >= 0L
    )
    structure(
        list(
            con = NULL, n = n, timeout = timeout,
            fds = integer(), client = client
        ),
        class = "local_cluster"
    )
}

#' @export
size <-
    function(srv)
{
    stopifnot(is(srv, "local_cluster"))
    srv$n
}

#' @export
start <-
    function(srv)
{
    stopifnot(is(srv, "local_cluster"))
    n <- srv$n

    path <- tempfile(fileext = ".skt")
    srv$con <- connection_local_server(
        path, timeout=srv$timeout, backlog = min(n, 128L)
    )
    open(srv$con, "w+b")

    fds <- NULL
    while (n > 0L) {
        n0 <- min(n, 128L)              # maximum backlog 128
        n <- n - n0
        replicate(n0, srv$client(path), simplify=FALSE)
        fds <- c(
            fds,
            replicate(n0, connection_server_accept(srv$con), simplify = TRUE)
        )
    }
    srv$fds <- fds

    srv
}        

.send1 <-
    function(con, fd, value)
{
    connection_server_set_activefd(con, fd)
    serialize(value, con)
}

#' @export
send1 <-
    function(srv, i, value)
{
    i <- as.integer(i)
    stopifnot(
        is(srv, "local_cluster"),
        is_scalar_integer(i),
        i > 0L && i <= length(srv$fds)
    )

    .send1(srv$con, srv$fds[[i]], value)
    invisible(srv)
}

.recv1 <-
    function(con, fd)
{
    connection_server_set_activefd(con, fd)
    unserialize(con)
}

#' @export
recv <-
    function(srv)
{
    fd <- connection_server_selectfd(srv$con)[[1]]
    if (!length(fd))
        stop("'recv()' timeout after ", srv$timeout, " seconds")

    .recv1(srv$con, fd)
}

#' @export
stop_cluster <-
    function(srv)
{
    stopifnot(is(srv, "local_cluster"))

    for (fd in srv$fds)
        .send1(srv$con, fd, "DONE")
    close(srv$con)

    invisible(srv)
}
