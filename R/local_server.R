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

local_server <-
    function(path, timeout = 30L, backlog = 5L)
{
    connection_local_server(path, "w+b", timeout, backlog)
}

local_server_accept <-
    function(srv)
{
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_accept, srv)
}

local_server_activefds <-
    function(srv)
{
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_activefds, srv);
}

local_server_selectfd <-
    function(srv, mode = c("r", "w"))
{
    mode <- match.arg(mode)
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_selectfd, srv, mode)
}

local_server_set_activefd <-
    function(srv, fd)
{
    stopifnot(
        is(srv, "local_server"),
        is_scalar_integer(fd)
    )

    srv <- .Call(.connection_server_set_activefd, srv, fd)
    invisible(srv)
}

isup.local_server <-
    function(x)
{
    status <- tryCatch(summary(x)$opened, error = function(...) NULL)
    identical(status, "opened")
}

send_to.local_server <-
    function(x, i, value)
{
    fd <- local_server_activefds(x)[i]
    local_server_set_activefd(x, fd)
    ## .Internal(serializeb(value, x, FALSE, NULL, NULL)) # 40% faster
    serialize(value, x, xdr = FALSE)
}

.recv_from_fd <-
    function(x, fd)
{
    local_server_set_activefd(x, fd)
    unserialize(x)
}

recv_from.local_server <-
    function(x, i)
{
    fd <- local_server_activefds(x)[i]
    value <- .recv_from_fd(x, fd)
    recv_from_class(value, i, fd)
}

recv_any.local_server <-
    function(x)
{
    fd <- local_server_selectfd(x)
    length(fd) || stop("'recv_any()' timeout")

    fd <- fd[sample.int(length(fd), 1L)]

    value <- .recv_from_fd(x, fd)
    i <- match(fd, local_server_activefds(x))
    recv_any_class(value, i, fd)
}
