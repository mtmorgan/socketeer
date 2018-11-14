#' @export
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

#' @export
connection_local_server <-
    function(path, mode = "w+b", timeout = 30L, backlog = 5L)
{
    timeout <- as.integer(timeout)
    backlog <- as.integer(backlog)
    stopifnot(
        is_scalar_character(path),
        is_scalar_character(mode),
        is_scalar_integer(timeout),
        is_scalar_integer(backlog)
    )
    .Call(.connection_local_server, path, mode, timeout, backlog)
}

connection_server_accept <-
    function(srv)
{
    stopifnot(is(srv, "local_server"))

    .Call(.connection_server_accept, srv)
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
