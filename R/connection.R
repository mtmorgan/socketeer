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

#' @importFrom parallel detectCores
.local_cluster_cores <-
    function()
{
    if (identical(.Platform$OS.type, "windows"))
        return(1L)

    cores <- max(1L, parallel::detectCores() - 2L)
    getOption("mc.cores", cores)
}
