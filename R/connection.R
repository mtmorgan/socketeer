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

#' @export
local_client <-
    function(path)
{
    connection_local_client(path)
}

#' @importFrom parallel mcparallel
#' @export
echo_client <-
    function(path)
{
    mcparallel({
        con <- local_client(path)
        open(con, "w+b")
        repeat {
            msg <- unserialize(con)
            if (identical(msg, "DONE"))
                break
            serialize(msg, con)
        }
        close(con)
    }, detached = TRUE)
}

#' @export
local_cluster <-
    function(n = .local_cluster_cores(), timeout = 30L * 24L * 60L * 60L,
             client = echo_client, client_id = "echo")
{
    n <- as.integer(n)
    timeout <- as.integer(timeout)
    stopifnot(
        is_scalar_integer(n),
        n > 0L && n < 1000L,
        is_scalar_integer(timeout),
        timeout >= 0L
    )
    srv <- new.env(parent = emptyenv())
    srv$con <- NULL
    srv$fds <- integer()
    structure(
        list(
            srv = srv, n = n, timeout = timeout,
            client = client, client_id = client_id
        ),
        class = "local_cluster"
    )
}

.con <- function(x)
    x$srv$con

.fds <- function(x)
    x$srv$fds

#' @export
size <-
    function(x)
{
    stopifnot(is(x, "local_cluster"))
    x$n
}

#' @export
isup <-
    function(x)
{
    stopifnot(is(x, "local_cluster"))
    status <- tryCatch(summary(.con(x))$opened, error = function(...) NULL)
    identical(status, "opened")
}

#' @export
print.local_cluster <-
    function(x, ...)
{
    cat(
        "class: ", class(x)[1], "\n",
        "client_id: ", x$client_id, "\n",
        "timeout: ", x$timeout, " seconds\n",
        "size(): ", size(x), "\n",
        "isup(): ", isup(x), "\n",
        sep = ""
    )
}

#' @export
start <-
    function(x)
{
    stopifnot(!isup(x))
    n <- x$n

    path <- tempfile(fileext = ".skt")
    x$srv$con <- connection_local_server(
        path, timeout=x$timeout, backlog = min(n, 128L)
    )
    open(.con(x), "w+b")

    fds <- NULL
    while (n > 0L) {
        n0 <- min(n, 128L)              # maximum backlog 128
        n <- n - n0
        replicate(n0, x$client(path), simplify=FALSE)
        fds <- c(
            fds,
            replicate(n0, connection_server_accept(.con(x)), simplify = TRUE)
        )
    }
    x$srv$fds <- fds

    invisible(x)
}

.send1 <-
    function(con, fd, value)
{
    connection_server_set_activefd(con, fd)
    serialize(value, con)
}

#' @export
send1 <-
    function(x, i, value)
{
    i <- as.integer(i)
    stopifnot(
        isup(x),
        is_scalar_integer(i),
        i > 0L && i <= length(.fds(x))
    )

    .send1(.con(x), .fds(x)[[i]], value)
    invisible(x)
}

.recv1 <-
    function(con, fd)
{
    connection_server_set_activefd(con, fd)
    unserialize(con)
}

#' @export
recv <-
    function(x)
{
    stopifnot(isup(x))
    fd <- connection_server_selectfd(.con(x))
    if (!length(fd)) {
        stop("'recv()' timeout after ", x$timeout, " seconds")
    }
    fd <- fd[sample.int(length(fd), 1L)]

    value <- .recv1(.con(x), fd)
    structure(
        list(fd = fd, value = value),
        class = "local_server_recv"
    )
}

#' @export
stop_cluster <-
    function(x)
{
    stopifnot(isup(x))

    for (fd in .fds(x))
        .send1(.con(x), fd, "DONE")
    close(.con(x))

    invisible(x)
}
