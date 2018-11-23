
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

    fields <- list(
        con = NULL, fds = integer(), n = n, timeout = timeout,
        client = client, client_id = client_id
    )
    structure(
        list2env(fields, parent=emptyenv()),
        class = "local_cluster"
    )
}

.con <- function(x)
    x$con

.fds <- function(x)
    x$fds

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
start_cluster <-
    function(x)
{
    stopifnot(!isup(x))
    n <- x$n

    path <- tempfile(fileext = ".skt")
    x$con <- connection_local_server(
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
    x$fds <- fds

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
        list(i = match(fd, .fds(x)), fd = fd, value = value),
        class = "local_server_recv"
    )
}

#' @export
print.local_server_recv <-
    function(x)
{
    cat(
        "recv from client ", x$i, " (fd ", x$fd, "):\n",
        sep=""
    )
    print(x$value)
}

#' @export
stop_cluster <-
    function(x)
{
    if (isup(x)) {
        for (fd in .fds(x))
            .send1(.con(x), fd, "DONE")
        close(.con(x))
    }

    invisible(x)
}
