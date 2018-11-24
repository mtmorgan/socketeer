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
        con = NULL, n = n, timeout = timeout,
        fds = integer(), client = client, client_id = client_id
    )
    env <- new.env(parent=emptyenv())
    reg.finalizer(env, .finalize_local_cluster, TRUE)
    structure(
        list2env(fields, env),
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
isup.local_cluster <-
    function(x)
{
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
open.local_cluster <-
    function(con)
{
    stopifnot(!isup(con))

    path <- tempfile(fileext = ".skt")
    n <- con$n
    con$con <- local_server(path, timeout=con$timeout, backlog = min(n, 128L))
    open(.con(con), "w+b")

    fds <- NULL
    while (n > 0L) {
        n0 <- min(n, 128L)              # maximum backlog 128
        n <- n - n0
        replicate(n0, con$client(path), simplify=FALSE)
        fds <- c(
            fds,
            replicate(n0, local_server_accept(.con(con)), simplify = TRUE)
        )
    }
    con$fds <- fds

    invisible(con)
}

.send_local_cluster <-
    function(con, fd, value)
{
    local_server_set_activefd(con, fd)
    serialize(value, con)
}

#' @export
send.local_cluster <-
    function(x, i, value)
{
    i <- as.integer(i)
    stopifnot(
        isup(x),
        is_scalar_integer(i), i > 0L && i <= length(.fds(x))
    )

    .send_local_cluster(.con(x), .fds(x)[[i]], value)
    invisible(x)
}

.recv1_local_cluster  <-
    function(con, fd)
{
    local_server_set_activefd(con, fd)
    unserialize(con)
}

#' @export
recv.local_cluster <-
    function(x)
{
    stopifnot(isup(x))
    fd <- local_server_selectfd(.con(x))
    if (!length(fd)) {
        stop("'recv()' timeout after ", x$timeout, " seconds")
    }
    fd <- fd[sample.int(length(fd), 1L)]

    value <- .recv1_local_cluster(.con(x), fd)
    structure(
        list(i = match(fd, .fds(x)), fd = fd, value = value),
        class = "local_cluster_recv"
    )
}

.recv_is_error <- function(x)
    is(x$value, "try-error")

.recv_stop_on_errors <-
    function(x, id)
{
    errors <- vapply(x, .recv_is_error, logical(1))
    if (any(errors))
        stop(
            "'", id, "' had ", sum(errors), " client error(s); first error:\n",
            x[which.max(errors)]$value
        )
}

#' @export
print.local_cluster_recv <-
    function(x)
{
    cat(
        "recv from client ", x$i, " (fd ", x$fd, "):\n",
        sep=""
    )
    print(x$value)
}

.finalize_local_cluster <-
    function(x)
{
    if (isup(x)) {
        for (fd in .fds(x))
            .send_local_cluster(.con(x), fd, "DONE")
        close(.con(x))
    }
    invisible(NULL)
}

#' @export
close.local_cluster <-
    function(x)
{
    .finalize_local_cluster(x)
    invisible(x)
}
