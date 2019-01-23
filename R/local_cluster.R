#' @importFrom parallel detectCores
.local_cluster_cores <-
    function()
{
    if (identical(.Platform$OS.type, "windows"))
        return(1L)

    cores <- max(1L, detectCores() - 2L)
    getOption("mc.cores", cores)
}

#' @export
local_cluster <-
    function(n = .local_cluster_cores(), timeout = 30L * 24L * 60L * 60L,
             client, client_id)
{
    n <- as.integer(n)
    timeout <- as.integer(timeout)
    stopifnot(
        is_scalar_integer(n),
        n > 0L && n < 1000L,
        is_scalar_integer(timeout),
        timeout >= 0L,
        is_scalar_character(client_id)
    )

    fields <- list(
        con = NULL, n = n, timeout = timeout,
        client = client, client_id = client_id
    )
    env <- new.env(parent = emptyenv())
    ## reg.finalizer(env, close.local_cluster, TRUE)
    structure(
        list2env(fields, env),
        class = "local_cluster"
    )
}

.con <- function(x)
    x$con

`.con<-` <-
    function(x, value)
{
    x$con <- value
}

#' @export
length.local_cluster <-
    function(x)
{
    stopifnot(is(x, "local_cluster"))
    x$n
}

isup.NULL <- function(x)
    FALSE

#' @export
isup.local_cluster <-
    function(x)
{
    isup(.con(x))
}

#' @export
print.local_cluster <-
    function(x, ...)
{
    cat(
        "class: ", class(x)[1], "\n",
        "client_id: ", x$client_id, "\n",
        "timeout: ", x$timeout, " seconds\n",
        "length(): ", length(x), "\n",
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
    timeout <- con$timeout

    con$con <- local_server(path, timeout = timeout, backlog = min(n, 128L))
    open(.con(con), "w+b")

    while (n > 0L) {
        n0 <- min(n, 128L)              # maximum backlog 128
        n <- n - n0
        replicate(n0, con$client(path), simplify = FALSE)
        replicate(n0, local_server_accept(.con(con)), simplify = FALSE)
    }

    invisible(con)
}

#' @export
send_to.local_cluster <-
    function(x, node, value)
{
    node <- as.integer(node)
    stopifnot(isup(x), is_scalar_integer(node), node > 0L && node <= length(x))

    send_to(.con(x), node, value)
    invisible(x)
}

#' @export
recv_from.local_cluster <-
    function(x, node)
{
    node <- as.integer(node)
    stopifnot(isup(x), is_scalar_integer(node), node > 0L && length(x))

    recv_from(.con(x), node)
}

#' @export
recv_any.local_cluster <-
    function(x)
{
    stopifnot(isup(x))
    recv_any(.con(x))
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

finalize.local_cluster <-
    function(x)
{
    close(.con(x))
    .con(x) <- NULL
}

#' @export
close.local_cluster <-
    function(x)
{
    if (isup(x))
        finalize(x)
    invisible(x)
}
