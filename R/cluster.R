#' @export
cluster <-
    function(...)
{
    srv <- local_cluster(
        ..., client = semaphore_client, client_id = "semaphore"
    )
    class(srv) <- c("cluster", class(srv))
    srv
}

#' @export
send_to.cluster <-
    function(x, i, value)
{
    value <- eval_semaphore(value)
    NextMethod()
}

recv_unpack <-
    function(x)
{
    x$value <- value(value(x))
    x
}

#' @export
recv_from.cluster <-
    function(x, i)
{
    result <- NextMethod()
    recv_unpack(result)
}

#' @export
recv_any.cluster <-
    function(x)
{
    result <- NextMethod()
    recv_unpack(result)
}

finalize.cluster <-
    function(x)
{
    for (i in seq_along(x))
        send_to(x, i, DONE_SEMAPHORE)
    invisible(NULL)
}
