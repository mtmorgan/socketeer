recv_from_class <-
    function(value, i, fd)
{
    structure(
        list(value = value, i = i, fd = fd),
        class = "recv_from"
    )
}

recv_any_class <-
    function(value, i, fd)
{
    result <- recv_from_class(value, i, fd)
    class(result) <- c("recv_any", class(result))
    result
}

#' @export
value.recv_from <- function(x)
    x$value

#' @export
print.recv_from <-
    function(x)
{
    cat(
        "recv_from `value()` (client ", x$i, ", fd ", x$fd, "):\n",
        sep=""
    )
    print(value(x))
}
