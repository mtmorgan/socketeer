recv_from_class <-
    function(value, node, fd)
{
    structure(
        list(value = value, node = node, fd = fd),
        class = "recv_from"
    )
}

recv_any_class <-
    function(value, node, fd)
{
    result <- recv_from_class(value, node, fd)
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
        "recv_from `value()` (client ", x$node, ", fd ", x$fd, "):\n",
        sep = ""
    )
    print(value(x))
}
