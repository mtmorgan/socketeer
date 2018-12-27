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
print.recv_from <-
    function(x)
{
    cat("recv from client ", x$i, " (fd ", x$fd, "):\n", sep="")
    print(x$value)
}
