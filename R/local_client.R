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
local_client <-
    function(path, timeout = 30L)
{
    connection_local_client(path, timeout = timeout)
}

#' @export
open.local_client <-
    function(con)
{
    NextMethod(mode = "w+b")
    invisible(con)
}

#' @export
recv.local_client <-
    function(x)
{
    unserialize(x)
}

#' @export
send.local_client <-
    function(x, value)
{
    ## .Internal(serializeb(value, x, FALSE, NULL, NULL)) # 40% faster
    serialize(value, x, xdr = FALSE)
    invisible(x)
}

#' @export
close.local_client <-
    function(con, ...)
{
    NextMethod()
}
