#' @export
local_client <-
    function(path, timeout = 30L)
{
    connection_local_client(path, timeout = timeout)
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
