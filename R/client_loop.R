spawn <-
    function(path, loop)
{
    parallel::mcparallel({
        con <- open(local_client(path))
        tryCatch(loop(con), error=warning)
        close(con)
    }, detached = TRUE)
}

echo_loop <-
    function(con)
{
    repeat {
        msg <- recv(con)
        if (identical(msg, "DONE"))
            break
        send(con, msg)
    }
}

#' @export
client_echo <-
    function(path)
{
    spawn(path, echo_loop)
}

sleep_loop <-
    function(con)
{
    repeat {
        msg <- recv(con)
        if (identical(msg, "DONE"))
            break
        Sys.sleep(msg)
        send(con, msg)
    }
}

#' @export
client_sleep <-
    function(path)
{
    spawn(path, sleep_loop)
}

##
## eval_loop
##

eval_loop <-
    function(con)
{
    repeat {
        msg <- recv(con)
        if (identical(msg, "DONE")) {
            break
        } else {
            value <- tryCatch({
                eval(msg)
            }, error = function(e) {
                paste("error:", conditionMessage(e))
            })
            send(con, value)
        }
    }
}

#' @export
client_eval <-
    function(path)
{
    spawn(path, eval_loop)
}
