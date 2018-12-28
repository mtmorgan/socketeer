semaphore <-
    function(x, subclass)
{
    if (!inherits(x, "semaphore"))
        x <- structure(list(value = x), class = c(subclass, "semaphore"))
    x
}

DONE_SEMAPHORE <- semaphore(NULL, "done_semaphore")

value_semaphore <- function(x) semaphore(x, "value_semaphore")

eval_semaphore <- function(x) semaphore(x, "eval_semaphore")

evalq_semaphore <- function(x)
    semaphore(substitute(x), c("evalq_semaphore", "eval_semaphore"))

call_semaphore <- function(what, args = list())
    semaphore(list(what = what, args = args), "call_semaphore")

value.semaphore <-
    function(x)
{
    x$value
}

do <- function(x)
    UseMethod("do")

do.default <-
    function(x)
{
    msg <- paste(
        "unknown semaphore class:",
        paste0("'", class(x), "'", collapse = ", ")
    )
    simpleError(msg)
}

do.value_semaphore <-
    function(x)
{
    value(x)
}

do.eval_semaphore <-
    function(x)
{
    value <- value(x)
    eval(value, envir = .GlobalEnv)
}

do.call_semaphore <-
    function(x)
{
    value <- value(x)
    do.call(value$what, value$args, envir = .GlobalEnv)
}

semaphore_loop <-
    function(con)
{
    repeat {
        msg <- recv(con)
        if (identical(msg, DONE_SEMAPHORE))
            break
        value <- tryCatch(do(msg), error = identity)
        send(con, value_semaphore(value))
    }
}

semaphore_client <-
    function(path)
{
    mcparallel({
        con <- open(local_client(path))
        tryCatch(semaphore_loop(con), error = warning)
        close(con)
    }, detached = TRUE)
}
