.client_loop <-
    function(con)
{
    repeat {
        tryCatch({
            msg <- recv(con)
            if (msg$type == "DONE") {
                break
            } else if (msg$type == "EXEC") {
                value <- tryCatch({
                    do.call(msg$data$fun, msg$data$args)
                }, error = function(e) {
                    paste("error:", conditionMessage(e))
                })
                .client_reply(value = value)
            }
        }, interrupt = function(e) {
            NULL
        })
    }
}

.client_reply <-
    function(x, value, ...)
{
    value <- list(
        type = "VALUE",
        data = list(value = value, ...)
    )
    serialize(value, x)
}

#' @export
send1_call <-
    function(x, i, fun, args, return = TRUE, tag = NULL)
{
    value <- list(
        type = "EXEC",
        data = list(fun = fun, args = args, tag = tag)
    )
    send1(x, i, value)
}

send1_done <-
    function(x, i)
{
    value <- list(type = "DONE")
    send1(x, i, value)
}

#' @importFrom parallel nextRNGStream
#' @export
rng_setup_cluster <-
    function(x, seed = NULL)
{
    oldseed <- if (exists(".Random.seed", envir = .GlobalEnv, 
        inherits = FALSE)) 
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else NULL
    RNGkind("L'Ecuyer-CMRG")
    if (!is.null(seed)) 
        set.seed(seed)

    seeds <- vector("list", size(x))
    seeds[[1L]] <- .Random.seed
    for (i in seq_len(size(x) - 1L))
        seeds[[i + 1L]] <- nextRNGStream(seeds[[i]])
    if (!is.null(oldseed)) 
        assign(".Random.seed", oldseed, envir = .GlobalEnv)
    else rm(.Random.seed, envir = .GlobalEnv)

    for (i in seq_len(size(cl))) {
        expr <- substitute(
            assign(".Random.seed", seed, envir = .GlobalEnv), 
            list(seed = seeds[[i]])
        )
        send1_call(x, i, eval, list(expr))
    }
    response <- replicate(size(x), recv(), simplify=FALSE)
    .recv_stop_on_errors(response, 'rng_setup_cluster')

    invisible(x)
}
