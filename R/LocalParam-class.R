#' @useDynLib socketeer, .registration=TRUE
#'
#' @import BiocParallel

##
## utilities
##

is_scalar_character <-
    function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

is_scalar_integer <-
    function(x)
{
    is.integer(x) && length(x) == 1L && !is.na(x)
}

##
## local_client
##

setOldClass("local_client")

local_client <-
    function(path, timeout = 30L)
{
    mode <- "w+b"
    timeout <- as.integer(timeout)
    stopifnot(
        is_scalar_character(path),
        is_scalar_integer(timeout)
    )
    .Call(.connection_local_client, path, mode, timeout);
}

#' @export
setMethod(
    ".recv", "local_client",
    function(worker)
{
    res <- tryCatch({
        unserialize(worker)
    }, error = function(e) {
        .error_worker_comm(e, "'.recv,local_client-method' failed")
    })
})

#' @export
setMethod(
    ".send", "local_client",
    function(worker, value)
{
    serialize(value, worker, xdr = FALSE)
})

#' @export
setMethod(
    ".close", "local_client",
    function(worker)
{
    close(worker)                       # close.connection
})

##
## local_server
##

setOldClass("local_server")

local_server <-
    function(path, timeout = 30L, backlog = 5L)
{
    mode <- "w+b"
    timeout <- as.integer(timeout)
    backlog <- as.integer(backlog)
    stopifnot(
        is_scalar_character(path),
        is_scalar_integer(timeout),
        is_scalar_integer(backlog),
        backlog > 0L && backlog < 129L
    )
    .Call(.connection_local_server, path, mode, timeout, backlog)
}

local_server_accept <-
    function(srv)
{
    .Call(.connection_server_accept, srv)
}

local_server_selectfd <-
    function(srv, mode = c("r", "w"))
{
    mode <- match.arg(mode)
    .Call(.connection_server_selectfd, srv, mode)
}

local_server_activefds <-
    function(srv)
{
    .Call(.connection_server_activefds, srv);
}

local_server_set_activefd <-
    function(srv, fd)
{
    .Call(.connection_server_set_activefd, srv, fd)
}

send_to_local_server <-
    function(x, node, value)
{
    fd <- local_server_activefds(x)[node]
    local_server_set_activefd(x, fd)
    serialize(value, x, xdr = FALSE)
}


recv_any_local_server <-
    function(x)
{
    fd <- local_server_selectfd(x)
    length(fd) || stop("'recv_any()' timeout")

    fd <- fd[sample.int(length(fd), 1L)]

    local_server_set_activefd(x, fd)
    value <- unserialize(x)

    node <- match(fd, local_server_activefds(x))
    structure(list(value = value, node = node), class = "recv_any")
}

isup_local_server <-
    function(x)
{
    status <- tryCatch(summary(x)$opened, error = function(...) NULL)
    identical(status, "opened")
}

##
## local_cluster
##

setOldClass("local_cluster")

#' @importFrom parallel detectCores
.local_cluster_cores <-
    function()
{
    if (identical(.Platform$OS.type, "windows"))
        return(1L)

    cores <- max(1L, detectCores() - 2L)
    getOption("mc.cores", cores)
}

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

open_local_cluster <-
    function(x)
{
    stopifnot(!isup_local_server(.con(x)))

    path <- tempfile(fileext = ".skt")
    n <- x$n
    timeout <- x$timeout

    x$con <- local_server(path, timeout = timeout, backlog = min(n, 128L))
    open(.con(x), "w+b")                # open.connection

    while (n > 0L) {
        n0 <- min(n, 128L)              # maximum backlog 128
        n <- n - n0
        replicate(n0, x$client(path), simplify = FALSE)
        replicate(n0, local_server_accept(.con(x)), simplify = FALSE)
    }

    invisible(x)
}

close_local_cluster <-
    function(x)
{
    if (isup_local_server(.con(x))) {
        close(.con(x))                  # close.connection
        .con(x) <- NULL
    }
    invisible(x)
}

#' @export
setMethod(
    ".send_to", "local_cluster",
    function(backend, node, value)
{
    send_to_local_server(.con(backend), node, value)
    TRUE
})

#' @export
setMethod(
    ".recv_any", "local_cluster",
    function(backend)
{
    tryCatch({
        recv_any_local_server(.con(backend))
    }, error = function(e) {
        stop(.error_worker_comm(e, "'.recv_any,local_cluster-method' failed"))
    })
})

#' @export
length.local_cluster <-
    function(x)
{
    stopifnot(is(x, "local_cluster"))
    x$n
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
        "bpisup(): ", isup_local_server(.con(x)), "\n",
        sep = ""
    )
}

##
## LocalParam
##

.LocalParam <- setRefClass(
    "LocalParam",
    fields = list(backend = "local_cluster"),
    contains = "BiocParallelParam"
)

.LocalParam_prototype <- c(
    list(backend = NULL),
    .BiocParallelParam_prototype
)
    
#' @importFrom parallel mcparallel
#'
#' @export
LocalParam <-
    function(workers = .local_cluster_cores(), ...)
{
    backend <- local_cluster(
        n = workers,
        client = .LocalParam_client,
        client_id = "BiocParallel"
    )

    prototype <- .prototype_update(
        .LocalParam_prototype,
        backend = backend,
        workers = workers,
        ...
    )
    do.call(.LocalParam, prototype)
}


.LocalParam_client <-
    function(path)
{
    mcparallel({
        open(con <- local_client(path)) # open.connection
        tryCatch(.bpworker_impl(con), error = warning)
    }, detached = TRUE)
}

#' @export
setMethod(
    "bpschedule", "LocalParam",
    function(x)
{
    !identical(.Platform$OS.type, "windows")
})

#' @export
setMethod(
    "bpbackend", "LocalParam",
    function(x)
{
    x$backend
})

#' @export
setMethod(
    "bpisup", "LocalParam",
    function(x)
{
    isup_local_server(.con(bpbackend(x)))
})

#' @export
setMethod(
    "bpstart", "LocalParam",
    function(x, ...)
{
    open_local_cluster(bpbackend(x))
    .bpstart_impl(x)
})

#' @export
setMethod(
    "bpstop", "LocalParam",
    function(x)
{
    if (!bpisup(x))
        return(invisible(x))

    .bpstop_impl(x)
    close_local_cluster(bpbackend(x))
    invisible(x)
})
