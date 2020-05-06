## constructor

setOldClass("local_cluster")

setOldClass("local_client")

#' @importClassesFrom BiocParallel BiocParallelParam
#'
#' @importFrom BiocParallel .BiocParallelParam_prototype .recv .send
#'     .close .send_to .recv_any .bpworker_impl .prototype_update
#'     .error_worker_comm .bpstart_impl .bpstop_impl bpschedule
#'     bpbackend bpisup bpstart bpstop
#'
#' @importFrom methods new is

.LocalParam <- setRefClass(
    "LocalParam",
    fields = list(
        backend = "local_cluster"
    ),
    contains = "BiocParallelParam"
)

.LocalParam_prototype <- c(
    list(
        backend = NULL
    ),
    .BiocParallelParam_prototype
)
    
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
        con <- open(local_client(path))
        tryCatch(.bpworker_impl(con), error = warning)
    }, detached = TRUE)
}

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

setMethod(
    ".send", "local_client",
    function(worker, value)
{
    serialize(value, worker, xdr = FALSE)
})

setMethod(
    ".close", "local_client",
    function(worker)
{
    close(worker)
})

setMethod(
    ".send_to", "local_cluster",
    function(backend, node, value)
{
    send_to(.con(backend), node, value)
    TRUE
})

setMethod(
    ".recv_any", "local_cluster",
    function(backend)
{
    tryCatch({
        recv_any(.con(backend))
    }, error = function(e) {
        stop(.error_worker_comm(e, "'.recv_any,local_cluster-method' failed"))
    })
})

setMethod(
    "bpschedule", "LocalParam",
    function(x)
{
    !identical(.Platform$OS.type, "windows")
})

setMethod(
    "bpbackend", "LocalParam",
    function(x)
{
    x$backend
})

setMethod(
    "bpisup", "LocalParam",
    function(x)
{
    isup(bpbackend(x))
})

setMethod(
    "bpstart", "LocalParam",
    function(x, ...)
{
    open(bpbackend(x))
    .bpstart_impl(x)
})

setMethod(
    "bpstop", "LocalParam",
    function(x)
{
    if (!bpisup(x))
        return(invisible(x))

    .bpstop_impl(x)
    close(bpbackend(x))
    invisible(x)
})
