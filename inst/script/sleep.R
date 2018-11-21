
devtools::load_all()

sleepy_client <- function(path)
{
    parallel::mcparallel({
        cl <- connection_local_client(path)
        open(cl, "w+b")
        repeat {
            msg <- unserialize(cl)
            if (identical(msg, "DONE"))
                break
            Sys.sleep(msg)
            serialize(msg, cl)
        }
        close.connection(cl)
    }, detached = TRUE)
}

n <- 200
srv <- local_cluster(n, timeout = 3L, client = sleepy_client)
srv <- start(srv)

sleep <- sample(5, n, TRUE)
for (i in seq_len(size(srv)))
    send1(srv, i, sleep[i])
system.time({
    res <- replicate(size(srv), recv(srv))
})
rle(res)

stop_cluster(srv)
