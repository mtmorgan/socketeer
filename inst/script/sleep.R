
devtools::load_all()

sleepy_client <- function(path)
{
    parallel::mcparallel({
        con <- local_client(path)
        open(con, "w+b")
        repeat {
            msg <- unserialize(con)
            if (identical(msg, "DONE"))
                break
            Sys.sleep(msg)
            serialize(msg, con)
        }
        close(con)
    }, detached = TRUE)
}

n <- 200
srv <-
    local_cluster(n, timeout = 3L, client = sleepy_client, client_id = "sleepy")
start(srv)

sleep <- sample(5, n, TRUE)
for (i in seq_len(size(srv)))
    send1(srv, i, sleep[i])
system.time({
    res <- replicate(size(srv), recv(srv)$value)
})
rle(res)

stop_cluster(srv)
