devtools::load_all()

sleepy_client <- function(path)
{
    parallel::mcparallel({
        con <- open(local_client(path))
        repeat {
            msg <- recv(con)
            if (identical(msg, "DONE"))
                break
            Sys.sleep(msg)
            send(con, msg)
        }
        close(con)
    }, detached = TRUE)
}

n <- 200
srv <- local_cluster(n, client = sleepy_client, client_id = "sleepy")
open(srv)

sleep <- sample(5, n, TRUE)
for (node in seq_along(srv))
    send_to(srv, node, sleep[node])
system.time({
    res <- replicate(length(srv), recv_any(srv)$value)
})
rle(res)

close(srv)
