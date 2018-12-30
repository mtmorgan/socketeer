devtools::load_all()

echo_client <- function(path)
{
    parallel::mcparallel({
        con <- open(local_client(path))
        repeat {
            msg <- recv(con)
            if (identical(msg, "DONE"))
                break
            send(con, msg)
        }
        close(con)
    }, detached = TRUE)
}

n <- 10; k = 20; value <- raw(1e7)
srv <- local_cluster(n, timeout = 3L, client = echo_client, client_id = "echo")
open(srv)

res <- integer(n * k)
system.time({
for (node in seq_len(length(srv) * k)) {
    if (node <= length(srv)) {
        send_to(srv, node, value)
    } else {
        res0 <- recv_any(srv)
        stopifnot(identical(value, res0$value))
        res[[node]] <- res0$fd
        send_to(srv, res0$node, value)
    }
}
for (node in seq_along(srv)) {
    res0 <- recv_any(srv)
    stopifnot(identical(value, res0$value))
    res[[node]] <- res0$fd
}
})
table(res)

close(srv)

pid_client <- function(path)
{
    parallel::mcparallel({
        pid <- Sys.getpid()
        con <- open(local_client(path))
        repeat {
            msg <- recv(con)
            if (identical(msg, "DONE"))
                break
            send(con, pid)
        }
        close(con)
    }, detached = TRUE)
}

n <- 5; k <- 2000
srv <- local_cluster(n, timeout = 3L, client = pid_client, client_id = "pid")
open(srv)

res <- integer(n * k)
for (node in seq_len(length(srv) * k)) {
    if (node <= length(srv)) {
        send_to(srv, node, NULL)
    } else {
        res0 <- recv_any(srv)
        res[[node]] <- res0$value
        send_to(srv, res0$node, NULL)
    }
}
for (node in seq_along(srv))
     res[[node]] <- recv_any(srv)$value
length(res)
table(res)

close(srv)
