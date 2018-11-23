
devtools::load_all()

echo_client <- function(path)
{
    parallel::mcparallel({
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

n <- 10; k = 20; value <- raw(1e7)
srv <- local_cluster(n, timeout = 3L, client = echo_client)
start(srv)

res <- integer(n * k)
system.time({
for (i in seq_len(size(srv) * k)) {
    if (i <= size(srv)) {
        send1(srv, i, value)
    } else {
        res0 <- recv(srv)
        stopifnot(identical(value, res0$value))
        res[[i]] <- res0$fd
        send1(srv, match(res0$fd, .fds(srv)), value)
    }
}
for (i in seq_len(size(srv))) {
    res0 <- recv(srv)
    stopifnot(identical(value, res0$value))
    res[[i]] <- res0$fd
}
})
table(res)

stop_cluster(srv)

pid_client <- function(path)
{
    parallel::mcparallel({
        pid <- Sys.getpid()
        con <- local_client(path)
        open(con, "w+b")
        repeat {
            msg <- unserialize(con)
            if (identical(msg, "DONE"))
                break
            serialize(pid, con)
        }
        close(con)
    }, detached = TRUE)
}

n <- 5; k <- 2000
srv <- local_cluster(n, timeout = 3L, client = pid_client, client_id = "pid")
start(srv)

res <- integer(n * k)
for (i in seq_len(size(srv) * k)) {
    if (i <= size(srv)) {
        send1(srv, i, i)
    } else {
        res0 <- recv(srv)
        res[[i]] <- res0$value
        send1(srv, match(res0$fd, .fds(srv)), i)
    }
}
for (i in seq_len(size(srv)))
     res[[i]] <- recv(srv)$value
length(res)
table(res)

stop_cluster(srv)
