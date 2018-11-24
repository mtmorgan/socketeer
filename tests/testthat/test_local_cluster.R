context("local_cluster")

test_that("open() / close() / isup() work", {
    srv <- local_cluster()
    expect_identical(FALSE, isup(srv))
    expect_identical(TRUE, isup(open(srv)))
    expect_identical(FALSE, isup(close(srv)))
    ## reopen
    expect_identical(TRUE, isup(open(srv)))
    expect_identical(FALSE, isup(close(srv)))
    ## finalizer
    ## expect_true({
    ##     open(local_cluster()); open(local_cluster()); gc(); gc()
    ##     TRUE
    ## })
})

test_that("many clients work", {
    sleepy_client <- function(path) {
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
    ## 
    n <- 400L
    srv <- local_cluster(n, client = sleepy_client, client_id = "sleepy")
    open(srv)
    sleep <- sample(1:4 / 2, n, TRUE)
    for (i in seq_len(size(srv)))
        send(srv, i, sleep[i])
    res <- replicate(size(srv), recv(srv)$value)
    close(srv)
    ## 
    expect_identical(1:4 / 2, rle(res)$values)
    expect_identical(sort(sleep), sort(res))
})

test_that("large-data transfer works", {
    echo_client <- function(path) {
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
    ## 
    n <- 5L; k = 10L; value <- raw(1e7); res <- integer(n * k)
    srv <- local_cluster(n, timeout = 3L, client = echo_client)
    open(srv)
    for (i in seq_len(size(srv) * k)) {
        if (i <= size(srv)) {
            send(srv, i, value)
        } else {
            res0 <- recv(srv)
            stopifnot(identical(value, res0$value))
            res[[i]] <- res0$i
            send(srv, res0$i, value)
        }
    }
    for (i in seq_len(size(srv))) {
        res0 <- recv(srv)
        stopifnot(identical(value, res0$value))
        res[[i]] <- res0$i
    }
    close(srv)
    ## 
    expect_identical(n, length(unique(res)))
    expect_identical(n * k, sum(tabulate(res)))
})

test_that("multiple clusters work", {
    n <- 5L
    cl1 <- open(local_cluster(n))
    cl2 <- open(local_cluster(n))
    expect_identical(integer(0), intersect(.fds(cl1), .fds(cl2)))
    for (i in seq_len(n)) {
        send(cl1, i, i)
        send(cl2, i, i + n)
    }
    res1 <- replicate(n, recv(cl1)$value)
    res2 <- replicate(n, recv(cl2)$value)
    close(cl1); close(cl2)
    ## 
    expect_identical(seq_len(n), sort(res1))
    expect_identical(seq_len(n) + n, sort(res2))
})
