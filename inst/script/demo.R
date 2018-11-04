library(parallel)
devtools::load_all()

remote_client <- function(port)
{
    mcparallel({
        cl <- client(port = port)
        repeat {
            msg <- unserialize(recv(cl))
            message(Sys.getpid(), ": ", msg)
            if (identical(msg, "DONE"))
                break
        }
        close(cl)
    })
}

port <- sample(1000, 1) + 1000
if (exists("srv") && is(srv, "server"))
    close(srv)
srv <- server(port = port)
listen(srv)

pid <- remote_client(port)
selectfd(srv)
clientof_srv <- accept(srv)

send(clientof_srv, serialize("foo", NULL))
send(clientof_srv, serialize("bars", NULL))
send(clientof_srv, serialize(raw(), NULL))
send(clientof_srv, serialize("DONE", NULL))

mccollect(pid)

##

echo_client <- function(port)
{
    mcparallel({
        cl <- client(port = port)
        repeat {
            msg <- unserialize(recv(cl))
            if (identical(msg, "DONE"))
                break
            send(cl, serialize(msg, NULL))
        }
        close(cl)
    })
}

port <- sample(1000, 1) + 1000
srv <- server(port = port)
listen(srv)

pid <- echo_client(port)
selectfd(srv)
clientof_srv <- accept(srv)

send(clientof_srv, serialize("foo", NULL))
unserialize(recv(clientof_srv))

send(clientof_srv, serialize("bars", NULL))
unserialize(recv(clientof_srv))

x <- seq_len(1e8)                       # 'large' data
send(clientof_srv, serialize(x, NULL))
length(value <- unserialize(recv(clientof_srv)))

send(clientof_srv, serialize("DONE", NULL))
mccollect(pid)

close(srv)

## 

port <- sample(1000, 1) + 1000
srv <- server(port = port)
listen(srv)

n <- 10
pids <- replicate(n, simplify = FALSE, echo_client(port))
clientsof_srv <- replicate(n, simplify = FALSE, {
    selectfd(srv)
    accept(srv)
})

for (i in sample(n))
    send(clientsof_srv[[i]], serialize(i, NULL))
sort(sapply(sample(n), function(i) unserialize(recv(clientsof_srv[[i]]))))

for (client in clientsof_srv)
    send(client, serialize("DONE", NULL))

names(mccollect(pids, timeout=5))

##

sleepy_client <- function(port)
{
    mcparallel({
        cl <- client(port = port)
        repeat {
            msg <- unserialize(recv(cl))
            if (identical(msg, "DONE"))
                break
            Sys.sleep(msg)
            send(cl, serialize(msg, NULL))
        }
        close(cl)
    })
}

port <- sample(1000, 1) + 1000
srv <- server(port = port)
listen(srv)

n <- 50
pids <- replicate(n, simplify = FALSE, sleepy_client(port))
clientsof_srv <- replicate(n, simplify = FALSE, {
    selectfd(srv)
    accept(srv)
})
names(clientsof_srv) <- vapply(clientsof_srv, socketeer_fd, integer(1))

for (client in clientsof_srv)
    send(client, serialize(sample(5L, 1), NULL))
vapply(seq_along(clientsof_srv), function(...) {
    fd <- as.character(selectfd(srv)[[2]][[1]])
    unserialize(recv(clientsof_srv[[fd]]))
}, integer(1))

for (client in clientsof_srv)
    send(client, serialize("DONE", NULL))
length(mccollect(pids, timeout=5))

close(srv)

## client/server_local

echo_client_local <- function(path)
{
    mcparallel({
        cl <- client_local(path)
        repeat {
            msg <- recv(cl)
            if (identical(msg, "DONE"))
                break
            send(cl, msg)
        }
        close(cl)
    })
}

path <- tempfile(fileext=".sktr")
srv <- server_local(path = path)
listen(srv)

pid <- echo_client_local(path)
selectfd(srv)
clientof_srv <- accept(srv)

send(clientof_srv, "foo")
recv(clientof_srv)

(send(clientof_srv, "bars"))
recv(clientof_srv)

x <- raw(1e4)           # 'large' data
system.time(serialize(x, NULL))
system.time({
    print(send(clientof_srv, x))
    length(value <- recv(clientof_srv))
})

send(clientof_srv, serialize("DONE", NULL))
mccollect(pid)

close(srv)
