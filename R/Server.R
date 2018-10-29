.Server <- setRefClass("Server",

    fields=list(server = "server",
                timeout = "integer",
                n_clients = "integer",
                buffer_block_size = "integer",
                clientsof="list"),

    methods = list(

        show = function() {
            cat("Object of class", sQuote(class(.self)), "\n")
            print(.self$server)
            cat("clients:", .self$n_clients, "\n")
        },

        clientsof_index = function() seq_along(.self$clientsof),

        clientsof_fd = function() {
            vapply(.self$clientsof, "[[", integer(1), "fd")
        },

        accept = function () {
            ## accept one
            .self$n_clients <- .self$n_clients + 1L
            clientof <- socketeer::accept(.self$server)
            .self$clientsof[[.self$n_clients]] <- clientof
            invisible(.self)
        },

        accept_all = function() {
            ## accept until no more new clients
            repeat {
                .self$accept()
                fd = selectfd(.self$server, 0L)
                if (!fd[[1]]) break
            }
            invisible(.self)
        },

        select_acceptfd = function() {
            ## wait until at least one client...
            repeat {
                fd = selectfd(.self$server, .self$timeout)
                if (fd[[1]]) break
            }
            .self$accept_all()
        },

        recv = function(clients=.self$clientsof_index()) {
            stopifnot(all(clients %in% .self$clientsof_index()))
            lapply(clients, .self$recvfrom)
        },

        recvfrom = function(client) {
            stopifnot(length(client) == 1L,
                      all(client %in% .self$clientsof_index()))

            value <- socketeer::recv(.self$clientsof[[client]],
                                     .self$buffer_block_size)

            ## FIXME attribute indicating where this came from
            unserialize(value)
        },

        accept_until_recvany = function() {
            repeat {
                fd = selectfd(.self$server, .self$timeout)
                if (fd[[1]])
                    .self$accept_all()
                if (length(fd[[2]]))
                    break
            }

            clients <- match(fd[[2]], .self$clientsof_fd())
            .self$recv(clients)
        },

        send = function(value, clients=.self$clientsof_index()) {
            stopifnot(all(clients %in% .self$clientsof_index()))

            for (client in clients)
                sendto(value, client)

            invisible(.self)
        },

        sendto = function(value, client) {
            stopifnot(length(client) == 1L,
                      all(client %in% .self$clientsof_index()))
            socketeer::send(.self$clientsof[[client]], serialize(value, NULL))
            invisible(.self)
        },

        close = function(clients=clientsof_index()) {
            stopifnot(all(clients %in% .self$clientsof_index()))

            for (i in unique(clients))
                close(.self$clientsof[[i]])
            idx = !.self$client_index() %in% clients
            .self$clientsof = .self$clientsof[idx]
            .self$n_clients = length(.self$clientsof)

            if (!.self$n_clients)
                socketeer::close(.self$server)

            invisible(.self)
        }
    )
)

Server <- function(hostname="localhost", port=NA_integer_, backlog=5L,
                   timeout = 30L, buffer_block_size=32768L)
{
    if (is.na(port))
        port <- 11000L + sample(1000, 1L)
    server <- server(hostname=hostname, port=port)
    listen(server, backlog=backlog)
    .Server(server=server, timeout=timeout, n_clients=0L,
            buffer_block_size=buffer_block_size)
}
