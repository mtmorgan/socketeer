.Client <- setRefClass("Client",

    fields=list(client = "client",
                buffer_block_size = "integer"),

    methods = list(

        show = function() {
            cat("Object of class", sQuote(class(.self)), "\n")
            print(.self$client)
        },

        send = function(value) {
            socketeer::send(.self$client, serialize(value, NULL))
            invisible(.self)
        },

        recv = function() {
            value <- socketeer::recv(.self$client)
            unserialize(value)
        },

        close = function() {
            close(.self$client)
        }
    )
)
        
Client <- function(hostname="localhost", port, buffer_block_size=32768L) {
    client <- client(hostname=hostname, port=port)
    .Client(client=client, buffer_block_size=as.integer(buffer_block_size))
}
