#' @useDynLib socketeer, .registration=TRUE

## base::open(con, ...)

#' @export
isup <- function(x)
    UseMethod("isup")

##
## server
##

#' @export
recv_from <- function(x, node)
    UseMethod("recv_from")

#' @export
recv_any <- function(x)
    UseMethod("recv_any")

#' @export
send_to <- function(x, node, value)
    UseMethod("send_to")

finalize <- function(x)
    UseMethod("finalize")

##
## client
##

#' @export
recv <- function(x)
    UseMethod("recv")

#' @export
send <- function(x, value)
    UseMethod("send")

## base::close(con, ...)

##
## other
##

#' @export
value <- function(x)
    UseMethod("value")

#' @export
value.default <- function(x) x

#' @export
value.list <- function(x) lapply(x, value)
