#' @useDynLib socketeer, .registration=TRUE

## base::open(con, ...)

#' @export
isup <- function(x)
    UseMethod("isup")

##
## server
##

#' @export
recv_from <- function(x, i)
    UseMethod("recv_from")

#' @export
recv_any <- function(x)
    UseMethod("recv_any")

#' @export
send_to <- function(x, i, value)
    UseMethod("send_to")

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