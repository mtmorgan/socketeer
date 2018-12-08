#' @useDynLib socketeer, .registration=TRUE

## base::open(con, ...)

#' @export
isup <- function(x)
    UseMethod("isup")

#' @export
recv <- function(x, ...)
    UseMethod("recv")

#' @export
send <- function(x, ...)
    UseMethod("send")

## base::close(con, ...)
