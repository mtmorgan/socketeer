library(sockets)

##

context("is_socket")

test_that("is_socket works", {
    expect_false(is_socket(1))
    expect_true(is_socket(server_bind("localhost", 11002L)))
})

##

context("socket accessors")

test_that("socket_hostname(), socket_port() check input", {
    expect_error(socket_hostname(1))
    expect_error(socket_port(1))
})

test_that("socket_hostname(), socket_port() works", {
    sock <- server_bind("localhost", 11003L)
    expect_equal(socket_hostname(sock), c("localhost", NA))
    expect_equal(socket_port(sock), c(11003L, NA))
})

##

context("server_bind")

test_that("server_bind checks inputs", {
    expect_error(server_bind())
    expect_error(server_bind(123, "foo"))
    expect_error(server_bind("", 123L))
    expect_error(server_bind(c("foo", "bar"), 123L))
    expect_error(server_bind("foo", c(123L, 456L)))
})

test_that("server_bind rejects bad hosts and ports", {
    expect_error(server_bind("foo", 11001L))
    expect_error(server_bind("localhost", 111L))
})

test_that("server_bind returns a socket", {
    expect_true(is_socket(server_bind("localhost", 11001L)))
})

test_that("server_bind accepts Sys.info()[['nodename']]", {
    hostname <- Sys.info()[["nodename"]]
    expect_true(is_socket(server_bind(hostname, 11006L)))
})

##

context("server_close")

test_that("server_close works", {
    sock <- server_bind("localhost", 11004L)
    expect_equal(is_open(sock), c(TRUE, FALSE))
    expect_equal(server_close(sock), TRUE)
    expect_equal(is_open(sock), c(FALSE, FALSE))
})

test_that("closed sockets can be re-used", {
    sock <- server_bind("localhost", 11005L)
    server_close(sock)

    sock <- server_bind("localhost", 11005L)
    expect_equal(is_open(sock), c(TRUE, FALSE))
    expect_equal(server_close(sock), TRUE)
    expect_equal(is_open(sock), c(FALSE, FALSE))
})
