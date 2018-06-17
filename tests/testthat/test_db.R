context("Database tests")

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
d1 <- data.frame(row_id = c(1, 2),
                 name = c("Karla", "Karel"),
                 last_name = c("Dvořáková", "Jouda"),
                 number = c(1, 2))
d2 <- data.frame(row_id = 1,
                 name = "Karla",
                 last_name = "Nováková Dvořáková",
                 number = 2)
DBI::dbWriteTable(conn, "d1", d1)
DBI::dbWriteTable(conn, "d2", d2)

test_that("", {
    expect_equal(find_all_similar(conn, "d1", "d2", eq = c("name")),
                 data.frame(d1 = c(1, 2), d2 = c(1, NA)))
})

DBI::dbDisconnect(conn)

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conn, "d1", d1)
DBI::dbWriteTable(conn, "d2", d2)

test_that("", {
    expect_equal(find_all_similar(conn, "d1", "d2", eq_sub = c("last_name")),
                 data.frame(d1 = c(1, 2), d2 = c(1, NA)))

})

DBI::dbDisconnect(conn)

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conn, "d1", d1)
DBI::dbWriteTable(conn, "d2", d2)

test_that("", {
    expect_equal(find_all_similar(conn, "d1", "d2", ht = c("number")),
                 data.frame(d1 = c(1, 2), d2 = c(1, NA)))
})

DBI::dbDisconnect(conn)

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conn, "d1", d1)
DBI::dbWriteTable(conn, "d2", d2)

test_that("", {
    expect_equal(find_all_similar(conn, "d1", "d2", eq = c("name"),
                                  eq_sub = c("last_name"),
                                  ht = c("number")),
                 data.frame(d1 = c(1, 2), d2 = c(1, NA)))
})

DBI::dbDisconnect(conn)
