context("Test helpers")

test_that("creating variable lists works", {
    expect_equal(create_var_list(c("first_name", "last_name"), "="),
                 list(list(column = "first_name",
                           operation = "=",
                           value = NA,
                           type = NA,
                           tolerance = NA),
                      list(column = "last_name",
                           operation = "=",
                           value = NA,
                           type = NA,
                           tolerance = NA)))
    expect_equal(create_var_list(list(list("birth_year", 1)), "~"),
                 list(list(column = "birth_year",
                           operation = "~",
                           value = NA,
                           type = NA,
                           tolerance = 1)))
})

test_that("creating query works", {
    expect_equal(create_query_filter(list(column = "first_name",
                                          operation = "=",
                                          value = "Karel",
                                          type = "text",
                                          tolerance = NA)),
                 "first_name='Karel'")
})
