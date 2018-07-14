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

test_that("adding quotation marks works", {
    expect_equal(add_q_marks("Karel"),
                 glue::glue("'Karel'"))
    expect_equal(add_q_marks(6),
                 glue::glue(6))
})

test_that("constructing predicate works", {
    expect_equal(construct_predicate("first_name", "==", "Karel"),
                 "first_name == 'Karel'")
    expect_equal(construct_predicate("last_name", "=s", "Nováková"),
                 "grepl('Nováková', last_name)")
})

qv1 <- list(column = "first_name",
            operation = "=",
            value = "Karel",
            type = NA,
            tolerance = NA)
qv2 <- list(column = "year",
            operation = "~",
            value = 1990,
            type = NA,
            tolerance = 1)
qv3 <- list(column = "year",
            operation = ">",
            value = 1990,
            type = NA,
            tolerance = 1)
test_that("creating predicate works", {
    expect_equal(create_predicate_from_query_var(qv1)$predicate,
                 "first_name == 'Karel'")
    expect_equal(create_predicate_from_query_var(qv1)$name,
                 "first_name")
    expect_equal(create_predicate_from_query_var(qv2)$predicate,
                 "year >= 1989&year <= 1991")
    expect_equal(create_predicate_from_query_var(qv3)$predicate,
                 "year > 1990")
})

