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
                 glue::glue('"Karel"'))
    expect_equal(add_q_marks(6),
                 glue::glue(6))
})

test_that("constructing predicate works", {
    expect_equal(construct_predicate("first_name", "==", "Karel"),
                 'first_name == "Karel"')
    expect_equal(construct_predicate("last_name", "=s", "Nováková"),
                 'grepl("Nováková", last_name)')
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
                 'first_name == "Karel"')
    expect_equal(create_predicate_from_query_var(qv1)$name,
                 "first_name")
    expect_equal(create_predicate_from_query_var(qv2)$predicate,
                 'year >= 1989&year <= 1991')
    expect_equal(create_predicate_from_query_var(qv3)$predicate,
                 "year > 1990")
})


test_that("compare values returns expected result", {
    expect_equal(compare_values("Karel", "Karel"), 1)
    expect_equal(compare_values(list(c("starosta", "Chrudim")),
                                list(c("starosta"))),
                 0.5)
})


original <- data.frame(first_name = "Karel", last_name = "Novák",
                       birth_year = 1980, stringsAsFactors = FALSE)
similar <- data.frame(first_name = "Karel", last_name = "Novák",
                      birth_year = 1983, stringsAsFactors = FALSE)

original2 <- data.frame(first_name = "Karel", last_name = "Novák",
                       birth_year = 1980,
                       occupation = c("starosta"),
                       stringsAsFactors = FALSE)
similar2 <- data.frame(first_name = c("Karel", "Varel"),
                      last_name = c("Novák", "Plovák"),
                      birth_year = c(1983, 1930),
                      occupation = c("starosta Chrudimi", "chobot"),
                      stringsAsFactors = FALSE)
original2$occupation <- strsplit(original2$occupation, " ")
similar2$occupation <- strsplit(similar2$occupation, " ")
test_that("calculating similarity works", {
    expect_equal(calculate_similarity_between_persons(original, similar),
                   2)
    expect_equal(calculate_similarity_between_persons(original2, similar2),
                 c(2.5, 0))
})

vars <- create_var_list(c("first_name", "last_name"), "=")
vars2 <- create_var_list(c("birth_year"), "=")
test_that("inserting query values works", {
    expect_equal(insert_query_values(original, 1, vars)[[1]]$value,
                 "Karel")
    expect_equal(insert_query_values(original, 1, vars)[[2]]$value,
                 "Novák")
    expect_equal(insert_query_values(original, 1, vars2)[[1]]$value,
                 1980)
})


vars <- create_var_list(c("first_name", "last_name"), "=")
query_vars <- insert_query_values(original, 1, vars)
test_that("creating filter returns expected output", {
    expect_equal(create_filter(query_vars),
                 'first_name == "Karel"&last_name == "Novák"')
})

original_cols <- data.frame(id = 1, first_name = "Karel", last_name = "Novák",
                       birth_year = 1980, stringsAsFactors = FALSE)
similar_cols <- data.frame(id = 1, first_name = "Karel", last_name = "Novák",
                      birth_year = 1983, stringsAsFactors = FALSE)

test_that("finding common columns returns expected output", {
    expect_equal(find_common_cols(original_cols, similar_cols, "id"),
                 c("id", "first_name", "last_name", "birth_year"))
    expect_equal(find_common_cols(original_cols, similar_cols, "id",
                                  compare_cols = NULL, remove_id = TRUE),
                 c("first_name", "last_name", "birth_year"))
    expect_equal(find_common_cols(original_cols, similar_cols, "id",
                                  compare_cols = c("first_name", "last_name"),
                                  remove_id = TRUE),
                 c("first_name", "last_name"))

})

context("Test C++ helpers")
test_that("find duplicated values return expected result", {
    expect_equal(find_duplicated_values(c(1,2,3,1,2)),
                 c(1,2))
    expect_equal(find_duplicated_values(c(1,NA,NA,1)),
                 c(1))

})

context("Find similar")

original2 <- original
original2$row_id <- 1
test_that("find_all_similar fails if column with IDs is missing", {
    expect_error(find_all_similar(original, similar, id = "row_id"),
                 message = "The column of row IDs is missing in the source dataset")
    expect_error(find_all_similar(original2, similar, id = "row_id"),
                 message = "The column of row IDs is missing in the target dataset ")
})

original$row_id <- 1
similar$row_id <- 1:nrow(similar)
test_that("find_similar returns expected result", {
    expect_equal(find_similar(original, similar, 1, eq = c("first_name", "last_name"), id = "row_id"),
                 data.frame(from = 1, to = 1))
})

test_that("find_all_similar returns expected result", {
    expect_equal(find_all_similar(original, similar, 1,
                                  eq = c("first_name", "last_name"),
                                  id = "row_id"),
                 data.frame(original = 1, similar = 1))
})

similar2 <- data.frame(first_name = c("Karel", "Josef"),
                       last_name = c("Novák", "Dvořák"),
                       birth_year = c(1983, 1950),
                       row_id = 1:2,
                       stringsAsFactors = FALSE)
test_that("find_missing returns expected output", {
    expect_equal(find_missing(similar2, "row_id", 1),
                 data.frame(from = NA, to = 2))
})

out2 <- find_all_similar(original, similar2, 1, cores = 1, id = "row_id",
                 eq = c("first_name", "last_name"))
test_that("append_missing returns expected output", {
    expect_equal(append_missing(target = similar2,
                                target_ids = "row_id",
                                out = out2,
                                found_ids = out2$similar2),
                 data.frame(original = c(1, NA),
                            similar2 = c(1, 2)))
})

mis2 <- find_missing(similar2, "row_id", 1)
test_that("return_missing_data returns expected output", {
    expect_equal(return_missing_data(similar2, "row_id", mis2),
                 similar2[2, ])
})

similar3 <- data.frame(
    first_name = c("Karel", "Josef", "Karel"),
    last_name = c("Novák", "Dvořák", "Novák"),
    birth_year = c(1983, 1950, 1980),
    row_id = 1:3,
    stringsAsFactors = FALSE
)
source <- data.frame(
    row_id = 1:4,
    first_name = c("Karel", "Karel", "Václav", "Václav"),
    last_name = c("Šlápota", "Šlápota", "Novák", "Novák"),
    year = c(1990, 1991, 1990, 1991),
    stringsAsFactors = FALSE
)
target <- data.frame(
    row_id = 1:4,
    first_name = c("Karel", "Karel", "Václav", "Václav"),
    last_name = c("Šlápota", "Daněk", "Novák", "Daněk"),
    year = c(1990, 1991, 1990, 1991),
    stringsAsFactors = FALSE
)
out1 <- find_all_similar(source, target, start = 1, cores = 1,
                         id = "row_id",
                         eq = c("first_name", "last_name"),
                         keep_duplicities = FALSE)

test_that("find_all_similar removes duplicities if expected", {
    expect_equal(find_all_similar(original, similar3, start = 1, id = "row_id",
                                  eq = c("first_name", "last_name"),
                                  keep_duplicities = FALSE),
                 data.frame(original = 1,
                            similar3 = 3))
    expect_equal(find_all_similar(source, target, start = 1, id = "row_id",
                                  eq = c("first_name", "last_name"),
                                  keep_duplicities = FALSE),
                 data.frame(source = 1:4,
                            target = c(1, NA, 3, NA)))
})

test_pivot <- data.frame(
    municipal_1994 = c(1, 2, 2, 3),
    municipal_1998 = c(1, NA, NA, NA),
    municipal_2002 = c(1, NA, NA, NA)
)

non_pivot <- data.frame(
    municipal_1994 = c(2, 3),
    municipal_2002 = c(2, 3)
)

test_that("insert nonconsecutive returns error if there are duplicities", {
    expect_error(insert_nonconsecutive(test_pivot, non_pivot,
                                       "municipal_1994", "municipal_2002"))
})

test_pivot2 <- data.frame(
    municipal_1994 = c(1, 2, 3),
    municipal_1998 = c(1, NA, NA),
    municipal_2002 = c(1, NA, NA)
)

expected_pivot <- data.frame(
    municipal_1994 = c(1, 2, 3),
    municipal_1998 = c(1, NA, NA),
    municipal_2002 = c(1, 2, 3)
)

test_that("insert nonconsecutive returns expected result", {
    expect_equal(insert_nonconsecutive(test_pivot2, non_pivot,
                                       "municipal_1994", "municipal_2002"),
                 expected_pivot)
})


# does not work because cannot find 'original' object
# final_out <- append_missing(similar2, "row_id", out2, out2$similar2)
# expected_output <- create_panel_output(final_out, "id", "data")
# test_that("create_panel_output returns expected output", {
#     expect_true(all(expected_output ==
#                  data.frame(id = c(1, 1, 2),
#                             data = c("original", "similar2", "similar2"),
#                             first_name = c("Karel", "Karel", "Josef"),
#                             last_name = c("Novák", "Novák", "Dvořák"),
#                             birth_year = c(1980, 1983, 1950),
#                             row_id = c(1, 1, 2))))
# })
