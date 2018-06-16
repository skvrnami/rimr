# TODO: Convert factors to numerical values so that comparison operations
# make sense

#' Append row_id and upload to DB
#'
#' @param connection SQLite connection
#' @param df data.frame to be uploaded
#' @param name name of the table
#' @export
upload_data <- function(connection, df, name){
    df$row_id <- 1:nrow(df)
    DBI::dbWriteTable(connection, name, df)
}

#' create list storing column with attributes for comparison,
#' operation on which the data should be compared (=, >, <, >=, <=)
#' @param x Vector of variables
#' @param operation Operation sign (=, >, <, >=, <=)
create_var_list <- function(x, operation){
    if (operation == "~"){
        lapply(x, function(y) list(column = y[[1]],
                                   operation = operation,
                                   value = NA,
                                   type = NA,
                                   tolerance = as.numeric(y[[2]])))
    }else{
        lapply(x, function(y) list(column = y,
                                   operation = operation,
                                   value = NA,
                                   type = NA,
                                   tolerance = NA))
    }
}


#' Create filter for querying the DB (WHERE clauses)
#'
#' @param query_var Variables to be queried
#' (list of column, value, tolerance and operation to be done)
create_query_filter <- function(query_var){
    if(query_var$operation == "~"){
        paste0(query_var$column, ">=", (query_var$value - query_var$tolerance), " AND ",
               query_var$column, "<=", (query_var$value + query_var$tolerance))
    }else if(query_var$type == "text"){
        if(query_var$operation == "=s"){
            # husband's last name should be first and it is separated by space
            # TODO: modify to consider divorce
            paste0(query_var$column, " LIKE ", "'%", gsub("'", "''", query_var$value), "'")
        }else{
            paste0(query_var$column, query_var$operation, "'",
                   gsub("'", "''", query_var$value), "'") # escape ' for query
        }
    }else{
        paste0(query_var$column, query_var$operation, query_var$value)
    }
}

get_var_column <- function(var){
    var$column
}

insert_value_and_type <- function(var, value){
    var$value <- value
    if(is.numeric(var$value)){
        var$type <- "number"
    }else{
        var$type <- "text"
    }
    var
}

#' insert query values into variable list
insert_query_values <- function(conn, t1, row, vars){
    var_values <-
        RSQLite::dbGetQuery(conn,
                            paste0("SELECT ",
                                   paste0(unlist(lapply(vars, function(x) get_var_column(x))),
                                          collapse = ", "),
                                   " FROM ", t1, " WHERE row_id = :row"),
                            params = list(row = row))
    new_vars <- purrr::map2(vars, var_values, function(x, y) insert_value_and_type(x, y))
    new_vars
}


#' Calculate similarity between persons
#'
#' @param original_person Data.frame with all data about original person
#' @param similar_person Data.frame with all data about similar persons
#' Calculate in how many columns the data about the persons match (strictly equals)
calculate_similarity_between_persons <- function(original_person, similar_persons){
    #' TODO: use other comparison than strict equality
    purrr::map(purrr::transpose(similar_persons), function(x) {
        sum(unlist(purrr::map2(original_person, x, function(y, z) {
            y == z})), na.rm = TRUE)
    }) %>% unlist
}


#' Find match between one person from the first dataset (t1) and
#' the whole second dataset (t2)
#' @param conn Connection to SQL database in which the data are stored
#' @param t1 Name of the first table
#' @param t2 Name of the second table
#' @param row Row from the first table for which similar person is searched
#' @param eq Vector of variables which should be equal in t1 and t2
#' @param eq_tol Vector of variables which should be approximately equal
#' in t1 and t2 within specified range (tolerance)
#' @param eq_sub Vector of variables in which value of t1 should be a substring of t2
#' (optimized for double last names after marriage)
#' @param ht Vector of variables which should be higher in t1
#' @param lt Vector of variables which should be lower in t1
#' @param hte Vector of variables which should be higher or equal in t1
#' @param lte Vector of variables which should be lower or equal in t1
#' @export
find_similar <- function(conn, t1, t2, row,
                         eq = NULL,
                         eq_tol = NULL,
                         eq_sub = NULL,
                         ht = NULL,
                         lt = NULL,
                         hte = NULL,
                         lte = NULL,
                         keep_duplicities = TRUE){
    eqs <- create_var_list(eq, "=")
    eq_sub <- create_var_list(eq_sub, "=s")
    eq_tols <- create_var_list(eq_tol, "~")
    hts <- create_var_list(ht, ">")
    lts <- create_var_list(lt, "<")
    htes <- create_var_list(hte, ">=")
    ltes <- create_var_list(lte, "<=")

    # get values for comparison
    query_vars <- c(eqs, eq_sub, eq_tols, hts, lts, htes, ltes)
    vars <- insert_query_values(conn, t1, row, query_vars)

    # construct the query
    query <- paste0('SELECT row_id FROM ',
                    t2,
                    ' WHERE ',
                    paste0(unlist(lapply(vars, create_query_filter)), collapse = " AND "))

    tmp <- RSQLite::dbGetQuery(conn, query)$row_id
    if(length(tmp) == 0){
        tmp <- NA
    }else if(length(tmp) > 1 & !keep_duplicities){
        orig_person <- dbGetQuery(con, paste0("SELECT * FROM ", t1,
                                         " WHERE row_id = ", row)) %>%
            dplyr::select(-row_id)
        similar_persons <- dbGetQuery(con, paste0("SELECT * FROM ", t2,
                                                  " WHERE row_id IN (",
                                                  paste0(tmp, collapse = ", "),
                                                  ")"))

        common_cols <- intersect(colnames(orig_person), colnames(orig_person))
        orig_person <- orig_person %>% select(common_cols)
        similar_persons <- similar_persons %>% select(common_cols)

        p_similarity <- calculate_similarity_between_persons(orig_person,
                                                             similar_persons)
        tmp <- tmp[which.max(p_similarity)]

    }

    if(row %% 250 == 0){
        cat(row, "->", tmp, "\n")
    }

    #' INSERT data
    if(length(tmp) > 1){
        purrr::map(tmp, function(x) {
            rs <- dbSendStatement(conn,
                                  paste0("INSERT INTO ", paste0(t1, "_", t2),
                                         " (", t1, ",", t2, ") VALUES (:row, :tmp)"),
                                  params = list(row = row, tmp = x))
            dbClearResult(rs)
        })
    }else{
        if(!is.na(tmp)){
                rs <- dbSendStatement(conn,
                                      paste0("INSERT INTO ", paste0(t1, "_", t2),
                                             " (", t1, ",", t2, ") VALUES (:row, :tmp)"),
                                      params = list(row = row, tmp = tmp))
                dbClearResult(rs)
        }
    }
    NULL
    #data.frame(from = row, to = tmp)

}

create_values_list <- function(missing_rows){
    paste0(unlist(purrr::map(missing_rows, function(x) paste0("(", x, ")"))), collapse = ", ")
}

#' Find all matches between tables t1 and t2 and add candidates who did not run in the
#' sequential election with missing values
#' @param conn Connection to SQL database in which the data are stored
#' @param t1 Name of the first table
#' @param t2 Name of the second table
#' @param start From which row the comparison should be made
#' @param ... Vectors containing variables for comparison (see find_similar)
#' @export
find_all_similar <- function(conn, t1, t2, start = 1, ...){
    #' Find all matches between first and second table

    #' Create DB to store the results
    browser()
    rs <- dbSendStatement(conn,
                    paste0("CREATE TABLE IF NOT EXISTS ",
                    paste0(t1, "_", t2),
                    " (", t1, ", ", t2, ")"))
    dbClearResult(rs)

    rows1 <- RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", t1))[[1]]

    purrr::map(start:rows1, function(x) find_similar(conn, t1, t2, x, ...))
    # out <- do.call(rbind, similars)

    out <- dbGetQuery(conn, paste0("SELECT * FROM ", paste0(t1, "_", t2)))
    #' Find all newly running candidates (who are not in the first table)
    #' and add them to output

    rows1 <- RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", t1))[[1]]
    rows2 <- RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", t2))[[1]]

    #' Insert persons which did not run in both election
    missing_rows1 <- 1:rows1
    missing_rows1 <- missing_rows1[!missing_rows1 %in% out[[t1]]]
    missing_rows2 <- 1:rows2
    missing_rows2 <- missing_rows2[!missing_rows2 %in% out[[t2]]]

    if(length(missing_rows1) > 0){
        dbClearResult(dbSendStatement(conn,
                                      paste0("INSERT INTO ",
                                             paste0(t1, "_", t2),
                                             " (", t1, ")",
                                             " VALUES ",
                                             create_values_list(missing_rows1))))
    }

    if(length(missing_rows2) > 0){
        dbClearResult(dbSendStatement(conn,
                                      paste0("INSERT INTO ",
                                             paste0(t1, "_", t2),
                                             " (", t2, ")",
                                             " VALUES ",
                                             create_values_list(missing_rows2))))
    }


    dbGetQuery(conn, paste0("SELECT * FROM ", paste0(t1, "_", t2)))
}

# TODO: Define recursive matching for sequence of election
# TODO: Enable different operations to define match between different
# types of election (e.g. the criteria for flagging the person as the same
# between regional <-> regional elections may be different than for national <-> regional)

#' Create tidy output
#'
#' @param output_df Data.frame resulting from find_all_similar
create_tidy_output <- function(output_df){
    output_df$person_id <- 1:nrow(output_df)
    # reorder columns so that the person_id is the first and then the rest
    output_df <- output_df[c(ncol(output_df), 2:ncol(output_df) - 1)]

    iter <- tidyr::gather(output_df, "col", "row_id", 2:ncol(output_df))
    iter <- iter[!is.na(iter$row_id), ]

    dplyr::bind_rows(purrr::map(purrr::transpose(iter), function(x) {
        cbind(dbGetQuery(con, paste0("SELECT * FROM ", x$col,
                                     " WHERE row_id = ", x$row_id)),
              data.frame(person_id = x$person_id,
                         election_id = x$col))
    }))
}
