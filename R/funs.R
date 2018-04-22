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
                          lte = NULL){

    eqs <- create_var_list(eq, "=")
    eq_sub <- create_var_list(eq_sub, "=s")
    eq_tols <- create_var_list(eq_tol, "~")
    hts <- create_var_list(ht, ">")
    lts <- create_var_list(lt, "<")
    htes <- create_var_list(hte, ">=")
    ltes <- create_var_list(lte, "<=")

    # get values for comparison
    query_vars <- c(eqs, eq_sub, eq_tols, hts, lts, htes, ltes)
    query_value <- function(conn, t1, row, var){
        var$value <- RSQLite::dbGetQuery(conn,
                                paste0("select ", var$column, " from ", t1,
                                       " WHERE row_id = :row"),
                   params = list(row = row))[[1]]
        if(is.numeric(var$value)){
            var$type <- "number"
        }else{
            var$type <- "text"
        }
        var
    }
    var_values <- lapply(query_vars, function(x) query_value(conn, t1, row, x))

    # construct the query
    query <- paste0('SELECT row_id FROM ',
                    t2,
                    ' WHERE ',
                    paste0(unlist(lapply(var_values, create_query_filter)), collapse = " AND "))

    tmp <- RSQLite::dbGetQuery(conn, query)$row_id
    tmp <- ifelse(length(tmp) == 0, NA, tmp)
    # make column
    data.frame(from = row, to = tmp)

}

#' Find all matches between tables t1 and t2 and add candidates who did not run in the
#' sequential election with missing values
#' @param conn Connection to SQL database in which the data are stored
#' @param t1 Name of the first table
#' @param t2 Name of the second table
#' @param ... Vectors containing variables for comparison (see find_similar)
#' @export
find_all_similar <- function(conn, t1, t2, ...){
    #' Find all matches between first and second table

    rows1 <- RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", t1))[[1]]
    similars <- purrr::map(1:rows1, function(x) find_similar(conn, t1, t2, x, ...))
    out <- do.call(rbind, similars)

    #' Find all newly running candidates (who are not in the first table)
    #' and add them to output
    rows2 <- RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", t2))[[1]]
    missing_rows <- 1:rows2
    missing_rows <- missing_rows[!missing_rows %in% out$to]
    if(length(missing_rows) > 0){
        out2 <- data.frame(from = NA,
                           to = missing_rows)
        tmp <- rbind(out, out2)
        colnames(tmp) <- c(t1, t2)
        tmp
    }else{
        colnames(out) <- c(t1, t2)
        out
    }
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

    dplyr::bind_rows(purrr::map(1:nrow(iter), function(x)
        cbind(dbGetQuery(con, paste0("SELECT * FROM ", iter$col[x],
                                     " WHERE row_id = ", iter$row_id[x])),
              data.frame(person_id = iter$person_id[x],
                         election_id = iter$col[x]))))
}
