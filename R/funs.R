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


#' Add quotation marks to value if necessary
#' @param value Value of a variable to be enquoted
add_q_marks <- function(value){
    UseMethod("add_q_marks")
}

add_q_marks.default <- function(value){
    glue::glue("'{value}'")
}

add_q_marks.numeric <- function(value){
    glue::glue("{value}")
}

construct_predicate <- function(var, operation, value){
    if(operation != "=s"){
        glue::glue("{var} {operation} {value}",
                   value = add_q_marks(value))
    }else{
        glue::glue("grepl('{value}', {var})")
    }

}

create_predicate_from_query_var <- function(query_var){
    if(query_var$operation == "="){
        list(name = query_var$column,
             predicate = construct_predicate(query_var$column, "==", query_var$value))
    }else if(query_var$operation == "~"){
        list(name = query_var$column,
             predicate = paste(c(construct_predicate(query_var$column, ">=",
                                                     query_var$value - query_var$tolerance),
                                 construct_predicate(query_var$column, "<=",
                                                     query_var$value + query_var$tolerance)),
                               collapse = "&"))

    }else{
        list(name = query_var$column,
             predicate = construct_predicate(query_var$column, query_var$operation, query_var$value))
    }
}

# collapse_vars <- function(x){
#     names <- lapply(x, function(y) y$name)
#     predicate <- lapply(x, function(y) y$predicate)
#     out <- purrr::map2(names, predicate, function(x, y) {
#         list(y)
#     })
#     names(out) <- names
#     out
# }

create_filter <- function(query_vars, collapse_sign = "&"){
    vars <- lapply(query_vars, create_predicate_from_query_var)
    paste0(lapply(vars, function(x) x$predicate),
           collapse = collapse_sign)
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
insert_query_values <- function(source, row, vars){
    select_vars <- unlist(lapply(vars, get_var_column))
    var_values <- source[row, ] %>%
        dplyr::select(!!select_vars)
    new_vars <- purrr::map2(vars, var_values, function(x, y) insert_value_and_type(x, y))
    new_vars
}

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
find_similar <- function(source, target, row,
                               eq = NULL,
                               eq_tol = NULL,
                               eq_sub = NULL,
                               ht = NULL,
                               lt = NULL,
                               hte = NULL,
                               lte = NULL,
                               id = "row_id",
                               compare_cols = NULL,
                               keep_duplicities = TRUE,
                               verbose = TRUE){
    eqs <- create_var_list(eq, "=")
    eq_sub <- create_var_list(eq_sub, "=s")
    eq_tols <- create_var_list(eq_tol, "~")
    hts <- create_var_list(ht, ">")
    lts <- create_var_list(lt, "<")
    htes <- create_var_list(hte, ">=")
    ltes <- create_var_list(lte, "<=")

    # get values for comparison
    vars <- c(eqs, eq_sub, eq_tols, hts, lts, htes, ltes)
    query_vars <- insert_query_values(source, row, vars)

    # construct the query
    filter_query <- create_filter(query_vars)

    tmp <- dplyr::filter_(target, filter_query)

    if(nrow(tmp) > 1){
        if(!keep_duplicities){
            common_cols <- intersect(colnames(source), colnames(target))
            if(!is.null(compare_cols)){
                common_cols <- intersect(compare_cols, common_cols)
            }
            original <- dplyr::select(source[row, ], !!common_cols)
            similars <- dplyr::select(tmp, !!common_cols)

            p_similarity <- rimr::calculate_similarity_between_persons(original,
                                                                       similars)
            tmp <- tmp[which.max(p_similarity), ]
        }
    }

    if(verbose && row %% 250 == 0){
        cat(row, "\n")
    }

    if(nrow(tmp) == 0){
        data.frame(from = row,
                   to = NA)
    }else{
        ids <- tmp %>% dplyr::select(!!id) %>% `[[`(., 1)
        data.frame(from = row,
                   to = ids)
    }

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
find_all_similar <- function(source, target, start = 1, cores = 1, ...){
    call <- as.list(match.call())
    col_names <- as.character(c(call$source, call$target))
    #' Find all matches between first and second table
    rows1 <- nrow(source)

    out <- parallel::mclapply(start:rows1,
                              function(x) find_similar(source, target, x, ...),
                              mc.cores = cores)

    out <- do.call(rbind, out)
    colnames(out) <- col_names
    out

}

#' Find IDs of entitites from target that does not occur in output
#'
#' @param target data.frame with target data
#' @param target_ids column name of IDs in target data.frame
#' @param out output returned by find_all_similar
#' @export
find_missing <- function(target, target_ids, out){
    missing_from_target <- target[[target_ids]]
    missing_from_target <- missing_from_target[!missing_from_target %in% out[[ncol(out)]]]

    data.frame(from = NA,
               to = missing_from_target)

    #rbind(out, tmp)
}

#' Append missing entities to output
#'
#' @param target data.frame with target data
#' @param target_ids column name of IDs in target data.frame
#' @param out output returned by find_all_similar
#' @export
append_missing <- function(target, target_ids, out){
    missing <- find_missing(target, target_ids, out)
    colnames(missing) <- colnames(out)
    rbind(out, missing)
}

# TODO: Define recursive matching for sequence of election
# TODO: Enable different operations to define match between different
# types of election (e.g. the criteria for flagging the person as the same
# between regional <-> regional elections may be different than for national <-> regional)

#' Create tidy output
#'
#' @param output Data.frame resulting from find_all_similar
create_panel_output <- function(output, id = "id"){
    output[[id]] <- 1:nrow(output)
    # reorder columns so that the person_id is the first and then the rest
    output <- output %>% dplyr::select(!!id, dplyr::everything())

    iter <- tidyr::gather(output, "data", "row", 2:ncol(output))
    iter <- iter[!is.na(iter$row), ]

    t_iter <- purrr::transpose(iter)

    out <- dplyr::bind_rows(lapply(t_iter, function(x) get_record(x, id)))
    out %>% dplyr::select(!!id, dplyr::everything())
}

get_record <- function(iter, id){
    data <- iter$data %>% as.name %>% eval
    tmp <- data[iter$row, ]
    tmp[[id]] <- iter[[id]]
    tmp
}
