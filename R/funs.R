#' @useDynLib rimr
#' @importFrom Rcpp sourceCpp
NULL


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


find_common_cols <- function(source, target, id, compare_cols = NULL,
                             remove_id = FALSE){
    common_cols <- intersect(colnames(source), colnames(target))
    if(remove_id){
        common_cols <- common_cols[!common_cols == id]
    }

    if(!is.null(compare_cols)){
        common_cols <- intersect(compare_cols, common_cols)
    }
    common_cols
}


#' Add quotation marks to value if necessary
#' @param value Value of a variable to be enquoted
add_q_marks <- function(value){
    UseMethod("add_q_marks")
}

add_q_marks.default <- function(value){
    glue::glue('"{value}"')
}

add_q_marks.numeric <- function(value){
    glue::glue("{value}")
}

construct_predicate <- function(var, operation, value){
    if(operation != "=s"){
        as.character(glue::glue('{var} {operation} {value}',
                                value = add_q_marks(value)))
    }else{
        as.character(glue::glue('grepl({value}, {var})',
                                value = add_q_marks(value)))
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


create_filter <- function(query_vars, collapse_sign = "&"){
    vars <- lapply(query_vars, create_predicate_from_query_var)
    paste0(lapply(vars, function(x) x$predicate),
           collapse = collapse_sign)
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


insert_query_values <- function(source, row, vars){
    select_vars <- unlist(lapply(vars, function(x) x$column))
    var_values <- dplyr::select(source[row, ], !!select_vars)
    new_vars <- purrr::map2(vars, var_values, function(x, y) insert_value_and_type(x, y))
    new_vars
}


#' Calculate similarity between persons
#'
#' @param original Data.frame with all data about original person
#' @param similar Data.frame with all data about similar persons
#' Calculate in how many columns the data about the persons match (strictly equals)
calculate_similarity_between_persons <- function(original, similar){
    #' TODO: use other comparison than strict equality
    purrr::map_dbl(purrr::transpose(similar), function(x) {
        sum(purrr::map2_dbl(original, x, compare_values), na.rm = TRUE)
    })
}


compare_values <- function(x, y){
    if(length(unlist(x)) == 1 & length(unlist(y)) == 1){
        as.numeric(x == y)
    }else{
        x <- unlist(x)
        y <- unlist(y)
        sum(x %in% y) / length(unique(c(x, y)))
    }
}


#' Find match between one person from the first dataset (t1) and
#' the whole second dataset (t2)
#'
#' @export
#' @param source Name of the first table
#' @param target Name of the second table
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
#' @param id Column in source and target datasets containing ID of a row
#' @param compare_cols Columns to be used for comparison to remove duplicates
#' @param verbose Specify if you want to display message for every 250th row
#' @param keep_duplicities Bool indicating whether entities with the same attributes
#' should be kept or the most similar entity to the original record should be found
#' (and duplicities should be removed)
find_similar <- function(source,
                         target,
                         row,
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
            common_cols <- find_common_cols(source, target, id, compare_cols,
                                            remove_id = TRUE)

            original <- dplyr::select(source[row, ], !!common_cols)
            similars <- dplyr::select(tmp, !!common_cols)

            p_similarity <- calculate_similarity_between_persons(original,
                                                                 similars)
            tmp <- tmp[which.max(p_similarity), ]
        }
    }

    if(verbose && row %% 250 == 0){
        cat(row, "\n")
    }

    if(nrow(tmp) == 0){
        data.frame(from = source[[id]][row],
                   to = NA)
    }else{
        ids <- `[[`(dplyr::select(tmp, !!id), 1)
        data.frame(from = source[[id]][row],
                   to = ids)
    }

}


#' Find all matches between tables t1 and t2 and add candidates who did not run in the
#' sequential election with missing values
#'
#' @export
#' @param source Name of the first table
#' @param target Name of the second table
#' @param start From which row the comparison should be made
#' @param id Column in source and target datasets containing ID of a row
#' @param cores Number of cores to be used for computation
#' @param keep_duplicities Parameter indicating if duplicities should be kept or
#' removed
#' @param compare_cols columns which should be used for comparison of similar persons
#' if there is more than 1 match (to find the most similar and remove duplicities)
#' @param ... Vectors containing variables for comparison (see find_similar)
find_all_similar <- function(source,
                             target,
                             start = 1,
                             cores = 1,
                             id,
                             keep_duplicities = FALSE,
                             compare_cols = NULL,
                             ...){

    if(!id %in% colnames(source)){
        stop("The column of row IDs is missing in the source dataset")
    }

    if(!id %in% colnames(target)){
        stop("The column of row IDs is missing in the target dataset")
    }

    call <- as.list(match.call())
    col_names <- as.character(c(call$source, call$target))
    #' Find all matches between first and second table
    rows1 <- nrow(source)

    out <- parallel::mclapply(start:rows1,
                              function(x) find_similar(source = source,
                                                       target = target,
                                                       row = x,
                                                       id = id,
                                                       keep_duplicities = keep_duplicities,
                                                       compare_cols = compare_cols,
                                                       ...),
                              mc.cores = cores)

    out <- do.call(rbind, out)

    #' Backward check if two persons from source are not assigned
    #' to the same person in target
    if(!keep_duplicities){
        duplicated_values <- find_duplicated_values(out$to)
        # non_na <- `[[`(filter(out, !is.na(to)), 2)
        # duplicated_values <- non_na[which(duplicated(non_na))]
        if(length(duplicated_values) > 0){
            sim_groups <- purrr::map(1:length(duplicated_values),
                                     function(x) out[out$to == duplicated_values[x] & !is.na(out$to), ])

            duplicity_ids <- unlist(purrr::map(sim_groups, function(x)
                find_all_duplicities(x, source, target, "row_id",
                                     compare_cols = compare_cols)))

            out[out$from %in% duplicity_ids, "to"] <- NA
        }
    }

    colnames(out) <- col_names
    out

}


get_original <- function(sim_group, source, id = "row_id"){
    source[source[[id]] %in% sim_group$from, ]
}

get_similar <- function(sim_group, target, id = "row_id"){
    target[target[[id]] %in% unique(sim_group$to), ]
}

# find_most_similar <- function(original, similar){
#     p_similarity <- calculate_similarity_between_persons(similar, original)
#     original[which.max(p_similarity), ]
# }

find_duplicity <- function(original, similar, id){
    p_similarity <- calculate_similarity_between_persons(similar, original)
    original[-which.max(p_similarity), id]
}


find_all_duplicities <- function(sim_group, source, target, id, compare_cols){

    original <- get_original(sim_group, source, id)
    similar <- get_similar(sim_group, target, id)

    common_cols <- find_common_cols(source, target, id, compare_cols,
                                    remove_id = FALSE)

    original <- dplyr::select(original, c(!!common_cols, id))
    similars <- dplyr::select(similar, c(!!common_cols, id))

    find_duplicity(original, similars, id)
}


#' Find IDs of entitites from target that does not occur in output
#'
#' @export
#' @param target Data.frame with target data
#' @param target_ids Column name of IDs in target data.frame
#' @param found_ids Vector with IDs found by find_all_similar function(s)
find_missing <- function(target, target_ids, found_ids){
    missing_from_target <- target[[target_ids]]
    missing_from_target <- missing_from_target[!missing_from_target %in% found_ids]

    data.frame(from = NA,
               to = missing_from_target)

    #rbind(out, tmp)
}

#' Append missing entities to output
#'
#' @export
#' @param target data.frame with target data
#' @param target_ids column name of IDs in target data.frame
#' @param out output returned by find_all_similar function
#' @param found_ids vector containing IDs from target which has
#' been found in the source dataset
append_missing <- function(target, target_ids, out, found_ids){
    missing <- find_missing(target, target_ids, found_ids)
    colnames(missing) <- colnames(out)
    dplyr::bind_rows(out, missing)
}


#' Create tidy output
#'
#' @export
#' @param output Data.frame resulting from find_all_similar
#' @param id Name of column containing IDs which will be created
#' @param data Name of column containing name of the dataset from which
#' the record originates to be created
create_panel_output <- function(output, id = "id", data = "data"){
    output[[id]] <- 1:nrow(output)
    # reorder columns so that the person_id is the first and then the rest
    output <- dplyr::select(output, !!id, dplyr::everything())

    iter <- tidyr::gather(output, "data", "row", 2:ncol(output))
    iter <- iter[!is.na(iter$row), ]

    t_iter <- purrr::transpose(iter)

    out <- dplyr::bind_rows(lapply(t_iter, function(x) get_record(x, id, data)))
    dplyr::select(out, !!id, !!data, dplyr::everything())
}

get_record <- function(iter, id, data){
    d <- eval(as.name(iter$data))
    tmp <- d[iter$row, ]
    tmp[[data]] <- iter[["data"]]
    tmp[[id]] <- iter[[id]]
    tmp
}


#' Return data of entities which occur in target dataset but not in source dataset
#'
#' @export
#' @param target target dataset
#' @param row_id Column name of row IDs
#' @param missing result of find_missing function
return_missing_data <- function(target, row_id, missing){
    missing_ids <- missing[["to"]]
    select_missing <- target[[row_id]] %in% missing_ids
    target[select_missing, ]
}


#' Return data of entities that occur in the source dataset but not in the target
#'
#' @export
#' @param result result of find_all_similar function
#' @param source source dataset
#' @param target target dataset
#' @param row_id Column name of row IDs
return_nonconsecutive_data <- function(result, source, target, row_id){
    args <- as.list(match.call())
    res_pivot <- result[is.na(result[[as.character(args$target)]]), ]
    noncons_ids <- res_pivot[[as.character(args$source)]]
    select_ids <- source[[row_id]] %in% noncons_ids
    source[select_ids, ]
}

#' Insert nonconsecutive entities into pivot table
#'
#' @export
#' @param out Pivot table returned by find_all_similar
#' @param noncons_pivot Pivot table returned by find_all_similar
#' between nonconsecutive datasets
#' @param source Name of the nonconsecutive pivot source
#' @param target Name of the nonconsecutive pivot target
insert_nonconsecutive <- function(out, noncons_pivot, source, target){
    last_column <- ncol(noncons_pivot)
    running <- noncons_pivot[!is.na(noncons_pivot[, last_column]), ]
    out <- dplyr::arrange_(out, source)
    duplicated_source <- find_duplicated_values(out[[source]])
    if(length(duplicated_source) > 0){
        stop("There are duplicated values in source: ", duplicated_source)
    }
    out[out[[source]] %in% running[[source]], target] <- running[[target]]
    out
}

