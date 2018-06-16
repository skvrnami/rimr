library(magrittr)
library(readr)
library(listr)
library(rimr)

kraj_2000 <- read_csv("data/kraj_20001112.csv")
kraj_2004 <- read_csv("data/kraj_20041105.csv")
kraj_2008 <- read_csv("data/kraj_20081017.csv")

process_df <- function(df){
    df$full_name <- unlist(purrr::map(df$name, listr::extract_text_before_titles))
    df$titles <- unlist(purrr::map(df$name, listr::extract_titles))
    df$highest_title <- listr::recode_titles(df$titles)
    df <- listr::add_names_to_df(df, "full_name")
    df$full_name %<>% stringr::str_trim()
    df <- listr::parse_gender(df, "first_name", "last_name")
    df$pct_votes %<>% gsub(",", ".", .) %>% as.numeric()
    df$abs_votes %<>% gsub("\\s", "", .) %>% as.numeric()
    df
}

kraj_2000 %<>% process_df
kraj_2004 %<>% process_df
# kraj_2008 %<>% process_df

kraj_2000$approx_birth_year <- 2000 - kraj_2000$age
kraj_2004$approx_birth_year <- 2004 - kraj_2004$age
# kraj_2008$approx_birth_year <- 2008 - kraj_2008$age
kraj_2004 <- kraj_2004[!is.na(kraj_2004$name), ]
# kraj_2008 <- kraj_2008[!is.na(kraj_2008$name), ]

library(RSQLite)
con <- DBI::dbConnect(RSQLite::SQLite(), "test7.sqlite")

upload_data(con, kraj_2000, "kraj_2000")
upload_data(con, kraj_2004, "kraj_2004")
# upload_data(con, kraj_2008, "kraj_2008")

t8 <- find_all_similar(con, "kraj_2000", "kraj_2004",
                           eq = c("first_name", "last_name", "region"),
                           eq_tol = list(c("approx_birth_year", 1)))

# t5 <- find_all_similar(con, "kraj_2000", "kraj_2004",
#                  eq = c("first_name", "last_name", "region"),
#                  eq_tol = list(c("approx_birth_year", 1)))
# beepr::beep(5)


find_all_similar_sequence <- function(conn, sequence_ts, ...){
    # TODO: Check if all columns have the same type

    combns <- expand.grid(1:length(sequence_ts), 1:length(sequence_ts)) %>%
        dplyr::rename(from = Var1,
                      to = Var2) %>%
        subset(., from < to) %>%
        dplyr::arrange(to)
    combns$from <- sequence_ts[combns$from]
    combns$to <- sequence_ts[combns$to]
    rows <- unlist(purrr::map(combns$from, function(x)
        RSQLite::dbGetQuery(conn, paste0("SELECT COUNT(row_id) FROM ", x))[[1]]))

    rename_cols <- function(df, name1, name2){
        colnames(df) <- c(name1, name2)
        df
    }

    similars_sequence <-
        purrr::map(1:nrow(combns),
                   function(x)
                       rename_cols(do.call(rbind,
                               purrr::map(1:rows[x],
                                          function(y) find_similar(conn, combns$from[x], combns$to[x], y, ...))),
                               combns$from[x], combns$to[x]))

    similars_sequence

}

out <- find_all_similar_sequence(con, c("kraj_2000", "kraj_2004", "kraj_2008"),
                          eq = c("first_name", "last_name", "region"),
                          eq_tol = list(c("approx_birth_year", 1)))
out1 <- dplyr::full_join(out[[1]], out[[3]], by = "kraj_2004")
out2 <- dplyr::full_join(out1, out[[2]], by = "kraj_2000")
out2$kraj_2008.x[is.na(out2$kraj_2008.x) & !is.na(out2$kraj_2008.y)] <-
    out2$kraj_2008.y[is.na(out2$kraj_2008.x) & !is.na(out2$kraj_2008.y)]
out2$kraj_2008.y <- NULL
colnames(out2)[3] <- "kraj_2008"

tidy_out <- create_tidy_output(out2)
