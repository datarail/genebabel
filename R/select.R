#' Iteratively query a database for matches to a query vector.
#'
#' `iterative_join()` returns a tibble with all entries of the database that
#' match the query vector in any of the selected columns.
#'
#' @param query A character vector.
#' @param columns A character vector. The columns in the data to look for matches
#'   with the query. In order of preference, if matches to a column are
#'   found, matches to subsequent columns are not reported by default, unless
#'   return_all is `TRUE`.
#' @param data A data.frame or tibble.
#' @param return_all A logical indicating whether matches to subsequent columns,
#'   after a match has already been found, should also be returned.
#' @return A tibble containing all rows of the dataframe that matched a query.
#'   The new column `match__` contains the name of the column that matched
#'   the query for this row.
#' @examples
#' iterative_select(c("FLG", "SGK2"),
#'                  c("symbol", "alias_symbol", "prev_symbol"),
#'                  hgnc)
#'
#' @export
iterative_select <- function(query, columns, data, return_all = FALSE) {
  remaining_query <- query
  data <- data %>%
    dplyr::mutate("uid__" := 1:dplyr::n())
  out_dfs <- list()
  for (i in seq_along(columns)) {
    if (rlang::is_empty(remaining_query)) {
      break()
    }
    if (return_all) {
      query_i <- query
    }
    else {
      query_i <- remaining_query
    }
    c <- columns[i]
    # For using dplyr programmatically have to turn some of these variables
    # into symbols or quosures, not exactly sure this is all done correctly,
    # but seems to work
    c_sym <- sym(c)
    d <- data
    # Some columns in the datasets are list column which can have multiple entries
    # per row. Flatten the list for merging and put back the list column afterwards
    # Should find better strategy, because this is very slow
    list_col <- FALSE
    if (is.list(data[[c]])) {
      list_col <- TRUE
      d <- d %>%
        tidyr::drop_na(!! c_sym) %>%
        tidyr::unnest(!! c_sym)
    }
    out <- tibble::tibble(query = query_i) %>%
      dplyr::inner_join(d, by = c("query" = c)) %>%
      dplyr::mutate(!! c_sym := .data$query, match__ = c)
    if (list_col) {
      out <- out %>%
        dplyr::select(-!! c_sym) %>%
        dplyr::left_join(dplyr::select(data, .data$uid__, !! c_sym), by = "uid__")
    }
    remaining_query <- base::setdiff(remaining_query, out$query)
    out_dfs[[i]] <- out
  }
  out_dfs[["leftover"]] <- tibble::tibble(query = remaining_query, match__ = "none")
  out <- dplyr::bind_rows(out_dfs) %>%
    dplyr::select(-.data$uid__)
  # checking if query matched more than one entry in the database
  multimatch <- out %>%
    dplyr::count(.data$query) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::mutate(message = paste0(.data$query, ": ", .data$n))
  if (nrow(multimatch) > 0) {
    warning(paste0(multimatch$message, "\n", collapse = " "))
  }
  out
}

