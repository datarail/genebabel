#' Iteratively query a database for matches to a query vector.
#'
#' `iterative_select()` returns a tibble with all entries of the database that
#' match the query vector in any of the selected columns.
#'
#' @param query A character vector.
#' @param database A data.frame or tibble  A database to be queried.
#'   See [databases()] for a list of included databases.
#' @param match_cols A character vector. The columns in the data to look for matches
#'   with the query. In order of preference, if matches to a column are
#'   found, matches to subsequent columns are not reported by default, unless
#'   return_all is `TRUE`.
#' @param return_all A logical indicating whether matches to subsequent columns,
#'   after a match has already been found, should also be returned.
#' @return A tibble containing all rows of the dataframe that matched a query.
#'   The new column `match__` contains the name of the column that matched
#'   the query for this row.
#' @examples
#' iterative_select(c("FLG", "SGK2"),
#'                  hgnc,
#'                  c("symbol", "alias_symbol", "prev_symbol"))
#'
#' @export
iterative_select <- function(query, database, match_cols, return_all = FALSE) {
  remaining_query <- query
  data <- database %>%
    dplyr::mutate("uid__" := 1:dplyr::n())
  out_dfs <- list()
  for (i in seq_along(match_cols)) {
    if (rlang::is_empty(remaining_query)) {
      break()
    }
    if (return_all) {
      query_i <- query
    }
    else {
      query_i <- remaining_query
    }
    c <- match_cols[i]
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
    warning(
      "Multile matches of same priority found for:\n",
      paste0(multimatch$message, "\n", collapse = " ")
    )
  }
  out
}

#' Join results from a database into an existing dataframe.
#'
#' `join_results()` queries a database for matches to a column in the supplied
#' dataset and returns it together with the matches found.
#'
#' @param df A data.frame or tibble. The data used for querying the database.
#' @param query_col A character vector of length one. Name of the column in `df`
#'   that will be used to query the database.
#' @param select_cols A character vector of column names in the database that
#'   will be merged in the ouput.
#' @inheritParams iterative_select
#' @return The input dataframe merged with the selected matching columns from
#'   the database.
#' @examples
#' d <- data.frame(a = 1:3, b = c("FLG", "SGK2", "CDK1"))
#' join_results(d, "b", hgnc,
#'   match_cols = c("symbol", "alias_symbol", "prev_symbol"),
#'   select_cols = c("entrez_id", "symbol", "refseq_accession"))
#'
#' @export
join_results <- function (df, query_col, database, match_cols, select_cols = NULL) {
  hits <- iterative_select(df[[query_col]], database, match_cols)
  if (!rlang::is_null(select_cols)) {
    hits <- hits %>%
      dplyr::select_at(unique(c(select_cols, "query")))
  }
  dplyr::left_join(df, hits, by = rlang::set_names(nm = query_col, x = "query"))
}
