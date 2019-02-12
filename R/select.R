




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
  if (rlang::is_empty(query))
    stop("'query' can not be empty.")
  database <- database %>%
    dplyr::mutate(uid__ = 1:n())
  orig_query <- query
  query <- unique(query)
  remaining_query <- query
  out_ids <- list()
  for (i in seq_along(match_cols)) {
    if (rlang::is_empty(remaining_query))
      break()
    query_cur <- if(return_all) query else remaining_query
    c <- match_cols[i]
    if (!(c %in% names(database)))
      stop("match_col '", c, "' not in database")
    # For using dplyr programmatically have to turn some of these variables
    # into symbols or quosures, not exactly sure this is all done correctly,
    # but seems to work
    c_sym <- sym(c)
    # Some columns in the datasets are list column which can have multiple entries
    # per row. Flatten the list for merging and put back the list column afterwards
    # Should find better strategy, because this is very slow
    d <- database %>%
      dplyr::select(uid__, !!c_sym) %>%
      tidyr::drop_na(!! c_sym)
    if (is.list(database[[c]])) {
      d <- d %>%
        tidyr::unnest(!! c_sym)
    }
    out <- tibble::tibble(
      query = query_cur,
      match__ = c
    ) %>%
      dplyr::inner_join(d, by = c("query" = c))
    remaining_query <- base::setdiff(remaining_query, out$query)
    out_ids[[i]] <- out
  }
  if (!rlang::is_empty(remaining_query))
    out_ids[["leftover"]] <- tibble::tibble(query = remaining_query, match__ = "none")
  out_ids_df <- dplyr::bind_rows(out_ids)
  # checking if query matched more than one entry in the database
  multimatch <- out_ids_df %>%
    dplyr::group_by(.data$match__) %>%
    dplyr::count(.data$query) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::mutate(message = paste0(.data$query, ": ", .data$n))
  if (nrow(multimatch) > 0) {
    warning(
      "Multiple matches of same priority found for some queries. All matches are
      reported.\n",
      paste0(multimatch$message, "\n", collapse = " ")
    )
  }
  # Warning user when any queries didn't match
  if (length(remaining_query) > 0) {
    warning(
      "No matches found for some queries. Reporting NA for these queries.\n",
      paste0(remaining_query, collapse = "\n")
    )
  }
  out_df <- out_ids_df %>%
    dplyr::left_join(database, by = "uid__") %>%
    dplyr::select(-uid__)
  out_df
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
