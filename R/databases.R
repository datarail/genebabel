#' Query the HUGO Gene Nomenclature Commitee approved symbol database
#'
#' `query_hgnc()` finds matches for the supplied query vector in the specified
#' columns of the database and returns a tibble of all the matches.
#'
#' Find which columns are available by checking `help(hgnc)`.
#'
#' @param query A character vector.
#' @param columns A character vector. The columns in the data to look for matches
#'   with the query. In order of preference, if matches to a column are
#'   found, matches to subsequent columns are not reported by default, unless
#'   return_all is `TRUE`.
#' @param return_all A logical indicating whether matches to subsequent columns,
#'   after a match has already been found, should also be returned.
#' @return A tibble containing all rows of the dataframe that matched a query.
#'   The new column `match__` contains the name of the column that matched
#'   the query for this row.
#' @examples
#' query_hgnc(c("FLG", "SGK2"),
#'            c("symbol", "alias_symbol", "prev_symbol"))
#'
#' @export
query_hgnc <- function (query, columns, return_all = FALSE) {
  iterative_select(query, columns, hgnc, return_all = return_all)
}
