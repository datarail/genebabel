#' List all databases included in the package
#'
#' @return A tibble listing all databases.
#'
#' @export
databases <- function() {
  tibble::tribble(
    ~id, ~description,
    "hgnc", "IDs and names of all genes in the HUGO Gene Naming Consortium (HGNC)"
  )
}

#' Query the HUGO Gene Nomenclature Commitee approved symbol database
#'
#' `query_hgnc()` finds matches for the supplied query vector in the specified
#' columns of the database and returns a tibble of all the matches.
#'
#' Find which columns are available by checking `help(hgnc)`.
#'
#' @inheritParams iterative_select
#' @return A tibble containing all rows of the dataframe that matched a query.
#'   The new column `match__` contains the name of the column that matched
#'   the query for this row.
#' @examples
#' query_hgnc(c("FLG", "SGK2"),
#'            c("symbol", "alias_symbol", "prev_symbol"))
#'
#' @export
query_hgnc <- function (query, match_cols, return_all = FALSE) {
  iterative_select(query, genebabel::hgnc, match_cols, return_all = return_all)
}

#' Join query dataframe with results from  the HUGO Gene Naming Consortium
#' approved symbol database
#'
#' Find which columns are available by checking `help(hgnc)`.
#'
#' @inheritParams join_results
#' @return The input dataframe merged with the selected matching columns from
#'   the HUGO database.
#' @examples
#' d <- data.frame(a = 1:3, b = c("FLG", "SGK2", "CDK1"))
#' join_hgnc(d, "b",
#'   match_cols = c("symbol", "alias_symbol", "prev_symbol"),
#'   select_cols = c("entrez_id", "symbol", "refseq_accession"))
#'
#' @export
join_hgnc <- function (df, query_col, match_cols, select_cols = NULL) {
  join_results(df, query_col, genebabel::hgnc, match_cols, select_cols = select_cols)
}
