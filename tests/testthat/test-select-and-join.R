context("Select and join functions")

test_that("select symbol, alias symbol", {
  expect_warning(
    hits <- iterative_select(
      c("CDK1", "MT-CO1", "DAC", "doesnt_exist"),
      hgnc,
      c("symbol", "alias_symbol"))
  )
  expect_equal(nrow(hits), 4)
  expect_equal(ncol(hits), 52)
  expect_type(hits$alias_symbol, "list")
  expect_true(is.na(hits[[4, "hgnc_id"]]))
})

test_that("join_hgnc", {
  q <- tibble::tibble(
    id = c("CDK1", "MT-CO1", "DAC", "doesnt_exist"),
    test = 1:4
  )
  expect_warning(
    hits <- join_results(
      q,
      "id",
      hgnc,
      c("symbol", "alias_symbol"),
       select_cols = c("entrez_id", "vega_id")
    )
  )
  expect_equal(nrow(hits), 4)
  expect_equal(ncol(hits), 4)
  expect_true(is.na(hits[[4, "entrez_id"]]))
})

test_that("Empty and ambigous hits create warnings", {
  expect_warning(
    iterative_select(
      c("CDK1", "doesnt_exist"),
      hgnc,
      c("symbol", "alias_symbol")
    ),
    "No matches found.+doesnt_exist"
  )
  expect_warning(
    iterative_select(
      c("ACSM2"),
      hgnc,
      c("symbol", "prev_symbol")
    ),
    "Multiple matches.+ACSM2"
  )
})
