context("hgnc")

test_that("select symbol, alias symbol", {
  hits <- iterative_select(c("CDK1", "MT-CO1", "DAC", "doesnt_exist"),
                           c("symbol", "alias_symbol"),
                           hgnc)
  expect_equal(nrow(hits), 4)
  expect_equal(ncol(hits), 52)
  expect_type(hits$alias_symbol, "list")
  expect_true(is.na(hits[4, "hgnc_id"]))
})
