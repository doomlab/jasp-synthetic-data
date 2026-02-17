library(testthat)

base_dataset <- data.frame(
  num = c(1, 2, 3),
  cat = factor(c("a", "b", "a")),
  stringsAsFactors = FALSE
)


with_seed_options <- function(seed, extra = list()) {
  c(list(seed = seed, variables = names(base_dataset)), extra)
}


test_that("syntheticData respects custom row count and reuses input values", {
  options <- with_seed_options(1, list(rowCountMode = "custom", n = 5))
  syn <- syntheticData(
    jaspResults = make_results_env(),
    dataset = base_dataset,
    options = options,
    state = make_state()
  )

  expect_equal(nrow(syn), 5)
  expect_equal(colnames(syn), names(base_dataset))
  expect_true(all(syn$num %in% base_dataset$num))
  expect_true(all(as.character(syn$cat) %in% as.character(base_dataset$cat)))
})


test_that("syntheticData uses the input row count when rowCountMode is same", {
  options <- with_seed_options(7)
  syn <- syntheticData(
    jaspResults = make_results_env(),
    dataset = base_dataset,
    options = options,
    state = make_state()
  )

  expect_equal(nrow(syn), nrow(base_dataset))
})
