library(testthat)

base_dataset <- data.frame(
  num = c(1, 2, 3),
  cat = factor(c("a", "b", "a")),
  stringsAsFactors = FALSE
)


make_results_env <- function() new.env(parent = emptyenv())
make_state <- function() new.env(parent = emptyenv())


get_synthetic <- function(syn, results) {
  if (!is.null(syn)) {
    return(syn)
  }
  if (!is.null(results) && exists("synthetic", envir = results)) {
    return(results[["synthetic"]])
  }
  NULL
}


with_seed_options <- function(seed, extra = list()) {
  c(list(seed = seed, variables = names(base_dataset)), extra)
}

syn_single <- function(col_data, n = length(col_data), seed = 1L) {
  getFromNamespace("synthesize_single_column", "jaspSyntheticData")(col_data, n = n, seed = seed)
}

test_that("synthesize_single_column: categorical samples observed levels with replacement", {
  col <- factor(c("a", "a", "b"), levels = c("a", "b"))
  out <- syn_single(col, n = 200L)
  expect_true(is.factor(out))
  expect_equal(levels(out), c("a", "b"))
  expect_true(all(out %in% c("a", "b")))
  expect_true("a" %in% out && "b" %in% out)
})

test_that("synthesize_single_column: integer stays integer within bounds", {
  col <- c(1L, 2L, 3L, 4L, 5L)
  out <- syn_single(col, n = 100L)
  expect_true(is.integer(out))
  expect_true(all(out >= 1L & out <= 5L))
})

test_that("synthesize_single_column: numeric stays within observed range", {
  col <- c(0.0, 1.0, 2.0, 3.0)
  out <- syn_single(col, n = 100L)
  expect_true(is.numeric(out))
  expect_true(all(out >= 0.0 & out <= 3.0))
})

test_that("synthesize_single_column: constant column returns constant output", {
  col <- c(7.0, 7.0, 7.0)
  out <- syn_single(col, n = 10L)
  expect_true(all(out == 7.0))
})

syn_main <- getFromNamespace("syntheticData", "jaspSyntheticData")

test_that("syntheticData returns early with no variables selected", {
  results <- make_results_env()
  ret <- syn_main(results, base_dataset, options = list(variables = character(0)))
  expect_null(ret)
  expect_true(exists("variableTypes", envir = results))
})

test_that("syntheticData handles single numeric variable without synthpop", {
  results <- make_results_env()
  ds      <- data.frame(num = c(1.0, 2.0, 3.0, 4.0, 5.0))
  ret     <- syn_main(results, ds, options = list(variables = "num", seed = 42L))
  syn     <- get_synthetic(ret, results)
  expect_false(is.null(syn))
  expect_equal(names(syn), "num")
  expect_true(all(syn$num >= 1.0 & syn$num <= 5.0))
})

test_that("syntheticData handles single categorical variable without synthpop", {
  results <- make_results_env()
  ds      <- data.frame(cat = factor(c("x", "y", "x", "y", "x")))
  ret     <- syn_main(results, ds, options = list(variables = "cat", seed = 42L))
  syn     <- get_synthetic(ret, results)
  expect_false(is.null(syn))
  expect_equal(names(syn), "cat")
  expect_true(all(syn$cat %in% c("x", "y")))
})

test_that("aggregate_synthpop_replicates averages numerics and honors factors", {
  replicates <- list(
    data.frame(
      num = c(1L, 2L),
      cat = factor(c("a", "b"), levels = c("a", "b")),
      stringsAsFactors = FALSE
    ),
    data.frame(
      num = c(3L, 4L),
      cat = factor(c("a", "a"), levels = c("a", "b")),
      stringsAsFactors = FALSE
    )
  )
  reference <- data.frame(
    num = c(1L, 4L),
    cat = factor(c("a", "b"), levels = c("a", "b")),
    stringsAsFactors = FALSE
  )
  synthetic_object <- structure(list(syn = replicates, m = length(replicates)), class = "synds")
  aggregate_fn <- getFromNamespace("aggregate_synthpop_replicates", "jaspSyntheticData")
  averaged <- aggregate_fn(synthetic_object, reference)

  expect_equal(averaged$num, c(2L, 3L))
  expect_true(is.factor(averaged$cat))
  expect_equal(as.character(averaged$cat), c("a", "a"))
  expect_equal(levels(averaged$cat), c("a", "b"))
})
