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
