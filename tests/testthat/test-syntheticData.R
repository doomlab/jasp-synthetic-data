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


test_that("syntheticData respects custom row count and reuses input values", {
  options <- with_seed_options(1, list(rowCountMode = "custom", n = 5))
  results <- make_results_env()
  syn <- syntheticData(
    jaspResults = results,
    dataset = base_dataset,
    options = options,
    state = make_state()
  )
  syn <- get_synthetic(syn, results)

  expect_equal(nrow(syn), 5)
  expect_equal(colnames(syn), names(base_dataset))
  expect_true(all(syn$num %in% base_dataset$num))
  expect_true(all(as.character(syn$cat) %in% as.character(base_dataset$cat)))
})


test_that("syntheticData uses the input row count when rowCountMode is same", {
  options <- with_seed_options(7)
  results <- make_results_env()
  syn <- syntheticData(
    jaspResults = results,
    dataset = base_dataset,
    options = options,
    state = make_state()
  )
  syn <- get_synthetic(syn, results)

  expect_equal(nrow(syn), nrow(base_dataset))
})

test_that("syntheticData preserves categorical labels when rowCountMode is same", {
  options <- with_seed_options(7)
  results <- make_results_env()
  syn <- syntheticData(
    jaspResults = results,
    dataset = base_dataset,
    options = options,
    state = make_state()
  )
  syn <- get_synthetic(syn, results)

  expect_setequal(
    unique(as.character(syn$cat)),
    unique(as.character(base_dataset$cat))
  )
  expect_equal(table(syn$cat), table(base_dataset$cat))
})

test_that("fallback resampler keeps categorical distribution when n changes", {
  options <- with_seed_options(1, list(rowCountMode = "custom", n = 9, forceSynthpopFallback = TRUE))
  results <- make_results_env()
  syn_val <- expect_warning(
    syntheticData(
      jaspResults = results,
      dataset = base_dataset,
      options = options,
      state = make_state()
    ),
    "forced synthpop fallback"
  )
  syn <- get_synthetic(syn_val, results)

  counts <- as.integer(table(syn$cat))
  expect_true(all(abs(counts - c(7L, 2L)) <= 1L))
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
