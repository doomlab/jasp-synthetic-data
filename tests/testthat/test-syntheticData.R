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
  ret     <- syn_main(results, ds, options = list(variables = "num", seed = 42L, comparisonPlots = FALSE))
  syn     <- get_synthetic(ret, results)
  expect_false(is.null(syn))
  expect_equal(names(syn), "num")
  expect_true(all(syn$num >= 1.0 & syn$num <= 5.0))
})

test_that("syntheticData handles single categorical variable without synthpop", {
  results <- make_results_env()
  ds      <- data.frame(cat = factor(c("x", "y", "x", "y", "x")))
  ret     <- syn_main(results, ds, options = list(variables = "cat", seed = 42L, comparisonPlots = FALSE))
  syn     <- get_synthetic(ret, results)
  expect_false(is.null(syn))
  expect_equal(names(syn), "cat")
  expect_true(all(syn$cat %in% c("x", "y")))
})

test_that("aggregate_synthpop_replicates keeps one whole replicate and honors types", {
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

  for (seed in 1:10) {
    result <- aggregate_fn(synthetic_object, reference, seed = seed)

    # Rows are never mixed across replicates: the result is one replicate intact
    matches <- vapply(replicates, function(r) {
      identical(result$num, r$num) &&
        identical(as.character(result$cat), as.character(r$cat))
    }, logical(1))
    expect_true(any(matches))

    expect_true(is.integer(result$num))
    expect_true(is.factor(result$cat))
    expect_equal(levels(result$cat), c("a", "b"))
  }

  # Same seed, same replicate
  expect_identical(
    aggregate_fn(synthetic_object, reference, seed = 7L),
    aggregate_fn(synthetic_object, reference, seed = 7L)
  )
})

test_that("calibrate_conditional_moments matches within-category means and SDs", {
  calibrate_fn <- getFromNamespace("calibrate_conditional_moments", "jaspSyntheticData")
  set.seed(1)
  reference <- data.frame(
    grp = factor(rep(c("a", "b"), each = 50)),
    x   = c(stats::rnorm(50, mean = 10, sd = 2), stats::rnorm(50, mean = 20, sd = 4))
  )
  synthetic <- data.frame(
    grp = factor(rep(c("a", "b"), each = 40)),
    x   = c(stats::rnorm(40, mean = 12, sd = 1), stats::rnorm(40, mean = 17, sd = 6))
  )

  result <- calibrate_fn(synthetic, reference, cat_cols = "grp", num_cols = "x")

  for (g in c("a", "b")) {
    ref_x <- reference$x[reference$grp == g]
    res_x <- result$x[result$grp == g]
    expect_equal(mean(res_x), mean(ref_x), tolerance = 0.02)
    expect_equal(stats::sd(res_x), stats::sd(ref_x), tolerance = 0.05)
  }

  # Group labels are untouched, and values stay inside the observed range
  expect_identical(result$grp, synthetic$grp)
  expect_true(all(result$x >= min(reference$x) & result$x <= max(reference$x)))
})

utility_dataset <- function(n = 200L) {
  set.seed(1)
  group <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  x     <- stats::rnorm(n)
  data.frame(
    x     = x,
    y     = 2 * x + as.numeric(group) + stats::rnorm(n),
    group = group
  )
}

# Plots can only be rendered inside JASP's graphics backend, so the full
# analysis output is tested through jaspTools; direct calls turn plots off.
run_utility_analysis <- function(ds, extra = list()) {
  testthat::skip_if_not_installed("jaspTools")
  jaspTools::setPkgOption("module.dirs", testthat::test_path("..", ".."))
  # Options are built by hand: jaspTools::analysisOptions() misparses this
  # module's QML (inline `// -> options$...` comments mangle the names).
  opts <- c(list(variables = names(ds), seed = 42L, rowCountMode = "same"), extra)
  attr(opts, "analysisName") <- "syntheticData"
  jaspTools::runAnalysis("syntheticData", ds, opts, view = FALSE, quiet = TRUE)
}

test_that("syntheticData reports per-variable utility and comparison plots", {
  ds  <- utility_dataset()
  res <- run_utility_analysis(ds)
  expect_equal(res$status, "complete")

  utility <- res$results$utilityTable$data
  expect_equal(vapply(utility, `[[`, character(1), "variable"), names(ds))
  expect_true(all(vapply(utility, `[[`, numeric(1), "pMSE") >= 0))
  expect_equal(res$results$comparisonPlots$status, "complete")
  expect_null(res$results$overallUtility)
})

test_that("syntheticData computes overall utility when requested", {
  ds  <- utility_dataset()
  res <- run_utility_analysis(ds, list(overallUtility = TRUE))
  overall <- res$results$overallUtility$data
  expect_length(overall, 1L)
  expect_true(overall[[1]]$pMSE >= 0)
})

test_that("utility options can be turned off", {
  results <- make_results_env()
  ds      <- utility_dataset()
  syn_main(results, ds, options = list(
    variables = names(ds), seed = 42L,
    utilityTable = FALSE, comparisonPlots = FALSE
  ))
  expect_false(exists("utilityTable", envir = results))
  expect_false(exists("comparisonPlots", envir = results))
})

test_that("utility table is computed on the final synthetic data", {
  results <- make_results_env()
  ds      <- utility_dataset()
  syn_main(results, ds, options = list(
    variables = names(ds), seed = 42L, comparisonPlots = FALSE
  ))
  expect_true(exists("utilityTable", envir = results))

  compare_fn <- getFromNamespace("compare_synthetic", "jaspSyntheticData")
  prep_fn    <- getFromNamespace("prepare_utility_data", "jaspSyntheticData")
  prepared   <- prep_fn(ds, results[["synthetic"]])
  cmp        <- compare_fn(prepared$original, prepared$synthetic)
  expect_equal(rownames(cmp$tab.utility), names(ds))
  expect_s3_class(cmp$plots, "ggplot")
})

test_that("compare_synthetic handles a single variable", {
  compare_fn <- getFromNamespace("compare_synthetic", "jaspSyntheticData")
  ds  <- utility_dataset()
  cmp <- compare_fn(ds[, "x", drop = FALSE], ds[sample(nrow(ds)), "x", drop = FALSE])
  expect_equal(rownames(cmp$tab.utility), "x")
})

test_that("parametric synthesis method runs", {
  results <- make_results_env()
  ds      <- utility_dataset()
  syn_main(results, ds, options = list(
    variables = names(ds), seed = 42L, synthpopMethod = "parametric",
    comparisonPlots = FALSE
  ))
  expect_equal(nrow(results[["synthetic"]]), nrow(ds))
})
