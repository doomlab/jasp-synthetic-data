

#' Synthetic Data generation using faux
#'
#' This analysis generates a synthetic dataset from the selected variables
#' of the active JASP dataset using the {faux} package. Numeric variables
#' are simulated via a multivariate normal with empirical mean/SD/correlation
#' estimated from the data; categorical variables are resampled from their
#' observed marginal distributions.
#'
#' Expected options (from QML):
#' - variables: character vector of column names to use
#' - n: integer synthetic sample size (defaults to nrow of input)
#' - seed: integer RNG seed
#'
#' Returns a JASP results object with a preview table, or a data.frame if
#' jaspBase is not available (for headless testing).
#'
#' @importFrom stats cor sd
#' @importFrom utils head
#' @keywords internal

`%||%` <- function(a, b) if (!is.null(a)) a else b

syntheticData <- function(dataset, options, state, ...) {
  # --- 1) Get data -----------------------------------------------------------
  if (missing(dataset) || is.null(dataset)) {
    if (requireNamespace("jaspBase", quietly = TRUE)) {
      # read only the needed columns if specified
      cols <- try(options$variables, silent = TRUE)
      dataset <- jaspBase::.readDataSetToEnd(columns = if (!inherits(cols, "try-error")) cols else NULL)
    } else {
      stop("Dataset not provided and jaspBase not available.")
    }
  }

  cols <- options$variables %||% names(dataset)
  dat  <- dataset[, cols, drop = FALSE]

  set.seed(options$seed %||% 123)
  n_out <- as.integer(options$n %||% nrow(dat))

  # --- 2) Split numeric / categorical --------------------------------------
  isNum <- vapply(dat, is.numeric, TRUE)
  num   <- dat[, isNum, drop = FALSE]

  # --- 3) Generate numeric columns with faux::rnorm_multi -------------------
  syn_num <- NULL
  if (ncol(num) > 0) {
    if (!requireNamespace("faux", quietly = TRUE)) {
      stop("The 'faux' package is required. Please add it to your module's DESCRIPTION and install it.")
    }

    mu <- vapply(num, function(x) mean(x, na.rm = TRUE), numeric(1))
    sd <- vapply(num, function(x) stats::sd(x, na.rm = TRUE), numeric(1))

    # Handle the 1-column case where cor() returns length-1
    if (ncol(num) == 1L) {
      R <- matrix(1, nrow = 1, ncol = 1, dimnames = list(colnames(num), colnames(num)))
    } else {
      R <- stats::cor(num, use = "pairwise.complete.obs")
      # ensure positive-definiteness fallback: replace NA/NaN with 0 and diag=1
      R[is.na(R)] <- 0
      diag(R) <- 1
    }

    syn_num <- faux::rnorm_multi(
      n         = n_out,
      mu        = mu,
      sd        = sd,
      r         = R,
      varnames  = colnames(num),
      empirical = TRUE
    )

    syn_num <- as.data.frame(syn_num)
  }

  # --- 4) Generate categorical columns by resampling marginals --------------
  syn <- if (!is.null(syn_num)) syn_num else data.frame(row = seq_len(n_out))
  catCols <- names(dat)[!isNum]
  if (length(catCols)) {
    for (cn in catCols) {
      x  <- dat[[cn]]
      # keep original levels order if factor/character
      lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
      tab <- table(as.character(x), useNA = "no")
      p   <- as.numeric(tab) / sum(tab)
      lv_tab <- names(tab)
      sampled <- sample(lv_tab, size = n_out, replace = TRUE, prob = p)
      # cast back to factor with original levels if possible
      if (!is.null(lv)) {
        syn[[cn]] <- factor(sampled, levels = lv)
      } else {
        syn[[cn]] <- sampled
      }
    }
  }

  # Remove helper column if we created it
  if ("row" %in% names(syn) && !"row" %in% names(dat)) syn$row <- NULL

  # --- 5) Return results / preview -----------------------------------------
  if (requireNamespace("jaspBase", quietly = TRUE)) {
    results <- jaspBase::createJaspResults()

    preview <- jaspBase::createJaspTable(title = "Synthetic Data Preview (first 10 rows)")
    # Dynamically add column metadata
    if (ncol(syn) == 0L) {
      preview$addColumnInfo(name = "info", title = "Info", type = "string")
      preview$setData(data.frame(info = "No columns generated."))
    } else {
      for (nm in names(syn)) {
        # use 'string' for simplicity; JASP will render fine for numeric too
        preview$addColumnInfo(name = nm, title = nm, type = "string")
      }
      headRows <- utils::head(syn, 10)
      # Convert factors to characters for table rendering
      headRows[] <- lapply(headRows, function(x) if (is.factor(x)) as.character(x) else x)
      preview$setData(headRows)
    }

    results$add(preview)
    # store the full synthetic data in results state for later use (plots, downloads)
    results$set("synthetic", syn)

    return(results)
  }

  # Fallback: return the data.frame directly (useful for testing outside JASP)
  syn
}
