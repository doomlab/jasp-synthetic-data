#' Synthetic data from categorical variables
#'
#' This analysis inspects the selected variables of the active JASP dataset,
#' reports whether each column is treated as continuous or categorical, and
#' generates synthetic data using the synthpop package. Categorical variables
#' are properly modelled via CART (or the user-selected method) and aggregated
#' across replicates by randomly selecting one replicate per categorical column,
#' preserving both marginal distributions and inter-variable associations.
#'
#' Expected options (from QML):
#' - variables: character vector of column names to use
#' - n: desired number of synthetic rows (0 means match input n)
#' - seed: RNG seed for reproducibility
#' - rowCountMode: "same" to mirror the input row count or "custom" to respect the supplied `n`
#'
#' Returns a JASP results object with tables for types and preview, or a data.frame if
#' jaspBase is not available (for headless testing).
#' @keywords internal
#' @export

`%||%` <- function(a, b) if (!is.null(a)) a else b

clamp_numeric <- function(values, reference) {
  if (length(values) == 0L || all(is.na(reference))) {
    return(values)
  }
  min_val <- suppressWarnings(min(reference, na.rm = TRUE))
  max_val <- suppressWarnings(max(reference, na.rm = TRUE))
  if (is.finite(min_val) && is.finite(max_val)) {
    values <- pmin(pmax(values, min_val), max_val)
  }
  values
}

#' Generate synthetic data for a single column without synthpop.
#'
#' - Categorical / factor / character: sample with observed category probabilities.
#' - Integer: round-sample from Normal(mean, sd), clamped to observed [min, max].
#' - Numeric: sample from Normal(mean, sd), clamped to observed [min, max].
synthesize_single_column <- function(col_data, n, seed = 123L) {
  set.seed(seed)
  values <- col_data[!is.na(col_data)]

  if (is.factor(col_data) || is.character(col_data) || is.logical(col_data)) {
    tbl   <- table(values)
    probs <- as.numeric(tbl) / sum(tbl)
    out   <- sample(names(tbl), size = n, replace = TRUE, prob = probs)
    if (is.factor(col_data)) {
      out <- factor(out, levels = levels(col_data), ordered = is.ordered(col_data))
    } else if (is.logical(col_data)) {
      out <- as.logical(out)
    }
    return(out)
  }

  mu  <- mean(values, na.rm = TRUE)
  sdev <- stats::sd(values, na.rm = TRUE)
  lo  <- min(values, na.rm = TRUE)
  hi  <- max(values, na.rm = TRUE)

  if (is.na(sdev) || sdev == 0) {
    out <- rep(mu, n)
  } else {
    out <- stats::rnorm(n, mean = mu, sd = sdev)
  }
  out <- pmin(pmax(out, lo), hi)

  if (is.integer(col_data)) as.integer(round(out)) else out
}

#' Prepare a data.frame for synthpop
#'
#' Coerces character columns to factors so synthpop treats them as proper
#' categorical variables (using polyreg / cart rather than pmm).
#' Ordered factors are preserved as-is.
prepare_for_synthpop <- function(dat) {
  for (col in names(dat)) {
    if (is.character(dat[[col]])) {
      dat[[col]] <- factor(dat[[col]])
    }
  }
  dat
}

#' Calibrate synthetic numeric values to match observed conditional means/SDs.
#'
#' For every continuous numeric column, and for every level of every categorical
#' column present in `result`, this function applies a per-cell location-scale
#' transform so that the synthetic within-category mean and SD match those of
#' the original data.
#'
#' Why this is necessary
#' ---------------------
#' Synthpop fits a sequential conditional model, so each replicate internally
#' preserves group-level structure. But once we pick a single replicate (the
#' right thing to do for categorical integrity), sampling noise means the
#' within-group moments in that replicate may drift from the originals.
#' Averaging across replicates would fix the moments but sever the row-level
#' cat→num relationship. Calibration fixes the moments *without* touching the
#' pairing of rows to groups.
#'
#' Transform applied per (numeric col, category level) cell
#' ---------------------------------------------------------
#'   x_cal = (x_syn - mean_syn) / sd_syn * sd_ref + mean_ref
#'
#' Edge cases:
#'   - Cell n < 2 or sd_syn ≈ 0: shift-only (mean matched, spread unchanged).
#'   - Cell absent in reference: values left as-is.
#'   - Result is clamped to the global observed range of the numeric column.
#'
#' @param result    Synthetic data.frame (one chosen replicate).
#' @param reference Original data.frame.
#' @param cat_cols  Character vector of categorical column names to condition on.
#' @param num_cols  Character vector of continuous numeric column names to adjust.
calibrate_conditional_moments <- function(result, reference, cat_cols, num_cols) {
  if (length(cat_cols) == 0L || length(num_cols) == 0L) {
    return(result)
  }

  # Build a single grouping key per row by pasting all category values together.
  # This handles any number of categorical columns simultaneously.
  make_key <- function(df) {
    if (length(cat_cols) == 1L) {
      as.character(df[[cat_cols]])
    } else {
      apply(df[, cat_cols, drop = FALSE], 1, paste, collapse = " ")
    }
  }

  ref_keys <- make_key(reference)
  syn_keys <- make_key(result)

  for (num_col in num_cols) {
    ref_vals <- reference[[num_col]]
    syn_vals <- as.numeric(result[[num_col]])

    global_min <- suppressWarnings(min(ref_vals, na.rm = TRUE))
    global_max <- suppressWarnings(max(ref_vals, na.rm = TRUE))

    unique_keys <- unique(syn_keys[!is.na(syn_keys)])

    for (key in unique_keys) {
      syn_idx <- which(syn_keys == key)
      ref_idx <- which(ref_keys == key)

      if (length(ref_idx) == 0L) next  # cell not seen in reference → leave as-is

      ref_cell <- ref_vals[ref_idx]
      syn_cell <- syn_vals[syn_idx]

      ref_mean <- mean(ref_cell, na.rm = TRUE)
      syn_mean <- mean(syn_cell, na.rm = TRUE)

      if (is.nan(ref_mean) || is.nan(syn_mean)) next

      ref_sd <- if (length(ref_idx) >= 2L) stats::sd(ref_cell, na.rm = TRUE) else NA_real_
      syn_sd <- if (length(syn_idx) >= 2L) stats::sd(syn_cell, na.rm = TRUE) else NA_real_

      can_scale <- !is.na(ref_sd) && !is.na(syn_sd) &&
                   is.finite(ref_sd) && is.finite(syn_sd) &&
                   syn_sd > .Machine$double.eps

      if (can_scale) {
        calibrated <- (syn_cell - syn_mean) / syn_sd * ref_sd + ref_mean
      } else {
        # SD unavailable or zero: shift only
        calibrated <- syn_cell - syn_mean + ref_mean
      }

      # Clamp to global observed range
      if (is.finite(global_min) && is.finite(global_max)) {
        calibrated <- pmin(pmax(calibrated, global_min), global_max)
      }

      syn_vals[syn_idx] <- calibrated
    }

    result[[num_col]] <- syn_vals
  }

  result
}

#' Aggregate multiple synthpop replicates into a single synthetic dataset.
#'
#' Strategy
#' --------
#' 1. Pick ONE replicate at random. This keeps every row internally consistent:
#'    the categorical label and all numeric values on that row come from the
#'    same synthpop draw, so the row-level cat→num relationship is intact.
#'    (Averaging numerics across replicates while drawing categoricals from one
#'    replicate would sever that relationship — row 47's group label and its
#'    numeric values would no longer belong together.)
#'
#' 2. Apply conditional moment calibration. Sampling noise in a single replicate
#'    means within-group means/SDs may drift from the originals. We correct that
#'    with a per-(group, numeric-col) location-scale transform that matches the
#'    observed conditional moments without disturbing which row belongs to which
#'    group. See `calibrate_conditional_moments` for details.
#'
#' 3. Cast columns back to their original R types (integer, ordered factor, …).
#'
#' @param syn_object  Return value of `synthpop::syn()`.
#' @param reference   The original data.frame (type metadata + clamping ranges).
#' @param cat_cols    Names of categorical columns to condition on during calibration.
#' @param num_cols    Names of continuous numeric columns to calibrate.
#' @param seed        Integer seed for reproducible replicate selection.
aggregate_synthpop_replicates <- function(syn_object, reference,
                                          cat_cols = character(),
                                          num_cols = character(),
                                          seed = 123L) {
  replicates <- syn_object$syn
  if (is.data.frame(replicates)) {
    replicates <- list(replicates)
  }
  if (length(replicates) == 0L) {
    return(reference[0, , drop = FALSE])
  }

  n_rows <- vapply(replicates, nrow, integer(1))
  if (length(unique(n_rows)) != 1L) {
    stop("Synthpop returned replicates with inconsistent row counts.")
  }
  n_reps <- length(replicates)

  # --- Step 1: pick one coherent replicate -----------------------------------
  set.seed(seed)
  result <- replicates[[sample.int(n_reps, 1L)]]

  # --- Step 2: fix column types to match reference ---------------------------
  for (col in names(reference)) {
    reference_col <- reference[[col]]
    raw_vals <- result[[col]]

    if (is.factor(reference_col)) {
      result[[col]] <- factor(
        as.character(raw_vals),
        levels  = levels(reference_col),
        ordered = is.ordered(reference_col)
      )
    } else if (is.logical(reference_col)) {
      result[[col]] <- as.logical(as.character(raw_vals))
    } else if (is.character(reference_col)) {
      result[[col]] <- factor(
        as.character(raw_vals),
        levels = levels(factor(reference_col))
      )
    } else if (is.integer(reference_col)) {
      result[[col]] <- as.integer(round(as.numeric(raw_vals)))
    } else if (is.numeric(reference_col)) {
      result[[col]] <- as.numeric(raw_vals)
    }
  }

  # --- Step 3: calibrate within-category conditional moments ----------------
  result <- calibrate_conditional_moments(result, reference, cat_cols, num_cols)

  result
}

syntheticData <- function(jaspResults, dataset, options, state, ...) {
  requestedCols <- options$variables
  if (is.list(requestedCols))
    requestedCols <- unlist(requestedCols, use.names = FALSE)
  if (is.null(requestedCols))
    requestedCols <- character(0)
  requestedCols <- trimws(requestedCols)
  requestedCols <- requestedCols[nzchar(requestedCols)]

  if (length(requestedCols) == 0L) {
    if (requireNamespace("jaspBase", quietly = TRUE)) {
      placeholder <- jaspBase::createJaspTable(title = "Variable Types")
      placeholder$addColumnInfo(name = "info", title = "Info", type = "string")
      placeholder$setData(data.frame(info = "No variables selected. Please select at least one variable."))
      jaspResults[["variableTypes"]] <- placeholder
    }
    return(invisible())
  }

  # --- 1) Get data -----------------------------------------------------------
  if (missing(dataset) || is.null(dataset)) {
    if (requireNamespace("jaspBase", quietly = TRUE)) {
      cols <- try(requestedCols, silent = TRUE)
      dataset <- jaspBase::readDatasetToEnd(
        columns = if (!inherits(cols, "try-error") && length(cols) > 0) cols else NULL
      )
    } else {
      stop("Dataset not provided and jaspBase not available.")
    }
  }

  selectedCols <- if (length(requestedCols) > 0) requestedCols else names(dataset)
  selectedCols <- intersect(selectedCols, names(dataset))
  dat <- dataset[, selectedCols, drop = FALSE]
  if (ncol(dat) > 0L) {
    names(dat) <- selectedCols
  }

  hasSynthpop <- requireNamespace("synthpop", quietly = TRUE)
  if (!hasSynthpop) {
    stop("The synthpop package is required to generate synthetic data.")
  }

  if (missing(state) || is.null(state) || !is.environment(state)) {
    state <- new.env(parent = emptyenv())
  }

  # --- 2) Classify variables -------------------------------------------------
  if (ncol(dat) == 0L) {
    typeInfo <- data.frame(variable = character(), type = character(), stringsAsFactors = FALSE)
  } else {
    isNum <- vapply(dat, is.numeric, FUN.VALUE = logical(1))
    typeInfo <- data.frame(
      variable = names(dat),
      type     = ifelse(isNum, "Continuous", "Categorical"),
      stringsAsFactors = FALSE
    )
  }

  # --- 3) Generate synthetic data --------------------------------------------
  seed     <- options$seed %||% 123L
  rowCountMode <- options$rowCountMode %||% "same"
  if (identical(rowCountMode, "same")) {
    n_target <- nrow(dat)
  } else {
    n_target <- options$n %||% options$nRows %||% nrow(dat)
    n_target <- suppressWarnings(as.integer(n_target))
    if (is.na(n_target) || n_target <= 0L)
      n_target <- nrow(dat)
  }

  if (ncol(dat) == 1L) {
    # Single-variable path: no synthpop needed
    if (nrow(dat) == 0L) {
      syn <- dat[0, , drop = FALSE]
    } else {
      col_name <- names(dat)[[1L]]
      syn_col  <- synthesize_single_column(dat[[1L]], n = n_target, seed = seed)
      syn      <- data.frame(syn_col, stringsAsFactors = FALSE)
      names(syn) <- col_name
    }
  } else if (nrow(dat) == 0L) {
    syn <- dat[0, , drop = FALSE]
  } else {
    set.seed(seed)

    # -- Identify column roles ------------------------------------------------
    numericCols <- names(dat)[vapply(dat, is.numeric, FUN.VALUE = logical(1))]

    minDiscreteLevels <- options$minDiscreteLevels %||% 5L
    minDiscreteLevels <- suppressWarnings(as.integer(minDiscreteLevels))
    if (is.na(minDiscreteLevels) || minDiscreteLevels < 1L) {
      minDiscreteLevels <- 5L
    }

    # FIX: was `dat[numericCols, drop=FALSE]` (row subsetting) — must be
    #      `dat[, numericCols, drop=FALSE]` to subset columns.
    numericSubset <- dat[, numericCols, drop = FALSE]
    discreteNumericCols <- names(Filter(function(x) {
      values <- x[!is.na(x)]
      length(unique(values)) <= minDiscreteLevels
    }, numericSubset))

    numericCols    <- setdiff(numericCols, discreteNumericCols)
    categoricalCols <- setdiff(names(dat), numericCols)  # includes discreteNumericCols

    n_simulations <- options$nSimulations %||% options$m %||% 5L
    n_simulations <- suppressWarnings(as.integer(n_simulations))
    if (is.na(n_simulations) || n_simulations <= 0L) {
      n_simulations <- 1L
    }

    method_input <- options$synthpopMethod %||% options$synthpopMethods %||% "cart"
    if (!is.character(method_input)) {
      method_input <- "cart"
    }
    method <- rep(method_input, length.out = ncol(dat))

    # FIX: coerce character columns → factors before calling synthpop so that
    #      the package fits an appropriate categorical model (polyreg / cart)
    #      rather than treating strings as continuous via pmm.
    dat_for_syn <- prepare_for_synthpop(dat)

    synthpop_args <- list(
      data          = dat_for_syn,
      m             = n_simulations,
      k             = n_target,
      seed          = seed,
      print.flag    = FALSE,
      drop.not.used = FALSE,
      drop.pred.only = FALSE,
      method        = method
    )

    min_num_levels <- options$minnumlevels %||% options$minNumLevels
    min_num_levels <- suppressWarnings(as.integer(min_num_levels))
    if (length(min_num_levels) == 1L && !is.na(min_num_levels) && min_num_levels > 0L) {
      synthpop_args$minnumlevels <- min_num_levels
    }

    max_fac_levels <- options$maxfaclevels %||% options$maxFacLevels
    max_fac_levels <- suppressWarnings(as.integer(max_fac_levels))
    if (length(max_fac_levels) == 1L && !is.na(max_fac_levels) && max_fac_levels >= 0L) {
      synthpop_args$maxfaclevels <- max_fac_levels
    }

    synthpop_call <- do.call(synthpop::syn, synthpop_args)

    # Pass original `dat` (not dat_for_syn) as reference so type metadata
    # (integer, ordered factor, logical) round-trips correctly.
    # cat_cols / num_cols allow calibrate_conditional_moments to correct
    # within-category means and SDs on the chosen replicate.
    syn <- aggregate_synthpop_replicates(
      synthpop_call, dat,
      cat_cols = categoricalCols,
      num_cols = numericCols,
      seed     = seed
    )

    if (ncol(syn) > 0L) {
      names(syn) <- selectedCols
    }

    # -- Snap discrete-numeric columns back to observed values ----------------
    if (length(discreteNumericCols) > 0L) {
      for (col in discreteNumericCols) {
        if (!(col %in% names(syn))) next
        allowed <- sort(unique(dat[[col]][!is.na(dat[[col]])]))
        if (length(allowed) == 0L) {
          syn[[col]] <- NA_real_
          next
        }
        mapped <- vapply(syn[[col]], function(value) {
          if (is.na(value)) return(NA_real_)
          allowed[which.min(abs(value - allowed))]
        }, numeric(1))
        if (is.integer(dat[[col]])) {
          mapped <- as.integer(mapped)
        }
        syn[[col]] <- mapped
      }
    }

    # -- Optional jitter on continuous numeric columns ------------------------
    jitterFraction <- options$jitterFraction %||% 0
    jitterFraction <- suppressWarnings(as.numeric(jitterFraction))
    if (is.na(jitterFraction) || jitterFraction < 0)
      jitterFraction <- 0
    if (jitterFraction > 0 && length(numericCols) > 0) {
      colSd <- vapply(
        dat[, numericCols, drop = FALSE],
        function(x) stats::sd(x, na.rm = TRUE),
        numeric(1)
      )
      colSd[is.na(colSd)] <- 0
      for (col in numericCols) {
        sd_val <- colSd[col]
        if (is.na(sd_val) || sd_val <= 0) next
        noise <- stats::rnorm(n = n_target, mean = 0, sd = jitterFraction * sd_val)
        syn[[col]] <- syn[[col]] + noise
      }
    }
  }

  # --- 4) Return results / preview ------------------------------------------
  if (requireNamespace("jaspBase", quietly = TRUE)) {
    preview <- jaspBase::createJaspTable(title = "Variable Types")
    preview$addColumnInfo(name = "variable", title = "Variable", type = "string")
    preview$addColumnInfo(name = "type",     title = "Type",     type = "string")
    preview$setData(typeInfo)
    jaspResults[["variableTypes"]]   <- preview
    jaspResults[["syntheticTypes"]]  <- typeInfo

    synPreview <- jaspBase::createJaspTable(title = "Synthetic Data Preview (first 10 rows)")
    if (ncol(syn) == 0L) {
      synPreview$addColumnInfo(name = "info", title = "Info", type = "string")
      synPreview$setData(data.frame(info = "No columns selected."))
    } else {
      for (nm in names(syn)) {
        synPreview$addColumnInfo(name = nm, title = nm, type = "string")
      }
      headRows <- utils::head(syn, 10)
      headRows[] <- lapply(headRows, function(x) if (is.factor(x)) as.character(x) else x)
      synPreview$setData(headRows)
    }
    jaspResults[["syntheticPreview"]] <- synPreview

    createSynDataset <- if (exists("createJaspDataset",
                                   envir    = asNamespace("jaspBase"),
                                   inherits = FALSE)) {
      jaspBase:::createJaspDataset
    } else {
      NULL
    }
    if (!is.null(createSynDataset)) {
      synDataset <- createSynDataset(title = "Synthetic Dataset", data = syn)
      jaspResults[["syntheticData"]] <- synDataset
    }
    jaspResults[["synthetic"]] <- syn

    sanitizeExportPath <- function(path) {
      clean <- path
      clean <- sub("^file://localhost", "", clean)
      if (startsWith(clean, "file://")) {
        clean <- substring(clean, nchar("file://") + 1L)
      } else if (startsWith(clean, "file:/")) {
        clean <- substring(clean, nchar("file:/") + 1L)
      }
      if (.Platform$OS.type == "windows" && grepl("^/[A-Za-z]:", clean))
        clean <- substring(clean, 2L)
      clean <- utils::URLdecode(clean)
      normalizePath(clean, winslash = "/", mustWork = FALSE)
    }

    exportPath <- trimws(options$fileFull %||% state$fileFull %||% "")
    if (nzchar(exportPath)) {
      exportPath <- sanitizeExportPath(exportPath)
      state$fileFull    <- exportPath
      state$lastSavePath <- exportPath
      exportError <- NULL
      tryCatch({
        exportDir <- dirname(exportPath)
        if (nzchar(exportDir) && !dir.exists(exportDir))
          dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)
        exportSyn <- syn
        if (ncol(exportSyn) > 0L) {
          decodedNames <- tryCatch(
            jaspBase::decodeColNames(names(exportSyn)),
            error = function(e) names(exportSyn)
          )
          names(exportSyn) <- decodedNames
        }
        utils::write.csv(exportSyn, file = exportPath, row.names = FALSE)
      }, error = function(e) {
        exportError <<- conditionMessage(e)
      })
      if (!is.null(exportError)) {
        exportBlock <- jaspBase::createJaspHtml(
          title = "Export error",
          text  = paste("Failed to save synthetic dataset:", exportError)
        )
        jaspResults[["syntheticExportError"]] <- exportBlock
      }
    } else if (!is.null(state$fileFull) && nzchar(state$fileFull)) {
      state$lastSavePath <- state$fileFull
    }

    return(invisible())
  }

  syn
}
