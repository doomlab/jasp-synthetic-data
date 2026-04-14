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

#' Aggregate multiple synthpop replicates into a single synthetic dataset.
#'
#' Strategy by column type:
#'
#'   Numeric (continuous): row-wise mean across replicates, then clamped to the
#'   observed range and rounded/cast back to integer if the original was integer.
#'
#'   Categorical (factor, character, logical): one replicate is chosen **at
#'   random** per column. Majority-voting is intentionally avoided because it
#'   compresses the marginal distribution toward the modal category and breaks
#'   inter-variable associations. Drawing from a single replicate preserves the
#'   joint distribution that synthpop's sequential model learned.
#'
#' @param syn_object  Return value of `synthpop::syn()`.
#' @param reference   The original data.frame (used for type metadata and
#'                    clamping ranges).
#' @param seed        Integer seed so the replicate selection is reproducible.
aggregate_synthpop_replicates <- function(syn_object, reference, seed = 123L) {
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
  k <- n_rows[1]
  n_reps <- length(replicates)

  # Start with a copy of the first replicate so column order / types are kept
  result <- replicates[[1]]

  set.seed(seed)

  for (col in names(reference)) {
    reference_col <- reference[[col]]

    if (is.numeric(reference_col)) {
      # ------------------------------------------------------------------ #
      # Continuous: average across all replicates, then clamp & cast        #
      # ------------------------------------------------------------------ #
      matrix_vals <- vapply(
        replicates,
        function(df) as.numeric(df[[col]]),
        numeric(k)
      )
      if (is.matrix(matrix_vals)) {
        row_mean <- rowMeans(matrix_vals, na.rm = TRUE)
      } else {
        row_mean <- matrix_vals  # single replicate edge-case
      }
      row_mean[is.nan(row_mean)] <- NA_real_
      row_mean <- clamp_numeric(row_mean, reference_col)
      if (is.integer(reference_col)) {
        row_mean <- as.integer(round(row_mean))
      }
      result[[col]] <- row_mean

    } else {
      # ------------------------------------------------------------------ #
      # Categorical: pick ONE replicate at random for this column            #
      #                                                                      #
      # Why not majority-vote?                                               #
      #   - Voting shrinks the distribution toward the mode, inflating its   #
      #     frequency at the expense of minority categories.                 #
      #   - It breaks correlations between columns because each column       #
      #     independently picks its "winner" row-by-row.                    #
      #   - A single synthpop replicate already reflects the full joint      #
      #     distribution; drawing from it preserves that structure.          #
      # ------------------------------------------------------------------ #
      chosen_rep <- replicates[[sample.int(n_reps, 1L)]]
      raw_vals   <- as.character(chosen_rep[[col]])

      if (is.factor(reference_col)) {
        result[[col]] <- factor(
          raw_vals,
          levels  = levels(reference_col),
          ordered = is.ordered(reference_col)
        )
      } else if (is.logical(reference_col)) {
        result[[col]] <- as.logical(raw_vals)
      } else {
        # character: return as factor to match prepare_for_synthpop coercion
        result[[col]] <- factor(raw_vals, levels = levels(factor(reference_col)))
      }
    }
  }

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

  # --- 3) Generate synthetic data via synthpop -------------------------------
  if (nrow(dat) == 0L) {
    syn <- dat[0, , drop = FALSE]
  } else {
    seed <- options$seed %||% 123L
    set.seed(seed)

    rowCountMode <- options$rowCountMode %||% "same"
    if (identical(rowCountMode, "same")) {
      n_target <- nrow(dat)
    } else {
      n_target <- options$n %||% options$nRows %||% nrow(dat)
      n_target <- suppressWarnings(as.integer(n_target))
      if (is.na(n_target) || n_target <= 0L)
        n_target <- nrow(dat)
    }

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
    syn <- aggregate_synthpop_replicates(synthpop_call, dat, seed = seed)

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
