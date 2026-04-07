#' Synthetic data from categorical variables
#'
#' This analysis inspects the selected variables of the active JASP dataset,
#' reports whether each column is treated as continuous or categorical, and
#' (in this simplified phase) generates synthetic data for categorical columns
#' by resampling observed rows with replacement. The row-level resampling
#' preserves the empirical joint distribution of the original data.
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

most_frequent <- function(values) {
  if (length(values) == 0L) {
    return(character(0))
  }
  if (all(is.na(values))) {
    return(NA_character_)
  }
  counts <- table(values, useNA = "ifany")
  max_count <- max(counts)
  winners <- names(counts)[counts == max_count]
  winner <- winners[1]
  if (is.na(winner)) {
    return(NA_character_)
  }
  winner
}

aggregate_synthpop_replicates <- function(syn_object, reference) {
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
  result <- replicates[[1]]
  for (col in names(reference)) {
    reference_col <- reference[[col]]
    if (is.numeric(reference_col)) {
      matrix_vals <- vapply(replicates, function(df) as.numeric(df[[col]]), numeric(k))
      row_mean <- rowMeans(matrix_vals, na.rm = TRUE)
      row_mean[is.nan(row_mean)] <- NA_real_
      row_mean <- clamp_numeric(row_mean, reference_col)
      if (is.integer(reference_col)) {
        row_mean <- round(row_mean)
        row_mean <- as.integer(row_mean)
      }
      result[[col]] <- row_mean
    } else {
      matrix_vals <- vapply(replicates, function(df) as.character(df[[col]]), character(k))
      resolved <- apply(matrix_vals, 1, function(row) most_frequent(row))
      if (is.factor(reference_col)) {
        result[[col]] <- factor(resolved, levels = levels(reference_col), ordered = is.ordered(reference_col))
      } else if (is.logical(reference_col)) {
        result[[col]] <- as.logical(resolved)
      } else {
        result[[col]] <- resolved
      }
    }
  }
  result
}

allocate_combo_counts <- function(target_counts) {
  total <- sum(target_counts)
  if (total < length(target_counts)) {
    return(target_counts)
  }
  zero_idx <- which(target_counts == 0L)
  donor_idx <- which(target_counts > 1L)
  i <- 1L
  while (i <= length(zero_idx) && length(donor_idx) > 0L) {
    zero <- zero_idx[i]
    donor <- donor_idx[1L]
    target_counts[[zero]] <- 1L
    target_counts[[donor]] <- target_counts[[donor]] - 1L
    if (target_counts[[donor]] <= 1L) {
      donor_idx <- donor_idx[-1L]
    }
    i <- i + 1L
  }
  target_counts
}

resample_categorical_groups <- function(dat, categoricalCols, n_target, rowCountMode) {
  if (nrow(dat) == 0L || n_target == 0L) {
    return(dat[0, , drop = FALSE])
  }

  if (length(categoricalCols) == 0L) {
    draw_idx <- sample.int(n = nrow(dat), size = n_target, replace = TRUE)
    return(dat[draw_idx, , drop = FALSE])
  }

  combos <- dat[, categoricalCols, drop = FALSE]
  combo_keys <- interaction(combos, drop = TRUE)
  combo_indices <- split(seq_len(nrow(dat)), combo_keys)
  combo_counts <- lengths(combo_indices)

  if (identical(rowCountMode, "same")) {
    target_counts <- combo_counts
  } else {
    raw_counts <- as.integer(stats::rmultinom(1L, size = n_target, prob = combo_counts))
    names(raw_counts) <- names(combo_counts)
    target_counts <- allocate_combo_counts(raw_counts)
  }

  total <- sum(target_counts)
  if (total == 0L) {
    return(dat[integer(0), , drop = FALSE])
  }

  result_idx <- integer(total)
  position <- 1L
  for (combo in names(target_counts)) {
    count <- target_counts[[combo]]
    if (count <= 0L) {
      next
    }
    available <- combo_indices[[combo]]
    if (length(available) == 0L) {
      next
    }
    sampled <- sample(available, size = count, replace = TRUE)
    end <- position + length(sampled) - 1L
    result_idx[position:end] <- sampled
    position <- end + 1L
  }

  if (length(result_idx) > 1L) {
    result_idx <- result_idx[sample.int(length(result_idx))]
  }

  dat[result_idx, , drop = FALSE]
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
      dataset <- jaspBase::readDatasetToEnd(columns = if (!inherits(cols, "try-error") && length(cols) > 0) cols else NULL)
    } else {
      stop("Dataset not provided and jaspBase not available.")
    }
  }

  selectedCols <- if (length(requestedCols) > 0) requestedCols else names(dataset)
  selectedCols <- intersect(selectedCols, names(dataset))
  dat  <- dataset[, selectedCols, drop = FALSE]
  if (ncol(dat) > 0L) {
    names(dat) <- selectedCols
  }

  forceSynthpopFallback <- isTRUE(options$forceSynthpopFallback)
  hasSynthpop <- requireNamespace("synthpop", quietly = TRUE)
  if (!forceSynthpopFallback && !hasSynthpop) {
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

  # --- 3) Generate synthetic data for each column via jittered resampling ---
  if (nrow(dat) == 0L) {
    syn <- dat[0, , drop = FALSE]
  } else {
    set.seed(options$seed %||% 123)
    rowCountMode <- options$rowCountMode %||% "same"
    if (identical(rowCountMode, "same")) {
      n_target <- nrow(dat)
    } else {
      n_target <- options$n %||% options$nRows %||% nrow(dat)
      n_target <- suppressWarnings(as.integer(n_target))
      if (is.na(n_target) || n_target <= 0)
        n_target <- nrow(dat)
    }

    numericCols <- names(dat)[vapply(dat, is.numeric, FUN.VALUE = logical(1))]
    minDiscreteLevels <- options$minDiscreteLevels %||% 5L
    minDiscreteLevels <- suppressWarnings(as.integer(minDiscreteLevels))
    if (is.na(minDiscreteLevels) || minDiscreteLevels < 1L) {
      minDiscreteLevels <- 5L
    }
    discreteNumericCols <- names(Filter(function(x) {
      values <- x[!is.na(x)]
      length(unique(values)) <= minDiscreteLevels
    }, dat[numericCols, drop = FALSE]))
    numericCols <- setdiff(numericCols, discreteNumericCols)
    categoricalCols <- setdiff(names(dat), numericCols)
    n_simulations <- options$nSimulations %||% options$m %||% 5
    n_simulations <- suppressWarnings(as.integer(n_simulations))
    if (is.na(n_simulations) || n_simulations <= 0) {
      n_simulations <- 1L
    }
    seed <- options$seed %||% 123

    syn <- NULL
    useSynthpop <- hasSynthpop && !forceSynthpopFallback
    if (useSynthpop) {
      synthpop_call <- tryCatch({
        synthpop::syn(
          data = dat,
          m = n_simulations,
          k = n_target,
          seed = seed,
          print.flag = FALSE,
          drop.not.used = FALSE,
          drop.pred.only = FALSE
        )
      }, error = function(e) {
        warning("synthpop::syn failed; falling back to row resampling: ", conditionMessage(e))
        NULL
      })
      if (!is.null(synthpop_call)) {
        syn <- aggregate_synthpop_replicates(synthpop_call, dat)
      }
    } else if (forceSynthpopFallback) {
      warning("forced synthpop fallback")
    }
    if (is.null(syn)) {
      syn <- resample_categorical_groups(dat, categoricalCols, n_target, rowCountMode)
    }
    if (ncol(syn) > 0L) {
      names(syn) <- selectedCols
    }
    if (length(discreteNumericCols) > 0L) {
      for (col in discreteNumericCols) {
        if (!(col %in% names(syn))) {
          next
        }
        allowed <- sort(unique(dat[[col]][!is.na(dat[[col]])]))
        if (length(allowed) == 0L) {
          syn[[col]] <- NA_real_
          next
        }
        mapped <- vapply(syn[[col]], function(value) {
          if (is.na(value)) {
            return(NA_real_)
          }
          allowed[which.min(abs(value - allowed))]
        }, numeric(1))
        if (is.integer(dat[[col]])) {
          mapped <- as.integer(mapped)
        }
        syn[[col]] <- mapped
      }
    }
    jitterFraction <- options$jitterFraction %||% 0
    jitterFraction <- suppressWarnings(as.numeric(jitterFraction))
    if (is.na(jitterFraction) || jitterFraction < 0)
      jitterFraction <- 0
    if (jitterFraction > 0 && length(numericCols) > 0) {
      colSd <- vapply(dat[, numericCols, drop = FALSE], function(x) stats::sd(x, na.rm = TRUE), numeric(1))
      colSd[is.na(colSd)] <- 0
      for (col in numericCols) {
        sd_val <- colSd[col]
        if (is.na(sd_val) || sd_val <= 0)
          next
        noise <- stats::rnorm(n = n_target, mean = 0, sd = jitterFraction * sd_val)
        syn[[col]] <- syn[[col]] + noise
      }
    }
  }

  # --- 5) Return results / preview -----------------------------------------
  if (requireNamespace("jaspBase", quietly = TRUE)) {
    preview <- jaspBase::createJaspTable(title = "Variable Types")
    preview$addColumnInfo(name = "variable", title = "Variable", type = "string")
    preview$addColumnInfo(name = "type", title = "Type", type = "string")
    preview$setData(typeInfo)
    jaspResults[["variableTypes"]] <- preview
    jaspResults[["syntheticTypes"]] <- typeInfo

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
    createSynDataset <- if (exists("createJaspDataset", envir = asNamespace("jaspBase"), inherits = FALSE)) {
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
      state$fileFull <- exportPath
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
          text = paste("Failed to save synthetic dataset:", exportError)
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
