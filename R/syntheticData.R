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

ensure_categorical_labels <- function(syn, dat, categoricalCols) {
  if (nrow(syn) == 0L || length(categoricalCols) == 0L) {
    return(syn)
  }

  for (col in categoricalCols) {
    original_labels <- unique(as.character(dat[[col]]))
    synthetic_labels <- unique(as.character(syn[[col]]))
    missing_labels <- setdiff(original_labels, synthetic_labels)
    if (length(missing_labels) == 0L) {
      next
    }

    replace_rows <- sample.int(n = nrow(syn), size = length(missing_labels), replace = FALSE)
    for (i in seq_along(missing_labels)) {
      label <- missing_labels[i]
      source_rows <- which(as.character(dat[[col]]) == label)
      if (length(source_rows) == 0L) {
        next
      }
      syn[replace_rows[i], ] <- dat[source_rows[1], , drop = FALSE]
    }
  }

  syn
}

resample_categorical_groups <- function(dat, categoricalCols, n_target, rowCountMode) {
  draw_idx <- sample.int(n = nrow(dat), size = n_target, replace = TRUE)
  syn <- dat[draw_idx, , drop = FALSE]
  if (identical(rowCountMode, "same")) {
    syn <- ensure_categorical_labels(syn, dat, categoricalCols)
  }
  syn
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
    categoricalCols <- setdiff(names(dat), numericCols)
    n_simulations <- options$nSimulations %||% options$m %||% 5
    n_simulations <- suppressWarnings(as.integer(n_simulations))
    if (is.na(n_simulations) || n_simulations <= 0) {
      n_simulations <- 1L
    }
    seed <- options$seed %||% 123

    syn <- NULL
    if (requireNamespace("synthpop", quietly = TRUE)) {
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
    }
    if (is.null(syn)) {
      syn <- resample_categorical_groups(dat, categoricalCols, n_target, rowCountMode)
    }
    if (ncol(syn) > 0L) {
      names(syn) <- selectedCols
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
