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
#'
#' Returns a JASP results object with tables for types and preview, or a data.frame if
#' jaspBase is not available (for headless testing).
#' @keywords internal
#' @export

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
      # read only the needed columns if specified
      cols <- try(requestedCols, silent = TRUE)
      dataset <- jaspBase::readDatasetToEnd(columns = if (!inherits(cols, "try-error") && length(cols) > 0) cols else NULL)
    } else {
      stop("Dataset not provided and jaspBase not available.")
    }
  }

  cols <- if (length(requestedCols) > 0) requestedCols else names(dataset)
  cols <- intersect(cols, names(dataset))
  dat  <- dataset[, cols, drop = FALSE]

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

  # --- 3) Generate categorical synthetic data -------------------------------
  if (nrow(dat) == 0L) {
    syn <- dat[0, , drop = FALSE]
  } else {
    set.seed(options$seed %||% 123)
    n_target <- options$n %||% options$nRows %||% nrow(dat)
    n_target <- suppressWarnings(as.integer(n_target))
    if (is.na(n_target) || n_target <= 0)
      n_target <- nrow(dat)

    draw_idx <- sample.int(n = nrow(dat), size = n_target, replace = TRUE)
    syn <- dat[draw_idx, , drop = FALSE]
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
    jaspResults[["synthetic"]] <- syn

    return(invisible())
  }

  # Fallback: return the data.frame directly (useful for testing outside JASP)
  syn
}
