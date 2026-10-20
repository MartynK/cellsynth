# Writing a synthetic table back in the layout of its source.

# Does openxlsx turn "\n" into "\r\n" on this platform? It does on Windows
# (text-mode file writes) and not on Linux or macOS, so probe once per
# session with a one-cell workbook instead of guessing from the OS.
.newline_cache <- new.env(parent = emptyenv())
openxlsx_expands_newlines <- function() {
  if (is.null(.newline_cache$expands)) {
    p <- tempfile(fileext = ".xlsx")
    on.exit(unlink(p), add = TRUE)
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "s")
    openxlsx::writeData(wb, "s", data.frame(x = "a\nb"), colNames = FALSE)
    openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
    back <- readxl::read_excel(p, col_names = FALSE, col_types = "text")[[1]][1]
    .newline_cache$expands <- identical(back, "a\r\nb")
  }
  .newline_cache$expands
}

# Restore one column to the cell type the source file had for it
restore_type <- function(x, cls) {
  if (cls == "logical") {
    v <- as.logical(toupper(trimws(x)))
    if (all(is.na(x) | !is.na(v))) return(v)
  }
  if (all(is.na(x))) return(x)
  # Where openxlsx writes every "\n" as "\r\n", collapse Excel's "\r\n"
  # first so a cell's line breaks round-trip unchanged
  if (any(grepl("\r\n", x, fixed = TRUE)) && openxlsx_expands_newlines())
    x <- gsub("\r\n", "\n", x, fixed = TRUE)
  if (cls == "numeric") {
    v <- parse_number_chr(x)
    if (all(is.na(x) | !is.na(v))) return(v)
  }
  if (cls %in% c("POSIXct", "Date")) {
    # readxl's text read gives date cells as serials; keep the time of day
    num <- parse_number_chr(x)
    if (all(is.na(x) | !is.na(num))) {
      v <- as.POSIXct(round((num - 25569) * 86400), origin = "1970-01-01", tz = "UTC")
      # Date displays better, but openxlsx shifts Date serials below 61 (the
      # Excel 1900 leap-year bug) by a day; POSIXct writes them exactly
      if (all((num %% 1) == 0, na.rm = TRUE) && !any(num < 61, na.rm = TRUE))
        v <- as.Date(v)
      return(v)
    }
    d <- parse_date_chr(x, allow_serial = TRUE)
    if (all(is.na(x) | !is.na(d))) return(as.Date(d, origin = "1970-01-01"))
  }
  x
}

#' Write a synthetic table (or workbook) in the format of its source
#'
#' Excel output keeps the original header (duplicate names included), the
#' sheet names and order and, column by column, the cell type readxl saw in
#' the source (numbers as numbers, dates as dates, everything else as text).
#' Formulas, formatting and merged cells are not reproduced (values only).
#' `.xls` sources are written as `.xlsx`. CSV output writes the raw strings
#' with the original header and delimiter, in UTF-8.
#'
#' @param res A `synth_result`, or a list of them (one per sheet, in order)
#'   for a workbook. Results passed through with `"drop"` are left out.
#' @param path Output path.
#' @return `path`, invisibly.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60,
#'                  code = rep(c(0, 1, NA), 20))
#' path <- file.path(tempdir(), "write_synthetic_source.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' res <- synthesize_table(src, n_folds = 2, seed = 1, verbose = FALSE)
#' out_path <- file.path(tempdir(), "write_synthetic_output.csv")
#' write_synthetic(res, out_path)
#' file.exists(out_path)
#' @export
write_synthetic <- function(res, path) {
  if (inherits(res, "synth_result")) res <- list(res)
  stopifnot(length(res) > 0, all(vapply(res, inherits, logical(1), "synth_result")))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  keep <- !vapply(res, function(r) identical(r$settings$passthrough, "drop"), logical(1))
  res <- res[keep]
  if (!length(res)) stop("Every sheet was dropped; nothing to write.")

  if (res[[1]]$format == "xlsx") {
    wb <- openxlsx::createWorkbook()
    used <- character(0)
    for (r in res) {
      base <- if (is.character(r$sheet)) r$sheet else "Sheet1"
      # Excel: at most 31 characters, unique ignoring case
      sheet <- substr(base, 1, 31)
      k <- 0L
      while (tolower(sheet) %in% tolower(used)) {
        k <- k + 1L
        suffix <- paste0(".", k)
        sheet <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
      }
      used <- c(used, sheet)
      openxlsx::addWorksheet(wb, sheet)
      df <- r$synthetic
      if (length(r$orig_names)) {
        openxlsx::writeData(wb, sheet, as.data.frame(t(r$orig_names)),
                            colNames = FALSE, startRow = 1, keepNA = FALSE)
      }
      if (ncol(df) && nrow(df)) {
        for (j in seq_along(df)) df[[j]] <- restore_type(df[[j]], r$orig_class[j])
        openxlsx::writeData(wb, sheet, df, colNames = FALSE, startRow = 2,
                            keepNA = FALSE)
      }
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  } else {
    if (length(res) > 1) stop("A CSV holds one table; got ", length(res))
    df <- res[[1]]$synthetic
    names(df) <- res[[1]]$orig_names
    delim <- if (is.null(res[[1]]$delim)) "," else res[[1]]$delim
    readr::write_delim(df, path, delim = delim, na = "")
  }
  invisible(path)
}
