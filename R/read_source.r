# Reading a source table exactly as it sits on disk.
#
# Everything is read as character so that the synthetic output can copy donor
# cells verbatim: "1.0" stays "1.0", an Excel date serial stays a serial, a
# typo stays a typo. Parsing happens later, only for modelling.

#' Read a source table (xlsx or csv) as raw character data
#'
#' The first row is always taken as the header. Excel formulas are read as
#' their cached values; cell formatting (colours, merged cells, number
#' formats) is not read. CSV files are read with an auto-detected delimiter
#' (comma, semicolon, tab or pipe) and encoding (Hungarian Excel exports are
#' often semicolon-separated windows-1250).
#'
#' @param path Path to an `.xlsx`/`.xlsm`/`.xls` or `.csv`/`.txt` file.
#' @param sheet Sheet name or index for Excel files (default: first sheet).
#' @param guess_max Rows used by readxl to guess the original column types,
#'   which are only used to restore cell types on writing.
#' @param delim,encoding CSV only; `NULL` detects them.
#'
#' @return A `synth_source` list: `raw` (data.frame of character, internal
#'   unique names), `orig_names` (header as on disk, duplicates allowed),
#'   `orig_class` (readxl/readr guessed class per column), `path`, `sheet`,
#'   `format`, and for CSV `delim` and `encoding`.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60,
#'                  code = rep(c(0, 1, NA), 20))
#' path <- file.path(tempdir(), "read_source.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' head(src$raw)
#' @export
read_source <- function(path, sheet = NULL, guess_max = 1e5, delim = NULL,
                        encoding = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  ext <- tolower(tools::file_ext(path))
  csv_meta <- list()

  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (is.null(sheet)) sheet <- 1
    if (is.numeric(sheet)) sheet <- readxl::excel_sheets(path)[sheet]
    raw <- suppressWarnings(suppressMessages(readxl::read_excel(
      path, sheet = sheet, col_types = "text", trim_ws = FALSE,
      .name_repair = "minimal")))
    guessed <- suppressWarnings(suppressMessages(readxl::read_excel(
      path, sheet = sheet, guess_max = guess_max, trim_ws = FALSE,
      .name_repair = "minimal")))
    orig_class <- vapply(guessed, function(x) class(x)[1], character(1))
    if (length(orig_class) != ncol(raw)) orig_class <- rep("character", ncol(raw))
    format <- "xlsx"
  } else if (ext %in% c("csv", "txt", "tsv")) {
    if (is.null(encoding)) encoding <- detect_encoding(path)
    if (is.null(delim)) delim <- detect_delim(path, encoding)
    raw <- readr::read_delim(path, delim = delim,
                             col_types = readr::cols(.default = "c"),
                             locale = readr::locale(encoding = encoding),
                             na = c("", "NA"), trim_ws = FALSE,
                             name_repair = "minimal", progress = FALSE)
    orig_class <- rep("character", ncol(raw))
    format <- "csv"
    csv_meta <- list(delim = delim, encoding = encoding)
  } else {
    stop("Unsupported file type: ", ext)
  }

  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
  orig_names <- names(raw)
  # Internal names must be unique and non-empty; the header on disk need not be
  safe <- orig_names
  safe[is.na(safe) | safe == ""] <- paste0("V", which(is.na(safe) | safe == ""))
  names(raw) <- make.unique(safe, sep = "...")
  for (j in seq_along(raw)) raw[[j]] <- as.character(raw[[j]])

  structure(c(list(raw = raw, orig_names = orig_names,
                   orig_class = unname(orig_class), path = path,
                   sheet = sheet, format = format), csv_meta),
            class = "synth_source")
}

#' Sheet names of an Excel workbook
#' @param path Path to an Excel file.
#' @return Character vector, in workbook order.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:6), value = 1:6)
#' path <- file.path(tempdir(), "source_sheets.xlsx")
#' sheets <- list(Subjects = df, Codes = data.frame(code = c(0, 1)))
#' openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
#' source_sheets(path)
#' @export
source_sheets <- function(path) readxl::excel_sheets(path)

# Most likely encoding of a text file; UTF-8 unless readr is fairly sure
detect_encoding <- function(path) {
  n_max <- 2e6
  bytes <- readBin(path, "raw", n = min(file.size(path), n_max))
  # A truncated sample may end inside a multibyte character: back up over the
  # trailing non-ASCII bytes (at most 3). A complete file is checked whole.
  if (file.size(path) > n_max) {
    k <- 0
    while (k < 3 && length(bytes) > k && as.integer(bytes[length(bytes) - k]) >= 128) k <- k + 1
    bytes <- bytes[seq_len(length(bytes) - k)]
  }
  bytes <- bytes[bytes != as.raw(0)]
  utf8_ok <- validUTF8(rawToChar(bytes))
  g <- tryCatch(readr::guess_encoding(path, n_max = 1e4), error = function(e) NULL)
  enc <- if (is.null(g) || !nrow(g)) NA_character_ else g$encoding[1]
  if (utf8_ok && (is.na(enc) || enc %in% c("ASCII", "UTF-8") || g$confidence[1] < 0.5))
    return("UTF-8")
  if (is.na(enc) || enc %in% c("ASCII", "UTF-8")) enc <- "windows-1250"
  # readr reports ISO-8859-2 for Hungarian text. Hungarian letters sit at the
  # same bytes in both; where they differ, windows-1250 is what Excel writes
  # (0xB1 is plus-minus there, a-ogonek in ISO-8859-2; also typographic quotes and dashes in 0x80-0x9F)
  if (enc == "ISO-8859-2") enc <- "windows-1250"
  enc
}

# Delimiter that splits the first lines most consistently
detect_delim <- function(path, encoding = "UTF-8") {
  con <- file(path, encoding = encoding)
  on.exit(close(con))
  l <- readLines(con, n = 20, warn = FALSE)
  # Quoted fields may contain any delimiter; do not count inside them
  l <- gsub('"[^"]*"', '""', l)
  l <- l[nzchar(l)]
  if (!length(l)) return(",")
  cand <- c(",", ";", "\t", "|")
  cnt <- vapply(cand, function(d) {
    k <- lengths(regmatches(l, gregexpr(d, l, fixed = TRUE)))
    # a real delimiter appears often and about equally often on every line
    if (stats::median(k) == 0) 0 else stats::median(k) * mean(k == stats::median(k))
  }, numeric(1))
  if (all(cnt == 0)) "," else cand[which.max(cnt)]
}

#' Print a synth_source
#' @param x A `synth_source`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.synth_source <- function(x, ...) {
  cat("<synth_source>", basename(x$path),
      if (!is.null(x$sheet)) paste0("[", x$sheet, "]"), "\n")
  cat(nrow(x$raw), "rows x", ncol(x$raw), "columns, format", x$format, "\n")
  invisible(x)
}
