# Helpers for the workbook, csv-format and screening tests.

# Sheet as raw text exactly as stored (all cells character), header kept
# as on disk (duplicates and blanks allowed)
read_sheet_text <- function(path, sheet) {
  suppressWarnings(suppressMessages(readxl::read_excel(
    path, sheet = sheet, col_types = "text", trim_ws = FALSE,
    .name_repair = "minimal")))
}

# Silence the expected chatter (progress messages, "no identifier" warnings)
quietly <- function(expr) suppressWarnings(suppressMessages(expr))

# Write a data.frame of character columns as a sheet, header from `header`
# (default: names(df)); NA cells stay empty, so a header may contain blanks.
add_text_sheet <- function(wb, sheet, df, header = names(df)) {
  openxlsx::addWorksheet(wb, sheet)
  if (length(header)) {
    openxlsx::writeData(wb, sheet, as.data.frame(t(header)),
                        colNames = FALSE, startRow = 1, keepNA = FALSE)
  }
  if (nrow(df) && ncol(df)) {
    openxlsx::writeData(wb, sheet, df, colNames = FALSE, startRow = 2,
                        keepNA = FALSE)
  }
  invisible(wb)
}

SHEET_ACCENT <- "Kórház adatok 1"

# The seven-sheet toy workbook, in this order:
# Data (200 rows), Legend (6), Empty, HeaderOnly, IdOnly, Dups, accented.
# Returns list(path, sheets = named list of the source data.frames (text)).
make_toy_workbook <- function(path) {
  src <- toy_source(200, seed = 11)$raw
  data_df <- src[src$patientNo != "TOTAL", , drop = FALSE]
  rownames(data_df) <- NULL

  legend <- data.frame(Term = c("alpha", "beta", "gamma", "delta", "kappa", "sigma"),
                       Meaning = c("first", "second", "third", "fourth", "tenth", "last"),
                       Unit = c("mg", "mg", "kg", "kg", "l", "l"),
                       stringsAsFactors = FALSE)

  set.seed(12)
  id_only <- data.frame(patient_id = sprintf("ZK%03d", 1:30), stringsAsFactors = FALSE)

  n <- 30
  dups <- data.frame(a = sprintf("PT%03d", 1:n),
                     b = sprintf("%.1f", rnorm(n, 50, 8)),
                     c = sprintf("%.1f", rnorm(n, 52, 8)),
                     d = sample(c("x", "y", "z"), n, TRUE),
                     stringsAsFactors = FALSE)

  m <- 40
  acc <- data.frame(az = sprintf("HU%03d", 1:m),
                    s = sprintf("%.1f", rnorm(m, 80, 9)),
                    nem = sample(c("nő", "férfi"), m, TRUE),
                    stringsAsFactors = FALSE)

  wb <- openxlsx::createWorkbook()
  add_text_sheet(wb, "Data", data_df)
  add_text_sheet(wb, "Legend", legend)
  openxlsx::addWorksheet(wb, "Empty")
  add_text_sheet(wb, "HeaderOnly", data.frame(A = character(), B = character(),
                                              C = character()))
  add_text_sheet(wb, "IdOnly", id_only)
  add_text_sheet(wb, "Dups", dups, header = c("ID", "val", "val", NA))
  add_text_sheet(wb, SHEET_ACCENT, acc, header = c("ID", "weight", "sex"))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(list(path = path))
}

# Sparse 0/1/NA character table: n rows x p columns, distinct random columns
sparse_binary_table <- function(n, p, seed = 3) {
  set.seed(seed)
  cols <- lapply(seq_len(p), function(j) {
    u <- stats::runif(n)
    ifelse(u < 0.10, "1", ifelse(u < 0.25, NA_character_, "0"))
  })
  names(cols) <- sprintf("v%04d", seq_len(p))
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

# Write text in a given encoding as raw bytes (no re-encoding by R)
write_text_enc <- function(lines, path, encoding) {
  bytes <- lapply(lines, function(l) c(iconv(l, "UTF-8", encoding, toRaw = TRUE)[[1]],
                                       as.raw(c(0x0d, 0x0a))))
  writeBin(do.call(c, bytes), path)
  invisible(path)
}
