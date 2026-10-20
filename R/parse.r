# Parsers that turn raw cell strings into numbers or dates for modelling only.
# They never touch the output: synthetic cells are copied from donors raw.

#' Parse raw strings as numbers, tolerating decimal commas
#'
#' @param x Character vector.
#' @return Numeric vector, `NA` where a cell is not a number.
#' @export
parse_number_chr <- function(x) {
  x <- trimws(x)
  # Hungarian decimal comma, only when unambiguous (one comma, no dot)
  comma <- !is.na(x) & grepl("^[-+]?[0-9]+,[0-9]+$", x)
  x[comma] <- sub(",", ".", x[comma], fixed = TRUE)
  ok <- !is.na(x) & grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(x[ok])
  out
}

#' Parse raw strings as dates (days since 1970-01-01)
#'
#' Recognises ISO `yyyy-mm-dd` (with optional time), `yyyy.mm.dd.`,
#' `yyyy/mm/dd`, `dd.mm.yyyy` and, when `allow_serial = TRUE`, Excel date
#' serials (numbers between 20000 and 60000, i.e. 1954 to 2064).
#'
#' @param x Character vector.
#' @param allow_serial Logical; treat plausible numbers as Excel serials.
#' @return Numeric vector of days since 1970-01-01, `NA` if not a date.
#' @export
parse_date_chr <- function(x, allow_serial = FALSE) {
  x <- trimws(x)
  out <- rep(NA_real_, length(x))
  try_fmt <- function(rx, fmt, transform = identity) {
    hit <- is.na(out) & !is.na(x) & grepl(rx, x)
    if (any(hit)) {
      d <- suppressWarnings(as.Date(transform(x[hit]), format = fmt))
      out[hit] <<- as.numeric(d)
    }
  }
  try_fmt("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}([ T].*)?$", "%Y-%m-%d",
          function(s) substr(s, 1, 10))
  try_fmt("^[0-9]{4}[.] ?[0-9]{1,2}[.] ?[0-9]{1,2}[.]?$", "%Y.%m.%d",
          function(s) sub("[.]$", "", gsub(" ", "", s)))
  try_fmt("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", "%Y/%m/%d")
  try_fmt("^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}[.]?$", "%d.%m.%Y",
          function(s) sub("[.]$", "", s))
  if (allow_serial) {
    # Only for date-role columns: 3000 to 73000 is 1908 to 2099. Times of day
    # (the fraction) are dropped for modelling; the output copies raw strings.
    # Assumes the 1900 date system (the 1904 Mac system is not detected).
    num <- parse_number_chr(x)
    hit <- is.na(out) & !is.na(num) & num >= 3000 & num <= 73000
    out[hit] <- floor(num[hit]) - 25569  # Excel origin 1899-12-30
  }
  # Implausible calendar values are not dates
  out[!is.na(out) & (out < as.numeric(as.Date("1900-01-01")) |
                     out > as.numeric(as.Date("2100-01-01")))] <- NA
  out
}

#' Normalise categorical labels for modelling
#'
#' Numeric-looking labels collapse ("1.0", "1", " 1" -> "1"); other labels are
#' trimmed. The raw strings are still what the output copies.
#'
#' @param x Character vector.
#' @return Character vector of modelling labels.
#' @keywords internal
normalise_label <- function(x) {
  y <- trimws(x)
  num <- parse_number_chr(y)
  # 15 significant digits: distinct values stay distinct, "1.0" == "1"
  hit <- !is.na(num)
  y[hit] <- sprintf("%.15g", num[hit] + 0)
  y
}
