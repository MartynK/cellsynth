# Column specification: what role each column plays in the synthesis.
#
# The spec is inferred heuristically, written to an editable CSV, and read back
# on later runs, so a human decision ("this is an ID", "this is free text")
# overrides the heuristics once and stays put.

ROLES <- c("id", "numeric", "date", "categorical", "text", "constant", "empty")

# Column names that look like identifiers (subject, device, record)
ID_NAME_RX <- paste0("(?i)(patient.?no|patient.?id|patient.?number|^id$|_id$|",
                     "betegsz|serial|\\bSN\\b|azonos|record.?id|subject)")
# Column names that hint at dates, so Excel serials are read as dates
DATE_NAME_RX <- paste0("(?i)(date|d[aá]tum|id[oő]pont|ideje|_at$|createdat|",
                       "since|fuv|vizit|visit|implant.*date)")

#' Infer a column specification for a source table
#'
#' Heuristics, in order: all-missing columns are `empty`; identifier-like
#' names with near-unique values (or code-like values such as `AB123`) are
#' `id`; a single value with no missing cells is `constant`; mostly-date
#' columns are `date`; mostly-numeric columns are `numeric`, or `categorical`
#' if they take few distinct values (coded 0/1/2 fields); remaining character
#' columns are `categorical` when the label set is small and `text` otherwise.
#'
#' @param src A `synth_source` from [read_source()].
#' @param id,text Character vectors of column names (internal or on-disk) to
#'   force to `id` or `text`.
#' @param roles Named character vector of further role overrides.
#' @param max_levels Numeric columns with at most this many distinct values
#'   are treated as categorical.
#' @param max_cat_levels Character columns with at most this many labels (and
#'   at most 10% of the observed rows) are categorical, otherwise text.
#'
#' @return A data.frame, one row per column: `column`, `orig_name`, `role`,
#'   `n_obs`, `n_unique`, `miss_frac`, `num_frac`, `date_frac`, `reason`.
#' @export
infer_spec <- function(src, id = NULL, text = NULL, roles = NULL,
                       max_levels = 10, max_cat_levels = 60) {
  stopifnot(inherits(src, "synth_source"))
  raw <- src$raw
  n <- nrow(raw)
  out <- vector("list", ncol(raw))

  for (j in seq_along(raw)) {
    nm <- names(raw)[j]
    on <- src$orig_names[j]
    x <- trimws(raw[[j]])
    x[!is.na(x) & x == ""] <- NA
    obs <- x[!is.na(x)]
    n_obs <- length(obs)
    n_uni <- length(unique(normalise_label(obs)))
    num_frac <- if (n_obs) mean(!is.na(parse_number_chr(obs))) else 0
    date_hint <- grepl(DATE_NAME_RX, on, perl = TRUE) ||
      src$orig_class[j] %in% c("POSIXct", "Date")
    date_frac <- if (n_obs) mean(!is.na(parse_date_chr(obs, date_hint))) else 0
    text_date_frac <- if (n_obs) mean(!is.na(parse_date_chr(obs, FALSE))) else 0
    code_like <- n_obs > 0 && mean(grepl("^[A-Za-z]{1,8}-?[0-9]{2,6}$", obs)) > 0.95

    role <- NA_character_
    reason <- ""
    if (n_obs == 0) {
      role <- "empty"; reason <- "all missing"
    } else if ((grepl(ID_NAME_RX, on, perl = TRUE) && n_uni >= 0.8 * n_obs) ||
               (code_like && n_uni >= 0.9 * n_obs)) {
      role <- "id"; reason <- "identifier name or code pattern, near-unique"
    } else if (n_uni == 1 && n_obs == n) {
      role <- "constant"; reason <- "single value, no missing"
    } else if (date_frac >= 0.8 && (date_hint || text_date_frac >= 0.8)) {
      role <- "date"; reason <- sprintf("%.0f%% parse as dates", 100 * date_frac)
    } else if (num_frac >= 0.8) {
      # Count distinct numbers only: a 0/1 code with a few free-text entries
      # ("1 (VT)") is still a code, its junk labels become extra levels
      n_uni_num <- length(unique(stats::na.omit(parse_number_chr(obs))))
      if (n_uni_num <= max_levels) {
        role <- "categorical"
        reason <- sprintf("numeric code, %d values, %d labels", n_uni_num, n_uni)
      } else {
        role <- "numeric"; reason <- sprintf("%.0f%% parse as numbers", 100 * num_frac)
      }
    } else if (n_uni <= max_cat_levels && n_uni <= max(max_levels, 0.1 * n_obs)) {
      role <- "categorical"; reason <- sprintf("%d labels", n_uni)
    } else {
      role <- "text"; reason <- sprintf("%d distinct strings of %d", n_uni, n_obs)
    }

    out[[j]] <- data.frame(column = nm, orig_name = on, role = role,
                           n_obs = n_obs, n_unique = n_uni,
                           miss_frac = round(1 - n_obs / n, 4),
                           num_frac = round(num_frac, 4),
                           date_frac = round(date_frac, 4),
                           reason = reason, stringsAsFactors = FALSE)
  }
  spec <- if (length(out)) do.call(rbind, out) else
    data.frame(column = character(), orig_name = character(), role = character(),
               n_obs = integer(), n_unique = integer(), miss_frac = numeric(),
               num_frac = numeric(), date_frac = numeric(), reason = character(),
               stringsAsFactors = FALSE)

  force_role <- function(spec, cols, role, why) {
    if (is.null(cols)) return(spec)
    hit <- spec$column %in% cols | spec$orig_name %in% cols
    miss <- setdiff(cols, c(spec$column, spec$orig_name))
    if (length(miss)) warning("Unknown columns in override: ", paste(miss, collapse = ", "))
    spec$role[hit] <- role
    spec$reason[hit] <- why
    spec
  }
  spec <- force_role(spec, id, "id", "user: id")
  spec <- force_role(spec, text, "text", "user: text")
  if (!is.null(roles)) {
    for (k in names(roles)) spec <- force_role(spec, k, roles[[k]], "user override")
  }
  validate_spec(spec, src)
  spec
}

#' Check a specification against its source
#' @param spec A spec data.frame.
#' @param src A `synth_source`.
#' @return `spec`, invisibly; stops on a mismatch.
#' @keywords internal
validate_spec <- function(spec, src) {
  if (!identical(spec$column, names(src$raw)))
    stop("Spec columns do not match the source columns; re-infer the spec ",
         "or fix the spec file (source has ", ncol(src$raw), " columns).")
  bad <- setdiff(spec$role, ROLES)
  if (length(bad)) stop("Unknown roles in spec: ", paste(bad, collapse = ", "))
  if (nrow(spec) && any(spec$role %in% MODEL_ROLES) && !any(spec$role == "id"))
    warning("No identifier column in the spec; tables will not be joinable.")
  invisible(spec)
}

#' Write a spec to CSV for review and editing
#'
#' Only the `role` column is meant to be edited by hand; the rest documents
#' why the heuristics chose it. The file holds column names and counts only,
#' no cell values.
#'
#' @param spec A spec data.frame.
#' @param path Output `.csv` path.
#' @export
write_spec <- function(spec, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(spec, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

#' Read a spec CSV written by [write_spec()]
#' @param path Path to the spec `.csv`.
#' @return A spec data.frame.
#' @export
read_spec <- function(path) {
  # Column names such as "001" or "NA" must stay character, and an empty
  # sheet's spec must not come back logical
  spec <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          check.names = FALSE, na.strings = character(0),
                          colClasses = c(column = "character", orig_name = "character",
                                         role = "character", reason = "character"))
  spec$role <- trimws(spec$role)
  spec
}

#' Detect rows that are not subjects (totals, site headers)
#'
#' A row is non-subject when any identifier cell reads like a summary label
#' (`TOTAL`, `Sum`, `Összesen`), or when the main identifier carries no digit
#' while at least 90% of the others do (site-name header rows).
#'
#' @param raw Raw character data.frame.
#' @param spec Its spec.
#' @return Logical vector, `TRUE` for non-subject rows.
#' @export
detect_nonsubject_rows <- function(raw, spec) {
  ids <- spec$column[spec$role == "id"]
  flag <- rep(FALSE, nrow(raw))
  if (!length(ids)) return(flag)
  summary_rx <- "(?i)^\\s*(total|sum|grand total|[oö]sszesen|[aá]tlag|mean)\\s*$"
  for (k in ids) flag <- flag | (!is.na(raw[[k]]) & grepl(summary_rx, raw[[k]], perl = TRUE))
  fills <- vapply(ids, function(k) sum(!is.na(raw[[k]])), numeric(1))
  main <- raw[[ids[which.max(fills)]]]
  has_digit <- grepl("[0-9]", main)
  observed <- !is.na(main)
  if (any(observed) && mean(has_digit[observed]) >= 0.9)
    flag <- flag | (observed & !has_digit)
  flag
}
