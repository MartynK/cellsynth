# Column specification: what role each column plays in the synthesis.
#
# The spec is inferred heuristically, written to an editable CSV, and read back
# on later runs, so a human decision ("this is an ID", "this is free text")
# overrides the heuristics once and stays put.

ROLES <- c("id", "numeric", "date", "categorical", "text", "constant", "empty")

# Column names that look like identifiers (subject, device, record); these
# become `id` only when their values are near-unique
ID_NAME_RX <- paste0("(?i)(patient.?no|patient.?id|patient.?number|^id$|_id$|",
                     "betegsz|serial|\\bSN\\b|azonos|record.?id|rec_?id|subject|subj)")
# Key names of clinical-trial and EDC exports (CDISC SDTM/ADaM and the usual
# EDC variants): always `id`, unique or not, since a site or study key is a
# key even though many subjects share it
KEY_NAME_RX <- paste0(
  "(?i)^(usubjid|subjid|subj_?(id|no|num)|subject_?(id|no|num|number)?|site|study|",
  "siteid|site_?(id|no|num|number)|studyid|study_?(id|no)|invid|inv_?(id|no)|",
  "pt_?(id|no|num)|patno|pat_?(id|no)|scr_?(id|no|num)|scrnum|scrno|",
  "rand_?(id|no|num)|randno|randnum|enrol+_?(id|no))$")
# Column names that hint at dates, so Excel serials are read as dates
DATE_NAME_RX <- paste0("(?i)(date|d[a\u00e1]tum|id[o\u0151]pont|ideje|_at$|createdat|",
                       "since|fuv|vizit|visit|implant.*date)")

#' Infer a column specification for a source table
#'
#' Heuristics, in order: all-missing columns are `empty`; trial and EDC key
#' names (`USUBJID`, `SUBJID`, `SITEID`, `STUDYID`, `RANDNO`, ...) are `id`;
#' identifier-like
#' names with near-unique values (or code-like values such as `AB123`) are
#' `id`; a single value with no missing cells is `constant`; mostly-date
#' columns are `date`; mostly-numeric columns are `numeric`, or `categorical`
#' if they take few distinct values (coded 0/1/2 fields); remaining character
#' columns are `categorical` when the label set is small and `text` otherwise.
#'
#' @param src A `synth_source` from [read_source()].
#' @param id,text Column names (internal or on-disk, case-insensitive) to
#'   force to `id` or `text`, whatever the heuristics say.
#' @param roles Named character vector of further role overrides.
#' @param id_pattern,text_pattern Regular expressions on column names, e.g.
#'   `id_pattern = "^(pat|subj).*(id|no)$"`. See [apply_role_overrides()],
#'   also for session-wide defaults via `options()`.
#' @param warn_unknown Warn about override names that match no column.
#' @param max_levels Numeric columns with at most this many distinct values
#'   are treated as categorical.
#' @param max_cat_levels Character columns with at most this many labels (and
#'   at most 10% of the observed rows) are categorical, otherwise text.
#'
#' @return A data.frame, one row per column: `column`, `orig_name`, `role`,
#'   `n_obs`, `n_unique`, `miss_frac`, `num_frac`, `date_frac`, `reason`.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60,
#'                  code = rep(c(0, 1, NA), 20), group = rep(c("A", "B"), 30))
#' path <- file.path(tempdir(), "infer_spec.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' spec <- infer_spec(src, id = "id", roles = c(code = "categorical"))
#' spec[, c("column", "role", "reason")]
#' @export
infer_spec <- function(src, id = NULL, text = NULL, roles = NULL,
                       id_pattern = NULL, text_pattern = NULL,
                       max_levels = 10, max_cat_levels = 60, warn_unknown = TRUE) {
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
    # Code-like values: "AB123", "XY-12", and multi-segment trial codes such
    # as "STUDY01-01-001" (with a digit, and not dates)
    code_like <- n_obs > 0 && text_date_frac < 0.5 &&
      mean(grepl("^[A-Za-z]{1,8}-?[0-9]{2,6}$", obs) |
           (grepl("^[A-Za-z0-9]+([-_/][A-Za-z0-9]+){1,4}$", obs) & grepl("[0-9]", obs))) > 0.95
    # Zero-padded fixed-width integers ("001", "0457") are codes, not counts
    padded <- n_obs > 0 && all(grepl("^[0-9]+$", obs)) && length(unique(nchar(obs))) == 1 &&
      nchar(obs[1]) >= 3 && any(startsWith(obs, "0"))

    role <- NA_character_
    reason <- ""
    if (n_obs == 0) {
      role <- "empty"; reason <- "all missing"
    } else if (grepl(KEY_NAME_RX, on, perl = TRUE)) {
      role <- "id"; reason <- "trial/EDC key name"
    } else if ((grepl(ID_NAME_RX, on, perl = TRUE) && n_uni >= 0.8 * n_obs) ||
               ((code_like || padded) && n_uni >= 0.9 * n_obs)) {
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

  spec <- apply_role_overrides(spec, id = id, text = text, roles = roles,
                               id_pattern = id_pattern, text_pattern = text_pattern,
                               warn_unknown = warn_unknown)
  validate_spec(spec, src)
  spec
}

#' Force roles onto a spec by column name or name pattern
#'
#' Used by [infer_spec()] and by [synthesize_file()] (where it also applies
#' to a reviewed spec read from disk, so identifiers named in the call always
#' win). Names match the internal or the on-disk column name, ignoring case;
#' patterns are regular expressions (perl, case-insensitive) matched against
#' the on-disk name. The options `cellsynth.id`, `cellsynth.id_pattern`,
#' `cellsynth.text` and `cellsynth.text_pattern` add session-wide defaults,
#' e.g. `options(cellsynth.id_pattern = "^(pat|subj)")` once per session.
#'
#' @param spec A spec data.frame.
#' @param id,text Column names to force to `id` / `text`.
#' @param roles Named character vector, `c(column = "role")`, for any role.
#' @param id_pattern,text_pattern Regular expressions on column names.
#' @param warn_unknown Warn about names in `id`/`text`/`roles` that match no
#'   column.
#' @return The spec with roles replaced and `reason` set to `"user: ..."`.
#' @examples
#' df <- data.frame(PatKey = sprintf("%04d", 1:60), Visit_Note = "seen",
#'                  hb = round(seq(110, 160, length.out = 60)))
#' path <- file.path(tempdir(), "overrides.csv")
#' utils::write.csv(df, path, row.names = FALSE)
#' spec <- infer_spec(read_source(path))
#' spec <- apply_role_overrides(spec, id = "patkey", text_pattern = "note$")
#' spec[, c("orig_name", "role", "reason")]
#' @export
apply_role_overrides <- function(spec, id = NULL, text = NULL, roles = NULL,
                                 id_pattern = NULL, text_pattern = NULL,
                                 warn_unknown = TRUE) {
  id <- unique(c(id, getOption("cellsynth.id")))
  text <- unique(c(text, getOption("cellsynth.text")))
  id_pattern <- unique(c(id_pattern, getOption("cellsynth.id_pattern")))
  text_pattern <- unique(c(text_pattern, getOption("cellsynth.text_pattern")))
  if (!nrow(spec)) return(spec)
  by_name <- function(cols) {
    tolower(spec$column) %in% tolower(cols) | tolower(spec$orig_name) %in% tolower(cols)
  }
  by_pattern <- function(rx) {
    hit <- rep(FALSE, nrow(spec))
    for (p in rx) hit <- hit | grepl(paste0("(?i)", p), spec$orig_name, perl = TRUE)
    hit
  }
  force <- function(spec, hit, role, why) {
    spec$role[hit] <- role
    spec$reason[hit] <- why
    spec
  }
  named <- c(id, text, names(roles))
  miss <- named[!tolower(named) %in% tolower(c(spec$column, spec$orig_name))]
  if (warn_unknown && length(miss))
    warning("Unknown columns in override: ", paste(miss, collapse = ", "))
  bad <- setdiff(unname(roles), ROLES)
  if (length(bad)) stop("Unknown roles: ", paste(bad, collapse = ", "))
  # Text first, identifiers last: a column named as an identifier stays one
  if (length(text_pattern)) spec <- force(spec, by_pattern(text_pattern), "text", "user: text pattern")
  if (length(text)) spec <- force(spec, by_name(text), "text", "user: text")
  for (k in names(roles)) spec <- force(spec, by_name(k), roles[[k]], "user: role")
  if (length(id_pattern)) spec <- force(spec, by_pattern(id_pattern), "id", "user: id pattern")
  if (length(id)) spec <- force(spec, by_name(id), "id", "user: id")
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
#' @return `path`, invisibly.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60)
#' path <- file.path(tempdir(), "write_spec_source.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' spec <- infer_spec(read_source(path))
#' spec_path <- file.path(tempdir(), "write_spec.csv")
#' write_spec(spec, spec_path)
#' file.exists(spec_path)
#' @export
write_spec <- function(spec, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(spec, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

#' Read a spec CSV written by [write_spec()]
#' @param path Path to the spec `.csv`.
#' @return A spec data.frame.
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60)
#' path <- file.path(tempdir(), "read_spec_source.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' spec <- infer_spec(read_source(path))
#' spec_path <- file.path(tempdir(), "read_spec.csv")
#' write_spec(spec, spec_path)
#' reviewed <- read_spec(spec_path)
#' reviewed[, c("column", "role")]
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
#' (`TOTAL`, `Sum`, `Osszesen` with or without accent), or when the main identifier carries no digit
#' while at least 90% of the others do (site-name header rows).
#'
#' @param raw Raw character data.frame.
#' @param spec Its spec.
#' @return Logical vector, `TRUE` for non-subject rows.
#' @examples
#' df <- data.frame(id = c(sprintf("AB%03d", 1:59), "TOTAL"), value = 1:60)
#' path <- file.path(tempdir(), "nonsubject_rows.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' src <- read_source(path)
#' spec <- infer_spec(src, id = "id")
#' flagged <- detect_nonsubject_rows(src$raw, spec)
#' src$raw[flagged, , drop = FALSE]
#' @export
detect_nonsubject_rows <- function(raw, spec) {
  ids <- spec$column[spec$role == "id"]
  flag <- rep(FALSE, nrow(raw))
  if (!length(ids)) return(flag)
  summary_rx <- "(?i)^\\s*(total|sum|grand total|[o\u00f6]sszesen|[a\u00e1]tlag|mean)\\s*$"
  for (k in ids) flag <- flag | (!is.na(raw[[k]]) & grepl(summary_rx, raw[[k]], perl = TRUE))
  fills <- vapply(ids, function(k) sum(!is.na(raw[[k]])), numeric(1))
  main <- raw[[ids[which.max(fills)]]]
  has_digit <- grepl("[0-9]", main)
  observed <- !is.na(main)
  if (any(observed) && mean(has_digit[observed]) >= 0.9)
    flag <- flag | (observed & !has_digit)
  flag
}
