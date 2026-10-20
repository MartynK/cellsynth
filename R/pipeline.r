# One call per file: point it at the proprietary table, get synthetic copies.

#' Synthesise a file end to end
#'
#' Reads the source (every sheet of a workbook unless `sheet` says
#' otherwise), infers a spec per sheet (or reads the reviewed one), writes
#' inferred specs for review, synthesises `m` independent copies and writes
#' each as `<stem>_synth<i>.<ext>` (all sheets in one workbook, in the
#' original order) with a run log.
#'
#' Review the spec CSVs after the first run: set `role` to `id` for every
#' identifier the heuristics missed and to `text` for free text, then run
#' again with the same `spec_dir` (or `spec_path`).
#'
#' Sheets that cannot be synthesised are handled by `small_sheets`: empty
#' sheets and sheets without modelled columns are copied as they are (they
#' hold no modelled values); sheets with fewer than `4 * minbucket` subject
#' rows (legends, codebooks, lookup tables, tiny pilots) get `small_sheets`:
#' `"blank"` (default; keeps header and identifiers, empties the rest),
#' `"copy"` (verbatim, for codebooks you know hold no subject data) or
#' `"drop"`.
#'
#' @param path Source `.xlsx`/`.xlsm`/`.xls` or `.csv`/`.txt`.
#' @param out_dir Output folder.
#' @param sheet Sheet names or indices to synthesise (default: all sheets).
#'   Sheets not listed are left out of the output.
#' @param spec_path Spec CSV for a single-sheet source: read when it exists,
#'   written when it does not.
#' @param spec_dir Folder for per-sheet specs (`<stem>__<position>_<sheet>_spec.csv`);
#'   default `out_dir`.
#' @param id,text Column names forced to `id` / `text` (case-insensitive).
#'   They apply to inferred specs and also on top of reviewed specs read
#'   from disk, so identifiers named here always stay untouched.
#' @param id_pattern,text_pattern Regular expressions on column names, same
#'   effect (see [apply_role_overrides()]).
#' @param m Number of synthetic copies.
#' @param seed Base seed; copy `i`, sheet `k` of `K` uses `seed + K * (i - 1) + k - 1`.
#' @param small_sheets Policy for sheets too small to synthesise.
#' @param minbucket Passed to [synthesize_table()] and used for the size check.
#' @param delim,encoding CSV only, passed to [read_source()]; `NULL` detects.
#' @param ... Passed to [synthesize_table()].
#' @return list(results = list over copies of lists over sheets of
#'   `synth_result`, files, spec_paths).
#' @examples
#' df <- data.frame(id = sprintf("AB%03d", 1:60), value = 1:60,
#'                  code = rep(c(0, 1, NA), 20), group = rep(c("A", "B"), 30))
#' path <- file.path(tempdir(), "synthesize_file.csv")
#' utils::write.csv(df, path, row.names = FALSE, na = "")
#' out <- synthesize_file(path, out_dir = file.path(tempdir(), "synth_output"),
#'                        m = 1, seed = 1, n_folds = 2, verbose = FALSE)
#' out$files
#' out$spec_paths
#' head(out$results[[1]][[1]]$synthetic)
#' @export
synthesize_file <- function(path, out_dir, sheet = NULL, spec_path = NULL,
                            spec_dir = out_dir, id = NULL, text = NULL,
                            id_pattern = NULL, text_pattern = NULL, m = 1,
                            seed = 1, small_sheets = c("blank", "copy", "drop"),
                            minbucket = 5, delim = NULL, encoding = NULL, ...) {
  small_sheets <- match.arg(small_sheets)
  ext_in <- tolower(tools::file_ext(path))
  is_xl <- ext_in %in% c("xlsx", "xlsm", "xls")
  stem <- tools::file_path_sans_ext(basename(path))
  all_sheets <- character(0)
  sheets <- if (!is_xl) list(NULL) else {
    all_sheets <- source_sheets(path)
    s <- if (is.null(sheet)) all_sheets else
      if (is.numeric(sheet)) all_sheets[sheet] else sheet
    bad <- setdiff(s, all_sheets)
    if (length(bad)) stop("Sheets not in workbook: ", paste(bad, collapse = ", "))
    as.list(s)
  }
  if (!is.null(spec_path) && length(sheets) > 1)
    stop("spec_path is for single-sheet sources; use spec_dir for workbooks")

  # Read and spec every sheet once
  srcs <- lapply(sheets, function(s) read_source(path, s, delim = delim,
                                                 encoding = encoding))
  spec_paths <- vapply(seq_along(srcs), function(k) {
    if (!is.null(spec_path)) return(spec_path)
    # The workbook position keeps "Clinical A" and "Clinical-A" (or case
    # variants on Windows) from sharing one spec file
    tag <- if (is_xl && length(all_sheets) > 1) {
      sprintf("__%02d_%s", match(sheets[[k]], all_sheets),
              gsub("[^A-Za-z0-9]+", "_", sheets[[k]]))
    } else ""
    file.path(spec_dir, paste0(stem, tag, "_spec.csv"))
  }, character(1))
  specs <- lapply(seq_along(srcs), function(k) {
    if (file.exists(spec_paths[k])) {
      message("Using reviewed spec: ", spec_paths[k])
      sp <- read_spec(spec_paths[k])
      before <- sp$role
      sp <- apply_role_overrides(sp, id = id, text = text, id_pattern = id_pattern,
                                 text_pattern = text_pattern, warn_unknown = FALSE)
      if (any(sp$role != before))
        message("  overrides changed ", sum(sp$role != before), " role(s) of the reviewed spec")
    } else {
      sp <- infer_spec(srcs[[k]], id = id, text = text, id_pattern = id_pattern,
                       text_pattern = text_pattern, warn_unknown = FALSE)
      write_spec(sp, spec_paths[k])
      message("Inferred spec written for review: ", spec_paths[k])
    }
    validate_spec(sp, srcs[[k]])
    sp
  })

  # A named identifier that matches no sheet at all is almost surely a typo
  all_cols <- tolower(unlist(lapply(srcs, function(x) c(names(x$raw), x$orig_names))))
  miss <- c(id, text)[!tolower(c(id, text)) %in% all_cols]
  if (length(miss)) warning("Override names match no column in any sheet: ",
                            paste(miss, collapse = ", "))

  ext_out <- if (is_xl) "xlsx" else "csv"
  results <- vector("list", m)
  files <- character(m)
  notes <- character(0)
  for (i in seq_len(m)) {
    results[[i]] <- lapply(seq_along(srcs), function(k) {
      src <- srcs[[k]]; sp <- specs[[k]]
      lab <- if (is.null(sheets[[k]])) basename(path) else sheets[[k]]
      n_subj <- sum(!detect_nonsubject_rows(src$raw, sp))
      has_targets <- any(sp$role %in% MODEL_ROLES)
      if (!ncol(src$raw) || !nrow(src$raw) || !has_targets) {
        r <- passthrough_result(src, sp, "copy", "empty or no modelled columns")
      } else if (n_subj < 4 * minbucket) {
        r <- passthrough_result(src, sp, small_sheets,
                                sprintf("%d subject rows < %d", n_subj, 4 * minbucket))
      } else {
        message(sprintf("Synthesising %s: %d x %d", lab, nrow(src$raw), ncol(src$raw)))
        r <- synthesize_table(src, sp, seed = seed + length(srcs) * (i - 1) + k - 1,
                              minbucket = minbucket, ...)
      }
      if (!is.null(r$settings$passthrough) && i == 1)
        notes <<- c(notes, sprintf("sheet '%s': %s (%s)", lab,
                                   r$settings$passthrough, r$settings$reason))
      r
    })
    files[i] <- file.path(out_dir, sprintf("%s_synth%d.%s", stem, i, ext_out))
    write_synthetic(results[[i]], files[i])
    message("Written: ", files[i])
  }

  first <- Filter(function(r) is.null(r$settings$passthrough), results[[1]])
  set_line <- if (length(first)) {
    st <- first[[1]]$settings
    keys <- c("n_folds", "n_cycles", "censor_mode", "lock_threshold",
              "max_predictors", "minbucket", "cp", "seed")
    paste(keys, vapply(keys, function(k) format(st[[k]]), character(1)),
          sep = "=", collapse = ", ")
  } else "none synthesised"
  log_path <- file.path(out_dir, paste0(stem, "_runlog.txt"))
  writeLines(c(
    paste("source:", path),
    paste("sheets:", paste(vapply(sheets, format, character(1)), collapse = " | ")),
    paste("run:", format(Sys.time())), paste("copies:", m),
    paste("settings (first synthesised sheet):", set_line),
    vapply(seq_along(specs), function(k) paste0(
      "roles [", format(sheets[[k]]), "]: ",
      paste(names(table(specs[[k]]$role)), table(specs[[k]]$role),
            sep = "=", collapse = ", ")), character(1)),
    if (length(notes)) c("not synthesised:", paste(" ", notes)),
    "WARNING: identifiers are kept and text columns are permuted real strings;",
    "the output is synthetic in values, NOT anonymised in identifiers or text."),
    log_path)
  invisible(list(results = results, files = files, spec_paths = spec_paths))
}
