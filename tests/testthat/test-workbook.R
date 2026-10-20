# Multi-sheet workbook handling in synthesize_file() / write_synthetic().
#
# One seven-sheet toy workbook (see make_toy_workbook() in helper-workbook.R):
# Data (200 rows), Legend (6 rows), Empty, HeaderOnly, IdOnly, Dups (duplicated
# header names and a blank header), and a sheet with accents and spaces.

WB_SHEETS <- c("Data", "Legend", "Empty", "HeaderOnly", "IdOnly", "Dups", SHEET_ACCENT)

# Build the workbook and the default / copy / drop runs once per file
wb_cache <- new.env()
wb_get <- function() {
  if (!is.null(wb_cache$ready)) return(wb_cache)
  base <- tempfile("wb_")
  dir.create(base)
  path <- file.path(base, "toy.xlsx")
  make_toy_workbook(path)
  wb_cache$base <- base
  wb_cache$path <- path
  wb_cache$src <- stats::setNames(lapply(WB_SHEETS, function(s) read_sheet_text(path, s)),
                                  WB_SHEETS)
  run <- function(dir, ...) {
    out <- file.path(base, dir)
    r <- quietly(synthesize_file(path, out, n_folds = 5, seed = 1, ...))
    list(r = r, dir = out, file = r$files[1])
  }
  wb_cache$blank <- run("out_blank")
  wb_cache$copy <- run("out_copy", small_sheets = "copy")
  wb_cache$drop <- run("out_drop", small_sheets = "drop")
  wb_cache$ready <- TRUE
  wb_cache
}

test_that("output keeps sheet names and order (accents and spaces included)", {
  w <- wb_get()
  expect_equal(readxl::excel_sheets(w$path), WB_SHEETS)
  expect_equal(readxl::excel_sheets(w$blank$file), WB_SHEETS)
  expect_equal(readxl::excel_sheets(w$copy$file), WB_SHEETS)
})

test_that("headers and dimensions survive per sheet", {
  w <- wb_get()
  for (s in WB_SHEETS) {
    o <- read_sheet_text(w$blank$file, s)
    i <- w$src[[s]]
    expect_equal(names(o), names(i), info = s)
    if (s != "Legend") expect_equal(dim(o), dim(i), info = s)
  }
  # duplicated and blank headers are reproduced as on disk
  o <- read_sheet_text(w$blank$file, "Dups")
  expect_equal(names(o), c("ID", "val", "val", ""))
})

test_that("Data is synthesised: ids kept, values borrowed from the donor pool", {
  w <- wb_get()
  o <- read_sheet_text(w$blank$file, "Data")
  i <- w$src$Data
  expect_identical(o$patientNo, i$patientNo)
  for (col in c("weight_kg", "response", "code01", "grp", "visit_date",
                "death_flag", "death_date", "notes")) {
    expect_true(all(stats::na.omit(o[[col]]) %in% stats::na.omit(i[[col]])), info = col)
  }
  expect_true(all(is.na(o$unused)))
  # it did something: at least one modelled column differs from the source
  differs <- vapply(c("weight_kg", "response", "grp", "visit_date", "notes"),
                    function(col) !identical(o[[col]], i[[col]]), logical(1))
  expect_true(any(differs))
  expect_null(w$blank$r$results[[1]][[1]]$settings$passthrough)
})

test_that("accented sheet and the Dups sheet are synthesised, not copied", {
  w <- wb_get()
  res <- w$blank$r$results[[1]]
  expect_null(res[[6]]$settings$passthrough)
  expect_null(res[[7]]$settings$passthrough)
  o <- read_sheet_text(w$blank$file, SHEET_ACCENT)
  i <- w$src[[SHEET_ACCENT]]
  expect_identical(o$ID, i$ID)
  expect_true(all(o$sex %in% c("nő", "férfi")))
  d <- read_sheet_text(w$blank$file, "Dups")
  expect_identical(d$ID, w$src$Dups$ID)
  expect_true(all(stats::na.omit(d[[2]]) %in% w$src$Dups[[2]]))
  expect_true(all(stats::na.omit(d[[3]]) %in% w$src$Dups[[3]]))
})

test_that("per-sheet passthrough kinds are recorded on the results", {
  w <- wb_get()
  kinds <- vapply(w$blank$r$results[[1]],
                  function(r) if (is.null(r$settings$passthrough)) "synth" else r$settings$passthrough,
                  character(1))
  expect_equal(unname(kinds),
               c("synth", "blank", "copy", "copy", "copy", "synth", "synth"))
  expect_length(w$blank$r$results, 1)
  expect_length(w$blank$r$results[[1]], 7)
  runlog <- readLines(file.path(w$blank$dir, "toy_runlog.txt"))
  expect_true(any(grepl("not synthesised", runlog)))
  expect_true(any(grepl("Legend", runlog)))
})

test_that("Legend: blanked by default, verbatim under copy, absent under drop", {
  w <- wb_get()
  blank <- read_sheet_text(w$blank$file, "Legend")
  expect_equal(names(blank), c("Term", "Meaning", "Unit"))
  expect_true(all(is.na(unlist(blank))))          # header only, no values
  cp <- read_sheet_text(w$copy$file, "Legend")
  expect_equal(as.data.frame(cp), as.data.frame(w$src$Legend))
  expect_equal(readxl::excel_sheets(w$drop$file), setdiff(WB_SHEETS, "Legend"))
  expect_false("Legend" %in% readxl::excel_sheets(w$drop$file))
})

test_that("Empty, HeaderOnly and ID-only sheets are copied verbatim", {
  w <- wb_get()
  for (f in c(w$blank$file, w$copy$file, w$drop$file)) {
    e <- read_sheet_text(f, "Empty")
    expect_equal(dim(e), c(0L, 0L))
    h <- read_sheet_text(f, "HeaderOnly")
    expect_equal(names(h), c("A", "B", "C"))
    expect_equal(nrow(h), 0L)
    o <- read_sheet_text(f, "IdOnly")
    expect_equal(as.data.frame(o), as.data.frame(w$src$IdOnly))
  }
})

test_that("a per-sheet spec file is written for every sheet", {
  w <- wb_get()
  sp <- w$blank$r$spec_paths
  expect_length(sp, 7)
  expect_true(all(file.exists(sp)))
  expect_equal(anyDuplicated(sp), 0L)
  expect_match(basename(sp[1]), "^toy__01_Data_spec\\.csv$")
  expect_match(basename(sp[2]), "^toy__02_Legend_spec\\.csv$")
  # file names stay portable although the sheet name has accents and spaces
  expect_false(any(grepl("[^ -~]", basename(sp))))
  expect_match(basename(sp[7]), "^toy__07_K.*adatok_1_spec\\.csv$")
})

test_that("a second run reuses the per-sheet specs, empty sheet included", {
  w <- wb_get()
  out <- file.path(w$base, "out_blank")        # same spec_dir as the first run
  msgs <- testthat::capture_messages(
    r2 <- suppressWarnings(synthesize_file(w$path, out, n_folds = 5, seed = 1)))
  expect_equal(sum(grepl("Using reviewed spec", msgs)), 7)
  expect_false(any(grepl("Inferred spec written", msgs)))
  expect_true(file.exists(r2$files[1]))
})

test_that("an edited role in a sheet spec is honoured on the next run", {
  w <- wb_get()
  out <- file.path(w$base, "out_edit")
  dir.create(out)
  specs <- list.files(file.path(w$base, "out_blank"), pattern = "_spec\\.csv$",
                      full.names = TRUE)
  expect_true(all(file.copy(specs, out)))
  fp <- file.path(out, "toy__01_Data_spec.csv")
  sp <- read_spec(fp)
  expect_equal(sp$role[sp$column == "response"], "numeric")
  sp$role[sp$column == "response"] <- "id"
  write_spec(sp, fp)
  # Legend rides along, as its spec file is reused too
  r <- quietly(synthesize_file(w$path, out, sheet = c("Data", "Legend"),
                               n_folds = 5, seed = 1))
  res <- r$results[[1]][[1]]
  expect_equal(res$spec$role[res$spec$column == "response"], "id")
  o <- read_sheet_text(r$files[1], "Data")
  expect_identical(o$response, w$src$Data$response)       # id: copied as is
  # an untouched modelled column is still redrawn
  expect_false(identical(o$notes, w$src$Data$notes) &&
                 identical(o$weight_kg, w$src$Data$weight_kg))
})

test_that("sheet = restricts the output to the named / indexed sheets", {
  w <- wb_get()
  for (sel in list("Data", 1, 1L)) {
    out <- tempfile("sel_")
    r <- quietly(synthesize_file(w$path, out, sheet = sel, n_folds = 5, seed = 2))
    expect_equal(readxl::excel_sheets(r$files[1]), "Data")
    expect_length(r$results[[1]], 1)
    o <- read_sheet_text(r$files[1], "Data")
    expect_identical(o$patientNo, w$src$Data$patientNo)
  }
  out <- tempfile("sel_")
  r <- quietly(synthesize_file(w$path, out, sheet = c(1, 7), n_folds = 5, seed = 2))
  expect_equal(readxl::excel_sheets(r$files[1]), c("Data", SHEET_ACCENT))
  r <- quietly(synthesize_file(w$path, tempfile("sel_"), sheet = c("Dups", "Data"),
                               n_folds = 5, seed = 2))
  expect_setequal(readxl::excel_sheets(r$files[1]), c("Dups", "Data"))
})

test_that("an unknown sheet name or index is an error", {
  w <- wb_get()
  expect_error(quietly(synthesize_file(w$path, tempfile("bad_"), sheet = "Nope")),
               "Sheets not in workbook")
  expect_error(quietly(synthesize_file(w$path, tempfile("bad_"), sheet = c("Data", "Nope"))),
               "Nope")
  expect_error(quietly(synthesize_file(w$path, tempfile("bad_"), sheet = 99)),
               "Sheets not in workbook")
  # spec_path is for single-sheet sources only
  expect_error(quietly(synthesize_file(w$path, tempfile("bad_"),
                                       spec_path = tempfile(fileext = ".csv"))),
               "spec_path")
})

test_that("a single-sheet workbook uses <stem>_spec.csv or spec_path", {
  f <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  set.seed(21)
  df <- data.frame(ID = sprintf("PT%03d", 1:50),
                   w = sprintf("%.1f", rnorm(50, 70, 9)),
                   g = sample(c("a", "b", "c"), 50, TRUE), stringsAsFactors = FALSE)
  add_text_sheet(wb, "Only", df)
  openxlsx::saveWorkbook(wb, f)
  stem <- tools::file_path_sans_ext(basename(f))
  out <- tempfile("single_")
  r <- quietly(synthesize_file(f, out, n_folds = 5, seed = 1))
  expect_equal(basename(r$spec_paths), paste0(stem, "_spec.csv"))
  expect_equal(readxl::excel_sheets(r$files[1]), "Only")
  sp <- file.path(tempfile("specs_"), "my_spec.csv")
  r2 <- quietly(synthesize_file(f, tempfile("single_"), spec_path = sp,
                                n_folds = 5, seed = 1))
  expect_true(file.exists(sp))
  expect_equal(r2$spec_paths, sp)
})

test_that("sheet names differing only in punctuation get distinct spec files", {
  # "A B" and "A_B" (and e.g. "Adat á" / "Adat é") must not share a spec
  f <- tempfile(fileext = ".xlsx")
  set.seed(22)
  mk <- function() data.frame(ID = sprintf("PT%03d", 1:30),
                              w = sprintf("%.1f", rnorm(30, 70, 9)),
                              stringsAsFactors = FALSE)
  wb <- openxlsx::createWorkbook()
  add_text_sheet(wb, "A B", mk())
  add_text_sheet(wb, "A_B", mk())
  openxlsx::saveWorkbook(wb, f)
  r <- quietly(synthesize_file(f, tempfile("coll_"), n_folds = 5, seed = 1))
  expect_equal(anyDuplicated(r$spec_paths), 0L)
})

test_that("two single-sheet selections from one workbook do not share a spec", {
  w <- wb_get()
  out <- tempfile("pick_")
  r0 <- quietly(synthesize_file(w$path, out, sheet = "Data", n_folds = 5, seed = 1))
  # the file name carries the sheet's position in the workbook
  expect_match(basename(r0$spec_paths), "^toy__01_Data_spec\\.csv$")
  # a different sheet of the same workbook, same spec_dir: must neither error
  # on the other sheet's spec nor silently reuse it
  expect_no_error(
    r <- quietly(synthesize_file(w$path, out, sheet = "Dups", n_folds = 5, seed = 1)))
  expect_equal(readxl::excel_sheets(r$files[1]), "Dups")
  expect_match(basename(r$spec_paths), "^toy__06_Dups_spec\\.csv$")
})

# ---- spec files round trip --------------------------------------------------

test_that("infer_spec handles 0-column sources and its CSV reads back valid", {
  w <- wb_get()
  src <- read_source(w$path, "Empty")
  expect_equal(ncol(src$raw), 0L)
  sp <- quietly(infer_spec(src))
  expect_equal(nrow(sp), 0L)
  expect_true(all(c("column", "orig_name", "role") %in% names(sp)))
  f <- tempfile(fileext = ".csv")
  write_spec(sp, f)
  back <- read_spec(f)
  expect_no_error(quietly(validate_spec(back, src)))
})

test_that("a spec CSV reads back identically even for all-numeric headers", {
  raw <- data.frame(`1` = c("a", "b", "a", "b"), `2` = c("x", "y", "x", "y"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  src <- structure(list(raw = raw, orig_names = names(raw),
                        orig_class = rep("character", 2), path = "x.csv",
                        sheet = NULL, format = "csv"), class = "synth_source")
  sp <- quietly(infer_spec(src))
  f <- tempfile(fileext = ".csv")
  write_spec(sp, f)
  back <- read_spec(f)
  expect_identical(back$column, sp$column)
  expect_no_error(quietly(validate_spec(back, src)))
})

# ---- passthrough_result() and write_synthetic() on lists ---------------------

test_that("passthrough_result: copy, blank and drop", {
  w <- wb_get()
  src <- read_source(w$path, "Dups")
  sp <- infer_spec(src, id = "ID")
  cp <- passthrough_result(src, sp, "copy", "test")
  expect_identical(cp$synthetic, src$raw)
  expect_equal(cp$settings$passthrough, "copy")
  expect_s3_class(cp, "synth_result")
  expect_output(print(cp), "not synthesised")
  bl <- passthrough_result(src, sp, "blank")
  expect_identical(bl$synthetic$ID, src$raw$ID)
  expect_true(all(is.na(unlist(bl$synthetic[sp$role %in% MODEL_ROLES]))))
  dr <- passthrough_result(src, sp, "drop")
  expect_equal(dr$settings$passthrough, "drop")
  expect_error(passthrough_result(src, sp, "nonsense"))
})

test_that("write_synthetic: workbook from a list, dropped sheets left out", {
  w <- wb_get()
  mk <- function(s, how) {
    src <- read_source(w$path, s)
    passthrough_result(src, quietly(infer_spec(src)), how)
  }
  a <- mk("IdOnly", "copy")
  b <- mk("Legend", "drop")
  c_ <- mk("HeaderOnly", "copy")
  out <- tempfile(fileext = ".xlsx")
  write_synthetic(list(a, b, c_), out)
  expect_equal(readxl::excel_sheets(out), c("IdOnly", "HeaderOnly"))
  expect_error(write_synthetic(list(b), tempfile(fileext = ".xlsx")),
               "dropped")
  expect_error(write_synthetic(list(), tempfile(fileext = ".xlsx")))
  expect_error(write_synthetic(list(a, "x"), tempfile(fileext = ".xlsx")))
  # a single result is accepted as well
  out1 <- tempfile(fileext = ".xlsx")
  write_synthetic(a, out1)
  expect_equal(readxl::excel_sheets(out1), "IdOnly")
})

test_that("write_synthetic: a CSV holds one table", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("ID,v", "P1,1", "P2,2"), f)
  src <- read_source(f)
  pr <- passthrough_result(src, quietly(infer_spec(src)), "copy")
  expect_error(write_synthetic(list(pr, pr), tempfile(fileext = ".csv")),
               "one table")
  out <- tempfile(fileext = ".csv")
  write_synthetic(list(pr), out)
  expect_equal(readLines(out), c("ID,v", "P1,1", "P2,2"))
})

test_that("write_synthetic: sheet names are unique ignoring case, within 31 chars", {
  w <- wb_get()
  src <- read_source(w$path, "IdOnly")
  base <- passthrough_result(src, quietly(infer_spec(src)), "copy")
  nm <- function(x) { b <- base; b$sheet <- x; b }
  long <- strrep("L", 40)
  out <- tempfile(fileext = ".xlsx")
  write_synthetic(list(nm("Abc"), nm("ABC"), nm("abc"), nm(long), nm(long)), out)
  got <- readxl::excel_sheets(out)
  expect_length(got, 5)
  expect_equal(anyDuplicated(tolower(got)), 0L)
  expect_true(all(nchar(got) <= 31))
  expect_equal(got[1], "Abc")
})
