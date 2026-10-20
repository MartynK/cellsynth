fast_synth <- function(src, spec = NULL, seed = 1, ...) {
  if (is.null(spec)) spec <- infer_spec(src)
  synthesize_table(src, spec, n_folds = 5, minbucket = 5, seed = seed,
                   verbose = FALSE, ...)
}

# A 60-row csv with a duplicated header name, written line by line so the
# raw strings are exactly what we intend
write_toy_csv <- function(path, n = 60) {
  set.seed(4)
  ids <- sprintf("PT%03d", seq_len(n))
  a <- sprintf("%.1f", rnorm(n, 10, 2))
  b <- sample(c("1.0", "1", "0.0", "0"), n, TRUE)
  cat_ <- sample(c("red", "green", "blue"), n, TRUE)
  a[c(3, 9)] <- ""
  lines <- c("ID,val,val,colour",
             paste(ids, a, b, cat_, sep = ","))
  writeLines(lines, path)
  invisible(path)
}

test_that("read_source keeps raw strings, header and duplicate names (csv)", {
  f <- tempfile(fileext = ".csv")
  write_toy_csv(f)
  src <- read_source(f)
  expect_s3_class(src, "synth_source")
  expect_equal(src$format, "csv")
  expect_equal(src$orig_names, c("ID", "val", "val", "colour"))
  expect_equal(anyDuplicated(names(src$raw)), 0L)
  expect_true(all(vapply(src$raw, is.character, logical(1))))
  expect_true(all(src$raw[[3]] %in% c("1.0", "1", "0.0", "0")))
  expect_true(all(grepl("^[0-9]+\\.[0-9]$", na.omit(src$raw[[2]]))))
  expect_equal(sum(is.na(src$raw[[2]])), 2)
  expect_output(print(src), "synth_source")
  expect_error(read_source(tempfile(fileext = ".csv")), "File not found")
  g <- tempfile(fileext = ".json")
  writeLines("{}", g)
  expect_error(read_source(g), "Unsupported")
})

test_that("csv round trip through synthesise + write_synthetic", {
  f <- tempfile(fileext = ".csv")
  write_toy_csv(f)
  src <- read_source(f)
  res <- fast_synth(src, seed = 2)
  out <- tempfile(fileext = ".csv")
  write_synthetic(res, out)
  expect_equal(readLines(out, n = 1), "ID,val,val,colour")
  back <- read_source(out)
  expect_equal(back$orig_names, src$orig_names)
  expect_identical(back$raw, res$synthetic)
  # raw strings: the "1.0" vs "1" distinction is preserved on disk
  expect_true(all(na.omit(back$raw[[3]]) %in% c("1.0", "1", "0.0", "0")))
  expect_identical(back$raw[[1]], src$raw[[1]])
})

make_xlsx <- function(path, n = 60) {
  set.seed(5)
  df <- data.frame(ID = sprintf("PT%03d", seq_len(n)),
                   weight = round(rnorm(n, 70, 10), 1),
                   weight2 = round(rnorm(n, 71, 10), 1),
                   grp = sample(c("a", "b", "c"), n, TRUE),
                   when = as.Date("2024-01-01") + sample(0:200, n, TRUE),
                   stringsAsFactors = FALSE)
  names(df)[3] <- "weight"             # duplicated header
  df$weight[4] <- NA
  openxlsx::write.xlsx(df, path, sheetName = "Data")
  invisible(df)
}

test_that("xlsx round trip keeps header and restores numeric / date types", {
  f <- tempfile(fileext = ".xlsx")
  orig <- make_xlsx(f)
  src <- read_source(f)
  expect_equal(src$format, "xlsx")
  expect_equal(src$sheet, "Data")
  expect_equal(src$orig_names, c("ID", "weight", "weight", "grp", "when"))
  expect_equal(src$orig_class[1:4], c("character", "numeric", "numeric", "character"))
  expect_true("POSIXct" %in% src$orig_class[5])

  res <- fast_synth(src, seed = 3)
  out <- tempfile(fileext = ".xlsx")
  write_synthetic(res, out)

  back <- suppressMessages(readxl::read_excel(out, .name_repair = "minimal"))
  expect_equal(names(back), c("ID", "weight", "weight", "grp", "when"))
  expect_equal(nrow(back), nrow(orig))
  expect_type(back[[2]], "double")
  expect_type(back[[3]], "double")
  expect_type(back[[4]], "character")
  expect_type(back[[1]], "character")
  expect_s3_class(back[[5]], "POSIXct")
  expect_equal(back[[1]], orig$ID)
  expect_equal(readxl::excel_sheets(out), "Data")
  # numbers written are donor numbers
  expect_true(all(stats::na.omit(back[[2]]) %in% stats::na.omit(orig$weight)))
})

test_that("restore_type handles numeric, logical, date and fall-through", {
  rt <- cellsynth:::restore_type
  expect_equal(rt(c("1,5", "2", NA), "numeric"), c(1.5, 2, NA))
  expect_equal(rt(c("1", "x"), "numeric"), c("1", "x"))      # not all numeric: left as text
  expect_equal(rt(c("TRUE", "false", NA), "logical"), c(TRUE, FALSE, NA))
  expect_equal(rt(c(NA_character_, NA_character_), "numeric"), c(NA_character_, NA_character_))
  d <- rt(c("45306", NA), "POSIXct")
  expect_s3_class(d, "Date")
  expect_equal(as.character(d), c("2024-01-15", NA))
  dt <- rt(c("45306.5"), "POSIXct")
  expect_s3_class(dt, "POSIXct")
  d2 <- rt(c("2024-01-15", "2024.01.16."), "Date")
  expect_equal(as.character(d2), c("2024-01-15", "2024-01-16"))
  expect_equal(rt(c("a", "b"), "character"), c("a", "b"))
})

test_that("synthesize_file runs end to end and reuses a reviewed spec", {
  f <- tempfile(fileext = ".csv")
  write_toy_csv(f)
  out_dir <- file.path(tempdir(), "synth_out")
  r <- suppressMessages(synthesize_file(f, out_dir, id = "ID", m = 2, seed = 7,
                                        n_folds = 5, verbose = FALSE))
  expect_length(r$results, 2)
  expect_true(all(file.exists(r$files)))
  expect_true(file.exists(r$spec_path))
  stem <- tools::file_path_sans_ext(basename(f))
  expect_true(file.exists(file.path(out_dir, paste0(stem, "_runlog.txt"))))
  expect_match(basename(r$files[2]), "_synth2\\.csv$")
  # copies differ (seed + i - 1)
  expect_false(identical(r$results[[1]][[1]]$synthetic, r$results[[2]][[1]]$synthetic))
  # second run picks up the spec on disk
  msgs <- testthat::capture_messages(
    synthesize_file(f, out_dir, m = 1, seed = 7, n_folds = 5, verbose = FALSE))
  expect_true(any(grepl("Using reviewed spec", msgs)))
  # a spec that no longer matches the source is rejected
  sp <- read_spec(r$spec_path)
  write_spec(sp[-1, ], r$spec_path)
  expect_error(suppressMessages(synthesize_file(f, out_dir, n_folds = 5)),
               "do not match")
})

test_that("xlsx round trip keeps in-cell line breaks and early Excel serials", {
  # openxlsx writes "\n" as "\r\n" and shifts Date serials < 61 by a day;
  # write_synthetic() compensates for both
  src <- structure(list(
    raw = data.frame(id = c("AB1", "AB2", "AB3"), txt = c("a\r\nb", "plain", NA),
                     dt = c("1", "45000", "61"), stringsAsFactors = FALSE),
    orig_names = c("id", "txt", "dt"), orig_class = c("character", "character", "POSIXct"),
    path = "toy.xlsx", sheet = "S", format = "xlsx"), class = "synth_source")
  spec <- infer_spec(src)
  res <- passthrough_result(src, spec, "copy")
  p <- tempfile(fileext = ".xlsx")
  write_synthetic(res, p)
  back <- read_source(p, "S")
  expect_identical(back$raw$txt, src$raw$txt)
  expect_identical(parse_number_chr(back$raw$dt), c(1, 45000, 61))
})
