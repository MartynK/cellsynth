# Text-file formats: delimiter and encoding detection, delimiter kept on output.

HU_HEADER <- c("Betegszám", "Súlyf", "Nem", "Megjegyzés", "Lelet")

# 60-row Hungarian Excel-style export: ';' delimited, decimal commas,
# windows-1250 bytes on disk, a few quoted fields holding commas
make_hu_csv <- function(path, n = 60, seed = 31) {
  set.seed(seed)
  notes <- c("őszülő vizsgálat", "tűző fájdalom",
             "kávé és süti", "ügyelet után", "átlagos")
  lelet <- c("normál", "érték felül", "\"a,b,c\"", "üres")
  lines <- c(paste(HU_HEADER, collapse = ";"),
             paste(sprintf("P%03d", seq_len(n)),
                   sub(".", ",", sprintf("%.1f", rnorm(n, 72, 10)), fixed = TRUE),
                   sample(c("nő", "férfi"), n, TRUE),
                   sample(notes, n, TRUE),
                   sample(lelet, n, TRUE), sep = ";"))
  write_text_enc(lines, path, "windows-1250")
  invisible(lines)
}

test_that("semicolon / windows-1250 file: delimiter, encoding, accents", {
  f <- tempfile(fileext = ".csv")
  make_hu_csv(f)
  # sanity: the bytes on disk are not UTF-8
  expect_false(validUTF8(rawToChar(readBin(f, "raw", file.size(f)))))
  src <- read_source(f)
  expect_equal(src$delim, ";")
  expect_equal(src$encoding, "windows-1250")
  expect_equal(src$orig_names, HU_HEADER)
  expect_equal(nrow(src$raw), 60L)
  expect_true(all(src$raw[[3]] %in% c("nő", "férfi")))
  expect_true(all(grepl("^[0-9]+,[0-9]$", src$raw[[2]])))     # decimal comma kept
  expect_true(any(grepl("ő", src$raw[[4]])))
  expect_true(any(grepl("ű", src$raw[[4]])))
  expect_true(all(src$raw[[5]] %in% c("normál", "érték felül", "a,b,c", "üres")))
  # explicit overrides win
  forced <- read_source(f, delim = ";", encoding = "windows-1250")
  expect_identical(forced$raw, src$raw)
})

test_that("semicolon / windows-1250 file round trips with the same delimiter", {
  f <- tempfile(fileext = ".csv")
  make_hu_csv(f)
  out <- tempfile("hu_out_")
  r <- quietly(synthesize_file(f, out, n_folds = 5, seed = 4))
  stem <- tools::file_path_sans_ext(basename(f))
  expect_equal(basename(r$files), paste0(stem, "_synth1.csv"))
  txt <- readLines(r$files, encoding = "UTF-8")
  expect_equal(txt[1], paste(HU_HEADER, collapse = ";"))
  expect_length(txt, 61)
  # UTF-8 on disk, accents intact
  expect_true(validUTF8(rawToChar(readBin(r$files, "raw", file.size(r$files)))))
  back <- read_source(r$files)
  expect_equal(back$delim, ";")
  expect_equal(back$encoding, "UTF-8")
  expect_equal(back$orig_names, HU_HEADER)
  src <- read_source(f)
  expect_identical(back$raw[[1]], src$raw[[1]])              # ids kept
  expect_true(all(back$raw[[3]] %in% c("nő", "férfi")))
  expect_true(all(stats::na.omit(back$raw[[2]]) %in% src$raw[[2]]))
  expect_true(all(stats::na.omit(back$raw[[4]]) %in% src$raw[[4]]))
  # the accented header survives a reviewed-spec second run
  msgs <- testthat::capture_messages(
    suppressWarnings(synthesize_file(f, out, n_folds = 5, seed = 4)))
  expect_true(any(grepl("Using reviewed spec", msgs)))
  # delim / encoding can be passed through synthesize_file()
  r3 <- quietly(synthesize_file(f, tempfile("hu_out_"), delim = ";",
                                encoding = "windows-1250", n_folds = 5, seed = 4))
  expect_equal(readLines(r3$files, encoding = "UTF-8")[1], paste(HU_HEADER, collapse = ";"))
})

make_delim_file <- function(path, delim, ext_ok = TRUE, n = 60, seed = 32) {
  set.seed(seed)
  # free-text-ish values carry commas, so a comma cannot be mistaken for the delimiter
  val <- sprintf("%.1f", rnorm(n, 10, 2))
  note <- sample(c("red, dark", "green", "blue, light, pale", "grey"), n, TRUE)
  grp <- sample(c("a", "b", "c"), n, TRUE)
  lines <- c(paste(c("ID", "val", "grp", "note"), collapse = delim),
             paste(sprintf("PT%03d", seq_len(n)), val, grp, note, sep = delim))
  writeLines(lines, path)
  invisible(list(val = val, note = note, grp = grp))
}

test_that("tab-delimited .txt: detected, synthesised, written back tab-separated", {
  f <- tempfile(fileext = ".txt")
  v <- make_delim_file(f, "\t")
  src <- read_source(f)
  expect_equal(src$delim, "\t")
  expect_equal(src$orig_names, c("ID", "val", "grp", "note"))
  expect_equal(src$raw$note, v$note)
  r <- quietly(synthesize_file(f, tempfile("tab_out_"), n_folds = 5, seed = 5))
  expect_match(basename(r$files), "_synth1\\.csv$")
  txt <- readLines(r$files)
  expect_equal(txt[1], "ID\tval\tgrp\tnote")
  expect_true(all(lengths(strsplit(txt, "\t", fixed = TRUE)) == 4))
  back <- read_source(r$files)
  expect_equal(back$delim, "\t")
  expect_identical(back$raw$ID, src$raw$ID)
  expect_true(all(back$raw$note %in% v$note | is.na(back$raw$note)))
})

test_that("pipe-delimited .csv: detected, synthesised, written back with pipes", {
  f <- tempfile(fileext = ".csv")
  v <- make_delim_file(f, "|")
  src <- read_source(f)
  expect_equal(src$delim, "|")
  expect_equal(ncol(src$raw), 4L)
  r <- quietly(synthesize_file(f, tempfile("pipe_out_"), n_folds = 5, seed = 6))
  txt <- readLines(r$files)
  expect_equal(txt[1], "ID|val|grp|note")
  back <- read_source(r$files)
  expect_equal(back$delim, "|")
  expect_equal(back$orig_names, c("ID", "val", "grp", "note"))
  expect_identical(back$raw$ID, src$raw$ID)
  expect_true(all(stats::na.omit(back$raw$val) %in% v$val))
})

test_that("comma and semicolon plain files are detected", {
  f <- tempfile(fileext = ".csv")
  # a header + rows where every line has the same number of commas
  writeLines(c("a,b,c", "1,2,3", "4,5,6", "7,8,9"), f)
  expect_equal(read_source(f)$delim, ",")
  g <- tempfile(fileext = ".csv")
  writeLines(c("a;b;c", "1;2;3", "4;5;6", "7;8;9"), g)
  expect_equal(read_source(g)$delim, ";")
  expect_equal(read_source(g)$encoding, "UTF-8")
  # an empty file defaults to comma without erroring in detection
  h <- tempfile(fileext = ".csv")
  writeLines(character(), h)
  expect_equal(cellsynth:::detect_delim(h), ",")
})

test_that("delimiters inside double-quoted fields are not counted", {
  # semicolon file whose quoted fields hold many commas
  f <- tempfile(fileext = ".csv")
  writeLines(c("id;note;v",
               "P1;\"a,b,c,d,e\";1",
               "P2;\"x,y,z,w\";2",
               "P3;\"1,2,3,4,5,6\";3",
               "P4;\"q,r,s,t,u,v,w\";4",
               "P5;\"k,l,m,n\";5"), f)
  src <- read_source(f)
  expect_equal(src$delim, ";")
  expect_equal(src$orig_names, c("id", "note", "v"))
  expect_equal(src$raw$note, c("a,b,c,d,e", "x,y,z,w", "1,2,3,4,5,6",
                               "q,r,s,t,u,v,w", "k,l,m,n"))
  # comma file whose quoted fields hold many semicolons
  g <- tempfile(fileext = ".csv")
  writeLines(c("id,note,v",
               "P1,\"a;b;c;d;e\",1",
               "P2,\"x;y;z;w\",2",
               "P3,\"1;2;3;4;5;6\",3",
               "P4,\"q;r;s;t;u;v;w\",4"), g)
  expect_equal(read_source(g)$delim, ",")
  expect_equal(read_source(g)$raw$note[1], "a;b;c;d;e")
})

test_that("UTF-8 files with accents stay UTF-8", {
  # No trailing newline and an accented letter 3 characters from the end:
  # detect_encoding() must not cut a multibyte character in half and conclude
  # that the file is not UTF-8.
  f <- tempfile(fileext = ".csv")
  lines <- c("Betegszám;Nem", sprintf("P%03d;%s", 1:40, rep(c("nő", "férfi"), 20)))
  con <- file(f, "wb"); writeBin(charToRaw(enc2utf8(paste(lines, collapse = "\n"))), con); close(con)
  src <- read_source(f)
  expect_equal(src$encoding, "UTF-8")
  expect_equal(src$delim, ";")
  expect_equal(src$orig_names, c("Betegszám", "Nem"))
  expect_true(all(src$raw[[2]] %in% c("nő", "férfi")))
})
