test_that("parse_number_chr handles plain, comma, scientific and junk", {
  x <- c("1", "1.5", " 2.50 ", "-3", "+4.2", ".5", "5.", "1,5", "-2,25",
         "1e3", "2.5E-2", "abc", "0 / 1", "1,234.5", "1.2.3", "", NA)
  out <- parse_number_chr(x)
  expect_length(out, length(x))
  expect_equal(out[1:11],
               c(1, 1.5, 2.5, -3, 4.2, 0.5, 5, 1.5, -2.25, 1000, 0.025))
  expect_true(all(is.na(out[12:17])))
})

test_that("parse_number_chr does not turn ambiguous comma strings into numbers", {
  expect_true(is.na(parse_number_chr("1,2,3")))
  expect_true(is.na(parse_number_chr("1,234.5")))
  expect_equal(parse_number_chr(character()), numeric())
})

test_that("parse_date_chr reads the supported formats", {
  ref <- as.numeric(as.Date("2024-01-15"))
  x <- c("2024-01-15", "2024-1-15", "2024-01-15 10:30:00", "2024-01-15T10:30",
         "2024.01.15.", "2024.01.15", "2024. 01. 15.", "2024/01/15",
         "15.01.2024", "15.01.2024.")
  expect_equal(parse_date_chr(x), rep(ref, length(x)))
})

test_that("parse_date_chr rejects junk and impossible dates", {
  x <- c("hello", "2024-13-45", "2024.02.31.", "", NA, "1800-01-01",
         "2200-01-01")
  expect_true(all(is.na(parse_date_chr(x))))
})

test_that("Excel serials are dates only when allow_serial = TRUE", {
  expect_true(is.na(parse_date_chr("45306")))
  expect_equal(parse_date_chr("45306", allow_serial = TRUE),
               as.numeric(as.Date("2024-01-15")))
  # time-of-day fraction is dropped
  expect_equal(parse_date_chr("45306.75", allow_serial = TRUE),
               as.numeric(as.Date("2024-01-15")))
  # implausible serials stay NA even when allowed
  expect_true(is.na(parse_date_chr("12", allow_serial = TRUE)))
  expect_true(is.na(parse_date_chr("999999", allow_serial = TRUE)))
  # serial and ISO agree
  expect_equal(parse_date_chr(c("45306", "2024-01-15"), allow_serial = TRUE),
               rep(as.numeric(as.Date("2024-01-15")), 2))
})

test_that("normalise_label collapses numeric-looking labels only", {
  expect_equal(normalise_label("1.0"), normalise_label("1"))
  expect_equal(normalise_label(" 1 "), "1")
  expect_equal(normalise_label("1,0"), "1")
  expect_equal(normalise_label("0.50"), normalise_label("0.5"))
  expect_false(normalise_label("1.0") == normalise_label("1.1"))
  expect_equal(normalise_label("  alpha "), "alpha")
  expect_equal(normalise_label("0 / 1"), "0 / 1")
  expect_true(is.na(normalise_label(NA_character_)))
})
