leaf_donors <- cellsynth:::leaf_donors

make_xy <- function(n = 100, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- sample(letters[1:3], n, TRUE)
  X <- data.frame(x1 = x1, x2 = factor(x2))
  list(X = X, y_num = ifelse(x1 > 0, 100, 0) + rnorm(n, 0, 0.1),
       y_fac = factor(ifelse(x1 > 0, "pos", "neg")))
}

test_that("donors are training rows, never prediction rows (numeric target)", {
  d <- make_xy()
  pred <- seq_len(100) %% 4 == 0
  set.seed(2)
  out <- leaf_donors(d$y_num, d$X, !pred, pred, minbucket = 5)
  expect_length(out$donor, sum(pred))
  expect_true(all(out$donor %in% which(!pred)))
  expect_false(any(out$donor %in% which(pred)))
  expect_false(out$fallback)
  expect_gt(out$leaves, 1)
  # the informative split is respected: donors come from the same side
  expect_equal(unname(d$y_num[out$donor] > 50), unname(d$y_num[pred] > 50))
})

test_that("works for factor targets", {
  d <- make_xy()
  pred <- seq_len(100) %% 5 == 0
  set.seed(3)
  out <- leaf_donors(d$y_fac, d$X, !pred, pred, minbucket = 5)
  expect_length(out$donor, sum(pred))
  expect_true(all(out$donor %in% which(!pred)))
  expect_equal(as.character(d$y_fac[out$donor]), as.character(d$y_fac[pred]))
})

test_that("falls back to marginal draws with too few training rows", {
  d <- make_xy(n = 30)
  train <- seq_len(30) <= 8      # 8 < 2 * minbucket
  pred <- !train
  out <- leaf_donors(d$y_num, d$X, train, pred, minbucket = 5)
  expect_true(out$fallback)
  expect_length(out$donor, sum(pred))
  expect_true(all(out$donor %in% which(train)))
})

test_that("falls back when the target is constant in the training rows", {
  d <- make_xy()
  y <- rep(1, 100)
  pred <- seq_len(100) %% 4 == 0
  out <- leaf_donors(y, d$X, !pred, pred)
  expect_true(out$fallback)
  expect_true(all(out$donor %in% which(!pred)))
})

test_that("works with a zero-column predictor frame", {
  d <- make_xy()
  X0 <- d$X[, 0, drop = FALSE]
  expect_equal(ncol(X0), 0)
  pred <- seq_len(100) %% 4 == 0
  out <- leaf_donors(d$y_num, X0, !pred, pred)
  expect_true(out$fallback)
  expect_length(out$donor, sum(pred))
  expect_true(all(out$donor %in% which(!pred)))
})

test_that("training rows with all predictors NA keep donors aligned (regression)", {
  d <- make_xy()
  X <- d$X
  X[c(2, 3, 7, 11, 40), ] <- NA     # all predictors missing in some training rows
  pred <- seq_len(100) %% 4 == 0
  pred[c(7, 40)] <- TRUE            # and some prediction rows too
  train <- !pred
  for (s in 1:5) {
    set.seed(s)
    out <- leaf_donors(d$y_num, X, train, pred, minbucket = 5)
    expect_length(out$donor, sum(pred))
    expect_true(all(out$donor %in% which(train)))
    expect_false(anyNA(out$donor))
  }
  out_f <- leaf_donors(d$y_fac, X, train, pred, minbucket = 5)
  expect_length(out_f$donor, sum(pred))
  expect_true(all(out_f$donor %in% which(train)))
  # a single predictor that is NA in many training rows
  X1 <- X[, "x1", drop = FALSE]
  X1$x1[sample(100, 40)] <- NA
  out1 <- leaf_donors(d$y_num, X1, train, pred, minbucket = 5)
  expect_length(out1$donor, sum(pred))
  expect_true(all(out1$donor %in% which(train)))
})

test_that("edge cases: nothing to predict, nothing to train on", {
  d <- make_xy()
  none <- rep(FALSE, 100)
  out <- leaf_donors(d$y_num, d$X, rep(TRUE, 100), none)
  expect_length(out$donor, 0)
  expect_error(leaf_donors(d$y_num, d$X, none, rep(TRUE, 100)), "No training rows")
})

test_that("donors never come from the prediction rows across seeds", {
  d <- make_xy()
  pred <- seq_len(100) %% 3 == 0
  for (s in 1:10) {
    set.seed(s)
    out <- leaf_donors(d$y_num, d$X, !pred, pred)
    expect_false(any(out$donor %in% which(pred)))
  }
})
