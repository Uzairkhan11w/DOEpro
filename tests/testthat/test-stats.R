test_that("every transformation is undone exactly by its back-transformation", {
  # each entry of TRANS holds f (forward) and b (backward); both take a lambda,
  # which only the Box-Cox transformation actually uses
  x <- c(0.5, 1, 2, 5, 10, 50)
  for (nm in setdiff(names(TRANS), c("arcsine", "arcsine01", "boxcox"))) {
    tr <- TRANS[[nm]]
    expect_equal(tr$b(tr$f(x, 1), 1), x, tolerance = 1e-8, info = nm)
  }
  # the arcsine transformations are defined on proportions and percentages
  p <- c(0.05, 0.2, 0.5, 0.8, 0.95)
  tr <- TRANS[["arcsine01"]]
  expect_equal(tr$b(tr$f(p, 1), 1), p, tolerance = 1e-8)
  pc <- c(5, 20, 50, 80, 95)
  tr <- TRANS[["arcsine"]]
  expect_equal(tr$b(tr$f(pc, 1), 1), pc, tolerance = 1e-8)
  # Box-Cox round-trips at several lambdas
  for (l in c(-1, -0.5, 0.5, 1, 2)) {
    tr <- TRANS[["boxcox"]]
    expect_equal(tr$b(tr$f(x, l), l), x, tolerance = 1e-8, info = paste("boxcox lambda", l))
  }
})

test_that("Levene's test matches its definition", {
  set.seed(1)
  y <- c(rnorm(10, 10, 1), rnorm(10, 10, 3))
  g <- factor(rep(c("a", "b"), each = 10))
  got <- levene_test(y, g)
  # by definition: a one-way ANOVA on the absolute deviations from the group medians
  z <- abs(y - ave(y, g, FUN = stats::median))
  ref <- stats::anova(stats::lm(z ~ g))
  expect_equal(got$F, ref[1, "F value"], tolerance = 1e-8)
  expect_equal(got$p, ref[1, "Pr(>F)"], tolerance = 1e-8)
})

test_that("Bartlett's test on mean squares matches stats::bartlett.test", {
  set.seed(2)
  g <- factor(rep(1:3, each = 8))
  y <- c(rnorm(8, 10, 1), rnorm(8, 10, 1.5), rnorm(8, 10, 2))
  # per-group variances and their degrees of freedom
  v  <- tapply(y, g, stats::var)
  df <- tapply(y, g, function(x) length(x) - 1)
  got <- bartlett_ms(as.numeric(df), as.numeric(v))
  ref <- stats::bartlett.test(y, g)
  expect_equal(got$chisq, unname(ref$statistic), tolerance = 1e-8)
  expect_equal(got$df, unname(ref$parameter))
  expect_equal(got$p, ref$p.value, tolerance = 1e-8)
})

test_that("the Box-Cox profile peaks at the lambda it reports", {
  d <- demo_data("CRD")
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  bc <- boxcox_profile(d$Yield, r$X)
  expect_false(is.null(bc))
  expect_true(is.finite(bc$lambda))
  expect_true(bc$lambda >= -2 && bc$lambda <= 2)
  # the reported lambda is the maximum of the profile likelihood
  expect_equal(bc$lambda, bc$x[which.max(bc$y)], tolerance = 1e-8)
  # and the profile matches the textbook expression at that lambda
  y <- d$Yield; n <- length(y); gm <- exp(mean(log(y))); l <- bc$lambda
  z <- if (abs(l) < 1e-9) gm * log(y) else (y^l - 1) / (l * gm^(l - 1))
  rss <- sum(qr.resid(qr(r$X), z)^2)
  expect_equal(max(bc$y, na.rm = TRUE), -n / 2 * log(rss / n), tolerance = 1e-6)
})

test_that("Box-Cox refuses data that is not strictly positive", {
  expect_null(boxcox_profile(c(1, 2, 0, 4), matrix(1, 4, 1)))
  expect_null(boxcox_profile(c(1, -2, 3, 4), matrix(1, 4, 1)))
})

test_that("the critical differences are ordered LSD <= Tukey <= Scheffe", {
  d <- demo_data("CRD")
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  e <- r$effects[["Treatment"]]
  k <- nrow(e$means); dfe <- r$dfe

  # computed from their definitions, not read back from the display table
  lsd     <- stats::qt(0.975, dfe) * e$sed
  tukey   <- stats::qtukey(0.95, k, dfe) / sqrt(2) * e$sed
  scheffe <- sqrt((k - 1) * stats::qf(0.95, k - 1, dfe)) * e$sed

  expect_lte(lsd, tukey + 1e-8)
  expect_lte(tukey, scheffe + 1e-8)
})

test_that("Duncan's shortest range equals the LSD, and its longest equals Tukey's", {
  d <- demo_data("CRD")
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  dun <- posthoc(r, "Treatment", "Duncan's DMRT", 0.05)
  snk <- posthoc(r, "Treatment", "Student-Newman-Keuls", 0.05)
  e <- r$effects[["Treatment"]]
  k <- nrow(e$means); dfe <- r$dfe
  lsd <- stats::qt(0.975, dfe) * e$sed
  tuk <- stats::qtukey(0.95, k, dfe) / sqrt(2) * e$sed

  dr <- dun$ranges[["Critical range"]]
  sr <- snk$ranges[["Critical range"]]
  # two means apart: both Duncan and SNK reduce to the LSD
  expect_equal(dr[1], lsd, tolerance = 1e-6)
  expect_equal(sr[1], lsd, tolerance = 1e-6)
  # at the full span, SNK reaches Tukey's HSD
  expect_equal(sr[length(sr)], tuk, tolerance = 1e-6)
  # Duncan is never more conservative than SNK
  expect_true(all(dr <= sr + 1e-8))
})

test_that("the coefficient of variation matches its definition", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  expect_equal(unname(r$cv[1]), 100 * sqrt(r$mse) / r$grand, tolerance = 1e-8)
})

test_that("standard errors follow from the error mean square", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  e <- r$effects[["Variety"]]
  expect_equal(e$sem, sqrt(r$mse / e$n_per_mean), tolerance = 1e-8)
  expect_equal(e$sed, sqrt(2 * r$mse / e$n_per_mean), tolerance = 1e-8)
  expect_equal(e$cd5, stats::qt(0.975, r$dfe) * e$sed, tolerance = 1e-8)
})
