## The Assumptions & Diagnostics tab: Tukey's test of additivity, Cook's
## distance, and the summary of the checks.

## ------------------------------------------------- Tukey's additivity test --

## Tukey's (1949) one degree of freedom for non-additivity, by its textbook
## formula for a complete block-by-treatment table
tukey_by_hand <- function(y, blk, trt) {
  Y <- tapply(y, list(blk, trt), mean)
  bi <- rowMeans(Y) - mean(Y); tj <- colMeans(Y) - mean(Y)
  ssn <- sum(Y * outer(bi, tj))^2 / (sum(bi^2) * sum(tj^2))
  res <- Y - outer(rowMeans(Y), colMeans(Y), "+") + mean(Y)
  sse <- sum(res^2); dfe <- (nrow(Y) - 1) * (ncol(Y) - 1)
  Fv <- ssn / ((sse - ssn) / (dfe - 1))
  c(F = Fv, p = pf(Fv, 1, dfe - 1, lower.tail = FALSE), df2 = dfe - 1)
}

test_that("Tukey's test of additivity matches its textbook formula", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  ta <- tukey_additivity(r)
  ref <- tukey_by_hand(d$Yield, d$Block, d$Variety)
  expect_true(ta$applies)
  expect_equal(ta$F, unname(ref["F"]), tolerance = 1e-8)
  expect_equal(ta$p, unname(ref["p"]), tolerance = 1e-8)
  expect_equal(ta$df2, unname(ref["df2"]))
})

test_that("Tukey's test finds effects that multiply rather than add", {
  set.seed(4)
  d <- expand.grid(Block = paste0("B", 1:5), Trt = paste0("T", 1:6))
  blk <- c(1, 2, 4, 8, 16)[as.integer(d$Block)]; trt <- c(1, 1.5, 2, 3, 4, 6)[as.integer(d$Trt)]
  d$Y <- blk * trt * exp(rnorm(nrow(d), 0, 0.05))
  r <- analyze(d, "RCBD", list(response = "Y", treat = "Trt", block = "Block"))
  ta <- tukey_additivity(r)
  expect_lt(ta$p, 0.01)
  expect_equal(ta$F, unname(tukey_by_hand(d$Y, d$Block, d$Trt)["F"]), tolerance = 1e-8)
  # on the log scale the effects add, and the test no longer objects
  d$logY <- log(d$Y)
  expect_gt(tukey_additivity(analyze(d, "RCBD", list(response = "logY", treat = "Trt", block = "Block")))$p, 0.05)
})

test_that("Tukey's test is offered only where it has a question to answer", {
  crd <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  expect_false(tukey_additivity(crd)$applies)
  expect_match(tukey_additivity(crd)$why, "no blocks", fixed = TRUE)
  sp <- analyze(demo_data("SPLIT"), "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
  expect_false(tukey_additivity(sp)$applies)
  lsd <- analyze(demo_data("LSD"), "LSD", list(response = "Yield", row = "Row", col = "Column", treat = "Treatment"))
  expect_true(tukey_additivity(lsd)$applies)
  expect_true(is.finite(tukey_additivity(lsd)$p))
})

## ------------------------------------------------------- Cook's distance --

test_that("Cook's distance is R's, with Cook's guide from the F distribution", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  ci <- cooks_info(r)
  expect_equal(ci$d, unname(cooks.distance(r$lm)), tolerance = 1e-12)
  expect_equal(ci$guide, qf(0.5, r$lm$rank, nrow(d) - r$lm$rank))
  # a balanced layout: every plot has the same leverage
  expect_true(ci$equal_h)
  expect_match(cooks_text(r)$reading, "Every plot has the same leverage", fixed = TRUE)
})

test_that("a plot far out of line is named, and one that alone fixes a mean is not given a distance", {
  d <- demo_data("CRD")
  d$Yield[7] <- d$Yield[7] + 40
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  ci <- cooks_info(r)
  expect_identical(ci$rows[ci$flag], 7L)
  expect_match(cooks_text(r)$reading, "Row 7 lies above the guide", fixed = TRUE)
  # a treatment with a single plot: its residual is zero whatever its value
  d2 <- rbind(demo_data("CRD"), data.frame(Treatment = "T9", Rep = 1, Yield = 30))
  r2 <- analyze(d2, "CRD", list(response = "Yield", treat = "Treatment"))
  ci2 <- cooks_info(r2)
  expect_length(ci2$lev1, 1)
  expect_true(is.na(ci2$d[ci2$lev1]))
  expect_match(cooks_text(r2)$reading, "alone determines its mean", fixed = TRUE)
})

## ------------------------------------------------- the summary of checks --

test_that("the summary separates what can be tested from what the design must ensure", {
  f <- run_all(demo_data("RCBD"), "RCBD", list(treat = "Variety", block = "Block"), "Yield")$fits$Yield
  s <- assum_summary(f)
  expect_true(all(c("Check", "Kind", "Result", "Meaning", "Action") %in% names(s)))
  expect_true(all(c("Normality of the residuals", "Equal variances", "Independence of the errors",
                    "Nature of the response") %in% s$Check))
  expect_true(any(grepl("Tukey's one-degree-of-freedom", s$Check, fixed = TRUE)))
  expect_identical(s$Kind[s$Check == "Independence of the errors"], "Design requirement")
  expect_identical(s$Result[s$Check == "Independence of the errors"], "Not testable from these data")
  expect_identical(s$Kind[s$Check == "Normality of the residuals"], "Tested")
  # the verdicts are at the analysis's own level
  expect_match(s$Meaning[s$Check == "Normality of the residuals"], "at the 5% level", fixed = TRUE)
  h <- assum_summary_html(s)
  for (k in c("Check", "Result", "What it means", "Recommended action")) expect_match(h, k, fixed = TRUE)
})

test_that("counts and percentages are not treated as automatically normal", {
  f <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")$fits$Yield
  for (nat in c("count", "percent")) {
    s <- assum_summary(f, nat)
    row <- s[s$Check == "Nature of the response", ]
    expect_match(paste(row$Meaning, row$Action), "does not guarantee the assumptions", fixed = TRUE, info = nat)
  }
  expect_match(assum_summary(f, "count")$Action[assum_summary(f, "count")$Check == "Nature of the response"],
               "generalised linear model", fixed = TRUE)
})

test_that("a pooled analysis checks the environments' error variances; other designs do not", {
  f <- run_all(demo_data("POOLRCBD"), "POOLRCBD", list(env = "Location", rep = "Rep", treat = "Variety"), "Yield")$fits$Yield
  s <- assum_summary(f)
  row <- s[grepl("across environments", s$Check, fixed = TRUE), ]
  expect_equal(nrow(row), 1)
  h <- f$final$homogeneity
  expect_match(row$Result, fmt(h$chisq, 3), fixed = TRUE)
  f2 <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")$fits$Yield
  expect_false(any(grepl("across environments", assum_summary(f2)$Check, fixed = TRUE)))
})

test_that("an exact fit gives no verdicts in the summary", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 3), Yield = rep(c(5, 6, 7), each = 3))
  f <- run_all(d, "CRD", list(treat = "Trt"), "Yield")$fits$Yield
  s <- assum_summary(f)
  expect_match(s$Result[s$Check == "Normality of the residuals"], "Not available", fixed = TRUE)
  expect_false(any(grepl("depart", s$Meaning[s$Kind == "Tested"])))
  expect_null(plot_cooks(f$final))
})
