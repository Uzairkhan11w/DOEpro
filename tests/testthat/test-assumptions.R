## The assumption checks when they cannot be computed: Levene's test with two
## values per cell, and an exact fit. Expectations come from the definitions:
##   * Levene's test (median-centred) is a one-way ANOVA on |y - cell median|;
##     with two values in a cell both deviations are equal, so such a cell
##     carries nothing and is left out; in a cell of odd size the median's own
##     deviation is always zero and is left out (Hines and O'Hara Hines, 2000);
##   * Bartlett's test needs a positive variance in every cell it uses;
##   * a test that could not be computed is reported as not available, with
##     the reason, and never as a significant departure.

two_reps <- function(seed = 1, k = 6) {
  set.seed(seed)
  data.frame(Trt = rep(paste0("T", seq_len(k)), each = 2), Yield = round(rnorm(2 * k, 50, 3), 2))
}

## ---------------------------------------------------------- Levene ----

test_that("Levene's test does not run on cells of two values", {
  d <- two_reps()
  lev <- levene_test(d$Yield, d$Trt)
  expect_true(is.na(lev$F)); expect_true(is.na(lev$p))
  expect_identical(lev$cells, 0L)
  expect_identical(lev$why, "no cell has three or more values")
  # the old computation: every deviation within a cell is the same
  z <- abs(d$Yield - ave(d$Yield, d$Trt, FUN = median))
  expect_equal(as.numeric(tapply(z, d$Trt, var)), rep(0, 6))
})

test_that("Levene's test leaves out the small cells and matches its definition on the rest", {
  set.seed(4)
  d <- data.frame(Trt = rep(c("A", "B", "C", "D"), c(4, 5, 4, 2)), y = round(rnorm(15, 20, 2), 2))
  lev <- levene_test(d$y, d$Trt)
  expect_identical(lev$dropped, 1L); expect_identical(lev$cells, 3L)
  k <- d[d$Trt != "D", ]
  z <- abs(k$y - ave(k$y, k$Trt, FUN = median))
  # cells of 4, 5 and 4 are of mixed size, so no zero is set aside
  ref <- anova(lm(z ~ factor(k$Trt)))
  expect_equal(lev$F, ref[1, "F value"], tolerance = 1e-10)
  expect_equal(lev$p, ref[1, "Pr(>F)"], tolerance = 1e-10)
  expect_equal(c(lev$df1, lev$df2), c(2, 10))
  expect_identical(lev$zeros_removed, 0L)
  # cells all of odd size (3, 5, 3): each loses its median's own zero, and only one
  o <- data.frame(Trt = rep(c("A", "B", "C"), c(3, 5, 3)), y = c(1, 2, 2, 4, 4, 4, 6, 9, 3, 5, 8))
  lo <- levene_test(o$y, o$Trt)
  expect_identical(lo$zeros_removed, 3L)
  zo <- abs(o$y - ave(o$y, o$Trt, FUN = median))
  drop <- vapply(split(seq_along(zo), o$Trt), function(i) i[which(zo[i] == 0)[1]], integer(1))
  ro <- anova(lm(zo[-drop] ~ factor(o$Trt[-drop])))
  expect_equal(lo$F, ro[1, "F value"], tolerance = 1e-10)
})

test_that("Levene's test with identical values in every cell says so", {
  lev <- levene_test(rep(c(5, 6, 7), each = 3), rep(c("A", "B", "C"), each = 3))
  expect_true(is.na(lev$p))
  expect_identical(lev$why, "every cell's values are identical")
  expect_no_warning(levene_test(rep(c(5, 6, 7), each = 3), rep(c("A", "B", "C"), each = 3)))
})

## --------------------------------------------------------- Bartlett ----

test_that("Bartlett's test matches stats::bartlett.test and refuses zero variances", {
  set.seed(2)
  g <- rep(1:3, each = 6); y <- rnorm(18, 10, rep(c(1, 1.5, 2), each = 6))
  b <- bartlett_cells(y, g); ref <- bartlett.test(y, g)
  expect_equal(b$statistic, unname(ref$statistic)); expect_equal(b$p.value, ref$p.value)
  # a cell of one value is left out
  b1 <- bartlett_cells(c(y, 30), c(g, 4))
  expect_identical(b1$dropped, 1L); expect_equal(b1$p.value, ref$p.value)
  # a cell whose values are all the same has no variance
  z <- bartlett_cells(c(5, 5, 5, 1, 2, 4), rep(c("A", "B"), each = 3))
  expect_true(is.na(z$p.value))
  expect_match(z$why, "have no variance", fixed = TRUE)
})

## ------------------------------------------- the checks, two reps ----

test_that("two values per cell: no false verdict of unequal variances", {
  for (design in c("CRD", "RCBD")) {
    if (design == "CRD") {
      d <- two_reps()
      r <- analyze(d, "CRD", list(response = "Yield", treat = "Trt"))
    } else {
      d <- demo_data("RCBD"); d <- d[d$Block %in% c("B1", "B2"), ]
      r <- analyze(d, "RCBD", list(response = "Yield", block = "Block", treat = "Variety"))
    }
    expect_no_warning(a <- check_assumptions(r))
    # the verdict comes from Bartlett's test, which works with two values per cell
    expect_identical(a$hov_test, "Bartlett", info = design)
    expect_equal(a$p_hov, bartlett.test(r$data[[r$resp]], interaction(r$data[r$facs], drop = TRUE))$p.value)
    h <- assum_table_html(a)
    expect_match(h, "not available: no cell has three or more values", fixed = TRUE)
    expect_match(h, "Levene's test needs at least three values in a cell.", fixed = TRUE)
    expect_match(h, "The verdict on equal variances uses Bartlett's test, which covers every cell but assumes the values are normal.", fixed = TRUE)
    s <- suggest_transform(r, a)
    txt <- interpret(r, a, s)
    expect_match(txt, "Bartlett's test (Levene's test could not be run: no cell has three or more values; Bartlett's assumes the values are normal)", fixed = TRUE)
    if (a$p_hov > r$alpha) {
      expect_no_match(txt, "heterogeneous", fixed = TRUE)
      expect_match(s$why, "the test of equal variances (Bartlett's)", fixed = TRUE)
    }
  }
  # the six-treatment case that used to give F = 2e29 and a Box-Cox suggestion
  r <- analyze(two_reps(), "CRD", list(response = "Yield", treat = "Trt"))
  a <- check_assumptions(r)
  expect_gt(a$p_hov, 0.05)
  expect_identical(suggest_transform(r, a)$method, "none")
})

test_that("equal variances that cannot be tested at all are said so", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 2), Yield = c(5, 5, 6.1, 6.4, 7.2, 7.9))
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Trt"))
  a <- check_assumptions(r)
  expect_true(is.na(a$p_hov))
  h <- assum_table_html(a)
  expect_no_match(h, "significant departure</b>", fixed = TRUE)
  expect_match(h, "Bartlett (homogeneity)</td><td>-</td><td>-</td><td>not available", fixed = TRUE)
  expect_match(interpret(r, a, suggest_transform(r, a)), "Equal variances could not be tested", fixed = TRUE)
  s <- suggest_transform(r, a)
  if (!is.na(a$p_norm) && a$p_norm > r$alpha)
    expect_match(s$why, "equal variances could not be tested", fixed = TRUE)
  expect_no_match(s$why, "Neither the normality test nor the test of equal variances", fixed = TRUE)
})

## ------------------------------------------------------ exact fit ----

test_that("an exact fit is reported as such, not tested on rounding residue", {
  e <- data.frame(Trt = rep(c("A", "B", "C"), each = 3), Yield = rep(c(5, 6, 7), each = 3))
  r <- analyze(e, "CRD", list(response = "Yield", treat = "Trt"))
  expect_true(exact_fit(r))
  a <- check_assumptions(r)
  expect_true(a$exact)
  expect_null(a$shapiro); expect_null(a$bc)
  expect_identical(a$outliers, integer(0))
  h <- assum_table_html(a)
  expect_no_match(h, "significant departure", fixed = TRUE)
  expect_identical(lengths(regmatches(h, gregexpr("not available: the model fits every value exactly", h))), 3L)
  expect_match(h, "not estimable (the model fits every value exactly)", fixed = TRUE)
  s <- suggest_transform(r, a)
  expect_identical(s$method, "none")
  expect_match(s$why, "The model fits every value exactly", fixed = TRUE)
  expect_match(interpret(r, a, s), "The model fits every value exactly, so the residuals are all zero", fixed = TRUE)
  # an RCBD whose values are block + treatment exactly
  d <- expand.grid(Block = paste0("B", 1:3), Trt = paste0("T", 1:4))
  d$Yield <- 10 + as.integer(d$Block) + 2 * as.integer(d$Trt)
  rb <- analyze(d, "RCBD", list(response = "Yield", block = "Block", treat = "Trt"))
  expect_true(check_assumptions(rb)$exact)
  # ordinary data are not an exact fit
  expect_false(exact_fit(analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))))
})

test_that("a test without a p-value is never called a significant departure", {
  a <- check_assumptions(analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment")))
  a$levene$p <- NA_real_; a$levene$F <- NA_real_; a$levene$why <- "a reason"
  a$bartlett$p.value <- NaN; a$bartlett$statistic <- NA_real_; a$bartlett$why <- NULL
  h <- assum_table_html(a)
  expect_match(h, "not available: a reason", fixed = TRUE)
  expect_match(h, "Bartlett (homogeneity)</td><td>-</td><td>-</td><td>not available</td>", fixed = TRUE)
})

test_that("the screening table names the test behind its equal-variance p", {
  s <- auto_scan(two_reps(), "CRD", list(treat = "Trt"), "Yield")
  expect_true("Equal variances p" %in% names(s))
  expect_match(s$`Equal variances p`, "(Bartlett)", fixed = TRUE)
  s <- auto_scan(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  expect_no_match(s$`Equal variances p`, "Bartlett", fixed = TRUE)
})

test_that("the Assumptions tab draws no residual plots for an exact fit", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = rep(c("A", "B", "C"), each = 3), Yield = rep(c(5, 6, 7), each = 3))
    session$setInputs(design = "CRD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", treat = "Trt", tr_1 = "none", run = 1)
    for (o in c("diagFit", "diagQQ", "diagHist", "diagScale"))
      expect_error(output[[o]], "fits every value exactly")
    expect_error(output$bcPlot, "fits every value exactly")
    expect_no_match(output$assumtxt$html, "significant departure", fixed = TRUE)
  })
})

## ------------------------------------------------ found in review ----

test_that("Levene's test is not given the verdict on treatments it left out", {
  # cells of 3, 3, 2, 2, 2, 2: Levene's compares only the first two
  set.seed(7)
  sds <- c(1.89, 0.32, 0.57, 1.41, 1.63, 8.34); ns <- c(3, 3, 2, 2, 2, 2)
  d <- data.frame(Trt = rep(paste0("V", 1:6), ns),
                  Yield = round(unlist(mapply(function(n, s) rnorm(n, 50, s), ns, sds)), 2))
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Trt"))
  a <- check_assumptions(r)
  expect_identical(a$levene$left_out, c("V3", "V4", "V5", "V6"))
  expect_identical(a$hov_test, "Bartlett")
  expect_equal(a$p_hov, bartlett.test(d$Yield, d$Trt)$p.value)
  h <- assum_table_html(a)
  expect_match(h, "Levene's test leaves out V3, V4, V5 and V6, which have fewer than three values, so it compares only the other cells.", fixed = TRUE)
  expect_match(interpret(r, a, suggest_transform(r, a)), "Bartlett's test (Levene's test leaves out V3, V4, V5 and V6", fixed = TRUE)
  # and why: keeping the two-value cells (as car::leveneTest does) calls equal
  # variances unequal far too often
  keep_all <- function(y, g) {
    z <- abs(y - ave(y, g, FUN = median))
    anova(lm(z ~ factor(g)))[1, "Pr(>F)"]
  }
  set.seed(11)
  g <- rep(1:6, ns)
  p <- replicate(2000, keep_all(rnorm(length(g)), g))
  expect_gt(mean(p < 0.05), 0.1)
  # whereas the verdict used here, Bartlett's on every cell, holds its level
  pb <- replicate(2000, bartlett_cells(rnorm(length(g)), g)$p.value)
  expect_lt(mean(pb < 0.05), 0.07)
  # the screening table says which test it used
  expect_match(auto_scan(d, "CRD", list(treat = "Trt"), "Yield")[["Equal variances p"]], "(Bartlett)", fixed = TRUE)
})

test_that("an untested assumption is not treated as passed", {
  # two blocks, and a treatment with no insects in either: neither test runs
  ins <- data.frame(Block = rep(c("B1", "B2"), 5), Trt = rep(paste0("T", 1:5), each = 2),
                    Insects = c(0, 0, 3, 5, 10, 16, 30, 52, 90, 150))
  r <- analyze(ins, "RCBD", list(response = "Insects", block = "Block", treat = "Trt"))
  a <- check_assumptions(r)
  expect_true(is.na(a$p_hov))
  expect_gt(a$slope, 1.5)
  s <- suggest_transform(r, a)
  expect_identical(s$method, "log1")
  expect_no_match(s$why, "no transformation is needed", fixed = TRUE)
  # both reasons, each named
  expect_match(interpret(r, a, s), paste0("Equal variances could not be tested: for Levene's test, no cell has three or more values; ",
    "for Bartlett's test, the values in some cells are all the same, so those cells have no variance."), fixed = TRUE)
  # with no sign of a mean-variance relationship the advice says only what was checked
  flat <- data.frame(Trt = rep(c("A", "B", "C"), each = 2), Y = c(5, 5, 6.2, 6.5, 5.6, 5.9))
  r <- analyze(flat, "CRD", list(response = "Y", treat = "Trt"))
  a <- check_assumptions(r)
  s <- suggest_transform(r, a)
  if (identical(s$method, "none") && !is.na(a$p_norm))
    expect_match(s$why, "the checks that could be run give no reason for a transformation", fixed = TRUE)
  expect_no_match(s$why, "no transformation is needed", fixed = TRUE)
})

test_that("the reason given is true of all the cells", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), c(3, 3, 2)), Count = c(0, 0, 0, 0, 0, 0, 4, 9))
  lev <- levene_test(d$Count, d$Trt)
  expect_identical(lev$why, "the values are identical within every cell of three or more")
  lev <- levene_test(rep(c(5, 6, 7), each = 3), rep(c("A", "B", "C"), each = 3))
  expect_identical(lev$why, "every cell's values are identical")
  # cells of four whose deviations do not vary: the reason is not the cell size
  sc <- data.frame(Trt = rep(c("A", "B", "C"), each = 4), Score = c(1, 1, 3, 3, 2, 2, 4, 4, 3, 3, 7, 7))
  r <- analyze(sc, "CRD", list(response = "Score", treat = "Trt"))
  a <- check_assumptions(r)
  txt <- interpret(r, a, suggest_transform(r, a))
  expect_match(txt, "Levene's test could not be run: the deviations from the cell medians do not vary within any cell", fixed = TRUE)
  expect_no_match(txt, "three values", fixed = TRUE)
  # the suggestion names the test briefly
  expect_match(suggest_transform(r, a)$why, "the test of equal variances (Bartlett's)", fixed = TRUE)
})

test_that("when the two tests disagree, the table says which verdict is used", {
  set.seed(3)
  # heavy tails: Bartlett's is fooled, Levene's is not
  for (i in 1:200) {
    d <- data.frame(Trt = rep(paste0("T", 1:4), each = 6), Y = round(50 + rt(24, 2), 2))
    a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
    if (is.finite(a$bartlett$p.value) && a$bartlett$p.value <= 0.05 && a$levene$p > 0.05) break
  }
  expect_true(a$bartlett$p.value <= 0.05 && a$levene$p > 0.05)
  expect_match(assum_table_html(a), "Bartlett's test finds a departure that Levene's does not", fixed = TRUE)
  expect_identical(a$hov_test, "Levene")
})

test_that("an exact fit gives no Taylor slope or mean-variance plot", {
  L <- expand.grid(Row = paste0("R", 1:4), Col = paste0("C", 1:4))
  L$Trt <- c("A", "B", "C", "D")[((as.integer(L$Row) + as.integer(L$Col)) %% 4) + 1]
  L$Yield <- 10 + as.integer(L$Row) + 2 * as.integer(L$Col) + 3 * match(L$Trt, c("A", "B", "C", "D"))
  a <- check_assumptions(analyze(L, "LSD", list(response = "Yield", row = "Row", col = "Col", treat = "Trt")))
  expect_true(a$exact)
  expect_true(is.na(a$slope)); expect_identical(nrow(a$mv), 0L)
  h <- assum_table_html(a)
  expect_match(h, "Taylor's power-law slope b</td><td colspan='2'>-</td><td>not estimable (the model fits every value exactly)", fixed = TRUE)
  expect_no_match(h, "appears to", fixed = TRUE)
})

test_that("normality that cannot be tested is said so", {
  set.seed(5)
  d <- data.frame(Trt = rep(paste0("T", 1:3), length.out = 5001), Y = rnorm(5001, 50, 4))
  r <- analyze(d, "CRD", list(response = "Y", treat = "Trt"))
  a <- check_assumptions(r)
  expect_null(a$shapiro)
  expect_match(interpret(r, a, suggest_transform(r, a)),
               "Normality could not be tested: Shapiro-Wilk needs between 3 and 5000 residuals.", fixed = TRUE)
})

test_that("the mean-variance plot is not drawn for an exact fit", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- expand.grid(Block = paste0("B", 1:3), Trt = paste0("T", 1:4))
    d$Yield <- 10 + as.integer(d$Block) + 2 * as.integer(d$Trt)
    rv$data <- d
    session$setInputs(design = "RCBD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", block = "Block", treat = "Trt", tr_1 = "none", run = 1)
    expect_error(output$mvPlot, "fits every value exactly")
  })
})

## ------------------------------------------ found in second review ----

test_that("with three values per cell Levene's test can find unequal variances", {
  # every cell of three: one deviation per cell is the median's own zero,
  # which capped F at 4 and the p-value above 0.05 for four treatments
  d <- data.frame(Trt = rep(paste0("T", 1:4), each = 3),
                  Y = c(50.1, 50.4, 49.8, 51.2, 50.9, 51.5, 49.6, 49.9, 50.3, 30, 50, 70))
  lev <- levene_test(d$Y, d$Trt)
  expect_identical(lev$zeros_removed, 4L)
  expect_lt(lev$p, 0.05)
  z <- abs(d$Y - ave(d$Y, d$Trt, FUN = median))
  old <- anova(lm(z ~ factor(d$Trt)))
  expect_lte(old[1, "F value"], 4 + 1e-9)            # the old test could not get there
  expect_gt(old[1, "Pr(>F)"], 0.05)
  a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
  expect_identical(a$hov_test, "Levene")
  expect_lt(a$p_hov, 0.05)
  expect_match(assum_table_html(a), "Levene's test leaves that one zero out (Hines and O'Hara Hines, 2000)", fixed = TRUE)
  # and it holds its level: normal data, three per cell
  set.seed(21)
  g <- rep(1:4, each = 3)
  p <- replicate(2000, levene_test(rnorm(12), g)$p)
  expect_lt(mean(p < 0.05), 0.07); expect_gt(mean(p < 0.05), 0.01)
})

test_that("the disagreement note does not blame non-normality without cause", {
  set.seed(1)
  for (i in 1:300) {
    d <- data.frame(Trt = rep(paste0("V", 1:4), each = 4), Y = round(rnorm(16, 50, rep(c(1, 1, 1, 4), each = 4)), 2))
    a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
    if (is.finite(a$bartlett$p.value) && a$bartlett$p.value <= 0.05 && a$levene$p > 0.05) break
  }
  h <- assum_table_html(a)
  expect_match(h, "Bartlett's is the more sensitive of the two when the values are normal but is easily misled when they are not", fixed = TRUE)
  expect_no_match(h, "Bartlett's is sensitive to values that are not normal, so", fixed = TRUE)
})

test_that("a slope from variances of two values each does not order a transformation", {
  d <- data.frame(Block = rep(c("B1", "B2"), 6), Var = rep(paste0("V", 1:6), each = 2),
                  Days = c(39, 40, 41, 46, 47, 44, 50, 50, 53, 51, 58, 56))
  r <- analyze(d, "RCBD", list(response = "Days", block = "Block", treat = "Var"))
  a <- check_assumptions(r)
  expect_true(is.na(a$p_hov))
  expect_gt(a$slope, 0.5)
  expect_gt(a$slope_p, 0.05)
  s <- suggest_transform(r, a)
  expect_identical(s$method, "none")
  expect_match(s$why, "the checks that could be run give no reason for a transformation", fixed = TRUE)
})

test_that("Levene's test on some cells is not a full pass", {
  d <- data.frame(Trt = rep(paste0("T", 1:5), c(2, 3, 3, 3, 3)),
                  Insects = c(0, 0, 2, 5, 3, 9, 14, 11, 25, 40, 31, 60, 95, 72))
  r <- analyze(d, "CRD", list(response = "Insects", treat = "Trt"))
  a <- check_assumptions(r)
  expect_identical(a$hov_test, "Levene"); expect_identical(a$levene$dropped, 1L)
  s <- suggest_transform(r, a)
  expect_no_match(s$why, "no transformation is needed", fixed = TRUE)
  expect_match(checks_text(a, r$alpha), "(Levene's, on some cells)", fixed = TRUE)
})

test_that("an exact fit is recognised at large means too", {
  for (base in c(1e4, 1e5, 1e6)) {
    e <- data.frame(Trt = rep(c("A", "B", "C", "D"), each = 3), Y = base + rep(1:4, each = 3) * 0.001)
    r <- analyze(e, "CRD", list(response = "Y", treat = "Trt"))
    expect_true(exact_fit(r), info = base)
    expect_no_match(assum_table_html(check_assumptions(r)), "significant departure", fixed = TRUE)
  }
  # real values with a tiny spread are not an exact fit
  set.seed(2)
  real <- data.frame(Trt = rep(c("A", "B", "C"), each = 3), Y = round(1000 + runif(9, 0.001, 0.009), 3))
  expect_false(exact_fit(analyze(real, "CRD", list(response = "Y", treat = "Trt"))))
})

test_that("an exact split-plot fit is described for the sub-plot stratum", {
  d <- expand.grid(Sub = paste0("S", 1:3), Main = paste0("M", 1:2), Rep = paste0("R", 1:3))
  wp <- c(1.3, -0.7, 2.1, -1.8, 0.4, 0.9)
  d$Y <- 40 + 2 * as.integer(d$Main) + 3 * as.integer(d$Sub) + wp[as.integer(interaction(d$Rep, d$Main))]
  r <- analyze(d, "SPLIT", list(response = "Y", rep = "Rep", main = "Main", sub = "Sub"))
  a <- check_assumptions(r)
  expect_true(a$exact)
  expect_match(assum_table_html(a), "so there is no sub-plot error variation to test or to plot", fixed = TRUE)
})

## ------------------------------------------- found in third review ----

test_that("mixed cell sizes keep Levene's test at its level", {
  # four replicates with a missing plot here and there: cells of 4 and 3
  set.seed(31)
  g <- rep(1:10, c(4, 4, 3, 4, 4, 4, 3, 4, 4, 4))
  p <- replicate(3000, levene_test(rnorm(length(g)), g)$p)
  expect_lt(mean(p < 0.05), 0.065)
  expect_identical(levene_test(rnorm(length(g)), g)$zeros_removed, 0L)
})

test_that("a refusal caused by setting the zero aside says so", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 3), Y = c(2, 3, 4, 1, 3, 5, 3, 3, 3))
  lev <- levene_test(d$Y, d$Trt)
  expect_true(is.na(lev$p))
  expect_identical(lev$why, "with the median's own zero set aside, the other deviations from the median are equal within every cell")
  a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
  expect_match(assum_table_html(a), "Levene's test leaves that one zero out", fixed = TRUE)
})

test_that("a single left-out cell is spoken of in the singular, and Bartlett's coverage is stated truly", {
  set.seed(8)
  d <- data.frame(Trt = rep(c("A", "B", "C", "D"), c(3, 3, 3, 1)), Y = round(rnorm(10, 20, 2), 2))
  a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
  expect_identical(a$hov_test, "Bartlett")
  expect_match(a$hov_label, "Levene's test leaves out D, which has fewer than three values", fixed = TRUE)
  h <- assum_table_html(a)
  expect_match(h, "which covers every cell with two or more values but assumes the values are normal", fixed = TRUE)
  expect_no_match(h, "covers every cell but", fixed = TRUE)
})

test_that("a perfect mean-variance line raises no warning", {
  d <- data.frame(Trt = rep(c("A", "B", "C", "D"), each = 3), Y = c(9, 10, 11, 18, 20, 22, 27, 30, 33, 36, 40, 44))
  expect_no_warning(a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt"))))
  expect_lt(a$slope_p, 0.001)
})
