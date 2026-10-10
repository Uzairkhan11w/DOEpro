## The readings after the ANOVA tables and plots: every claim checked against
## the figures it describes (the letters and C.D.s of the tables of means,
## the counts in the post-hoc tables, the row numbers of the data, the
## Box-Cox interval), at the significance level the analysis was run at.

RD_MAPS <- list(
  CRD = list(treat = "Treatment"), RCBD = list(treat = "Variety", block = "Block"),
  LSD = list(treat = "Treatment", row = "Row", col = "Column"),
  FRCBD = list(factors = c("Nitrogen", "Variety"), block = "Block"),
  SPLIT = list(rep = "Rep", main = "Irrigation", sub = "Variety"),
  STRIP = list(rep = "Rep", main = "Tillage", sub = "Mulch"),
  POOLRCBD = list(env = "Location", rep = "Rep", treat = "Variety"))
rd_run <- function(des, alpha = 0.05, trans = NULL)
  run_all(demo_data(des), des, RD_MAPS[[des]], "Yield", alpha = alpha, trans = trans)

## two letter strings share a letter
rd_share <- function(a, b) any(strsplit(a, "")[[1]] %in% strsplit(b, "")[[1]])

## -------------------------------------------------------------- ANOVA ----

test_that("every row with an F-test is read, in order, at the chosen level", {
  for (des in names(RD_MAPS)) for (a in c(0.05, 0.01)) {
    r <- rd_run(des, a)$fits$Yield$final
    txt <- anova_text(r)
    tested <- !is.na(r$anova$p)
    rows <- r$anova$Source[tested]; p <- r$anova$p[tested]
    expect_identical(substr(txt[seq_along(rows)], 1, nchar(rows) + 1), paste0(rows, ":"), info = des)
    for (i in seq_along(rows)) {
      if (p[i] < a)
        expect_match(txt[i], sprintf(": significant at the %s%% level (", pct(if (p[i] < a / 5) a / 5 else a)), fixed = TRUE)
      else {
        expect_match(txt[i], sprintf(": not significant at the %s%% level (", pct(a)), fixed = TRUE)
        expect_match(txt[i], "insufficient evidence", fixed = TRUE)
      }
      expect_match(txt[i], rd_p(p[i]), fixed = TRUE)
      expect_no_match(txt[i], "no difference|no effect|are equal")
    }
  }
})

test_that("the error each row is tested against is named where there is more than one", {
  expect_match(paste(anova_text(rd_run("SPLIT")$fits$Yield$final), collapse = " "),
               "Irrigation, the main-plot factor, is tested against Error (a)", fixed = TRUE)
  expect_match(paste(anova_text(rd_run("STRIP")$fits$Yield$final), collapse = " "),
               "the Tillage x Mulch interaction against Error (c)", fixed = TRUE)
  pool <- paste(anova_text(rd_run("POOLRCBD")$fits$Yield$final), collapse = " ")
  expect_match(pool, "Each treatment row is tested against its own interaction with the environment", fixed = TRUE)
  expect_match(pool, "and the environments against the replications within environments.", fixed = TRUE)
  expect_match(pool, "Location x Variety: significant at the 1% level (p = 0.00123): there is evidence that the differences between the Variety means change from one environment to another", fixed = TRUE)
  for (des in c("CRD", "RCBD", "LSD", "FRCBD"))
    expect_no_match(paste(anova_text(rd_run(des)$fits$Yield$final), collapse = " "), "tested against", fixed = TRUE)
})

test_that("a significant interaction is read as one factor's effect depending on the other", {
  t <- anova_text(rd_run("FRCBD")$fits$Yield$final)
  expect_match(t[4], "Nitrogen:Variety: significant at the 5% level (p = 0.0216): there is evidence that the effect of Nitrogen depends on the level of Variety.", fixed = TRUE)
  # three factors: the two-factor interaction differs between the levels of the third
  set.seed(4)
  d <- expand.grid(Block = paste0("B", 1:3), A = c("A1", "A2"), B = c("B1", "B2"), C = c("C1", "C2"))
  d$Y <- 20 + 6 * (d$A == "A2" & d$B == "B2" & d$C == "C2") + rnorm(nrow(d), 0, 0.5)
  r <- analyze(d, "FRCBD", list(response = "Y", factors = c("A", "B", "C"), block = "Block"))
  t3 <- anova_text(r)
  expect_match(t3[grepl("^A:B:C:", t3)], "the interaction of A and B differs between the levels of C", fixed = TRUE)
})

test_that("the CV is judged by the guide on the figure as printed", {
  one <- function(cv, grand = 10, tr = "none") rd_cv(list(cv = c("CV (%)" = cv), grand = grand), tr)
  expect_match(one(9.994), "The CV is 9.99%, low", fixed = TRUE)
  expect_match(one(9.996), "The CV is 10.00%, moderate", fixed = TRUE)    # prints as 10.00
  expect_match(one(25), "high by the usual guide", fixed = TRUE)
  expect_match(one(25), "only large differences between treatments can be detected.", fixed = TRUE)
  expect_match(one(35), "check the data for recording errors.", fixed = TRUE)
  expect_match(one(8, tr = "log"), "is on the transformed scale, so the usual guide", fixed = TRUE)
  expect_match(one(8, grand = -2), "The grand mean is not positive", fixed = TRUE)
  two <- rd_cv(list(cv = c("CV(a) (%)" = 24, "CV(b) (%)" = 6), grand = 10))
  expect_match(two, "The CVs are CV(a) 24.00% (high) and CV(b) 6.00% (low)", fixed = TRUE)
  expect_match(two, "With CV(a) this high, only large differences can be detected in the comparisons it applies to.", fixed = TRUE)
})

test_that("the table of mean squares is read response by response", {
  d <- demo_data("RCBD"); d$Noise <- 50 + demo_noise(nrow(d))
  rr <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), c("Yield", "Noise"))
  t <- anova_combined_text(rr)
  pv <- vapply(rr$fits, function(f) f$final$anova$p[2], numeric(1))
  expect_true(pv[["Yield"]] < 0.05 && pv[["Noise"]] >= 0.05)
  expect_match(t[2], "Variety is significant at the 5% level for Yield, but not for Noise.", fixed = TRUE)
  expect_length(anova_combined_text(rd_run("RCBD")), 0)
})

## -------------------------------------------------------------- means ----

test_that("the means said not to differ from a mean are exactly those sharing its letter", {
  set.seed(11)
  for (i in 1:40) {
    d <- data.frame(T = rep(paste0("T", 1:6), each = 3),
                    Y = rnorm(18, rep(c(10, 11, 12, 12, 13, 15), each = 3), 1.2))
    if (i %% 2 == 0) d <- d[-c(2, 9), ]                                     # unequal replication
    e <- analyze(d, "CRD", list(response = "Y", treat = "T"))$effects$T
    for (j in seq_len(nrow(e$means)))
      expect_identical(rd_par(e, j), unname(which(vapply(e$means$Letter, rd_share, logical(1), e$means$Letter[j]))))
  }
})

test_that("a table of means is read from its highest and lowest means", {
  e <- rd_run("CRD")$fits$Yield$final$effects$Treatment
  t <- means_text(e)
  expect_match(t, "T5 has the highest mean (38.29) and T1 the lowest (29.05).", fixed = TRUE)
  expect_match(t, "Two Treatment means must differ by more than the C.D., 3.54, to be declared different at the 5% level.", fixed = TRUE)
  expect_match(t, "Every other Treatment mean is significantly lower than that of T5.", fixed = TRUE)
  expect_match(t, "T4 (29.24) does not differ significantly from T1; the means of T2, T3 and T5 are significantly higher.", fixed = TRUE)
  # the letters say the same
  m <- e$means
  expect_identical(m$Letter[m$Treatment %in% c("T1", "T4")], c("c", "c"))
  expect_false(any(vapply(m$Letter[m$Treatment != "T5"], rd_share, logical(1), "a")))
  # two levels: the difference against the C.D.
  t2 <- means_text(rd_run("FRCBD")$fits$Yield$final$effects$Variety)
  expect_match(t2, "V2 has the higher mean (40.06) and V1 the lower (35.90). The difference, 4.16, is larger than the C.D. (2.30), so it is significant at the 5% level.", fixed = TRUE)
  # at the 1% level the rule says so
  t1 <- means_text(rd_run("RCBD", 0.01)$fits$Yield$final$effects$Variety)
  expect_match(t1, "to be declared different at the 1% level", fixed = TRUE)
})

test_that("a table of means whose F-test is not significant claims no differences", {
  set.seed(2)
  d <- data.frame(T = rep(paste0("T", 1:4), each = 4), Y = rnorm(16, 20, 2))
  r <- analyze(d, "CRD", list(response = "Y", treat = "T"))
  expect_gt(r$effects$T$p, 0.05)
  t <- means_text(r$effects$T)
  expect_match(t, "The F-test for T is not significant at the 5% level", fixed = TRUE)
  expect_match(t, "there is insufficient evidence that the T means differ.", fixed = TRUE)
  expect_no_match(t, "significantly (lower|higher)|C\\.D\\., [0-9]")
})

test_that("a main effect in a significant interaction carries a caution", {
  r <- rd_run("FRCBD")$fits$Yield$final
  expect_match(means_text(r$effects$Nitrogen, 2, "none", r),
               "The Nitrogen x Variety interaction is significant at the 5% level, so these Nitrogen means average over conditions that behave differently", fixed = TRUE)
  expect_identical(rd_avg_note(rd_run("RCBD")$fits$Yield$final, "Variety"), "")
  # a pooled analysis: the treatment's interaction with the environment
  p <- rd_run("POOLRCBD")$fits$Yield$final
  expect_match(rd_avg_note(p, "Variety"), "The Location x Variety interaction is significant", fixed = TRUE)
})

test_that("with a transformation the means are named as shown and the C.D. on its own scale", {
  d <- demo_data("CRD")
  rr <- run_all(d, "CRD", list(treat = "Treatment"), "Yield", trans = list(Yield = "reciprocal"))
  e <- rr$fits$Yield$final$effects$Treatment
  t <- means_text(e, 2, "reciprocal")
  bt <- e$means$Mean_bt
  # the reciprocal turns the order round: the highest back-transformed mean
  # has the lowest transformed one
  expect_identical(which.max(bt), which.min(e$means$Mean))
  expect_match(t, sprintf("%s has the highest mean (%s)", e$means$Treatment[which.max(bt)], fmt(max(bt), 2)), fixed = TRUE)
  expect_match(t, "it applies to the transformed values in parentheses, not to the back-transformed means", fixed = TRUE)
})

test_that("a two-way table is read from its pattern, row by row", {
  r <- rd_run("FRCBD")$fits$Yield$final
  t <- twoway_text(r, r$effects[["Nitrogen:Variety"]], "Nitrogen", "Variety")
  expect_match(t, "V2 has the higher mean at every level of Nitrogen, so the interaction lies in the size of the difference, which ranges from 1.47 at N60 to 9.11 at N120.", fixed = TRUE)
  g <- r$effects[["Nitrogen:Variety"]]$means
  gap <- tapply(g$Mean, g$Nitrogen, function(z) diff(range(z)))
  expect_equal(as.vector(round(gap, 2)), c(1.89, 1.47, 9.11))
  # the levels change places: the highest is named at each row
  d <- expand.grid(Block = paste0("B", 1:3), Dose = c("N0", "N60", "N120"), V = c("V1", "V2"))
  d$Y <- 30 + ifelse(d$V == "V1", c(N0 = 6, N60 = -2, N120 = -6)[as.character(d$Dose)], 0) + demo_noise(nrow(d), 0, 0.5)
  r2 <- analyze(d, "FRCBD", list(response = "Y", factors = c("Dose", "V"), block = "Block"))
  t2 <- twoway_text(r2, r2$effects[["Dose:V"]], "Dose", "V")
  expect_match(t2, "The V with the highest mean changes with Dose: V1 at N0 and V2 at N60 and N120.", fixed = TRUE)
  # a split plot: the C.D. for two sub-plot means at the same main plot
  s <- rd_run("SPLIT")$fits$Yield$final
  ts <- twoway_text(s, s$effects[["Irrigation:Variety"]], "Irrigation", "Variety")
  expect_match(ts, "Two Variety means at the same level of Irrigation must differ by more than 1.55", fixed = TRUE)
  expect_match(ts, "V3 has the highest mean at every level of Irrigation, but the order of the other Variety means changes.", fixed = TRUE)
  # no interaction: the margins can be read on their own
  d$Y <- 30 + 3 * (d$V == "V2") + 2 * as.integer(d$Dose) + demo_noise(nrow(d), 0, 0.5)
  r3 <- analyze(d, "FRCBD", list(response = "Y", factors = c("Dose", "V"), block = "Block"))
  expect_gt(r3$effects[["Dose:V"]]$p, 0.05)
  expect_match(twoway_text(r3, r3$effects[["Dose:V"]], "Dose", "V"),
               "there is insufficient evidence that the differences between the V means change with Dose", fixed = TRUE)
})

test_that("the tables of means carry their readings in the app and not in the report", {
  rr <- rd_run("FRCBD")
  with <- means_section_html(rr, detailed = TRUE, readings = TRUE)
  expect_match(with, "What the table shows.", fixed = TRUE)
  expect_match(with, "V2 has the higher mean at every level of Nitrogen", fixed = TRUE)
  # the detailed tables print their means to three decimals, and so do their readings
  expect_match(with, "N120 has the highest mean (43.452)", fixed = TRUE)
  expect_no_match(means_section_html(rr, detailed = TRUE), "What the table shows.", fixed = TRUE)
  expect_no_match(build_report(rr), "What the table shows.", fixed = TRUE)
})

## ----------------------------------------------------------- post-hoc ----

test_that("the post-hoc readings count what the tables declare", {
  for (des in c("CRD", "RCBD", "LSD", "FRCBD", "SPLIT", "STRIP", "POOLRCBD")) {
    r <- rd_run(des)$fits$Yield$final
    ## Dunnett's test compares with a control, not every pair: test-dunnett.R
    for (en in names(r$effects)) for (meth in setdiff(PH_METHODS, "Dunnett")) {
      x <- tryCatch(gate_posthoc(posthoc(r, en, meth, r$alpha)), error = function(e) NULL)
      if (is.null(x) || !x$f_sig) next
      e <- r$effects[[en]]
      # the families agree with the pairwise table
      k <- sum(vapply(x$families, function(fm) sum(fm$sig[upper.tri(fm$sig)]), numeric(1)))
      expect_identical(as.integer(k), sum(x$pairs$Significant == "Yes"), info = paste(des, en, meth))
      lsd <- posthoc(r, en, PH_METHODS[1], r$alpha)
      t <- posthoc_pairs_text(x, e, meth, lsd)
      m <- nrow(x$pairs); y <- sum(x$pairs$Significant == "Yes")
      expect_match(tolower(t), if (m == 1) "the one pair" else if (y == m) sprintf("all %d pairs", m)
                               else if (y == 0) sprintf("none of the %d pairs", m) else sprintf("%d of the %d pairs", y, m),
                   fixed = TRUE)
      held <- sum(grepl("inside a non-significant range", x$pairs$Significant, fixed = TRUE))
      if (held) expect_match(t, sprintf("%d more pair", held), fixed = TRUE)
      if (meth != PH_METHODS[1]) {
        kl <- sum(lsd$pairs$Significant == "Yes")
        expect_match(t, if (kl == y) "declares the same number" else sprintf("declares %d pair", kl), fixed = TRUE)
      }
    }
  }
})

test_that("the post-hoc groups are read from the test's own verdicts", {
  r <- rd_run("CRD")$fits$Yield$final
  x <- gate_posthoc(posthoc(r, "Treatment", "Tukey HSD", 0.05))
  t <- posthoc_groups_text(x, r$effects$Treatment)
  expect_match(t, "T5 has the highest mean (38.290); by this test T3 (33.575) does not differ significantly from it, and the means of T2, T4 and T1 are significantly lower.", fixed = TRUE)
  g <- x$groups
  expect_identical(g$Treatment[vapply(g$Group, rd_share, logical(1), g$Group[1])], c("T5", "T3"))
  # within the levels of a main plot only the highest at each level is read
  s <- rd_run("SPLIT")$fits$Yield$final
  xs <- gate_posthoc(posthoc(s, "Irrigation:Variety", "Tukey HSD", 0.05))
  ts <- posthoc_groups_text(xs, s$effects[["Irrigation:Variety"]])
  expect_match(ts, "By this test, comparing Variety means within each level of Irrigation: At I1, V3 has the highest mean (47.038), and every other mean there is significantly lower.", fixed = TRUE)
  # a test under a non-significant F declares nothing
  set.seed(2)
  d <- data.frame(T = rep(paste0("T", 1:4), each = 4), Y = rnorm(16, 20, 2))
  n <- analyze(d, "CRD", list(response = "Y", treat = "T"))
  xn <- gate_posthoc(posthoc(n, "T", "Tukey HSD", 0.05))
  expect_match(posthoc_groups_text(xn, n$effects$T), "this test declares no pair different and gives no letters", fixed = TRUE)
  expect_match(posthoc_pairs_text(xn, n$effects$T, "Tukey HSD"), "so no pair is declared different.", fixed = TRUE)
})

## -------------------------------------------------------- assumptions ----

test_that("outliers are named by their row numbers in the data, not their positions", {
  set.seed(3)
  d <- data.frame(Trt = rep(paste0("T", 1:4), each = 6), Y = round(rnorm(24, 50, 1), 2))
  d$Y[10] <- 80
  d$Y[2] <- NA                                       # row 2 is left out
  r <- analyze(d, "CRD", list(response = "Y", treat = "Trt"))
  a <- check_assumptions(r)
  expect_identical(a$outliers, 10L)
  expect_identical(which(abs(r$resid / sd(r$resid)) > 3), c(`10` = 9L))   # the position is 9
  expect_match(assum_text(r, a), "Row 10 has a standardised residual beyond -3 or 3; check it for a recording error.", fixed = TRUE)
  expect_match(assum_table_html(a), "<td colspan='2'>10</td>", fixed = TRUE)
  expect_match(interpret(r, a, suggest_transform(r, a)), "(rows 10)", fixed = TRUE)
  expect_match(resid_text(r, a)$reading, "Row 10 lies beyond -3 or 3", fixed = TRUE)
  lab <- ggplot2::ggplot_build(plot_diag(r)[[1]])$data
  expect_identical(lab[[length(lab)]]$label, "row 10")
})

test_that("no stray value is looked for where none can lie beyond 3", {
  r <- rd_run("LSD")$fits$Yield$final                # 6 error degrees of freedom
  expect_false(rd_can_flag(r))
  expect_lt(max(abs(r$resid / sd(r$resid))), 3)
  expect_match(assum_text(r, check_assumptions(r)), "With 6 error degrees of freedom no standardised residual can go beyond -3 or 3", fixed = TRUE)
  expect_true(rd_can_flag(rd_run("RCBD")$fits$Yield$final))
  # the bound holds: no residual of any data on this layout gets past it
  set.seed(9)
  d <- demo_data("LSD")
  for (i in 1:200) {
    d$Yield <- rnorm(16) + (i %% 3 == 0) * 50 * (seq_len(16) == (i %% 16) + 1)
    s <- analyze(d, "LSD", list(response = "Yield", treat = "Treatment", row = "Row", col = "Column"))
    expect_lte(max(abs(s$resid / sd(s$resid))), 3)
  }
})

test_that("the assumption checks are read with their verdicts and what they mean", {
  r <- rd_run("CRD")$fits$Yield
  t <- assum_text(r$final, r$asm)
  expect_match(t, "The Shapiro-Wilk test finds insufficient evidence at the 5% level that the residuals depart from a normal distribution (p = 0.244).", fixed = TRUE)
  expect_match(t, "Levene's test finds insufficient evidence at the 5% level that the variances differ between the treatments (p = 0.757).", fixed = TRUE)
  expect_match(t, "they give no reason to doubt the F-tests and C.D.s", fixed = TRUE)
  # a departure points to the suggestion
  set.seed(8)
  d <- data.frame(Trt = rep(paste0("T", 1:4), each = 6), Y = rexp(24, 1 / rep(c(1, 4, 16, 64), each = 6)))
  b <- analyze(d, "CRD", list(response = "Y", treat = "Trt")); ab <- check_assumptions(b)
  expect_true(ab$p_hov < 0.05 || ab$p_norm < 0.05)
  expect_match(assum_text(b, ab), "so with this departure they are less trustworthy", fixed = TRUE)
  # an exact fit has nothing to check
  d <- expand.grid(Block = paste0("B", 1:3), Trt = paste0("T", 1:4))
  d$Yield <- 10 + as.integer(d$Block) + 2 * as.integer(d$Trt)
  x <- analyze(d, "RCBD", list(response = "Yield", block = "Block", treat = "Trt")); ax <- check_assumptions(x)
  expect_match(assum_text(x, ax), "The model fits every value exactly", fixed = TRUE)
  for (f in list(boxcox_text(ax), meanvar_text(x, ax), resid_text(x, ax), qq_aov_text(x, ax), hist_aov_text(x, ax), scale_text(x, ax)))
    expect_null(f)
  expect_identical(rd_note(NULL), "")
})

test_that("with one error degree of freedom normality is not tested", {
  # the residuals are one pattern set by the layout: Shapiro-Wilk gave
  # p = 0.0239 whatever the data
  d <- expand.grid(Block = c("B1", "B2"), Trt = c("T1", "T2"))
  set.seed(1)
  for (i in 1:5) {
    d$Y <- rnorm(4, 10)
    r <- analyze(d, "RCBD", list(response = "Y", treat = "Trt", block = "Block"))
    expect_equal(abs(r$resid / sd(r$resid)), rep(sqrt(3) / 2, 4), ignore_attr = TRUE)
    expect_equal(shapiro.test(r$resid)$p.value, 0.02385679, tolerance = 1e-6)
    a <- check_assumptions(r)
    expect_null(a$shapiro); expect_true(is.na(a$p_norm))
    expect_match(a$norm_why, "with one error degree of freedom the residuals take the same pattern whatever the data", fixed = TRUE)
    expect_match(assum_table_html(a), "not available: with one error degree of freedom", fixed = TRUE)
    expect_match(qq_aov_text(r, a)$reading, "With one error degree of freedom the residuals take the same pattern whatever the data, so the plot cannot show", fixed = TRUE)
  }
})

test_that("an exact fit's verdicts are not read as findings", {
  d <- expand.grid(Block = paste0("B", 1:3), Trt = paste0("T", 1:4))
  d$Yield <- 10 + as.integer(d$Block) + 2 * as.integer(d$Trt)
  rr <- run_all(d, "RCBD", list(treat = "Trt", block = "Block"), "Yield")
  r <- rr$fits$Yield$final
  expect_identical(anova_text(r), RD_EXACT)
  expect_identical(means_text(r$effects$Trt, 2, "none", r), RD_EXACT)
  x <- gate_posthoc(posthoc(r, "Trt", "Tukey HSD", 0.05))
  expect_identical(posthoc_groups_text(x, r$effects$Trt, exact = TRUE), RD_EXACT)
  expect_identical(posthoc_pairs_text(x, r$effects$Trt, "Tukey HSD", exact = TRUE), RD_EXACT)
  expect_match(main_plot_text(r, "Trt", "bar")$reading, RD_EXACT, fixed = TRUE)
  expect_no_match(main_plot_text(r, "Trt", "bar")$reading, "significant at", fixed = TRUE)
  expect_match(means_section_html(rr, readings = TRUE), "The model fits every value exactly", fixed = TRUE)
})

test_that("the Box-Cox reading follows the interval as the plot prints it", {
  for (des in names(RD_MAPS)) {
    a <- rd_run(des)$fits$Yield$asm
    t <- boxcox_text(a)$reading
    lo <- as.numeric(sprintf("%.2f", a$bc$ci[1])); hi <- as.numeric(sprintf("%.2f", a$bc$ci[2]))
    expect_match(t, sprintf("confidence interval runs from %.2f to %.2f", a$bc$ci[1], a$bc$ci[2]), fixed = TRUE)
    if (lo <= 1 && hi >= 1) expect_match(t, "The interval includes lambda = 1", fixed = TRUE)
    else expect_match(t, "The interval excludes lambda = 1", fixed = TRUE)
    if (hi >= 2) expect_match(t, "upper ends? of the range drawn")
    if (a$bc$lambda >= 2) expect_match(t, "The curve is highest at the upper end", fixed = TRUE)
  }
  # an interval that leaves out 1 and the square root, and takes in the log
  set.seed(6)
  d <- data.frame(Trt = rep(paste0("T", 1:5), each = 6), Y = exp(rnorm(30, rep(1:5, each = 6), 0.4)))
  a <- check_assumptions(analyze(d, "CRD", list(response = "Y", treat = "Trt")))
  t <- boxcox_text(a)$reading
  expect_true(a$bc$ci[2] < 0.5 && a$bc$ci[1] < 0)
  expect_match(t, "The interval excludes lambda = 1: at the 5% level the values as they are fit significantly worse than the best power.", fixed = TRUE)
  expect_match(t, "Of the usual transformations, the logarithm (0)", fixed = TRUE)
})

test_that("the mean-variance reading follows the slope's own test", {
  set.seed(6)
  d <- data.frame(Trt = rep(paste0("T", 1:6), each = 5), Y = rpois(30, rep(c(2, 5, 10, 20, 40, 80), each = 5)))
  r <- analyze(d, "CRD", list(response = "Y", treat = "Trt")); a <- check_assumptions(r)
  t <- meanvar_text(r, a)$reading
  expect_lt(a$slope_p, 0.05)
  expect_match(t, sprintf("The slope, b = %.2f, differs significantly from 0 at the 5%% level", a$slope), fixed = TRUE)
  expect_match(t, "rises roughly in proportion to the mean", fixed = TRUE)
  f <- rd_run("FRCBD")$fits$Yield
  expect_match(meanvar_text(f$final, f$asm)$reading, "Each variance rests on only 3 values", fixed = TRUE)
})

test_that("the histogram's direction is the one the extremes show", {
  for (des in names(RD_MAPS)) {
    r <- rd_run(des)$fits$Yield
    t <- hist_aov_text(r$final, r$asm)$reading
    s <- desc_one(r$final$resid)
    if (grepl("the bars reach further below 0", t, fixed = TRUE)) expect_gt(-s$Min, s$Max)
    if (grepl("the bars reach further above 0", t, fixed = TRUE)) expect_gt(s$Max, -s$Min)
  }
  # a skewness whose sign the extremes do not bear out is not read as a tail
  r <- list(resid = c(-3, -0.2, -0.2, -0.1, -0.1, 0.1, 0.1, 0.2, 0.2, 0.5, 0.6, 0.7, 0.8, 0.9, 1.2, -1.5, -1.2, 1.3))
  r$resid <- r$resid - mean(r$resid)
  s <- desc_one(r$resid); sh <- shape_of(s)
  t <- hist_aov_text(r, list(exact = FALSE))$reading
  if (sh$kind == "right") expect_match(t, "the skew does not come from a long tail of high residuals", fixed = TRUE)
  if (sh$kind == "left") expect_match(t, "the bars reach further below 0", fixed = TRUE)
})

## --------------------------------------------------------------- plots ----

test_that("the main plot is read from the means it draws", {
  r <- rd_run("FRCBD")$fits$Yield$final
  t <- main_plot_text(r, "Nitrogen", "bar")
  expect_match(t$reading, "The highest mean is that of N120 (43.45) and the lowest that of N0 (32.09).", fixed = TRUE)
  expect_match(t$reading, "Error bars that do not overlap are not enough to show that two means differ significantly", fixed = TRUE)
  expect_match(t$reading, "The Nitrogen x Variety interaction is significant at the 5% level, so these Nitrogen means average", fixed = TRUE)
  # the lines keep their order but the interaction is significant
  l <- main_plot_text(r, "Nitrogen:Variety", "line", TRUE, "Nitrogen")
  expect_match(l$reading, "The Variety levels keep the same order at every level of Nitrogen, so the lines do not cross.", fixed = TRUE)
  expect_match(l$reading, "The order does not change, so the interaction lies in the size of the differences.", fixed = TRUE)
  # crossing lines, named at each level of the X-axis
  d <- expand.grid(Block = paste0("B", 1:3), Dose = c("N0", "N60", "N120"), V = c("V1", "V2"))
  d$Y <- 30 + ifelse(d$V == "V1", c(N0 = 6, N60 = 0.5, N120 = -6)[as.character(d$Dose)], 0) + demo_noise(nrow(d), 0, 0.5)
  r2 <- analyze(d, "FRCBD", list(response = "Y", factors = c("Dose", "V"), block = "Block"))
  c2 <- main_plot_text(r2, "Dose:V", "line", TRUE, "Dose")
  expect_match(c2$reading, "The order of the V levels changes along the X-axis, so some lines cross: the highest is V1 at N0 and N60 and V2 at N120.", fixed = TRUE)
  m <- r2$effects[["Dose:V"]]$means
  expect_identical(as.character(m$V[m$Dose == "N120"][which.max(m$Mean[m$Dose == "N120"])]), "V2")
  # the heat map names its darkest and palest tiles as their figures print
  h <- main_plot_text(r, "Nitrogen:Variety", "heat")
  expect_match(h$reading, "The darkest tile is N120 x V2 (48.0) and the palest N0 x V1 (31.1).", fixed = TRUE)
  # the box plot reads the medians of the observations drawn
  b <- main_plot_text(r, "Variety", "box")
  med <- tapply(r$data$Yield, r$data$Variety, median)
  expect_match(b$reading, sprintf("The median is highest for V2 (%s) and lowest for V1 (%s).", fmt(med[["V2"]], 2), fmt(med[["V1"]], 2)), fixed = TRUE)
})

## -------------------------------------------------------------- the app ----

test_that("the analysis tabs show their readings", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("FRCBD")
    session$setInputs(design = "FRCBD", nfac = 2, alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", block = "Block", f1 = "Nitrogen", f2 = "Variety", tr_1 = "none", run = 1)
    expect_match(output$anovaOut$html, "Nitrogen:Variety: significant at the 5% level", fixed = TRUE)
    session$setInputs(digits = 2, letters = TRUE, detailed = FALSE)
    expect_match(output$meansOut$html, "Every other Nitrogen mean is significantly lower than that of N120.", fixed = TRUE)
    session$setInputs(aResp = "Yield")
    expect_match(output$asmText$html, "What the checks show.", fixed = TRUE)
    for (o in c("bcText", "mvText", "diagFitText", "diagQQText", "diagHistText", "diagScaleText"))
      expect_match(output[[o]]$html, "What it shows.", fixed = TRUE)
    session$setInputs(aResp2 = "Yield", phEff = "Nitrogen", phMethod = "Tukey HSD")
    expect_match(output$phText$html, "Tukey&#39;s test judges every pair|Tukey's test judges every pair")
    expect_match(output$phPairsText$html, "All 3 pairs differ significantly by this test at the 5% level.", fixed = TRUE)
    session$setInputs(aResp3 = "Yield", plEff = "Nitrogen:Variety", plType = "line", plLetters = TRUE)
    expect_match(output$mainPlotText$html, "Each line joins the means of one level of", fixed = TRUE)
  })
})
