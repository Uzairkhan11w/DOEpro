## Dunnett's test: the critical values against Dunnett's (1955) tables and
## Student's t, unequal replication against a simulation of the largest |t|,
## the comparisons and verdicts in posthoc(), the readings, and the control
## selector in the app.

eq <- function(k) { R <- matrix(0.5, k, k); diag(R) <- 1; R }

test_that("the critical values reproduce Dunnett's tables", {
  ## Dunnett (1955) and its one-sided companion, 5% level; columns are the
  ## number of treatments compared with the control
  two <- rbind(`5` = c(2.57, 3.03, 3.29, 3.48, 3.62), `10` = c(2.23, 2.57, 2.76, 2.89, 2.99),
               `20` = c(2.09, 2.38, 2.54, 2.65, 2.73))
  one <- rbind(`5` = c(2.02, 2.44, 2.68, 2.85, 2.98), `10` = c(1.81, 2.15, 2.34, 2.47, 2.56),
               `20` = c(1.72, 2.03, 2.19, 2.30, 2.39))
  for (df in c(5, 10, 20)) for (k in 1:5) {
    expect_equal(round(dunnett_crit(eq(k), df, 0.05, TRUE)$value, 2), unname(two[as.character(df), k]), info = paste(df, k))
    expect_equal(round(dunnett_crit(eq(k), df, 0.05, FALSE)$value, 2), unname(one[as.character(df), k]), info = paste(df, k))
  }
})

test_that("one comparison gives Student's t, at any degrees of freedom", {
  for (df in c(1, 2, 3, 6, 15, 60, 1000)) for (a in c(0.05, 0.01)) {
    expect_equal(dunnett_crit(matrix(1), df, a, TRUE)$value, qt(1 - a / 2, df))
    ## the integral itself, with a second comparison that cannot matter
    R <- matrix(c(1, 0.5, 0.5, 1), 2)
    g <- dunnett_grid(df)
    expect_equal(dunnett_prob(qt(1 - a / 2, df), sqrt(0.5), 1, TRUE, g), 1 - a, tolerance = 1e-8)
    expect_equal(dunnett_prob(qt(1 - a, df), sqrt(0.5), 1, FALSE, g), 1 - a, tolerance = 1e-8)
  }
})

test_that("the critical value lies between one comparison's t and Bonferroni's", {
  for (k in c(2, 5, 9)) for (df in c(4, 12, 40)) {
    v <- dunnett_crit(eq(k), df, 0.05, TRUE)$value
    expect_gt(v, qt(0.975, df)); expect_lt(v, qt(1 - 0.025 / k, df))
  }
})

test_that("with unequal replication the comparisons' correlations are exact, and the value holds its level", {
  d <- demo_data("CRD")[-c(2, 7, 8), ]                 # 3, 2, 4, 4 and 4 plots
  e <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))$effects$Treatment
  R <- dunnett_cor(e$sed_mat, 1)
  v <- (e$means$SE)^2
  lam <- sqrt(v[1] / (v[-1] + v[1]))
  P <- outer(lam, lam); diag(P) <- 1
  expect_equal(R, P)
  fit <- dunnett_lambda(R)
  expect_equal(fit$lambda, lam, tolerance = 1e-10); expect_lt(fit$departure, 1e-12)
  ## a simulation of the largest |t| among these comparisons
  cr <- dunnett_crit(R, e$df, 0.05, TRUE)
  expect_true(cr$exact)
  set.seed(31); n <- 2e5; k <- nrow(R)
  Z <- matrix(rnorm(n * k), n, k) %*% chol(R)
  s <- sqrt(rchisq(n, e$df) / e$df)
  T <- Z / s
  p2 <- mean(apply(abs(T), 1, max) <= cr$value)
  expect_lt(abs(p2 - 0.95), 3 * sqrt(0.95 * 0.05 / n))
  c1 <- dunnett_crit(R, e$df, 0.05, FALSE)$value
  p1 <- mean(apply(T, 1, max) <= c1)
  expect_lt(abs(p1 - 0.95), 3 * sqrt(0.95 * 0.05 / n))
})

test_that("two missing plots are given the nearest exact form, and say so", {
  d <- demo_data("RCBD"); d$Yield[5] <- NA
  e1 <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))$effects$Variety
  expect_lt(dunnett_lambda(dunnett_cor(e1$sed_mat, 1))$departure, 1e-12)     # one missing plot: exact
  d$Yield[14] <- NA
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  x <- posthoc(r, "Variety", "Dunnett", 0.05, control = "V1")
  expect_false(x$dunnett$exact)
  expect_gt(x$dunnett$departure, 1e-4)
  expect_match(paste(x$note, collapse = " "), "This is an approximation", fixed = TRUE)
  x1 <- posthoc(analyze(demo_data("RCBD"), "RCBD", list(response = "Yield", treat = "Variety", block = "Block")),
                "Variety", "Dunnett", 0.05, control = "V1")
  expect_match(paste(x1$note, collapse = " "), "computed exactly", fixed = TRUE)
})

test_that("each treatment is compared with the control, with the verdict the question asks for", {
  r <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  for (alt in c("two.sided", "greater", "less")) {
    x <- posthoc(r, "Treatment", "Dunnett", 0.05, control = "T1", alternative = alt)
    p <- x$pairs
    expect_identical(nrow(p), 4L)
    expect_identical(p$Comparison, paste(c("T2", "T3", "T4", "T5"), "vs T1"))
    m <- r$effects$Treatment$means
    expect_equal(p$Difference, m$Mean[2:5] - m$Mean[1])
    expect_equal(p$`Critical difference`, p$`Critical value` * p$SEd)
    sig <- switch(alt, two.sided = abs(p$Difference) > p$`Critical difference`,
                  greater = p$Difference > p$`Critical difference`, less = -p$Difference > p$`Critical difference`)
    expect_identical(p$Significant == "Yes", sig)
    expect_identical(x$groups$Treatment[1], "T1"); expect_identical(x$groups$`Versus the control`[1], "Control")
    expect_true(all(diff(x$groups$Mean[-1]) <= 0))
  }
  ## two-sided at 5% with four comparisons on 15 df: Dunnett's 2.73
  expect_equal(round(posthoc(r, "Treatment", "Dunnett", 0.05, control = "T1")$pairs$`Critical value`[1], 2), 2.73)
  expect_error(posthoc(r, "Treatment", "Dunnett", 0.05, control = "T9"), "Choose the control")
  expect_identical(posthoc(r, "Treatment", "Dunnett", 0.05)$dunnett$control, "T1")    # the first level by default
})

test_that("a non-significant F-test declares nothing, as for every other test", {
  set.seed(2)
  d <- data.frame(T = rep(paste0("T", 1:4), each = 4), Y = rnorm(16, 20, 2))
  r <- analyze(d, "CRD", list(response = "Y", treat = "T"))
  x <- gate_posthoc(posthoc(r, "T", "Dunnett", 0.05, control = "T1"))
  expect_false(x$f_sig)
  expect_identical(unique(x$groups$`Versus the control`[-1]), "")
  expect_match(x$pairs$Significant[1], "Not declared", fixed = TRUE)
  expect_match(x$note[1], "no treatment is declared different from the control", fixed = TRUE)
})

test_that("a sliced interaction is compared with the control within each level", {
  s <- analyze(demo_data("SPLIT"), "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
  e <- s$effects[["Irrigation:Variety"]]
  expect_identical(dunnett_levels(e), c("V1", "V2", "V3", "V4"))
  x <- posthoc(s, "Irrigation:Variety", "Dunnett", 0.05, control = "V1")
  expect_identical(nrow(x$pairs), 9L)
  expect_identical(unique(x$pairs$Within), c("I1", "I2", "I3"))
  expect_true(all(grepl("vs I[123] : V1$", x$pairs$Comparison)))
  expect_equal(x$pairs$SEd, rep(e$sed, 9))
  ## three comparisons on Error (b)'s 27 df: between Dunnett's 2.51 (24 df) and 2.47 (30 df)
  expect_gt(x$dunnett$crit[1], 2.47); expect_lt(x$dunnett$crit[1], 2.51)
})

test_that("a level with no number is offered as the control first", {
  d <- data.frame(Trt = rep(c("T1", "T2", "Control", "T3"), each = 3), Y = c(10, 11, 12, 14, 15, 16, 9, 10, 11, 18, 19, 20))
  e <- analyze(d, "CRD", list(response = "Y", treat = "Trt"))$effects$Trt
  expect_identical(dunnett_levels(e)[1], "Control")
})

test_that("the readings follow Dunnett's verdicts", {
  r <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  x <- gate_posthoc(posthoc(r, "Treatment", "Dunnett", 0.05, control = "T1"))
  lsd <- posthoc(r, "Treatment", PH_METHODS[1], 0.05)
  t <- posthoc_groups_text(x, r$effects$Treatment)
  expect_match(t, "Compared with the control, T1 (29.047), by Dunnett's test at the 5% level: T5 (38.290) is significantly higher", fixed = TRUE)
  expect_match(t, "T4 (29.238) do not differ significantly from it.", fixed = TRUE)
  p <- posthoc_pairs_text(x, r$effects$Treatment, "Dunnett", lsd)
  expect_match(p, "1 of the 4 comparisons with the control is significant by this test at the 5% level.", fixed = TRUE)
  expect_match(p, "the same critical value, 2.727", fixed = TRUE)
  kl <- sum(lsd$families[[1]]$sig[1, 2:5])
  expect_match(p, sprintf("finds %d of these comparisons significant", kl), fixed = TRUE)
  g <- gate_posthoc(posthoc(r, "Treatment", "Dunnett", 0.05, control = "T1", alternative = "greater"))
  expect_match(posthoc_groups_text(g, r$effects$Treatment), "there is insufficient evidence that they are higher", fixed = TRUE)
  expect_match(PH_WHAT[["Dunnett"]], "compares each treatment with the control and with nothing else", fixed = TRUE)
})

test_that("the post-hoc tab offers a control for Dunnett's test", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("CRD")
    session$setInputs(design = "CRD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", treat = "Treatment", tr_1 = "none", run = 1)
    session$setInputs(aResp2 = "Yield", phEff = "Treatment", phMethod = "Dunnett", phAlt = "two.sided")
    expect_match(output$phControlUI$html, '<option value="T1" selected>T1</option>', fixed = TRUE)
    session$setInputs(phControl = "T3")
    x <- ph()
    expect_identical(x$groups$Treatment[1], "T3")
    expect_match(output$phText$html, "Compared with the control, T3", fixed = TRUE)
    expect_match(output$phPairsText$html, "comparisons with the control", fixed = TRUE)
    ## a control from another effect stands aside for the first level
    session$setInputs(phControl = "not a level")
    expect_identical(ph()$groups$Treatment[1], "T1")
  })
})
