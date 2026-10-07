## The significance level the user chooses (known bug 2). Nothing may stay at a
## hard-coded 5% or 1%: critical differences, letters, stars, assumption
## verdicts, the Box-Cox interval and every sentence of the report follow alpha.
## Expected values come from base R and textbook formulas, not from the
## package's own helpers.

## ------------------------------------------------------------------ helpers --

## Three treatments, four plots each, built so that the treatment F-test falls
## between the 1% and 5% points: between-treatment MS = 4, MSE = 0.6, F = 6.67
## on 2 and 9 df.
crd_between <- function() {
  data.frame(Treatment = factor(rep(c("T1", "T2", "T3"), each = 4)),
             Yield = rep(c(10, 11, 12), each = 4) +
                     rep(c(-1.5, -0.5, 0.5, 1.5) * 0.6, 3))
}

crd_unbalanced <- function() {
  data.frame(Treatment = factor(rep(paste0("T", 1:4), c(3, 5, 4, 6))),
             Yield = c(10.2, 11.1, 9.8,
                       12.5, 13.0, 12.1, 13.8, 12.9,
                       9.1, 8.7, 10.0, 9.5,
                       14.2, 15.1, 13.7, 14.8, 15.6, 14.0))
}

## a run_all() report for one demo dataset at a given level
demo_report <- function(nm, alpha) {
  s <- switch(nm,
    CRD      = list("CRD", list(treat = "Treatment")),
    RCBD     = list("RCBD", list(treat = "Variety", block = "Block")),
    LSD      = list("LSD", list(treat = "Treatment", row = "Row", col = "Column")),
    FRCBD    = list("FRCBD", list(factors = c("Nitrogen", "Variety"), block = "Block")),
    SPLIT    = list("SPLIT", list(rep = "Rep", main = "Irrigation", sub = "Variety")),
    STRIP    = list("STRIP", list(rep = "Rep", main = "Tillage", sub = "Mulch")),
    POOLRCBD = list("POOLRCBD", list(env = "Location", rep = "Rep", treat = "Variety")),
    POOLFACT = list("POOLFRCBD", list(env = "Location", rep = "Rep",
                                      factors = c("Nitrogen", "Variety"))))
  build_report(run_all(demo_data(nm), s[[1]], s[[2]], "Yield", alpha = alpha))
}

## ---------------------------------------------------- critical differences --

test_that("the chosen alpha sets the C.D. of a balanced design", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"),
               alpha = 0.01)
  # error mean square from the two-way table: residual of the additive fit
  Y <- tapply(d$Yield, list(d$Variety, d$Block), mean)
  nt <- nrow(Y); nb <- ncol(Y); dfe <- (nt - 1) * (nb - 1)
  mse <- sum((Y - outer(rowMeans(Y), colMeans(Y), "+") + mean(Y))^2) / dfe
  e <- r$effects[["Variety"]]
  expect_equal(r$alpha, 0.01)
  expect_equal(e$alpha, 0.01)
  expect_true(e$equal_rep)
  expect_equal(e$sed, sqrt(2 * mse / nb), tolerance = 1e-10)
  expect_equal(e$tcrit, stats::qt(0.995, dfe), tolerance = 1e-12)
  expect_equal(e$cd, stats::qt(0.995, dfe) * e$sed, tolerance = 1e-10)
  off <- row(e$cd_mat) != col(e$cd_mat)
  expect_equal(unname(e$cd_mat[off]), rep(e$cd, sum(off)), tolerance = 1e-10)
  # the fixed 5% and 1% fields are gone
  expect_null(e$cd5)
  expect_null(e$cd1)
  # and the letters are cut at the 1% C.D.
  mu <- e$means$Mean
  for (i in seq_along(mu)) for (j in seq_along(mu)) if (i < j) {
    share <- length(intersect(strsplit(e$means$Letter[i], "")[[1]],
                              strsplit(e$means$Letter[j], "")[[1]])) > 0
    expect_identical(share, abs(mu[i] - mu[j]) <= e$cd, info = paste(i, j))
  }
})

test_that("every split-plot comparison uses the chosen alpha on its own error df", {
  d <- demo_data("SPLIT")
  r <- analyze(d, "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation",
                                sub = "Variety"), alpha = 0.01)
  # textbook split-plot sums of squares
  Y <- d$Yield; CF <- sum(Y)^2 / length(Y)
  R <- 4; A <- 3; B <- 4
  tot <- function(by, div) sum(tapply(Y, d[by], sum)^2) / div - CF
  ss_r <- tot("Rep", A * B); ss_a <- tot("Irrigation", R * B); ss_ra <- tot(c("Rep", "Irrigation"), B)
  ss_b <- tot("Variety", R * A); ss_ab <- tot(c("Irrigation", "Variety"), R) - ss_a - ss_b
  dfa <- (R - 1) * (A - 1); dfb <- A * (R - 1) * (B - 1)
  Ea <- (ss_ra - ss_r - ss_a) / dfa
  Eb <- (sum(Y^2) - CF - ss_ra - ss_b - ss_ab) / dfb

  for (nm in names(r$effects)) expect_equal(r$effects[[nm]]$alpha, 0.01, info = nm)
  expect_equal(r$effects[["Irrigation"]]$cd, stats::qt(0.995, dfa) * sqrt(2 * Ea / (R * B)),
               tolerance = 1e-8)
  expect_equal(r$effects[["Variety"]]$cd, stats::qt(0.995, dfb) * sqrt(2 * Eb / (R * A)),
               tolerance = 1e-8)
  expect_equal(r$effects[["Irrigation:Variety"]]$cd, stats::qt(0.995, dfb) * sqrt(2 * Eb / R),
               tolerance = 1e-8)
})

## ------------------------------------------------------ letters and stars --

test_that("letters follow the F-test at the chosen level", {
  d <- crd_between()
  # the construction really does give 0.01 < p < 0.05
  p <- stats::pf(4 / 0.6, 2, 9, lower.tail = FALSE)
  expect_gt(p, 0.01)
  expect_lt(p, 0.05)

  map <- list(response = "Yield", treat = "Treatment")
  e05 <- analyze(d, "CRD", map, alpha = 0.05)$effects[["Treatment"]]
  e01 <- analyze(d, "CRD", map, alpha = 0.01)$effects[["Treatment"]]
  expect_equal(e05$p, p, tolerance = 1e-10)
  expect_equal(e01$p, p, tolerance = 1e-10)
  expect_true(effect_sig(e05))
  expect_false(effect_sig(e01))
  expect_true(any(nzchar(gate_letters(e05)$Letter)))
  expect_false(any(nzchar(gate_letters(e01)$Letter)))

  # the tables of means agree: letters and a C.D. at 5%, "NS" and none at 1%
  sup_letter <- "<sup>[a-z]+</sup>"
  h05 <- means_section_html(run_all(d, "CRD", list(treat = "Treatment"), "Yield", alpha = 0.05))
  h01 <- means_section_html(run_all(d, "CRD", list(treat = "Treatment"), "Yield", alpha = 0.01))
  expect_match(h05, sup_letter)
  expect_false(grepl(sup_letter, h01))
  expect_match(h01, "C.D. (P&le;0.01)", fixed = TRUE)
  expect_match(h01, ">NS<", fixed = TRUE)
})

test_that("letters in a pooled analysis are gated at the chosen level too", {
  d <- demo_data("POOLFACT")
  r <- analyze(d, "POOLFRCBD", list(response = "Yield", env = "Location", rep = "Rep",
                                    factors = c("Nitrogen", "Variety")), alpha = 0.01)
  for (nm in names(r$effects)) {
    e <- r$effects[[nm]]
    expect_equal(e$alpha, 0.01, info = nm)
    m <- gate_letters(e)
    cols <- intersect(LETTER_COLS, names(m))
    if (!length(cols)) next
    shown <- any(nzchar(unlist(m[cols])))
    expect_identical(shown, !is.na(e$p) && e$p < 0.01, info = nm)
  }
})

test_that("significance stars are set by the chosen level", {
  # one star below alpha, two below alpha / 5
  expect_identical(star(c(0.03, 0.009, 0.06, 0.05), 0.05), c("*", "**", "NS", "NS"))
  expect_identical(star(c(0.03, 0.005, 0.001, 0.002), 0.01), c("NS", "*", "**", "*"))
  expect_identical(star(c(0.08, 0.015, 0.12), 0.10), c("*", "**", "NS"))
  expect_identical(star(NA, 0.05), "")
  # the ANOVA table's column agrees with the definition
  d <- crd_between()
  an <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"), alpha = 0.01)$anova
  disp <- anova_display(an, 0.01)
  p <- an$p
  want <- ifelse(is.na(p), "", ifelse(p < 0.002, "**", ifelse(p < 0.01, "*", "NS")))
  expect_identical(disp$Signif, want)
  # and so does the key printed under it
  key <- star_key(0.01)
  expect_match(key, "p &lt; 0.002", fixed = TRUE)
  expect_match(key, "p &lt; 0.01", fixed = TRUE)
  expect_match(key, "1% level", fixed = TRUE)
})

test_that("labels name the chosen level", {
  expect_identical(cd_name(0.01), "C.D. (1%)")
  expect_identical(cd_name(0.05), "C.D. (5%)")
  expect_identical(cd_head(0.01), "C.D. (P&le;0.01)")
  expect_identical(cd_head(0.1), "C.D. (P&le;0.1)")
  # plot captions too
  d <- crd_between()
  e01 <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"),
                 alpha = 0.01)$effects[["Treatment"]]
  expect_match(plot_caption(e01, TRUE, TRUE), "not significant at the 1% level", fixed = TRUE)
  e05 <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"),
                 alpha = 0.05)$effects[["Treatment"]]
  expect_match(plot_caption(e05, TRUE, TRUE), "at the 5% level", fixed = TRUE)
})

## ------------------------------------------------------------- the report --

test_that("a report at 1% never mentions 5%", {
  for (nm in c("CRD", "RCBD", "LSD", "FRCBD", "SPLIT", "STRIP", "POOLRCBD", "POOLFACT")) {
    h <- demo_report(nm, 0.01)
    expect_match(h, "P&le;0.01", fixed = TRUE, info = nm)
    expect_false(grepl("P&le;0.05", h, fixed = TRUE), info = nm)
    expect_false(grepl("5%", h, fixed = TRUE), info = nm)
    expect_false(grepl("(5%", h, fixed = TRUE), info = nm)
  }
  # unbalanced and transformed data take other paths through the report
  rr <- run_all(crd_unbalanced(), "CRD", list(treat = "Treatment"), "Yield",
                alpha = 0.01, trans = list(Yield = "log"))
  h <- build_report(rr)
  expect_match(h, "P&le;0.01", fixed = TRUE)
  expect_false(grepl("P&le;0.05", h, fixed = TRUE))
  expect_false(grepl("5%", h, fixed = TRUE))
})

test_that("a report at 5% says 5%, and one at 10% says neither 5% nor 1%", {
  for (nm in c("CRD", "FRCBD", "SPLIT")) {
    expect_match(demo_report(nm, 0.05), "P&le;0.05", fixed = TRUE, info = nm)
    h <- demo_report(nm, 0.10)
    expect_match(h, "P&le;0.1", fixed = TRUE, info = nm)
    expect_match(h, "10% level", fixed = TRUE, info = nm)
    expect_false(grepl("5%", h, fixed = TRUE), info = nm)
    expect_false(grepl("[^0-9.]1%", h), info = nm)
  }
})

test_that("the interpretation is worded at the chosen level", {
  d <- crd_between()
  f <- run_all(d, "CRD", list(treat = "Treatment"), "Yield", alpha = 0.01)$fits[["Yield"]]
  txt <- interpret(f$final, f$asm, f$sug)
  expect_match(txt, "1% level", fixed = TRUE)
  expect_match(txt, "not significant at the 1% level", fixed = TRUE)
  # a non-significant effect is insufficient evidence, never proof of equality
  expect_match(txt, "insufficient evidence to conclude that its means differ at the 1% level",
               fixed = TRUE)
  expect_false(grepl("no difference", txt, ignore.case = TRUE))
  expect_false(grepl("do not differ", txt, ignore.case = TRUE))
  expect_false(grepl("5%", txt, fixed = TRUE))

  # the same data at 5%: now significant, and said to be so at 5%
  f <- run_all(d, "CRD", list(treat = "Treatment"), "Yield", alpha = 0.05)$fits[["Yield"]]
  txt <- interpret(f$final, f$asm, f$sug)
  expect_match(txt, "significant at the 5% level", fixed = TRUE)
  expect_false(grepl("1% level", txt, fixed = TRUE))
})

## ------------------------------------------------------------ assumptions --

test_that("assumption checks and the Box-Cox interval follow the chosen level", {
  d <- demo_data("CRD")
  map <- list(response = "Yield", treat = "Treatment")
  # the Box-Cox profile log-likelihood by hand on the same grid
  y <- d$Yield; X <- stats::model.matrix(~ factor(Treatment), d)
  lam <- seq(-2, 2, 0.02); n <- length(y); gm <- exp(mean(log(y)))
  ll <- vapply(lam, function(l) {
    z <- if (abs(l) < 1e-9) gm * log(y) else (y^l - 1) / (l * gm^(l - 1))
    -n / 2 * log(sum(qr.resid(qr(X), z)^2) / n)
  }, numeric(1))
  ci <- list()
  for (a in c(0.05, 0.01)) {
    asm <- check_assumptions(analyze(d, "CRD", map, alpha = a))
    expect_equal(asm$alpha, a)
    expect_equal(asm$bc$level, 1 - a)
    # every lambda within qchisq(1 - alpha, 1) / 2 of the maximum
    inside <- lam[ll > max(ll) - stats::qchisq(1 - a, 1) / 2]
    expect_equal(asm$bc$ci, range(inside), tolerance = 1e-8, info = paste("alpha", a))
    ci[[as.character(a)]] <- asm$bc$ci
  }
  # a stricter level gives a wider interval
  expect_lte(ci[["0.01"]][1], ci[["0.05"]][1])
  expect_gte(ci[["0.01"]][2], ci[["0.05"]][2])

  # the printed verdicts say which level they used
  asm <- check_assumptions(analyze(d, "CRD", map, alpha = 0.01))
  h <- assum_table_html(asm)
  expect_match(h, "99% CI", fixed = TRUE)
  expect_match(h, "Verdicts are at the 1% significance level", fixed = TRUE)
})

## --------------------------------------------------------------- post-hoc --

test_that("posthoc() defaults to the analysis's own alpha", {
  d <- crd_unbalanced()
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"), alpha = 0.01)
  dfe <- nrow(d) - nlevels(d$Treatment)
  for (m in PH_METHODS) {
    expect_identical(posthoc(r, "Treatment", m), posthoc(r, "Treatment", m, alpha = 0.01),
                     info = m)
  }
  ph <- posthoc(r, "Treatment", "LSD (Fisher's protected)")
  expect_identical(ph$stats$Value[ph$stats$Item == "Significance level"], "0.01")
  expect_equal(ph$pairs[["Critical value"]], rep(stats::qt(0.995, dfe), nrow(ph$pairs)),
               tolerance = 1e-12)
  # Bonferroni divides the chosen alpha over the number of pairs
  ph <- posthoc(r, "Treatment", "LSD (Bonferroni-adjusted)")
  expect_equal(ph$pairs[["Critical value"]],
               rep(stats::qt(1 - 0.01 / (2 * choose(4, 2)), dfe), nrow(ph$pairs)),
               tolerance = 1e-12)
  # Scheffe likewise
  ph <- posthoc(r, "Treatment", "Scheffe")
  expect_equal(ph$pairs[["Critical value"]],
               rep(sqrt(3 * stats::qf(0.99, 3, dfe)), nrow(ph$pairs)), tolerance = 1e-12)
})

## ----------------------------------------------------------------- inputs --

test_that("analyze() rejects a significance level outside (0, 0.5)", {
  d <- crd_between()
  map <- list(response = "Yield", treat = "Treatment")
  for (a in list(0, 0.5, 1, NA, NA_real_, c(0.05, 0.01), -0.05, "0.05"))
    expect_error(analyze(d, "CRD", map, alpha = a), "significance level",
                 info = paste(deparse(a), collapse = ""))
  # the ends of the range that are allowed
  expect_silent(analyze(d, "CRD", map, alpha = 0.001))
  expect_silent(analyze(d, "CRD", map, alpha = 0.2))
})

test_that("run_all() carries alpha into every fit", {
  d <- demo_data("FRCBD")
  d$Score <- d$Yield * 0.8 + 1
  rr <- run_all(d, "FRCBD", list(factors = c("Nitrogen", "Variety"), block = "Block"),
                responses = c("Yield", "Score"), alpha = 0.01)
  expect_equal(rr$alpha, 0.01)
  for (v in names(rr$fits)) {
    expect_equal(rr$fits[[v]]$final$alpha, 0.01, info = v)
    expect_equal(rr$fits[[v]]$asm$alpha, 0.01, info = v)
    for (nm in names(rr$fits[[v]]$final$effects))
      expect_equal(rr$fits[[v]]$final$effects[[nm]]$alpha, 0.01, info = paste(v, nm))
  }
})
