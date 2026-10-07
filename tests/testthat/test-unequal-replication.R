## Unequal replication (known bug 1). Every expected value below is worked out
## from a textbook formula or from base R, never from the package's own helpers,
## so a mistake in the package cannot be copied into the expectation.

## ------------------------------------------------------------------ helpers --

## Four treatments replicated 3, 5, 4 and 6 times.
crd_unbalanced <- function() {
  data.frame(Treatment = factor(rep(paste0("T", 1:4), c(3, 5, 4, 6))),
             Yield = c(10.2, 11.1, 9.8,
                       12.5, 13.0, 12.1, 13.8, 12.9,
                       9.1, 8.7, 10.0, 9.5,
                       14.2, 15.1, 13.7, 14.8, 15.6, 14.0))
}

## Error mean square of a one-way layout from its definition: the pooled
## within-treatment sum of squares over N - t.
mse_oneway <- function(y, g) {
  g <- factor(g)
  sum(tapply(y, g, function(x) sum((x - mean(x))^2))) / (length(y) - nlevels(g))
}

## Do two compact-letter strings have a letter in common? (Single letters only,
## which holds while fewer than 27 groups are needed.)
share_letter <- function(a, b)
  length(intersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]])) > 0

## Type III F-test of one term, by hand: the full model with sum-to-zero
## contrasts against the same model with that term's columns removed.
type3_by_hand <- function(form, d, term) {
  fac <- names(d)[vapply(d, is.factor, logical(1))]
  ctr <- stats::setNames(rep(list("contr.sum"), length(fac)), fac)
  X <- stats::model.matrix(form, d, contrasts.arg = ctr)
  y <- d[[all.vars(form)[1]]]
  drop <- attr(X, "assign") == match(term, attr(stats::terms(form), "term.labels"))
  rss <- function(M) sum(qr.resid(qr(M), y)^2)
  rf <- rss(X); rr <- rss(X[, !drop, drop = FALSE])
  df1 <- sum(drop); df2 <- nrow(X) - qr(X)$rank
  Fv <- ((rr - rf) / df1) / (rf / df2)
  c(F = Fv, p = stats::pf(Fv, df1, df2, lower.tail = FALSE))
}

## The step-down multiple-range test worked out from scratch for a one-way
## layout: each pair's critical difference is the studentized range for the
## number of means it spans, divided by sqrt(2), times that pair's own SE(d)
## (Kramer's adjustment); a pair is significant only if it exceeds its critical
## difference and every wider range enclosing it was significant too.
range_test_by_hand <- function(d, method, alpha) {
  mu <- tapply(d$Yield, d$Treatment, mean)
  n <- tapply(d$Yield, d$Treatment, length)
  dfe <- length(d$Yield) - length(mu)
  mse <- mse_oneway(d$Yield, d$Treatment)
  o <- names(sort(mu, decreasing = TRUE))
  k <- length(o)
  raw <- sig <- matrix(FALSE, k, k)
  for (w in rev(seq_len(k - 1))) for (i in seq_len(k - w)) {
    j <- i + w; p <- w + 1
    q <- if (method == "Student-Newman-Keuls") stats::qtukey(1 - alpha, p, dfe)
         else stats::qtukey((1 - alpha)^(p - 1), p, dfe)
    cd <- q / sqrt(2) * sqrt(mse * (1 / n[[o[i]]] + 1 / n[[o[j]]]))
    raw[i, j] <- (mu[[o[i]]] - mu[[o[j]]]) > cd
    enclosing <- c(if (i > 1) sig[i - 1, j], if (j < k) sig[i, j + 1])
    sig[i, j] <- raw[i, j] && all(enclosing)
  }
  ij <- which(upper.tri(raw), arr.ind = TRUE)
  data.frame(Comparison = paste(o[ij[, 1]], "vs", o[ij[, 2]]),
             Significant = ifelse(sig[ij], "Yes",
                           ifelse(raw[ij], "No (inside a non-significant range)", "No")),
             stringsAsFactors = FALSE)
}

## Textbook sums of squares for a split plot in r replications, a main plots
## and b sub plots (Gomez & Gomez 1984, ch. 3).
split_errors_by_hand <- function(d, R, A, B, y) {
  r <- nlevels(factor(d[[R]])); a <- nlevels(factor(d[[A]])); b <- nlevels(factor(d[[B]]))
  Y <- d[[y]]; CF <- sum(Y)^2 / length(Y)
  tot <- function(by, div) sum(tapply(Y, d[by], sum)^2) / div - CF
  ss_t <- sum(Y^2) - CF
  ss_r <- tot(R, a * b); ss_a <- tot(A, r * b); ss_ra <- tot(c(R, A), b)
  ss_b <- tot(B, r * a); ss_ab <- tot(c(A, B), r) - ss_a - ss_b
  ea <- ss_ra - ss_r - ss_a
  eb <- ss_t - ss_ra - ss_b - ss_ab
  list(r = r, a = a, b = b,
       Ea = ea / ((r - 1) * (a - 1)), dfa = (r - 1) * (a - 1),
       Eb = eb / (a * (r - 1) * (b - 1)), dfb = a * (r - 1) * (b - 1))
}

## ---------------------------------------------------------- one-way (CRD) --

test_that("each CRD mean gets its own standard error under unequal replication", {
  d <- crd_unbalanced()
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  e <- r$effects[["Treatment"]]
  n_i <- tapply(d$Yield, d$Treatment, length)
  mse <- mse_oneway(d$Yield, d$Treatment)
  dfe <- nrow(d) - nlevels(d$Treatment)
  expect_equal(r$mse, mse, tolerance = 1e-10)
  expect_equal(r$dfe, dfe)

  lv <- as.character(e$means$Treatment)
  expect_equal(e$means$N, unname(as.vector(n_i[lv])))
  # SE(m)_i = sqrt(MSE / n_i), not one figure for every mean
  expect_equal(e$means$SE, unname(as.vector(sqrt(mse / n_i[lv]))), tolerance = 1e-10)
  # the same thing as the standard errors of a cell-means regression
  vc <- sqrt(diag(stats::vcov(stats::lm(Yield ~ 0 + Treatment, data = d))))
  expect_equal(e$means$SE, unname(vc[paste0("Treatment", lv)]), tolerance = 1e-10)
  expect_equal(e$se_range, range(sqrt(mse / n_i)), tolerance = 1e-10)
})

test_that("each pair of CRD means gets its own SE(d) and C.D.", {
  d <- crd_unbalanced()
  n_i <- tapply(d$Yield, d$Treatment, length)
  mse <- mse_oneway(d$Yield, d$Treatment)
  dfe <- nrow(d) - nlevels(d$Treatment)
  for (a in c(0.05, 0.01)) {
    e <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"),
                 alpha = a)$effects[["Treatment"]]
    for (i in names(n_i)) for (j in names(n_i)) {
      # SE(d)_ij = sqrt(MSE (1/n_i + 1/n_j)); looked up by level, not position
      want <- if (i == j) 0 else sqrt(mse * (1 / n_i[[i]] + 1 / n_i[[j]]))
      expect_equal(e$sed_mat[i, j], want, tolerance = 1e-10, info = paste(i, j))
      expect_equal(e$cd_mat[i, j], stats::qt(1 - a / 2, dfe) * want,
                   tolerance = 1e-10, info = paste(i, j, "alpha", a))
    }
    # no single SE(m), SE(d) or C.D. fits every mean, so none is reported
    expect_false(e$equal_rep)
    expect_true(is.na(e$sem))
    expect_true(is.na(e$sed))
    expect_true(is.na(e$cd))
    off <- row(e$sed_mat) != col(e$sed_mat)
    expect_equal(e$sed_range, range(e$sed_mat[off]))
    expect_equal(e$cd_range, range(stats::qt(1 - a / 2, dfe) * e$sed_mat[off]),
                 tolerance = 1e-10)
  }
})

## ----------------------------------------------------- blocked designs ------

test_that("an RCBD with a missing plot gives Yates's adjusted mean and the textbook SE(d)", {
  t <- 4; r <- 4
  d <- expand.grid(Block = paste0("B", 1:r), Variety = paste0("V", 1:t))
  d$Yield <- c(20.1, 21.9, 19.2, 22.4,   23.0, 24.8, 22.1, 25.6,
               18.7, 20.9, 17.4, 21.2,   26.3, 26.9, 25.0, 28.4)
  dm <- d[!(d$Block == "B2" & d$Variety == "V3"), ]
  res <- analyze(dm, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  e <- res$effects[["Variety"]]
  expect_false(res$balanced)

  # Yates's (1933) estimate of the missing plot from the remaining data
  Tm <- sum(dm$Yield[dm$Variety == "V3"])
  Bm <- sum(dm$Yield[dm$Block == "B2"])
  G  <- sum(dm$Yield)
  yhat <- (t * Tm + r * Bm - G) / ((t - 1) * (r - 1))
  mu <- stats::setNames(e$means$Mean, as.character(e$means$Variety))
  expect_equal(mu[["V3"]], (Tm + yhat) / r, tolerance = 1e-10)
  # the complete treatments keep their plain averages
  for (v in c("V1", "V2", "V4"))
    expect_equal(mu[[v]], mean(dm$Yield[dm$Variety == v]), tolerance = 1e-10, info = v)
  # and the plain average of what was observed is still shown
  expect_true("Raw_mean" %in% names(e$means))
  raw <- stats::setNames(e$means$Raw_mean, as.character(e$means$Variety))
  expect_equal(raw[["V3"]], Tm / (r - 1), tolerance = 1e-10)

  # error mean square: the residual SS of the table completed with yhat (the
  # estimate has no residual of its own), on one degree of freedom fewer
  full <- d
  full$Yield[full$Block == "B2" & full$Variety == "V3"] <- yhat
  Y <- tapply(full$Yield, list(full$Variety, full$Block), sum)
  sse <- sum((Y - outer(rowMeans(Y), colMeans(Y), "+") + mean(Y))^2)
  dfe <- (r - 1) * (t - 1) - 1
  mse <- sse / dfe
  expect_equal(res$dfe, dfe)
  expect_equal(res$anova$Df[res$anova$Source == "Residuals"], dfe)
  expect_equal(res$mse, mse, tolerance = 1e-10)

  # SE(d) (Gomez & Gomez 1984, ch. 7): the treatment with the missing plot
  # against any other, and two complete treatments
  sed_miss <- sqrt(mse * (2 / r + t / (r * (r - 1) * (t - 1))))
  for (v in c("V1", "V2", "V4")) {
    expect_equal(e$sed_mat["V3", v], sed_miss, tolerance = 1e-10, info = v)
    expect_equal(e$sed_mat[v, "V3"], sed_miss, tolerance = 1e-10, info = v)
  }
  for (pr in list(c("V1", "V2"), c("V1", "V4"), c("V2", "V4")))
    expect_equal(e$sed_mat[pr[1], pr[2]], sqrt(2 * mse / r), tolerance = 1e-10,
                 info = paste(pr, collapse = " vs "))
  expect_false(e$equal_rep)

  # SE of each adjusted mean, from the regression's own covariance matrix with
  # a contrast vector written out by hand (treatment contrasts: intercept, the
  # blocks averaged with B1 as zero, and the variety's own coefficient)
  fit <- stats::lm(Yield ~ Block + Variety, data = dm)
  V <- stats::vcov(fit)
  for (v in paste0("V", 1:t)) {
    l <- stats::setNames(numeric(length(stats::coef(fit))), names(stats::coef(fit)))
    l["(Intercept)"] <- 1
    l[paste0("BlockB", 2:r)] <- 1 / r
    if (v != "V1") l[paste0("Variety", v)] <- 1
    se_v <- sqrt(drop(crossprod(l, V %*% l)))
    expect_equal(e$means$SE[e$means$Variety == v], se_v, tolerance = 1e-10, info = v)
  }
  # complete treatments: SE(m) = sqrt(MSE / r) exactly
  expect_equal(e$means$SE[e$means$Variety == "V1"], sqrt(mse / r), tolerance = 1e-10)

  # the variety F-test is adjusted for blocks: the error SS of a blocks-only
  # fit minus the full error SS
  sse_blocks <- sum(tapply(dm$Yield, dm$Block, function(x) sum((x - mean(x))^2)))
  F_trt <- ((sse_blocks - sse) / (t - 1)) / mse
  expect_equal(res$anova$F[res$anova$Source == "Variety"], F_trt, tolerance = 1e-8)

  # the Total row is the total sum of squares of the data
  tot <- res$anova[res$anova$Source == "Total", ]
  expect_equal(tot$Df, nrow(dm) - 1)
  expect_equal(tot$SS, sum((dm$Yield - mean(dm$Yield))^2), tolerance = 1e-10)
})

test_that("a Latin square with a missing plot uses Yates's estimate and loses one error df", {
  d <- demo_data("LSD")
  t <- nlevels(factor(d$Treatment))
  gone <- d$Row == "R3" & d$Column == "C2"
  trt <- as.character(d$Treatment[gone])
  dm <- d[!gone, ]
  res <- analyze(dm, "LSD", list(response = "Yield", treat = "Treatment",
                                 row = "Row", col = "Column"))
  e <- res$effects[["Treatment"]]
  dfe <- (t - 1) * (t - 2) - 1
  expect_equal(res$dfe, dfe)
  expect_false(res$balanced)

  # Yates: yhat = (t (R + C + T) - 2G) / ((t - 1)(t - 2))
  Rt <- sum(dm$Yield[dm$Row == "R3"])
  Ct <- sum(dm$Yield[dm$Column == "C2"])
  Tt <- sum(dm$Yield[dm$Treatment == trt])
  G  <- sum(dm$Yield)
  yhat <- (t * (Rt + Ct + Tt) - 2 * G) / ((t - 1) * (t - 2))
  mu <- stats::setNames(e$means$Mean, as.character(e$means$Treatment))
  expect_equal(mu[[trt]], (Tt + yhat) / t, tolerance = 1e-10)
  for (v in setdiff(names(mu), trt))
    expect_equal(mu[[v]], mean(dm$Yield[dm$Treatment == v]), tolerance = 1e-10, info = v)

  # error mean square from the square completed with yhat
  full <- d; full$Yield[gone] <- yhat
  g <- mean(full$Yield)
  fit_val <- ave(full$Yield, full$Row) + ave(full$Yield, full$Column) +
             ave(full$Yield, full$Treatment) - 2 * g
  mse <- sum((full$Yield - fit_val)^2) / dfe
  expect_equal(res$mse, mse, tolerance = 1e-10)
  # SE(d) for the treatment with the missing plot (Gomez & Gomez 1984, ch. 7)
  other <- setdiff(names(mu), trt)[1]
  expect_equal(e$sed_mat[trt, other], sqrt(mse * (2 / t + 1 / ((t - 1) * (t - 2)))),
               tolerance = 1e-10)
  expect_equal(e$sed_mat[setdiff(names(mu), trt)[1], setdiff(names(mu), trt)[2]],
               sqrt(2 * mse / t), tolerance = 1e-10)
})

test_that("a factorial with unequal cells reports adjusted marginal means and Type III tests", {
  n <- c(3, 2, 4, 2, 4, 3)            # N0V1, N0V2, N0V3, N1V1, N1V2, N1V3
  d <- data.frame(
    Nitrogen = factor(rep(rep(c("N0", "N1"), each = 3), n)),
    Variety  = factor(rep(rep(c("V1", "V2", "V3"), 2), n)),
    Yield = c(20.1, 21.3, 19.6,   23.4, 24.6,   22.0, 23.1, 21.5, 22.8,
              25.2, 26.0,   27.9, 29.1, 28.3, 27.4,   30.2, 31.5, 29.8))
  res <- analyze(d, "FCRD", list(response = "Yield", factors = c("Nitrogen", "Variety")))
  expect_false(res$balanced)

  cell_mu <- tapply(d$Yield, list(d$Nitrogen, d$Variety), mean)
  cell_n  <- tapply(d$Yield, list(d$Nitrogen, d$Variety), length)
  cells <- interaction(d$Nitrogen, d$Variety)
  mse <- mse_oneway(d$Yield, cells)
  expect_equal(res$mse, mse, tolerance = 1e-10)
  a <- nrow(cell_mu); b <- ncol(cell_mu)

  # a marginal mean is the unweighted mean of its cell means, with variance
  # MSE / b^2 * sum_j 1 / n_ij
  eN <- res$effects[["Nitrogen"]]
  for (lv in rownames(cell_mu)) {
    i <- eN$means$Nitrogen == lv
    expect_equal(eN$means$Mean[i], mean(cell_mu[lv, ]), tolerance = 1e-10, info = lv)
    expect_equal(eN$means$SE[i], sqrt(mse / b^2 * sum(1 / cell_n[lv, ])),
                 tolerance = 1e-10, info = lv)
  }
  expect_equal(eN$sed_mat["N0", "N1"],
               sqrt(mse / b^2 * sum(1 / cell_n["N0", ] + 1 / cell_n["N1", ])),
               tolerance = 1e-10)
  eV <- res$effects[["Variety"]]
  for (lv in colnames(cell_mu)) {
    i <- eV$means$Variety == lv
    expect_equal(eV$means$Mean[i], mean(cell_mu[, lv]), tolerance = 1e-10, info = lv)
    expect_equal(eV$means$SE[i], sqrt(mse / a^2 * sum(1 / cell_n[, lv])),
                 tolerance = 1e-10, info = lv)
  }
  # cell means are the plain cell averages, each with SE sqrt(MSE / n_ij)
  eI <- res$effects[["Nitrogen:Variety"]]
  for (k in seq_len(nrow(eI$means))) {
    ni <- as.character(eI$means$Nitrogen[k]); vi <- as.character(eI$means$Variety[k])
    expect_equal(eI$means$Mean[k], cell_mu[ni, vi], tolerance = 1e-10)
    expect_equal(eI$means$SE[k], sqrt(mse / cell_n[ni, vi]), tolerance = 1e-10)
  }

  # every F-test is the Type III test of the adjusted means
  form <- Yield ~ Nitrogen * Variety
  for (term in c("Nitrogen", "Variety", "Nitrogen:Variety")) {
    want <- type3_by_hand(form, d, term)
    row <- res$anova[res$anova$Source == term, ]
    expect_equal(row$F, unname(want["F"]), tolerance = 1e-8, info = term)
    expect_equal(row$p, unname(want["p"]), tolerance = 1e-8, info = term)
  }
  # the Total row still describes the data
  tot <- res$anova[res$anova$Source == "Total", ]
  expect_equal(tot$SS, sum((d$Yield - mean(d$Yield))^2), tolerance = 1e-10)
  expect_equal(tot$Df, nrow(d) - 1)
})

test_that("equal treatment counts in incomplete blocks are still unbalanced", {
  # every variety appears three times, but not in every block
  d <- data.frame(
    Block   = factor(c("B1", "B1", "B2", "B2", "B3", "B3", "B4", "B4", "B4")),
    Variety = factor(c("T1", "T2", "T1", "T3", "T2", "T3", "T1", "T2", "T3")),
    Yield   = c(18.2, 21.5, 19.8, 24.9, 22.7, 25.3, 17.6, 20.9, 23.8))
  expect_true(all(table(d$Variety) == 3))
  res <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  expect_false(res$balanced)
  e <- res$effects[["Variety"]]
  # least-squares means by hand from the additive fit: intercept, the average
  # block effect (B1 = 0) and the variety's own effect (T1 = 0)
  b <- stats::coef(stats::lm(Yield ~ Block + Variety, data = d))
  base <- b[["(Intercept)"]] + mean(c(0, b[paste0("BlockB", 2:4)]))
  want <- base + c(T1 = 0, T2 = b[["VarietyT2"]], T3 = b[["VarietyT3"]])
  expect_equal(e$means$Mean, unname(want[as.character(e$means$Variety)]), tolerance = 1e-10)
  expect_equal(e$means$Raw_mean,
               unname(as.vector(tapply(d$Yield, d$Variety, mean)[as.character(e$means$Variety)])),
               tolerance = 1e-10)
})

## ------------------------------------------------------------- post-hoc -----

test_that("Tukey HSD with unequal replication is Tukey-Kramer and matches TukeyHSD()", {
  d <- crd_unbalanced()
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  pair_key <- function(x, sep) vapply(strsplit(x, sep, fixed = TRUE),
                                      function(z) paste(sort(z), collapse = "|"), character(1))
  for (a in c(0.05, 0.01)) {
    ph <- posthoc(r, "Treatment", "Tukey HSD", alpha = a)
    tk <- stats::TukeyHSD(stats::aov(Yield ~ Treatment, data = d), conf.level = 1 - a)$Treatment
    k_tk <- pair_key(rownames(tk), "-")
    k_ph <- pair_key(ph$pairs$Comparison, " vs ")
    expect_setequal(k_ph, k_tk)
    ix <- match(k_tk, k_ph)
    # the half-width of TukeyHSD's interval is the pair's critical difference
    expect_equal(ph$pairs[["Critical difference"]][ix], unname(tk[, "diff"] - tk[, "lwr"]),
                 tolerance = 1e-8, info = paste("alpha", a))
    expect_equal(ph$pairs$Significant[ix] == "Yes", unname(tk[, "p adj"] < a),
                 info = paste("alpha", a))
    # and it says what it is
    expect_match(ph$stats$Value[ph$stats$Item == "Method"], "Tukey-Kramer", fixed = TRUE)
  }
})

test_that("two means share a letter exactly when their difference is within the C.D.", {
  set.seed(20261007)
  bad_letters <- 0; bad_cd <- 0
  for (it in 1:80) {
    k <- sample(3:7, 1)
    n <- sample(2:6, k, replace = TRUE)
    if (length(unique(n)) == 1) n[1] <- n[1] + 1        # keep it unbalanced
    trt <- factor(rep(sprintf("T%d", seq_len(k)), n))
    y <- rnorm(k, 10, 1.5)[as.integer(trt)] + rnorm(length(trt))
    d <- data.frame(Treatment = trt, Yield = y)
    e <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))$effects$Treatment
    n_i <- tapply(y, trt, length); mse <- mse_oneway(y, trt)
    lv <- as.character(e$means$Treatment)
    for (i in seq_len(k - 1)) for (j in (i + 1):k) {
      cd <- stats::qt(0.975, length(y) - k) * sqrt(mse * (1 / n_i[[lv[i]]] + 1 / n_i[[lv[j]]]))
      if (abs(e$cd_mat[lv[i], lv[j]] - cd) > 1e-8) bad_cd <- bad_cd + 1
      same <- abs(e$means$Mean[i] - e$means$Mean[j]) <= cd
      if (share_letter(e$means$Letter[i], e$means$Letter[j]) != same)
        bad_letters <- bad_letters + 1
    }
  }
  expect_equal(bad_cd, 0)
  expect_equal(bad_letters, 0)
})

test_that("compact letters are exact for any pattern of significant pairs", {
  # Piepho's algorithm must work from the significance matrix alone, whatever
  # produced it, including patterns no single critical difference could give
  set.seed(42)
  bad <- 0; no_a <- 0
  for (it in 1:300) {
    k <- sample(2:8, 1)
    mu <- stats::runif(k)
    sig <- matrix(FALSE, k, k)
    sig[upper.tri(sig)] <- stats::runif(k * (k - 1) / 2) < stats::runif(1)
    sig <- sig | t(sig)
    lets <- cld_from_sig(mu, sig)
    if (any(!nzchar(lets))) bad <- bad + 1
    for (i in seq_len(k - 1)) for (j in (i + 1):k)
      if (share_letter(lets[i], lets[j]) == sig[i, j]) bad <- bad + 1
    # letter a goes to the group holding the highest mean
    if (!grepl("a", lets[which.max(mu)], fixed = TRUE)) no_a <- no_a + 1
  }
  expect_equal(bad, 0)
  expect_equal(no_a, 0)
})

test_that("cld_lsd takes one critical difference or one per pair", {
  mu <- c(10, 10.8, 11.9, 13.5, 13.9)
  expect_identical(cld_lsd(mu, 1), cld_lsd(mu, matrix(1, 5, 5)))
  # a pair-specific matrix: only 1 vs 2 is given a tight critical difference
  cd <- matrix(5, 5, 5); cd[1, 2] <- cd[2, 1] <- 0.5
  lets <- cld_lsd(mu, cd)
  expect_false(share_letter(lets[1], lets[2]))
  expect_true(share_letter(lets[1], lets[5]))
  # a missing critical difference means no letters at all
  expect_identical(cld_lsd(mu, NA), rep("", 5))
})

test_that("SNK and Duncan follow the step-down rule, pair by pair", {
  set.seed(7)
  for (method in c("Student-Newman-Keuls", "Duncan's DMRT")) {
    mism <- 0; encl <- 0
    for (it in 1:24) {
      k <- sample(3:7, 1)
      n <- if (it %% 2) rep(4, k) else sample(2:6, k, replace = TRUE)
      trt <- factor(rep(sprintf("T%d", seq_len(k)), n))
      y <- rnorm(k, 10, 1.2)[as.integer(trt)] + rnorm(length(trt), 0, 0.8)
      d <- data.frame(Treatment = trt, Yield = y)
      r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
      ph <- posthoc(r, "Treatment", method, alpha = 0.05)
      want <- range_test_by_hand(d, method, 0.05)
      got <- ph$pairs$Significant[match(want$Comparison, ph$pairs$Comparison)]
      mism <- mism + sum(got != want$Significant | is.na(got))

      # every "Yes" sits inside ranges (in mean order) that are all "Yes"
      g <- ph$groups
      rk <- stats::setNames(rank(-g$Mean, ties.method = "first"), g$Treatment)
      st <- matrix("", k, k)
      ends <- strsplit(ph$pairs$Comparison, " vs ", fixed = TRUE)
      for (p in seq_along(ends)) {
        i <- rk[[ends[[p]][1]]]; j <- rk[[ends[[p]][2]]]
        st[min(i, j), max(i, j)] <- ph$pairs$Significant[p]
      }
      for (i in seq_len(k - 1)) for (j in (i + 1):k) if (st[i, j] == "Yes")
        for (i2 in seq_len(i)) for (j2 in j:k)
          if (st[i2, j2] != "Yes") encl <- encl + 1
    }
    expect_equal(mism, 0, info = method)
    expect_equal(encl, 0, info = method)
  }
})

test_that("a pair inside a non-significant wider range is not declared different", {
  # three treatments, four plots each, built so that SE(d) = 1 exactly:
  # deviations of +/- sqrt(1.5) give MSE = 2, and sqrt(2 * 2 / 4) = 1
  dev <- rep(c(-1, -1, 1, 1) * sqrt(1.5), 3)
  make <- function(mu) data.frame(Treatment = factor(rep(names(mu), each = 4)),
                                  Yield = rep(mu, each = 4) + dev)
  dfe <- 9
  r2  <- stats::qt(0.975, dfe)                         # two means apart
  snk3 <- stats::qtukey(0.95, 3, dfe) / sqrt(2)        # three apart, SNK
  dun3 <- stats::qtukey(0.95^2, 3, dfe) / sqrt(2)      # three apart, Duncan

  # SNK: T1 - T2 = 2.4 exceeds its range, but T1 - T3 = 2.5 does not exceed the
  # wider range that contains it
  d <- make(c(T1 = 12.5, T2 = 10.1, T3 = 10.0))
  expect_gt(2.4, r2); expect_lt(2.5, snk3)
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_equal(r$effects$Treatment$sed, 1, tolerance = 1e-10)
  ph <- posthoc(r, "Treatment", "Student-Newman-Keuls")
  sg <- stats::setNames(ph$pairs$Significant, ph$pairs$Comparison)
  expect_identical(sg[["T1 vs T2"]], "No (inside a non-significant range)")
  expect_identical(sg[["T1 vs T3"]], "No")
  cv <- stats::setNames(ph$pairs[["Critical value"]], ph$pairs$Comparison)
  expect_equal(cv[["T1 vs T2"]], r2, tolerance = 1e-8)
  expect_equal(cv[["T1 vs T3"]], snk3, tolerance = 1e-8)
  # and the letters agree: no pair is separated
  expect_true(all(ph$groups$Group == "a"))

  # Duncan: the same situation with its narrower three-mean range
  d <- make(c(T1 = 12.33, T2 = 10.03, T3 = 10.0))
  expect_gt(2.30, r2); expect_lt(2.33, dun3)
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  ph <- posthoc(r, "Treatment", "Duncan's DMRT")
  sg <- stats::setNames(ph$pairs$Significant, ph$pairs$Comparison)
  expect_identical(sg[["T1 vs T2"]], "No (inside a non-significant range)")
  expect_identical(sg[["T1 vs T3"]], "No")
  cv <- stats::setNames(ph$pairs[["Critical value"]], ph$pairs$Comparison)
  expect_equal(cv[["T1 vs T3"]], dun3, tolerance = 1e-8)
})

test_that("Duncan's test runs with 25 treatments and gives finite critical ranges", {
  # qtukey() returns NaN for Duncan's long ranges at about twenty means
  k <- 25; reps <- 3
  d <- data.frame(Treatment = factor(rep(sprintf("T%02d", 1:k), each = reps)))
  d$Yield <- 50 + rep((1:k) * 0.7, each = reps) + rep(c(-1.1, 0.3, 0.8), k)
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  dun <- posthoc(r, "Treatment", "Duncan's DMRT")
  cr <- dun$ranges[["Critical range"]]
  expect_length(cr, k - 1)
  expect_true(all(is.finite(cr)))
  expect_true(all(diff(cr) > 0))
  # each critical range is the studentized range at Duncan's protection level
  # (1 - alpha)^(p - 1), checked through ptukey() rather than qtukey()
  dfe <- k * (reps - 1)
  sed <- sqrt(2 * mse_oneway(d$Yield, d$Treatment) / reps)
  p <- dun$ranges[["Means apart (p)"]]
  expect_equal(p, 2:k)
  expect_equal(stats::ptukey(cr / sed * sqrt(2), p, dfe), 0.95^(p - 1), tolerance = 1e-7)
  expect_false(anyNA(dun$pairs$Significant))
  expect_equal(nrow(dun$pairs), k * (k - 1) / 2)
})

## ------------------------------------------------- designs that must refuse --

test_that("designs with several error strata refuse incomplete data and say what is missing", {
  sp <- demo_data("SPLIT")
  sp_map <- list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety")
  expect_error(analyze(sp[!(sp$Rep == "R2" & sp$Irrigation == "I1" & sp$Variety == "V3"), ],
                       "SPLIT", sp_map),
               "Missing: R2 x I1 x V3", fixed = TRUE)
  expect_error(analyze(rbind(sp, sp[sp$Rep == "R1" & sp$Irrigation == "I1" & sp$Variety == "V1", ]),
                       "SPLIT", sp_map),
               "More than one row: R1 x I1 x V1", fixed = TRUE)

  st <- demo_data("STRIP")
  expect_error(analyze(st[!(st$Rep == "R1" & st$Tillage == "Minimum" & st$Mulch == "M0"), ],
                       "STRIP", list(response = "Yield", rep = "Rep", main = "Tillage",
                                     sub = "Mulch")),
               "Missing: R1 x Minimum x M0", fixed = TRUE)

  pr <- demo_data("POOLRCBD")
  pr_map <- list(response = "Yield", env = "Location", rep = "Rep", treat = "Variety")
  expect_error(analyze(pr[!(pr$Location == "Wadura" & pr$Rep == "R2" & pr$Variety == "V3"), ],
                       "POOLRCBD", pr_map),
               "in Wadura, R2 x V3 is missing", fixed = TRUE)
  # a whole replication gone from one environment leaves no hole in that
  # environment's table, so the message must name the replication counts
  expect_error(analyze(pr[!(pr$Location == "Wadura" & pr$Rep == "R3"), ], "POOLRCBD", pr_map),
               "Shalimar 3, Wadura 2, Zampathan 3", fixed = TRUE)

  pfa <- demo_data("POOLFACT")
  expect_error(analyze(pfa[!(pfa$Location == "Wadura" & pfa$Rep == "R2" &
                            pfa$Nitrogen == "N1" & pfa$Variety == "V2"), ],
                       "POOLFRCBD", list(response = "Yield", env = "Location", rep = "Rep",
                                         factors = c("Nitrogen", "Variety"))),
               "in Wadura, R2 x N1 x V2 is missing", fixed = TRUE)

  pc <- demo_data("POOLCRD")
  gone <- which(pc$Season == "Kharif" & pc$Treatment == "T2")[1]
  expect_error(analyze(pc[-gone, ], "POOLCRD",
                       list(response = "Yield", env = "Season", treat = "Treatment")),
               "Kharif x T2 has 3", fixed = TRUE)
})

test_that("a factorial with an empty treatment combination is refused", {
  fr <- demo_data("FRCBD")
  gap <- fr[!(fr$Nitrogen == "N60" & fr$Variety == "V2"), ]
  expect_error(analyze(gap, "FRCBD", list(response = "Yield", block = "Block",
                                          factors = c("Nitrogen", "Variety"))),
               "none for N60 x V2", fixed = TRUE)
  expect_error(analyze(gap, "FCRD", list(response = "Yield",
                                         factors = c("Nitrogen", "Variety"))),
               "none for N60 x V2", fixed = TRUE)
})

## ------------------------------------------- split and strip interactions --

test_that("split-plot interaction post-hoc compares sub plots only within a main plot", {
  d <- demo_data("SPLIT")
  r <- analyze(d, "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation",
                                sub = "Variety"))
  h <- split_errors_by_hand(d, "Rep", "Irrigation", "Variety", "Yield")
  expect_equal(r$errors[["Error (b)"]]$ms, h$Eb, tolerance = 1e-8)
  expect_equal(r$errors[["Error (a)"]]$ms, h$Ea, tolerance = 1e-8)

  # main effects carry their own error terms
  expect_equal(r$effects[["Irrigation"]]$means$SE, rep(sqrt(h$Ea / (h$r * h$b)), h$a),
               tolerance = 1e-10)
  expect_equal(r$effects[["Variety"]]$means$SE, rep(sqrt(h$Eb / (h$r * h$a)), h$b),
               tolerance = 1e-10)

  ph <- posthoc(r, "Irrigation:Variety", "Tukey HSD")
  expect_true("Within" %in% names(ph$groups))
  expect_true("Within" %in% names(ph$pairs))
  # a main plots, each with choose(b, 2) pairs of sub plots, and nothing across
  expect_equal(nrow(ph$pairs), h$a * choose(h$b, 2))
  ends <- strsplit(ph$pairs$Comparison, " vs ", fixed = TRUE)
  mp1 <- vapply(ends, function(z) sub(" : .*", "", z[1]), character(1))
  mp2 <- vapply(ends, function(z) sub(" : .*", "", z[2]), character(1))
  expect_identical(mp1, mp2)
  expect_identical(mp1, as.character(ph$pairs$Within))
  # every pair: SE(d) = sqrt(2 Eb / r), Tukey on Error (b) df
  sed <- sqrt(2 * h$Eb / h$r)
  expect_equal(ph$pairs$SEd, rep(sed, nrow(ph$pairs)), tolerance = 1e-10)
  expect_equal(ph$pairs[["Critical difference"]],
               rep(stats::qtukey(0.95, h$b, h$dfb) / sqrt(2) * sed, nrow(ph$pairs)),
               tolerance = 1e-8)
  # comparisons across main plots have no single SE(d) here and are left empty
  e <- r$effects[["Irrigation:Variety"]]
  expect_true(is.na(e$sed_mat["I1 : V1", "I2 : V1"]))
  expect_equal(e$sed_mat["I1 : V1", "I1 : V2"], sed, tolerance = 1e-10)
})

test_that("strip-plot interaction uses the weighted t and Satterthwaite's df", {
  d <- demo_data("STRIP")
  r <- analyze(d, "STRIP", list(response = "Yield", rep = "Rep", main = "Tillage",
                                sub = "Mulch"))
  # textbook strip-plot sums of squares (Gomez & Gomez 1984, ch. 3)
  Y <- d$Yield; CF <- sum(Y)^2 / length(Y)
  R <- 4; A <- 3; B <- 4
  tot <- function(by, div) sum(tapply(Y, d[by], sum)^2) / div - CF
  ss_r <- tot("Rep", A * B); ss_a <- tot("Tillage", R * B); ss_b <- tot("Mulch", R * A)
  ea <- tot(c("Rep", "Tillage"), B) - ss_r - ss_a
  eb <- tot(c("Rep", "Mulch"), A) - ss_r - ss_b
  ss_ab <- tot(c("Tillage", "Mulch"), R) - ss_a - ss_b
  ec <- sum(Y^2) - CF - ss_r - ss_a - ea - ss_b - eb - ss_ab
  dfa <- (R - 1) * (A - 1); dfb <- (R - 1) * (B - 1); dfc <- (R - 1) * (A - 1) * (B - 1)
  Ea <- ea / dfa; Eb <- eb / dfb; Ec <- ec / dfc

  # two Mulch means at one Tillage level mix Error (b) and Error (c)
  w <- c((A - 1) * Ec, Eb)
  sed <- sqrt(2 * sum(w) / (R * A))
  tw <- sum(w * stats::qt(0.975, c(dfc, dfb))) / sum(w)
  df_s <- sum(w)^2 / sum(w^2 / c(dfc, dfb))
  e <- r$effects[["Tillage:Mulch"]]
  expect_equal(e$sed, sed, tolerance = 1e-10)
  expect_equal(e$df, df_s, tolerance = 1e-10)
  expect_equal(e$cd, tw * sed, tolerance = 1e-10)
  ph <- posthoc(r, "Tillage:Mulch", "LSD (Fisher's protected)")
  expect_equal(ph$pairs[["Critical value"]], rep(tw, nrow(ph$pairs)), tolerance = 1e-10)
  expect_equal(nrow(ph$pairs), A * choose(B, 2))
})
