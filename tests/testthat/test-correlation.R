## Correlation: the coefficients, p-values, intervals and marks against
## stats::cor.test(), the strength words against their stated cut-offs, the
## readings against the numbers, and the Explore tab.
##
## Expectations come from base R and from the definitions:
##   * each pair uses every row with both values (pairwise deletion);
##   * Spearman's and Kendall's p-values are exact when the distinct
##     orderings of one variable can be counted (ties included), estimated
##     from random orderings with ties and too many orderings (Kendall's up
##     to 20 rows), exact from cor.test() without ties where it is, and
##     flagged as an approximation otherwise;
##   * marks are * at alpha and ** at alpha / 5, NS otherwise;
##   * strength uses Cohen's 0.1, 0.3, 0.5 for r and the bivariate-normal
##     equivalents for rho, (6 / pi) asin(r / 2), and tau, (2 / pi) asin(r);
##   * a result short of significance is "insufficient evidence", never "no
##     correlation", and nothing claims a cause.

cor_data <- function(seed = 1, n = 24) {
  set.seed(seed)
  x <- rnorm(n, 50, 5)
  data.frame(Yield = round(x, 2), Height = round(60 + 0.8 * x + rnorm(n, 0, 2), 1),
             Tillers = rpois(n, 12), Disease = round(30 - 0.3 * x + rnorm(n, 0, 3), 1))
}

## ----------------------------------------------------- the numbers ----

test_that("each pair matches cor.test() for all three coefficients", {
  d <- cor_data()
  vars <- names(d)
  for (m in c("pearson", "spearman", "kendall")) for (a in c(0.05, 0.01)) {
    tab <- correlate_data(d, vars, m, a)
    expect_equal(nrow(tab), choose(4, 2))
    for (i in seq_len(nrow(tab))) {
      x <- d[[tab$Variable1[i]]]; y <- d[[tab$Variable2[i]]]
      ct <- if (m == "pearson") cor.test(x, y, conf.level = 1 - a)
            else suppressWarnings(cor.test(x, y, method = m, exact = FALSE))
      expect_equal(tab$Coefficient[i], unname(ct$estimate), info = paste(m, i))
      ## the p-value is checked against cor.test where cor.test is what is used
      if (m == "pearson" || identical(tab$P_method[i], "approximation"))
        expect_equal(tab$p[i], ct$p.value, info = paste(m, i))
      if (m == "pearson") expect_equal(c(tab$CI_lower[i], tab$CI_upper[i]), ct$conf.int[1:2])
      else expect_true(is.na(tab$CI_lower[i]))
      expect_identical(tab$Mark[i], if (tab$p[i] < a / 5) "**" else if (tab$p[i] < a) "*" else "NS")
    }
  }
  # Kendall's coefficient is tau-b, which allows for ties
  x <- c(1, 1, 2, 3, 4, 5, 5, 6); y <- c(2, 3, 3, 4, 4, 6, 7, 7)
  expect_equal(correlate_data(data.frame(x, y), c("x", "y"), "kendall")$Coefficient, cor(x, y, method = "kendall"))
})

test_that("each pair uses every row with both values", {
  d <- cor_data()
  d$Yield[c(2, 5)] <- NA; d$Height[c(5, 9)] <- NA; d$Disease[3] <- "5,6"
  tab <- correlate_data(d, c("Yield", "Height", "Disease"))
  expect_equal(tab$N, c(21, 21, 21))
  ok <- !is.na(d$Yield) & !is.na(d$Height)
  expect_equal(tab$Coefficient[1], cor(d$Yield[ok], d$Height[ok]))
  # the reading says N varies when it does
  d2 <- cor_data(); d2$Yield[1:3] <- NA
  expect_match(cor_text(correlate_data(d2, names(d2))), "so N runs from 21 to 24", fixed = TRUE)
})

test_that("pairs that cannot be worked out say why", {
  d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 2, 2, 2, 2), c = c(1, NA, NA, NA, 3), e = c(5, 3, 4, 1, 2))
  tab <- correlate_data(d, c("a", "b", "c", "e"))
  r <- function(v1, v2) tab$Reading[tab$Variable1 == v1 & tab$Variable2 == v2]
  expect_identical(r("a", "b"), "Not available: 'b' does not vary in the rows that have both values.")
  expect_identical(r("a", "c"), "Not available: only 2 rows have both values.")
  expect_true(is.na(tab$p[tab$Variable1 == "a" & tab$Variable2 == "b"]))
  expect_identical(tab$Mark[is.na(tab$p)], rep("", sum(is.na(tab$p))))
  expect_match(cor_text(tab), "pairs could not be worked out; the table says why.", fixed = TRUE)
  expect_no_warning(correlate_data(d, c("a", "b", "c", "e"), "spearman"))
  expect_no_warning(correlate_data(d, c("a", "b", "c", "e"), "pearson"))
  expect_no_warning(cor_text(tab)); expect_no_warning(cor_heat_text(tab))
  expect_no_warning(correlate_data(d, c("a", "b", "c", "e"), "kendall"))
})

test_that("bad requests are refused in plain words", {
  d <- cor_data()
  expect_error(correlate_data(d, "Yield"), "at least two variables", fixed = TRUE)
  expect_error(correlate_data(d, c("Yield", "Yeild")), "'Yeild' is not a column of the data.", fixed = TRUE)
  expect_error(correlate_data(d, c("Yield", "Height"), alpha = 0.7), "between 0 and 0.5", fixed = TRUE)
})

test_that("each p-value says how it was found", {
  pm <- function(d, m) correlate_data(d, c("x", "y"), m)$P_method
  d <- data.frame(x = 1:8, y = c(2, 1, 4, 3, 6, 5, 8, 7))
  expect_identical(pm(d, "spearman"), "exact"); expect_identical(pm(d, "kendall"), "exact")
  d$y[2] <- 2
  expect_identical(pm(d, "spearman"), "exact")                       # 8 rows with a tie
  # nine rows with heavy ties: few enough orderings to count (9! / 3!^3 = 1680)
  d9 <- data.frame(x = 1:9, y = c(1, 1, 2, 1, 2, 3, 2, 3, 3))
  expect_identical(pm(d9, "spearman"), "exact"); expect_identical(pm(d9, "kendall"), "exact")
  expect_no_warning(correlate_data(d9, c("x", "y"), "spearman"))
  # one tie only: 181,440 orderings, too many to count, so an estimate
  d9b <- data.frame(x = 1:9, y = c(2, 2, 4, 3, 6, 5, 8, 7, 9))
  expect_identical(pm(d9b, "spearman"), "estimate")
  # nine rows without ties: cor.test is exact for both
  set.seed(2)
  d9n <- data.frame(x = rnorm(9), y = rnorm(9))
  expect_identical(pm(d9n, "spearman"), "exact"); expect_identical(pm(d9n, "kendall"), "exact")
  # more rows without ties: Spearman's is an approximation, Kendall's exact below 50
  d11 <- data.frame(x = rnorm(11), y = rnorm(11))
  expect_identical(pm(d11, "spearman"), "approximation"); expect_identical(pm(d11, "kendall"), "exact")
  # many rows with ties: estimated from random orderings
  d25 <- data.frame(x = round(rnorm(25)), y = round(rnorm(25)))
  expect_identical(pm(d25, "spearman"), "estimate")
  expect_identical(pm(d, "pearson"), "exact")
  h <- cor_pairs_html(correlate_data(d11, c("x", "y"), "spearman"))
  expect_match(h, "<sup>a</sup>", fixed = TRUE)
  expect_match(h, "a: a large-sample approximation, used when the orderings of the values are too many to count", fixed = TRUE)
  h <- cor_pairs_html(correlate_data(d25, c("x", "y"), "spearman"))
  expect_match(h, "<sup>b</sup>", fixed = TRUE)
  expect_match(h, "b: estimated from 20,000 random orderings", fixed = TRUE)
  expect_match(cor_pairs_html(correlate_data(d, c("x", "y"), "kendall")), "Unmarked p-values are exact", fixed = TRUE)
})

## ---------------------------------------------------- the words ----

test_that("strength follows the stated cut-offs, judged as printed", {
  expect_equal(COR_CUTS$spearman, round(6 / pi * asin(c(0.1, 0.3, 0.5) / 2), 2))
  expect_equal(COR_CUTS$kendall, round(2 / pi * asin(c(0.1, 0.3, 0.5)), 2))
  expect_identical(cor_strength(c(0.05, 0.1, 0.29, 0.3, 0.49, 0.5, -0.8), "pearson"),
                   c("negligible", "weak", "weak", "moderate", "moderate", "strong", "strong"))
  expect_identical(cor_strength(c(0.05, 0.06, 0.19, 0.33), "kendall"), c("negligible", "weak", "moderate", "strong"))
  # 0.0951 prints as 0.10, which is weak
  expect_identical(cor_strength(0.0951, "pearson"), "weak")
})

test_that("the reading follows the result and never claims no correlation", {
  expect_identical(cor_reading(0.62, 0.001, "strong", 0.05), "Strong positive association, significant at the 5% level.")
  expect_identical(cor_reading(-0.21, 0.3, "weak", 0.05),
                   "Weak negative coefficient; insufficient evidence of an association at the 5% level.")
  expect_identical(cor_reading(0.001, 0.97, "negligible", 0.01),
                   "A coefficient of 0.00; insufficient evidence of an association at the 1% level.")
  expect_match(cor_reading(0.04, 0.001, "negligible", 0.05), "even a very small coefficient can be significant", fixed = TRUE)
  # p exactly at alpha is not significant, as everywhere in the app
  expect_match(cor_reading(0.4, 0.05, "moderate", 0.05), "insufficient evidence", fixed = TRUE)
  for (seed in 1:5) for (m in c("pearson", "spearman", "kendall")) {
    tab <- correlate_data(cor_data(seed), c("Yield", "Height", "Tillers", "Disease"), m)
    txt <- c(tab$Reading, cor_text(tab), cor_heat_text(tab)$reading,
             cor_scatter_text(cor_data(seed), "Yield", "Tillers", m, 0.05)$reading)
    expect_no_match(txt, "no correlation", ignore.case = TRUE)
    expect_no_match(txt, "no relationship", ignore.case = TRUE)
    expect_no_match(sub("it does not show that one causes the other", "", txt, fixed = TRUE),
                    "proves|causes|caused by|because of", ignore.case = TRUE)
  }
})

test_that("the table reading names the significant pairs, largest first", {
  tab <- correlate_data(cor_data(), c("Yield", "Height", "Tillers", "Disease"))
  txt <- cor_text(tab)
  sig <- tab[tab$p < 0.05, ]
  sig <- sig[order(-abs(round(sig$Coefficient, 2))), ]
  expect_match(txt, sprintf("%d of the 6 pairs show evidence of an association at the 5%% level: %s and %s (%s%s)",
    nrow(sig), sig$Variable1[1], sig$Variable2[1], fmt(sig$Coefficient[1], 2), sig$Mark[1]), fixed = TRUE)
  expect_match(txt, "it does not show that one causes the other", fixed = TRUE)
  expect_match(txt, "two variables can be correlated simply because both respond to the same treatments", fixed = TRUE)
  expect_match(txt, "would make, on average, up to 0.3 pairs significant by chance (6 x 0.05)", fixed = TRUE)
  # all significant, and none
  two <- correlate_data(cor_data(), c("Yield", "Height"))
  expect_match(cor_text(two), "The pair shows evidence of an association at the 5% level: Yield and Height (", fixed = TRUE)
  expect_no_match(cor_text(two), "1 of the 1", fixed = TRUE)
  set.seed(4)
  none <- correlate_data(data.frame(a = rnorm(15), b = rnorm(15), c = rnorm(15)), c("a", "b", "c"), alpha = 0.01)
  if (all(none$p >= 0.01)) expect_match(cor_text(none), "None of the 3 pairs shows evidence of an association at the 1% level.", fixed = TRUE)
})

test_that("each coefficient is explained in the reading", {
  for (m in c("pearson", "spearman", "kendall"))
    expect_match(cor_text(correlate_data(cor_data(), c("Yield", "Height"), m)), cor_about(m), fixed = TRUE)
  expect_match(cor_about("kendall"), "tau-b", fixed = TRUE)
})

## ------------------------------------------------- tables and plots ----

test_that("the matrix prints each pair once with its mark", {
  d <- cor_data(); vars <- c("Yield", "Height", "Disease")
  tab <- correlate_data(d, vars)
  html <- cor_matrix_html(tab, vars)
  for (i in seq_len(nrow(tab))) {
    lab <- paste0(fmt(tab$Coefficient[i], 2), if (tab$Mark[i] == "NS") "" else tab$Mark[i])
    expect_identical(lengths(regmatches(html, gregexpr(sprintf(">%s<", lab), html, fixed = TRUE))), 1L)
  }
  expect_match(html, "Correlation matrix (Pearson's r)", fixed = TRUE)
  p <- cor_pairs_html(tab)
  expect_match(p, "95% CI", fixed = TRUE)
  expect_no_match(cor_pairs_html(correlate_data(d, vars, "spearman")), "CI", fixed = TRUE)
  # names are escaped
  e <- data.frame(`<a>` = 1:5, `b&c` = c(2, 1, 4, 3, 5), check.names = FALSE)
  h <- cor_matrix_html(correlate_data(e, names(e)), names(e))
  expect_match(h, "&lt;a&gt;", fixed = TRUE); expect_match(h, "b&amp;c", fixed = TRUE)
})

test_that("the heat map draws every coefficient where the matrix puts it", {
  d <- cor_data(); vars <- names(d)
  tab <- correlate_data(d, vars)
  b <- ggplot2::ggplot_build(plot_cor_heat(tab, vars))$data
  expect_identical(nrow(b[[1]]), 16L)
  sq <- cor_square(tab, vars)
  expect_equal(sq$coef[sq$Row == "Height" & sq$Col == "Yield"], tab$Coefficient[1])
  expect_equal(sq$coef[sq$Row == "Yield" & sq$Col == "Height"], tab$Coefficient[1])
  expect_true(all(sq$label[sq$diag] == "1"))
  t <- cor_heat_text(tab)
  pc <- round(tab$Coefficient, 2)
  i <- which(pc == max(pc))
  expect_match(t$reading, sprintf("The largest positive coefficient is for %s and %s", tab$Variable1[i[1]], tab$Variable2[i[1]]), fixed = TRUE)
  allpos <- correlate_data(d, c("Yield", "Height"))
  expect_match(cor_heat_text(allpos)$reading, "No pair has a negative coefficient.", fixed = TRUE)
})

test_that("the scatter plot draws the pair and its note states the facts", {
  d <- cor_data()
  p <- plot_cor_scatter(d, "Yield", "Height", "pearson", 0.05)
  b <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(nrow(b), 24)
  expect_match(p$labels$subtitle, "Pearson's r = ", fixed = TRUE)
  expect_null(plot_cor_scatter(d, "Yield", "Yield", "pearson", 0.05))
  t <- cor_scatter_text(d, "Yield", "Height", "pearson", 0.05)
  expect_match(t$reading, "If the points bend along a curve that keeps rising (or keeps falling), or one or two sit far from the rest, Pearson's r can mislead", fixed = TRUE)
  expect_match(t$reading, "A curve that rises and then falls (or the reverse) can give a coefficient near 0 whichever coefficient is used.", fixed = TRUE)
  expect_identical(cor_scatter_text(d, "Yield", "Yield", "pearson", 0.05)$reading, "Choose two different variables to plot.")
  # overlapping points are counted
  o <- data.frame(x = c(1, 1, 2, 3, 4), y = c(2, 2, 3, 5, 4))
  expect_match(cor_scatter_text(o, "x", "y", "spearman", 0.05)$reading,
               "1 row sits exactly on another row's point and is hidden under it.", fixed = TRUE)
  expect_match(cor_scatter_text(o, "x", "y", "spearman", 0.05)$reading, "With only 5 rows", fixed = TRUE)
})

## ------------------------------------------------------------- app ----

test_that("the correlation tab computes, reads and draws", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- cor_data(); d$Rep <- rep(1:4, 6)
    rv$data <- d
    session$flushReact()
    html <- output$corUI$html
    vars <- regmatches(html, regexpr('(?s)id="corVars".*?</select>', html, perl = TRUE))
    expect_length(vars, 1)
    expect_match(vars, '<option value="Yield" selected>', fixed = TRUE)
    expect_match(vars, '<option value="Rep">', fixed = TRUE)                # a label column, not selected
    session$setInputs(corVars = c("Yield", "Height", "Disease"), corMethod = "spearman", corAlpha = "0.01",
                      corX = "Yield", corY = "Disease")
    expect_match(output$corMatrix$html, "Correlation matrix (Spearman's rho)", fixed = TRUE)
    expect_match(output$corMatrix$html, "Strength of rho: under 0.10 negligible", fixed = TRUE)
    expect_match(output$corText$html, "at the 1% level", fixed = TRUE)
    expect_match(output$corPairs$html, "Every pair", fixed = TRUE)
    expect_match(output$corHeat$src, "^data:image/png")
    expect_match(output$corScatter$src, "^data:image/png")
    expect_match(output$corScatterText$html, "Spearman's rho is", fixed = TRUE)
    expect_identical(nrow(cor_tab()), 3L)
    # one variable is not enough
    session$setInputs(corVars = "Yield")
    expect_error(output$corMatrix, "Only 'Yield' is chosen; choose a second variable to correlate.")
  })
})

## ------------------------------------------------ found in review ----

test_that("with few rows and ties the p-value is exact, not a large-sample guess", {
  # three tied rows rising together: the exact p is 1/3, never significant
  d <- data.frame(Score = c(1, 1, 2), Yield = c(30, 30, 41))
  for (m in c("spearman", "kendall")) {
    tab <- correlate_data(d, c("Score", "Yield"), m, alpha = 0.01)
    expect_equal(tab$p, 1 / 3)
    expect_identical(tab$Mark, "NS")
    expect_false(tab$Approximate)
  }
  # the exact p by counting orderings matches cor.test's exact p without ties
  for (n in 4:8) {
    set.seed(n); x <- rnorm(n); y <- x + rnorm(n)
    expect_equal(rank_p(x, y, "spearman")$p, cor.test(x, y, method = "spearman")$p.value, info = n)
    expect_equal(rank_p(x, y, "kendall")$p, cor.test(x, y, method = "kendall")$p.value, info = n)
  }
  # and with ties it is the share of all orderings at least as far from 0
  perms <- function(v) if (length(v) <= 1) list(v) else
    unlist(lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(q) c(v[i], q))), recursive = FALSE)
  brute <- function(x, y, m) {
    obs <- cor(x, y, method = m)
    mean(vapply(perms(seq_along(y)), function(o) abs(cor(x, y[o], method = m)), 0) >= abs(obs) - 1e-9)
  }
  for (i in 1:6) {
    set.seed(10 + i); x <- round(rnorm(6)); y <- round(x + rnorm(6))
    if (sd(x) > 0 && sd(y) > 0) for (m in c("spearman", "kendall"))
      expect_equal(rank_p(x, y, m)$p, brute(x, y, m), info = paste(i, m))
  }
  # the distinct orderings of a tied set: all of them, each once
  v <- c(1, 1, 2, 3, 3, 3)
  A <- distinct_orders(v)
  expect_identical(nrow(A), as.integer(n_orders(v))); expect_identical(nrow(unique(A)), nrow(A))
})

test_that("sparse and tied data beyond eight rows still get an exact p-value", {
  # ten plots, one non-zero in both: the exact p is 1/10, never '**'
  d <- data.frame(Aphids = c(rep(0, 9), 5), Damage = c(rep(0, 9), 3))
  for (m in c("spearman", "kendall")) {
    tab <- correlate_data(d, c("Aphids", "Damage"), m, 0.01)
    expect_equal(tab$p, 0.1); expect_identical(tab$Mark, "NS"); expect_identical(tab$P_method, "exact")
  }
  # twelve plots, two non-zero in both and in the same order: 1 ordering in 132
  d <- data.frame(a = c(rep(0, 10), 5, 7), b = c(rep(0, 10), 3, 4))
  expect_equal(correlate_data(d, c("a", "b"), "spearman")$p, 1 / 132)
})

test_that("an estimate from random orderings is the same every time and leaves the user's stream alone", {
  set.seed(42); x <- round(rnorm(25)); y <- round(rnorm(25))
  s1 <- .Random.seed
  r1 <- rank_p(x, y, "spearman")
  expect_identical(r1$how, "estimate")
  expect_identical(.Random.seed, s1)
  r2 <- rank_p(x, y, "spearman")
  expect_identical(r1$p, r2$p)
  expect_gte(r1$p, 1 / (COR_PERM_B + 1))
  # and it agrees with the large-sample p where that is reliable
  set.seed(7); x <- round(rnorm(40), 1); y <- round(x + rnorm(40), 1)
  expect_equal(rank_p(x, y, "spearman")$p, suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))$p.value,
               tolerance = 0.25)
})

test_that("a p-value below a threshold never prints at or above it", {
  expect_identical(p_show(0.049973, 0.05), "0.04997")
  expect_identical(p_show(0.0099951, 0.05), "0.009995")
  expect_identical(p_show(0.0099995, 0.05), "0.0099995")
  expect_identical(p_show(0.0234, 0.05), "0.0234")
  expect_identical(p_show(0.05, 0.05), "0.05")
  expect_identical(p_show(NA, 0.05), "-")
  for (p in c(0.0499999, 0.009999, 0.0019996)) for (a in c(0.05, 0.01))
    for (cut in c(a, a / 5)) if (p < cut) expect_lt(as.numeric(p_show(p, a)), cut)
})

test_that("Pearson's interval and test are reconciled where they disagree", {
  # four rows with r = 0.955: p = 0.045 but the 95% interval reaches below 0
  x <- c(1, 2, 3, 4); y <- c(1.1, 1.9, 3.4, 3.6)
  ct <- cor.test(x, y)
  tab <- correlate_data(data.frame(x, y), c("x", "y"))
  if ((ct$p.value < 0.05) != (ct$conf.int[1] > 0 || ct$conf.int[2] < 0))
    expect_match(tab$Reading, "the interval, an approximation, and the test disagree; the mark follows the test", fixed = TRUE)
  expect_match(cor_pairs_html(tab), "The interval is Fisher's approximation", fixed = TRUE)
  # an interval needs four rows
  t3 <- correlate_data(data.frame(x = c(1, 2, 3), y = c(2, 1, 4)), c("x", "y"))
  expect_match(cor_pairs_html(t3), "An interval needs at least four rows.", fixed = TRUE)
})

test_that("columns waiting for a correction are not said to have no values", {
  d <- data.frame(Yield = c("36,2", "38,1", "35,4", "40,0", "37,7", "39,3", "36,8", "41,2"),
                  Height = c("87,5", "90,1", "85,2", "95,0", "89,9", "93,3", "88,0", "96,4"),
                  stringsAsFactors = FALSE)
  tab <- correlate_data(d, c("Yield", "Height"))
  expect_identical(tab$Held, 8L)
  expect_no_match(tab$Reading, "no row has both values.", fixed = TRUE)
  expect_match(tab$Reading, "8 rows have an entry the data check has flagged (such as '36,2'), which will count once corrected on the Data tab", fixed = TRUE)
  # one entry held: the pair is computed, and the reading says what was left out
  d2 <- data.frame(Yield = c(36.2, 38.1, 35.4, 40.0, 37.7, 39.3, 36.8, 41.2),
                   Height = c("87.5", "90.1", "85,2", "95.0", "89.9", "93.3", "88.0", "96.4"), stringsAsFactors = FALSE)
  t2 <- correlate_data(d2, c("Yield", "Height"))
  expect_identical(t2$N, 7L)
  expect_match(t2$Reading, "1 row has an entry the data check has flagged (such as '85,2') and is left out until it is corrected on the Data tab.", fixed = TRUE)
  expect_match(cor_text(t2), "1 row has an entry the data check has flagged (such as '85,2') and is left out of its pairs", fixed = TRUE)
})

test_that("the readings stay true at the edges", {
  # N range over every pair, the unavailable ones too
  d <- data.frame(a = c(1, 2, NA, NA, NA, NA, NA, 3), b = c(NA, NA, 1, 2, 3, 4, 5, 6), c = c(2, 4, 3, 5, 6, 8, 7, 9))
  tab <- correlate_data(d, c("a", "b", "c"))
  expect_match(cor_text(tab), sprintf("so N runs from %d to %d", min(tab$N), max(tab$N)), fixed = TRUE)
  # one computable pair, not significant, named
  set.seed(9)
  one <- correlate_data(data.frame(a = rnorm(6), b = rnorm(6)), c("a", "b"))
  if (one$p >= 0.05) expect_match(cor_text(one), "For a and b (", fixed = TRUE)
  # the chance sentence appears only when a pair is significant
  set.seed(3)
  noise <- as.data.frame(matrix(rnorm(40), 4))
  tn <- correlate_data(noise, names(noise), "kendall")
  if (!any(tn$p < 0.05, na.rm = TRUE)) expect_no_match(cor_text(tn), "by chance", fixed = TRUE)
  # a significant coefficient that prints 0.00 still has a direction
  expect_identical(cor_reading(0.0042, 0.001, "negligible", 0.05),
    "Negligible positive association, though significant at the 5% level: with many rows even a very small coefficient can be significant.")
  # the strength bands do not overlap
  expect_identical(cor_bands("spearman"), "under 0.10 negligible, 0.10 to 0.28 weak, 0.29 to 0.47 moderate, 0.48 or more strong")
  expect_identical(cor_bands("pearson"), "under 0.10 negligible, 0.10 to 0.29 weak, 0.30 to 0.49 moderate, 0.50 or more strong")
  # the key explains an unmarked coefficient and the dash
  expect_match(cor_key(0.05), "no mark: not significant at the 5% level; -: cannot be worked out", fixed = TRUE)
  # a variable that does not vary has no 1 on the diagonal
  f <- data.frame(Year = rep(2024, 6), y = c(1, 3, 2, 5, 4, 6), z = c(2, 1, 4, 3, 6, 5))
  tf <- correlate_data(f, c("Year", "y", "z"))
  expect_match(cor_matrix_html(tf, c("Year", "y", "z")), "<tr><td>Year</td><td>-</td>", fixed = TRUE)
  sq <- cor_square(tf, c("Year", "y", "z"))
  expect_identical(sq$label[sq$diag & sq$Row == "Year"], "-")
  # the scatter note says nothing about a coefficient that cannot be worked out
  st <- cor_scatter_text(f, "Year", "y", "pearson", 0.05)
  expect_no_match(st$reading, "can mislead", fixed = TRUE)
  expect_no_match(st$reading, "a single point can change", fixed = TRUE)
  expect_match(plot_cor_scatter(f, "Year", "y", "pearson", 0.05)$labels$subtitle, "N = 6; Pearson's r cannot be worked out", fixed = TRUE)
  # Kendall's description allows for ties and for negative coefficients
  expect_match(cor_about("kendall"), "pairs tied on either variable count for neither", fixed = TRUE)
  expect_match(cor_about("kendall"), "it runs closer to 0 than r or rho", fixed = TRUE)
  # a printed 0.00 that is not significant is not called negative
  z <- correlate_data(data.frame(a = 1:20, b = c(5, 3, 8, 1, 9, 2, 7, 4, 6, 10, 5, 3, 8, 1, 9, 2, 7, 4, 6, 10)), c("a", "b"))
  if (round(z$Coefficient, 2) == 0 && z$Mark == "NS")
    expect_no_match(cor_heat_text(z)$reading, "largest negative coefficient", fixed = TRUE)
})

test_that("the heat map stays readable with many variables", {
  set.seed(5)
  big <- as.data.frame(matrix(rnorm(30 * 20), 30)); names(big) <- paste0("Trait", 1:20)
  tab <- correlate_data(big, names(big))
  p <- plot_cor_heat(tab, names(big))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  expect_match(cor_heat_text(tab, 20)$what, "too small to label", fixed = TRUE)
  small <- plot_cor_heat(correlate_data(big, names(big)[1:6]), names(big)[1:6])
  expect_true(any(vapply(small$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("the scatter defaults follow the table and the axes always differ", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- data.frame(Treatment = rep(c("A", "B"), each = 6), Rep = rep(1:6, 2),
                    Yield = c(30, 34, 39, 41, 29, 35, 38, 43, 31, 33, 40, 42),
                    Height = c(80, 84, 90, 95, 79, 86, 91, 96, 81, 85, 89, 97),
                    Weight = c(5, 7, 6, 8, 5, 6, 9, 7, 6, 8, 7, 9))
    rv$data <- d
    session$flushReact()
    pair <- output$corPairUI$html
    x <- regmatches(pair, regexpr('(?s)id="corX".*?</select>', pair, perl = TRUE))
    expect_match(x, '<option value="Yield" selected>', fixed = TRUE)          # not Rep
    expect_no_match(x, '<option value="Rep" selected>', fixed = TRUE)
    session$setInputs(corVars = c("Yield", "Height", "Weight"), corX = "Height", corY = "Weight")
    # new data without Weight: X stays, Y moves to a different column
    rv$data <- d[, c("Treatment", "Yield", "Height")]
    session$flushReact()
    pair <- output$corPairUI$html
    y <- regmatches(pair, regexpr('(?s)id="corY".*?</select>', pair, perl = TRUE))
    expect_no_match(y, '<option value="Height" selected>', fixed = TRUE)
    # one numeric column: the main panel says so rather than ask for a choice
    rv$data <- data.frame(Treatment = c("A", "B", "C"), Yield = c(1, 2, 3))
    session$flushReact()
    expect_error(output$corMatrix, "Correlation needs at least two columns of numbers.")
  })
})

## ------------------------------------------ found in second review ----

test_that("a label column is never chosen for the user", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("CRD")                       # Treatment, Rep, Yield
    session$flushReact()
    v <- regmatches(output$corUI$html, regexpr('(?s)id="corVars".*?</select>', output$corUI$html, perl = TRUE))
    expect_no_match(v, '<option value="Rep" selected>', fixed = TRUE)
    pair <- output$corPairUI$html
    expect_no_match(pair, '<option value="Rep" selected>', fixed = TRUE)
    session$setInputs(corVars = "Yield")
    expect_error(output$corMatrix, "Only 'Yield' is chosen; choose a second variable to correlate.")
  })
})

test_that("the heat map's diagonal is not the grey of pairs that cannot be worked out", {
  d <- data.frame(a = c(1, 3, 2, 5, 4), b = c(2, 1, 4, 3, 5), c = rep(2, 5))
  tab <- correlate_data(d, c("a", "b", "c"))
  b <- ggplot2::ggplot_build(plot_cor_heat(tab, c("a", "b", "c")))$data
  expect_true(all(b[[2]]$fill == "white"))           # the diagonal of a and b
  expect_identical(nrow(b[[2]]), 2L)                 # c does not vary: its diagonal stays grey
  expect_match(plot_cor_heat(tab, c("a", "b", "c"))$labels$caption, "White: each variable with itself. Grey: cannot be worked out.", fixed = TRUE)
})

test_that("the estimate reuses work without changing the answer", {
  P <- random_orders(12, COR_PERM_B)
  expect_true(all(apply(P, 1, function(r) identical(sort(r), 1:12))))
  # a variable's terms kept for the next pair give the same p as working them out afresh
  set.seed(6); x <- rpois(20, 1); y <- rpois(20, 1); z <- rpois(20, 2)
  ca <- new.env()
  for (m in c("spearman", "kendall")) {
    expect_identical(perm_estimate(x, y, m, COR_PERM_B, ca, "x"), perm_estimate(x, y, m, COR_PERM_B))
    expect_identical(perm_estimate(x, z, m, COR_PERM_B, ca, "x"), perm_estimate(x, z, m, COR_PERM_B))
  }
  # the estimate is close to the exact p where both can be had
  set.seed(3); x <- rpois(9, 1); y <- rpois(9, 1)
  for (m in c("spearman", "kendall")) {
    ex <- rank_p(x, y, m)$p
    expect_lt(abs(perm_estimate(x, y, m, COR_PERM_B) - ex), 4 * sqrt(ex * (1 - ex) / COR_PERM_B) + 1 / COR_PERM_B)
  }
  # many rows are worked through in blocks; with many rows the approximation is good
  set.seed(9); x <- rpois(300, 3); y <- round(x / 3 + rpois(300, 3))
  expect_identical(rank_p(x, y, "spearman")$how, "estimate")
  expect_equal(rank_p(x, y, "spearman")$p, suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))$p.value,
               tolerance = 0.15)
})

test_that("an estimate close to a threshold is worked out again more closely", {
  set.seed(42); x <- round(rnorm(25)); y <- round(x / 3 + rnorm(25))
  p20 <- perm_estimate(x, y, "spearman", COR_PERM_B)
  expect_identical(rank_p(x, y, "spearman", alpha = 0.5)$p, if (abs(p20 - 0.5) < 3 * sqrt(p20 * (1 - p20) / COR_PERM_B) ||
                     abs(p20 - 0.1) < 3 * sqrt(p20 * (1 - p20) / COR_PERM_B)) perm_estimate(x, y, "spearman", COR_PERM_B_CLOSE) else p20)
  # a threshold at the estimate itself
  expect_identical(rank_p(x, y, "spearman", alpha = p20)$p, perm_estimate(x, y, "spearman", COR_PERM_B_CLOSE))
  expect_identical(rank_p(x, y, "spearman", alpha = 5 * p20)$p, perm_estimate(x, y, "spearman", COR_PERM_B_CLOSE))
  # the closer count includes the first 20,000 and stays close to them
  expect_lt(abs(perm_estimate(x, y, "spearman", COR_PERM_B_CLOSE) - p20), 4 * sqrt(p20 * (1 - p20) / COR_PERM_B))
  h <- cor_pairs_html(correlate_data(data.frame(x, y), c("x", "y"), "spearman"))
  expect_match(h, "(from 200,000 when it falls close to a significance threshold); it can fall either side of the exact value",
               fixed = TRUE)
})

test_that("Kendall's tau is estimated up to 20 rows and approximated beyond", {
  set.seed(8); x <- round(rnorm(21)); y <- round(rnorm(21))
  expect_identical(rank_p(x, y, "kendall")$how, "approximation")
  expect_identical(rank_p(x[-1], y[-1], "kendall")$how, "estimate")
  expect_identical(rank_p(x, y, "spearman")$how, "estimate")
})

test_that("only rows a pair would gain are counted as waiting for a correction", {
  d <- data.frame(A = c("5,6", "2", "3.1", "4", "2.5", "3.3"), B = c(NA, 1, 2, 3.5, 2, 4),
                  C = c(NA, 2, 1, 3, 4, 2.2), stringsAsFactors = FALSE)
  tab <- correlate_data(d, c("A", "B", "C"))
  expect_equal(attr(tab, "held_rows"), 0)
  expect_no_match(cor_text(tab), "flagged", fixed = TRUE)
  d$B[1] <- 1.5
  tab <- correlate_data(d, c("A", "B", "C"))
  expect_equal(attr(tab, "held_rows"), 1)
  expect_match(cor_text(tab), "1 row has an entry the data check has flagged (such as '5,6') and is left out of its pairs",
               fixed = TRUE)
})

test_that("the count expected by chance is the exact product", {
  set.seed(1); d <- as.data.frame(matrix(rnorm(20 * 7), 20)); d$V2 <- d$V1 + rnorm(20, 0, 0.1)
  tab <- correlate_data(d, names(d))
  expect_match(cor_text(tab), "would make, on average, up to 1.05 pairs significant by chance (21 x 0.05)", fixed = TRUE)
  tab <- correlate_data(d[1:5], names(d)[1:5], alpha = 0.01)
  expect_match(cor_text(tab), "would make, on average, up to 0.1 pairs significant by chance (10 x 0.01)", fixed = TRUE)
})
