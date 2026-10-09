## Principal components: the eigenvalues, loadings and scores against
## prcomp(), parallel analysis against its definition, the readings against
## the numbers on screen, the plots against what their notes say, and the
## Explore tab.
##
## Expectations come from base R and from the definitions:
##   * the rows used are those with a value of every variable chosen;
##   * loadings are the correlations between the variables and the scores;
##   * each component's largest loading is positive;
##   * parallel analysis keeps components, from the first, whose eigenvalue
##     exceeds the 95th percentile from random normal data of the same size;
##   * a loading of 0.50 or more either way, as printed, is strong.

pca_df <- function(seed = 1, n = 24) {
  set.seed(seed)
  d <- data.frame(Variety = rep(paste0("V", 1:4), length.out = n), Height = rnorm(n, 100, 10))
  d$Tillers <- 0.1 * d$Height + rnorm(n)
  d$Yield <- 0.3 * d$Height + 0.5 * d$Tillers + rnorm(n, 0, 3)
  d$Protein <- rnorm(n, 12, 1) - 0.05 * d$Yield
  d$Moisture <- rnorm(n, 14, 1)
  d
}
PCA_V <- c("Height", "Tillers", "Yield", "Protein", "Moisture")

## ------------------------------------------------- the numbers ----

test_that("the eigenvalues, loadings and scores match prcomp()", {
  d <- pca_df()
  p <- pca_data(d, PCA_V)
  pc <- prcomp(d[, PCA_V], scale. = TRUE)
  expect_equal(p$eigen$Eigenvalue, pc$sdev^2)
  expect_equal(sum(p$eigen$Eigenvalue), 5)
  expect_equal(p$eigen$Proportion, pc$sdev^2 / 5)
  expect_equal(p$eigen$Cumulative, cumsum(pc$sdev^2) / 5)
  # loadings are correlations with the scores, and each component's largest is positive
  expect_equal(unname(p$loadings), unname(cor(d[, PCA_V], p$scores)))
  for (k in 1:5) expect_gt(p$loadings[which.max(abs(p$loadings[, k])), k], 0)
  # the scores are prcomp's, up to each component's sign
  for (k in 1:5) expect_equal(abs(unname(p$scores[, k])), abs(unname(pc$x[, k])))
  expect_equal(unname(p$vectors^2), unname(pc$rotation^2))
  # covariances: the variables as measured
  q <- pca_data(d, PCA_V, scale = FALSE)
  pv <- prcomp(d[, PCA_V])
  expect_equal(q$eigen$Eigenvalue, pv$sdev^2)
  expect_equal(unname(q$loadings), unname(cor(d[, PCA_V], q$scores)))
  expect_equal(q$average, mean(pv$sdev^2))
})

test_that("parallel analysis keeps what beats random data, and leaves the user's stream alone", {
  d <- pca_df()
  set.seed(42); s1 <- .Random.seed
  p <- pca_data(d, PCA_V)
  expect_identical(.Random.seed, s1)
  expect_identical(pca_data(d, PCA_V)$eigen$PA_threshold, p$eigen$PA_threshold)
  # the thresholds are the 95th percentiles of random correlation eigenvalues
  sims <- with_fixed_seed(COR_PERM_SEED + 11, vapply(seq_len(p$pa_sets), function(b)
    eigen(cor(matrix(rnorm(24 * 5), 24)), symmetric = TRUE, only.values = TRUE)$values, numeric(5)))
  expect_equal(p$eigen$PA_threshold, apply(sims, 1, quantile, probs = 0.95, names = FALSE))
  above <- p$eigen$Eigenvalue > p$eigen$PA_threshold
  expect_identical(p$retained, if (all(above)) 5L else which(!above)[1] - 1L)
  expect_identical(p$kaiser, sum(p$eigen$Eigenvalue > 1))
  # strongly related variables give a first component that beats chance
  set.seed(3); z <- rnorm(40); X <- data.frame(a = z + rnorm(40, 0, 0.3), b = z + rnorm(40, 0, 0.3), c = z + rnorm(40, 0, 0.3))
  expect_gte(pca_data(X, c("a", "b", "c"))$retained, 1L)
})

test_that("each row used has every value, and what cannot be analysed is refused in plain words", {
  d <- pca_df()
  d$Yield[3] <- NA
  d$Protein <- as.character(round(d$Protein, 1)); d$Protein[5] <- "11,9"
  p <- pca_data(d, PCA_V)
  expect_identical(p$n, 22L)
  expect_identical(p$rows, setdiff(1:24, c(3, 5)))
  t <- pca_text(p)
  expect_match(t, "2 of the 24 rows are left out because they have no usable value of some variable.", fixed = TRUE)
  expect_match(t, "Of these, 1 has an entry the data check has flagged (such as '11,9')", fixed = TRUE)
  expect_error(pca_data(d, "Height"), "Choose at least two variables", fixed = TRUE)
  expect_error(pca_data(d, c("Height", "Weight")), "'Weight' is not a column of the data.", fixed = TRUE)
  expect_error(pca_data(d[1:2, ], PCA_V), "Only 2 rows have a value of every variable chosen; principal components need at least 3.",
               fixed = TRUE)
  d$Const <- 5
  expect_error(pca_data(d, c("Height", "Const")), "'Const' has the same value in every row used", fixed = TRUE)
})

test_that("components with no variance are left out and the table says why", {
  d <- pca_df(n = 4)
  p <- pca_data(d, PCA_V)
  expect_identical(p$m, 3L)
  expect_match(pca_eigen_html(p), "2 more components have an eigenvalue of 0 and are left out: there are no more rows than variables.",
               fixed = TRUE)
  d <- pca_df(); d$Total <- d$Height + d$Tillers
  q <- pca_data(d, c("Height", "Tillers", "Total", "Yield"))
  expect_identical(q$m, 3L)
  expect_match(pca_eigen_html(q), "some variable can be worked out exactly from the others", fixed = TRUE)
})

## ------------------------------------------------- the readings ----

test_that("each component is read from its strong loadings as printed", {
  p <- pca_data(pca_df(), PCA_V)
  L <- p$loadings
  t1 <- pca_component_text(p, 1)
  strong <- which(abs(as.numeric(dfmt(L[, 1], 2))) >= 0.5)
  for (v in PCA_V[strong]) expect_match(t1, v, fixed = TRUE)
  expect_match(t1, sprintf("PC1 (%s%% of the variation)", fmt(100 * p$eigen$Proportion[1], 1)), fixed = TRUE)
  if (any(L[strong, 1] < 0)) expect_match(t1, "it contrasts", fixed = TRUE)
  # a loading that prints as 0.50 counts; one that prints as 0.49 does not
  q <- p; q$loadings[, 2] <- c(0.4951, 0.4949, 0.1, -0.2, 0.3)
  t2 <- pca_component_text(q, 2)
  expect_match(t2, "high Height (loading 0.50)", fixed = TRUE)
  expect_no_match(t2, "Tillers", fixed = TRUE)
  q$loadings[, 2] <- c(0.3, -0.2, 0.1, 0.4, -0.45)
  expect_match(pca_component_text(q, 2), "no variable is strongly related to it (every loading lies between -0.50 and 0.50)", fixed = TRUE)
})

test_that("the reading names the rules, their disagreement and the weak variables", {
  p <- pca_data(pca_df(), PCA_V)
  t <- pca_text(p)
  expect_match(t, sprintf("The first component carries %s%% of the variation, and the first two together %s%%.",
                          fmt(100 * p$eigen$Proportion[1], 1), fmt(100 * p$eigen$Cumulative[2], 1)), fixed = TRUE)
  expect_match(t, sprintf("Kaiser's rule keeps %d", p$kaiser), fixed = TRUE)
  if (p$kaiser != p$retained) expect_match(t, "the two rules disagree", fixed = TRUE)
  weak <- which(rowSums(p$loadings[, 1:2]^2) < 0.5)
  for (v in PCA_V[weak]) expect_match(t, sprintf("%s ", v), fixed = TRUE)
  expect_match(t, "they do not show what causes it", fixed = TRUE)
  expect_no_match(t, "measures|represents|means that", perl = TRUE)
  # covariances: a large variance makes its own component
  q <- pca_data(pca_df(), PCA_V, scale = FALSE)
  expect_match(pca_text(q), "a variable with a much larger variance than the others makes a component almost on its own", fixed = TRUE)
  expect_match(pca_text(q), "The rule of an eigenvalue above the average keeps", fixed = TRUE)
})

## ------------------------------------------------- the plots ----

test_that("the plots draw what their notes describe", {
  d <- pca_df(); p <- pca_data(d, PCA_V)
  b <- ggplot2::ggplot_build(plot_pca_scree(p))$data
  expect_equal(b[[5]]$y, p$eigen$Eigenvalue)
  expect_equal(b[[3]]$y, p$eigen$PA_threshold)
  expect_equal(b[[1]]$yintercept, 1)
  s <- ggplot2::ggplot_build(plot_pca_scores(p, c(1, 2)))$data
  expect_equal(s[[3]]$x, unname(p$scores[, 1])); expect_equal(s[[3]]$y, unname(p$scores[, 2]))
  l <- ggplot2::ggplot_build(plot_pca_loadings(p, c(1, 3)))$data
  expect_equal(l[[4]]$xend, unname(p$loadings[, 1])); expect_equal(l[[4]]$yend, unname(p$loadings[, 3]))
  f <- pca_stretch(p, c(1, 2))
  bp <- plot_pca_biplot(p, c(1, 2))
  expect_equal(ggplot2::ggplot_build(bp)$data[[4]]$xend, unname(f * p$loadings[, 1]))
  expect_match(gsub("[[:space:]]+", " ", bp$labels$caption), sprintf("stretched %s times", sig_fmt(f, 3)), fixed = TRUE)
  expect_match(pca_biplot_text(p)$what, sprintf("stretched %s times", sig_fmt(f, 3)), fixed = TRUE)
  expect_identical(pca_axes(p, 2, 2), c(2L, 1L))
  expect_identical(pca_axes(p, 9, 1), c(1L, 2L))
  expect_identical(pca_axes(p, NA_integer_, NA_integer_), c(1L, 2L))
  expect_identical(pca_axes(p, 3L, NA_integer_), c(3L, 1L))
})

test_that("rows far out on the plotted components are named, and groups compared as printed", {
  d <- pca_df(n = 40); d$Height[7] <- d$Height[7] + 80; d$Tillers[7] <- d$Tillers[7] + 8
  p <- pca_data(d, PCA_V)
  far <- pca_far(p, c(1, 2))
  expect_true(7 %in% p$rows[far])
  expect_match(pca_scores_text(p)$reading, "Row 7 lies more than three standard deviations from 0 on PC1 or PC2", fixed = TRUE)
  g <- factor(d$Variety[p$pos])
  r <- pca_scores_text(p, c(1, 2), g, "Variety")$reading
  mm <- tapply(p$scores[, 1], g, mean)
  top <- names(mm)[as.numeric(dfmt(mm, 2)) == max(as.numeric(dfmt(mm, 2)))]
  expect_match(r, sprintf("Among the levels of Variety, on PC1 %s %s the highest average score (%s)", join_and(top),
                          if (length(top) == 1) "has" else "share", dfmt(max(mm), 2)), fixed = TRUE)
})

test_that("printing gives the eigenvalues and loadings", {
  out <- capture.output(print(pca_data(pca_df(), PCA_V)))
  expect_true(any(grepl("Eigenvalue", out))); expect_true(any(grepl("Loadings", out)))
})

## ------------------------------------------------- the app ----

test_that("the principal components tab analyses, reads and draws", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- pca_df(); d$Rep <- rep(1:6, 4)
    rv$data <- d
    session$flushReact()
    v <- regmatches(output$pcaUI$html, regexpr('(?s)id="pcaVars".*?</select>', output$pcaUI$html, perl = TRUE))
    expect_match(v, '<option value="Height" selected>', fixed = TRUE)
    expect_no_match(v, '<option value="Rep" selected>', fixed = TRUE)
    session$setInputs(pcaVars = PCA_V, pcaScale = "cor", pcaGroup = "Variety", pcaX = "1", pcaY = "2")
    expect_match(output$pcaEigen$html, "Eigenvalues", fixed = TRUE)
    expect_match(output$pcaText$html, "Parallel analysis", fixed = TRUE)
    expect_match(output$pcaLoadings$html, "Communality", fixed = TRUE)
    expect_match(output$pcaScree$src, "^data:image/png")
    expect_match(output$pcaScores$src, "^data:image/png")
    expect_match(output$pcaScoresText$html, "Among the levels of Variety", fixed = TRUE)
    expect_match(output$pcaLoadPlot$src, "^data:image/png")
    expect_match(output$pcaBiplot$src, "^data:image/png")
    expect_match(output$pcaScoreTable$html, "Scores", fixed = TRUE)
    session$setInputs(pcaVars = "Height")
    expect_error(output$pcaEigen, "Only 'Height' is chosen; choose at least two variables.")
  })
})

## ------------------------------------------------ found in review ----

test_that("the average eigenvalue is 1 for correlations even with fewer rows than variables", {
  set.seed(1); d <- as.data.frame(matrix(rnorm(4 * 6), 4))
  p <- pca_data(d, names(d))
  expect_equal(p$average, 1)
  expect_identical(p$kaiser, sum(p$eigen$Eigenvalue > 1))
})

test_that("the scree note names a later eigenvalue above its cross, which parallel analysis does not keep", {
  set.seed(19); X <- as.data.frame(matrix(rnorm(12 * 6), 12)); X$V2 <- X$V1 + rnorm(12, 0, 0.6)
  p <- pca_data(X, names(X))
  above <- which(p$eigen$Eigenvalue > p$eigen$PA_threshold)
  skip_if(p$retained > 0 || !length(above))
  r <- pca_scree_text(p)$reading
  expect_match(r, "The first eigenvalue lies below its cross, so parallel analysis keeps no component.", fixed = TRUE)
  expect_match(r, sprintf("%s also lies above its cross, but parallel analysis stops at the first component that falls short.",
                          p$eigen$Component[above[1]]), fixed = TRUE)
  expect_no_match(r, "  ", fixed = TRUE)
})

test_that("a component parallel analysis does not keep is read with a caution", {
  set.seed(2); p <- pca_data(as.data.frame(matrix(rnorm(30 * 4), 30)), paste0("V", 1:4))
  expect_identical(p$retained, 0L)
  t <- pca_text(p)
  if (length(pca_strong(p, 1))) expect_match(t, "Parallel analysis does not keep this component, so the pattern may be no more than chance.", fixed = TRUE)
  q <- pca_data(pca_df(), PCA_V)
  expect_no_match(pca_component_text(q, 1), "no more than chance", fixed = TRUE)
})

test_that("a communality is judged as printed", {
  p <- pca_data(pca_df(), PCA_V)
  q <- p; q$loadings[1, 1:2] <- c(sqrt(0.4952), 0)          # prints as 0.50: not weak
  expect_no_match(pca_text(q), "Height has less than half", fixed = TRUE)
  q$loadings[1, 1:2] <- c(sqrt(0.4948), 0)                  # prints as 0.49: weak
  expect_match(pca_text(q), "Height ", fixed = TRUE)
})

test_that("parallel analysis uses fewer sets for large data, and says how many", {
  set.seed(5); d <- as.data.frame(matrix(rnorm(2000 * 25), 2000))
  p <- pca_data(d, names(d))
  expect_identical(p$pa_sets, 100L)
  expect_match(pca_eigen_html(p), "in 100 sets of random normal data", fixed = TRUE)
})

test_that("the grouping selector draws before any design is mapped, and follows the treatment once it is", {
  skip_on_cran()
  set.seed(1)
  d <- data.frame(Variety = rep(paste0("V", 1:4), 6), Rep = rep(1:6, each = 4), Height = rnorm(24, 100, 10), Yield = rnorm(24, 30, 3))
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$flushReact()
    g <- output$pcaGroupUI$html
    expect_match(g, '<option value="" selected>Nothing</option>', fixed = TRUE)
  })
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$setInputs(design = "CRD", resp = "Yield", treat = "Variety")
    expect_match(output$pcaGroupUI$html, '<option value="Variety" selected>Variety</option>', fixed = TRUE)
  })
})
