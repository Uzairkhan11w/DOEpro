## Factor analysis: the solution against factanal(), the Kaiser-Meyer-Olkin
## measure and Bartlett's test against their definitions, parallel analysis
## as on the principal components tab, the readings against the tables as
## printed, the plots, and the Explore tab.

fa_df <- function(seed = 1, n = 80) {
  set.seed(seed)
  f1 <- rnorm(n); f2 <- rnorm(n)
  data.frame(Height = f1 + rnorm(n, 0, 0.5), Tillers = f1 + rnorm(n, 0, 0.5), Yield = f1 + 0.4 * f2 + rnorm(n, 0, 0.6),
             Protein = f2 + rnorm(n, 0, 0.5), Oil = f2 + rnorm(n, 0, 0.5), Moisture = f2 + rnorm(n, 0, 0.6))
}
FA_V <- c("Height", "Tillers", "Yield", "Protein", "Oil", "Moisture")

## match factanal()'s columns to ours by order and sign
fa_align <- function(ours, theirs) {
  theirs <- unclass(theirs)[, seq_len(ncol(ours)), drop = FALSE]
  o <- vapply(seq_len(ncol(ours)), function(k) which.max(abs(colSums(ours[, k] * theirs))), 1L)
  s <- sign(colSums(ours * theirs[, o, drop = FALSE]))
  sweep(theirs[, o, drop = FALSE], 2, s, "*")
}

## ------------------------------------------------- the numbers ----

test_that("the solution matches factanal()", {
  d <- fa_df()
  for (rot in c("varimax", "promax", "none")) {
    fa <- fa_data(d, FA_V, rotation = rot)
    ff <- factanal(d[, FA_V], fa$factors, rotation = rot, scores = "regression")
    expect_equal(unname(fa$uniqueness), unname(ff$uniquenesses))
    expect_equal(unname(fa$communality), unname(1 - ff$uniquenesses))
    expect_equal(unname(fa$loadings), unname(fa_align(fa$loadings, ff$loadings)), tolerance = 1e-6)
    expect_equal(fa$variance$SS, unname(colSums(fa$loadings^2)))
    expect_equal(fa$variance$Proportion, fa$variance$SS / 6)
    # each factor's largest loading is positive, and the factors run from the largest variance down
    for (k in seq_len(fa$factors)) expect_gt(fa$loadings[which.max(abs(fa$loadings[, k])), k], 0)
    expect_identical(order(-fa$variance$SS), seq_len(fa$factors))
    expect_equal(fa$fit$p, unname(ff$PVAL)); expect_equal(fa$fit$statistic, unname(ff$STATISTIC))
  }
  # the factor correlations after promax are those factanal prints, in our order and signs
  fp <- fa_data(d, FA_V, rotation = "promax")
  ff <- factanal(d[, FA_V], 2, rotation = "promax")
  ti <- solve(ff$rotmat); ph <- ti %*% t(ti)
  expect_equal(abs(fp$phi[1, 2]), abs(ph[1, 2]), tolerance = 1e-6)
  expect_equal(unname(diag(fp$phi)), c(1, 1))
  fv <- fa_data(d, FA_V)
  expect_equal(unname(fv$phi), diag(2), tolerance = 1e-8)
})

test_that("the Kaiser-Meyer-Olkin measure and Bartlett's test follow their definitions", {
  d <- fa_df(); fa <- fa_data(d, FA_V)
  R <- cor(d[, FA_V]); Ri <- solve(R)
  P <- -Ri / sqrt(diag(Ri) %o% diag(Ri))
  off <- row(R) != col(R)
  expect_equal(fa$kmo, sum(R[off]^2) / (sum(R[off]^2) + sum(P[off]^2)))
  for (j in 1:6) {
    o <- setdiff(1:6, j)
    expect_equal(unname(fa$msa[j]), sum(R[j, o]^2) / (sum(R[j, o]^2) + sum(P[j, o]^2)))
  }
  chi <- -(80 - 1 - (2 * 6 + 5) / 6) * log(det(R))
  expect_equal(fa$bartlett$statistic, chi); expect_equal(fa$bartlett$df, 15)
  expect_equal(fa$bartlett$p, pchisq(chi, 15, lower.tail = FALSE))
})

test_that("the number of factors comes from parallel analysis, within what the data allow", {
  d <- fa_df(); fa <- fa_data(d, FA_V)
  pp <- pca_parallel(80, 6)
  expect_equal(fa$eigen$PA_threshold, pp$threshold)
  expect_equal(fa$eigen$Eigenvalue, eigen(cor(d[, FA_V]), only.values = TRUE)$values)
  above <- fa$eigen$Eigenvalue > fa$eigen$PA_threshold
  expect_identical(fa$pa_keep, if (all(above)) 6L else which(!above)[1] - 1L)
  expect_identical(fa$suggested, max(1L, min(fa$pa_keep, fa$mmax)))
  expect_identical(fa$factors, fa$suggested)
  expect_identical(fa_data(d, FA_V[1:3])$mmax, 1L)
  expect_identical(fa_data(d, FA_V[1:5])$mmax, 2L)
  expect_identical(fa$mmax, 3L)
  expect_identical(fa_data(d, FA_V, factors = 1)$factors, 1L)
})

test_that("what cannot be factored is refused in plain words", {
  d <- fa_df()
  expect_error(fa_data(d, FA_V[1:2]), "Factor analysis needs at least three variables.", fixed = TRUE)
  expect_error(fa_data(d, c(FA_V[1:2], "Weight")), "'Weight' is not a column of the data.", fixed = TRUE)
  expect_error(fa_data(d[1:5, ], FA_V), "Only 5 rows have a value of every variable chosen; factor analysis of 6 variables needs at least 8.",
               fixed = TRUE)
  expect_error(fa_data(d, FA_V, factors = 4), "With 6 variables, maximum likelihood can fit from 1 to 3 factors.", fixed = TRUE)
  d$Const <- 5
  expect_error(fa_data(d, c(FA_V[1:3], "Const")), "'Const' has the same value in every row used", fixed = TRUE)
  d$Total <- d$Height + d$Tillers
  expect_error(fa_data(d, c("Height", "Tillers", "Total", "Yield")), "can be worked out exactly (or almost exactly) from the others",
               fixed = TRUE)
  expect_error(fa_data(d, FA_V, alpha = 0.6), "between 0 and 0.5", fixed = TRUE)
})

test_that("rows with flagged entries are counted, not called missing", {
  d <- fa_df(); d$Oil <- as.character(round(d$Oil, 2)); d$Oil[4] <- "0,51"; d$Yield[9] <- NA
  fa <- fa_data(d, FA_V)
  expect_identical(fa$n, 78L)
  t <- fa_text(fa)
  expect_match(t, "2 of the 80 rows are left out because they have no usable value of some variable.", fixed = TRUE)
  expect_match(t, "Of these, 1 has an entry the data check has flagged (such as '0,51')", fixed = TRUE)
})

## ------------------------------------------------- the readings ----

test_that("Kaiser's words follow the printed measure", {
  expect_identical(fa_kmo_word(0.8951), "marvellous")
  expect_identical(fa_kmo_word(0.8949), "meritorious")
  expect_identical(fa_kmo_word(0.7951), "meritorious")
  expect_identical(fa_kmo_word(0.6049), "mediocre")
  expect_identical(fa_kmo_word(0.4951), "miserable")
  expect_identical(fa_kmo_word(0.4949), "unacceptable")
})

test_that("the reading follows the result", {
  d <- fa_df(); fa <- fa_data(d, FA_V)
  t <- fa_text(fa)
  expect_match(t, sprintf("The Kaiser-Meyer-Olkin measure is %s, which Kaiser called %s", fmt(fa$kmo, 2), fa_kmo_word(fa$kmo)), fixed = TRUE)
  expect_match(t, "Bartlett's test finds the correlations, taken together, larger than chance would give", fixed = TRUE)
  expect_match(t, sprintf("The test that 2 factors are enough finds insufficient evidence at the 5%% level that more are needed (chi-square = %s",
                          fmt(fa$fit$statistic, 2)), fixed = TRUE)
  expect_match(t, "Varimax keeps the factors uncorrelated.", fixed = TRUE)
  expect_match(t, "the analysis does not show what they are or what causes them", fixed = TRUE)
  expect_no_match(t, "represents|measures the|is the factor of", perl = TRUE)
  for (k in 1:2) {
    pr <- as.numeric(dfmt(fa$loadings[, k], 2))
    for (v in FA_V[abs(pr) >= 0.4]) expect_match(fa_factor_text(fa, k), v, fixed = TRUE)
  }
  # uncorrelated variables: Bartlett's test finds too little, and no factor stands out
  set.seed(3); u <- as.data.frame(matrix(rnorm(60 * 5), 60))
  fu <- fa_data(u, names(u))
  if (fu$bartlett$p >= 0.05) expect_match(fa_text(fu), "insufficient evidence at the 5% level that the variables are correlated at all", fixed = TRUE)
  if (fu$pa_keep == 0) expect_match(fa_text(fu), "1 factor is fitted, the fewest possible.", fixed = TRUE)
  # one factor for three variables: no test, and 'the factor accounts'
  f3 <- fa_data(d, FA_V[1:3])
  expect_match(fa_text(f3), "no degrees of freedom are left, so there is no test of whether that is enough", fixed = TRUE)
  expect_match(fa_text(f3), "The factor accounts for", fixed = TRUE)
})

test_that("salience, cross-loadings and low communalities are judged as printed", {
  fa <- fa_data(fa_df(), FA_V)
  q <- fa; q$loadings[, 1] <- c(0.3951, 0.3949, 0.7, 0.1, 0.45, -0.6); q$loadings[, 2] <- c(0.1, 0.1, 0.5, 0.8, 0.1, 0.1)
  t <- fa_factor_text(q, 1)
  expect_match(t, "Height", fixed = TRUE); expect_no_match(t, "Tillers", fixed = TRUE)
  expect_match(t, "and negative ones for Moisture (-0.60)", fixed = TRUE)
  tt <- fa_text(q)
  expect_match(tt, "Yield is salient on more than one factor", fixed = TRUE)
  expect_match(tt, "Tillers is salient on no factor.", fixed = TRUE)
  q$communality[1] <- 0.3951; q$communality[2] <- 0.3949
  t2 <- fa_text(q)
  expect_match(t2, "Tillers has a communality below 0.40", fixed = TRUE)
})

test_that("a Heywood case is named", {
  # a variable that is almost a copy of the factor: its uniqueness stops at factanal's lower limit
  set.seed(1); f <- rnorm(60)
  d <- data.frame(a = f + rnorm(60, 0, 0.005), b = f + rnorm(60, 0, 0.5), c = f + rnorm(60, 0, 0.6), e = rnorm(60))
  fa <- suppressWarnings(fa_data(d, c("a", "b", "c", "e")))
  expect_identical(fa$heywood, "a")
  expect_match(fa_text(fa), "The uniqueness of a stopped at its lower limit (0.005), a Heywood case", fixed = TRUE)
  expect_true(all(fa$uniqueness >= 0.005 - 1e-9))
})

## ------------------------------------------------- the tables and plots ----

test_that("the tables and plots show what their notes say", {
  d <- fa_df(); fa <- fa_data(d, FA_V); fp <- fa_data(d, FA_V, rotation = "promax")
  expect_match(fa_adequacy_html(fa), "Bartlett&#39;s test of sphericity|Bartlett's test of sphericity", perl = TRUE)
  expect_match(fa_loadings_html(fa), "Communality", fixed = TRUE)
  expect_match(fa_loadings_html(fp), "Promax pattern loadings", fixed = TRUE)
  expect_match(fa_variance_html(fa), "Cumulative %", fixed = TRUE)
  expect_no_match(fa_variance_html(fp), "Cumulative %", fixed = TRUE)
  expect_match(fa_phi_html(fp), "Factor correlations", fixed = TRUE)
  expect_identical(fa_phi_html(fa), "")
  b <- ggplot2::ggplot_build(plot_fa_heat(fa))$data[[1]]
  expect_equal(sort(b$fill != ""), rep(TRUE, 12))
  sc <- ggplot2::ggplot_build(plot_fa_scree(fa))$data
  expect_equal(sc[[5]]$y, fa$eigen$Eigenvalue)
  lv <- plot_fa_loadings(fa); lp <- plot_fa_loadings(fp)
  expect_true(any(vapply(lv$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))))
  expect_false(any(vapply(lp$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))))
  expect_match(fa_scores_html(fa), "The first 10 of 80 rows", fixed = TRUE)
  out <- capture.output(print(fa)); expect_match(out[1], "KMO", fixed = TRUE); expect_no_match(out[1], "= <", fixed = TRUE)
})

## ------------------------------------------------- the app ----

test_that("the factor analysis tab analyses, reads and draws", {
  skip_on_cran()
  d <- fa_df(); d$Rep <- rep(1:4, 20)
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$flushReact()
    v <- regmatches(output$faUI$html, regexpr('(?s)id="faVars".*?</select>', output$faUI$html, perl = TRUE))
    expect_match(v, '<option value="Height" selected>', fixed = TRUE)
    expect_no_match(v, '<option value="Rep" selected>', fixed = TRUE)
    session$setInputs(faVars = FA_V, faRot = "promax", faAlpha = "0.01")
    expect_match(output$faKUI$html, "Suggested (2)", fixed = TRUE)
    expect_match(output$faText$html, "at the 1% level", fixed = TRUE)
    expect_match(output$faAdequacy$html, "Kaiser", fixed = TRUE)
    expect_match(output$faPhi$html, "Factor correlations", fixed = TRUE)
    expect_match(output$faScree$src, "^data:image/png")
    expect_match(output$faHeat$src, "^data:image/png")
    expect_match(output$faLoadPlot$src, "^data:image/png")
    session$setInputs(faK = "1")
    expect_identical(fa_res()$factors, 1L)
    expect_error(output$faLoadPlot, "With one factor there is no second axis")
    session$setInputs(faVars = c("Height", "Yield"))
    expect_error(output$faText, "Only 'Height' and 'Yield' are chosen; factor analysis needs at least three variables.")
  })
})
