## Regression: the estimates, tests and intervals against lm(), summary.lm()
## and confint(), the fit statistics against their definitions, the checks
## on the residuals against hand-built versions, the readings against the
## numbers, the plots against what their notes say, and the Explore tab.
##
## Expectations come from base R and from the definitions:
##   * the rows used are those with a value of every variable chosen;
##   * MSE = SSE / (N - k - 1), RMSE = sqrt(MSE), MAE = mean |residual|;
##   * VIF_j = 1 / (1 - R^2_j), R^2_j from predictor j on the others;
##   * Breusch-Pagan (Koenker) = N R^2 of the squared residuals on the
##     predictors; the curve check adds the square of the predictor (with one)
##     or of the fitted values (RESET, with several); up to 200 rows the
##     p-values of Shapiro-Wilk and Breusch-Pagan come from normal data
##     simulated on the same predictor values;
##   * a result short of significance is "insufficient evidence", the level
##     named is the level chosen, and nothing claims a cause.

reg_data <- function(seed = 1, n = 20) {
  set.seed(seed)
  d <- data.frame(Nitrogen = rep(c(40, 80, 120, 160), length.out = n), Rain = round(runif(n, 10, 50)))
  d$Yield <- round(20 + 0.12 * d$Nitrogen + 0.1 * d$Rain + rnorm(n, 0, 2), 1)
  d$Rep <- rep(1:5, length.out = n)
  d
}

## ------------------------------------------------- the numbers ----

test_that("a simple regression matches lm(), summary() and confint()", {
  d <- reg_data()
  r <- regress_data(d, "Yield", "Nitrogen")
  f <- lm(Yield ~ Nitrogen, d); s <- summary(f); ci <- confint(f)
  cf <- r$coefficients
  expect_identical(cf$Term, c("Intercept", "Nitrogen"))
  expect_equal(cf$Estimate, unname(coef(f)))
  expect_equal(cf$SE, unname(s$coefficients[, 2]))
  expect_equal(cf$t, unname(s$coefficients[, 3]))
  expect_equal(cf$p, unname(s$coefficients[, 4]))
  expect_equal(cf$CI_lower, unname(ci[, 1])); expect_equal(cf$CI_upper, unname(ci[, 2]))
  expect_identical(cf$Mark, star(cf$p, 0.05))
  expect_null(cf$VIF)
  m <- r$model
  expect_equal(m$R2, s$r.squared); expect_equal(m$Adj_R2, s$adj.r.squared)
  expect_equal(m$MSE, s$sigma^2); expect_equal(m$RMSE, s$sigma)
  expect_equal(m$MAE, mean(abs(residuals(f))))
  expect_equal(m$F, unname(s$fstatistic[1])); expect_identical(c(m$df1, m$df2), c(1L, 18L))
  expect_equal(m$p, pf(s$fstatistic[1], 1, 18, lower.tail = FALSE), ignore_attr = TRUE)
  expect_identical(m$N, 20L)
  # the intervals follow the level chosen
  r1 <- regress_data(d, "Yield", "Nitrogen", alpha = 0.01)
  expect_equal(r1$coefficients$CI_lower, unname(confint(f, level = 0.99)[, 1]))
  expect_identical(r1$coefficients$Mark, star(r1$coefficients$p, 0.01))
})

test_that("a multiple regression matches lm(), with each VIF from its predictor on the others", {
  d <- reg_data(2)
  d$Temp <- round(25 + 0.05 * d$Rain + rnorm(20), 1)
  r <- regress_data(d, "Yield", c("Nitrogen", "Rain", "Temp"))
  f <- lm(Yield ~ Nitrogen + Rain + Temp, d); s <- summary(f)
  expect_equal(r$coefficients$Estimate, unname(coef(f)))
  expect_equal(r$coefficients$p, unname(s$coefficients[, 4]))
  expect_equal(r$model$R2, s$r.squared); expect_equal(r$model$Adj_R2, s$adj.r.squared)
  vif <- sapply(c("Nitrogen", "Rain", "Temp"), function(v)
    1 / (1 - summary(lm(reformulate(setdiff(c("Nitrogen", "Rain", "Temp"), v), v), d))$r.squared))
  expect_equal(r$coefficients$VIF, c(NA, unname(vif)))
  expect_identical(c(r$model$df1, r$model$df2), c(3L, 16L))
})

test_that("the analysis of variance adds up and matches anova()", {
  d <- reg_data(3)
  r <- regress_data(d, "Yield", c("Nitrogen", "Rain"))
  a <- anova(lm(Yield ~ Nitrogen + Rain, d))
  expect_equal(r$anova$SS[1], sum(a$`Sum Sq`[1:2]))
  expect_equal(r$anova$SS[2], a$`Sum Sq`[3])
  expect_equal(r$anova$SS[1] + r$anova$SS[2], r$anova$SS[3])
  expect_identical(r$anova$df, c(2L, 17L, 19L))
  expect_equal(r$anova$MS[2], r$model$MSE)
  expect_equal(r$anova$F[1], r$model$F)
  expect_equal(r$anova$SS[1] / r$anova$SS[3], r$model$R2)
})

test_that("each row used has every value, and flagged entries are counted", {
  d <- reg_data()
  d$Yield[3] <- NA
  d$Rain <- as.character(d$Rain); d$Rain[7] <- "5,6"; d$Rain[9] <- "n/a"
  r <- regress_data(d, "Yield", c("Nitrogen", "Rain"))
  expect_identical(r$n, 17L)
  expect_identical(r$rows, setdiff(1:20, c(3, 7, 9)))
  expect_identical(r$held_rows, 1L)
  txt <- reg_text(r)
  expect_match(txt, "3 of the 20 rows are left out because they have no usable value of the response or a predictor.", fixed = TRUE)
  expect_match(txt, "Of these, 1 has an entry the data check has flagged (such as '5,6') and will count once corrected on the Data tab.", fixed = TRUE)
  # the simple regression does not lose rows to a column it does not use
  r1 <- regress_data(d, "Yield", "Nitrogen")
  expect_identical(r1$n, 19L)
  expect_match(reg_text(r1), "1 of the 20 rows is left out because it has no usable value of one of the two variables.", fixed = TRUE)
  expect_no_match(reg_text(r1), "flagged", fixed = TRUE)
})

## ------------------------------------------------- refusals ----

test_that("requests that cannot be fitted are refused in plain words", {
  d <- reg_data()
  expect_error(regress_data(d, "Yield", "Yield"), "'Yield' is the response, so it cannot also be a predictor.", fixed = TRUE)
  expect_error(regress_data(d, "Yield", character(0)), "Choose at least one predictor.", fixed = TRUE)
  expect_error(regress_data(d, "Yield", c("Nitrogen", "Weight")), "'Weight' is not a column of the data.", fixed = TRUE)
  expect_error(regress_data(d, "Yield", "Nitrogen", alpha = 0.5), "between 0 and 0.5", fixed = TRUE)
  # too few rows: two predictors need four
  small <- d[1:3, ]
  expect_error(regress_data(small, "Yield", c("Nitrogen", "Rain")),
               "Only 3 rows have a value of every variable chosen; a regression on 2 predictors needs at least 4", fixed = TRUE)
  small$Rain <- as.character(small$Rain); small$Rain[1] <- "5,6"
  expect_error(regress_data(small, "Yield", c("Nitrogen", "Rain")),
               "1 more row has an entry the data check has flagged (such as '5,6')", fixed = TRUE)
  # a predictor that does not vary, a response that does not vary
  d2 <- d; d2$Const <- 5
  expect_error(regress_data(d2, "Yield", c("Nitrogen", "Const")),
               "'Const' has the same value in every row used, so its effect cannot be estimated; leave it out.", fixed = TRUE)
  expect_error(regress_data(d2, "Const", "Nitrogen"), "'Const' has the same value in every row used", fixed = TRUE)
  # a predictor that is an exact combination of others
  d2$Total <- d2$Nitrogen + d2$Rain
  expect_error(regress_data(d2, "Yield", c("Nitrogen", "Rain", "Total")),
               "'Total' can be worked out exactly from the other predictors in the rows used", fixed = TRUE)
  d2$NitroKg <- d2$Nitrogen / 1000
  expect_error(regress_data(d2, "Yield", c("Nitrogen", "NitroKg")),
               "'NitroKg' can be worked out exactly from the other predictor in the rows used", fixed = TRUE)
  expect_error(regress_data(d2, "Yield", c("Nitrogen", "NitroKg")),
               "so its effect cannot be told apart from the other predictor's; leave it out.", fixed = TRUE)
  # several, each redundant given the rest: leaving them all out works
  d3 <- data.frame(x1 = c(1, 3, 2, 5, 4, 6), y = c(2, 5, 3, 8, 7, 9)); d3$x2 <- 2 * d3$x1; d3$x3 <- d3$x1 + 5
  expect_error(regress_data(d3, "y", c("x1", "x2", "x3")),
               "'x2' and 'x3' can be worked out exactly from the other predictor in the rows used (one may be the sum of others, or the same measurement in other units), so their effects cannot be told apart from the other predictor's; leave them out.",
               fixed = TRUE)
  expect_no_error(regress_data(d3, "y", "x1"))
  # a time stamp in seconds varies too little beside its size to be told from a constant
  tm <- data.frame(Time = 1.7e9 + (0:7) * 10, Temp = c(21.2, 21.5, 21.4, 21.9, 22.1, 22.0, 22.4, 22.6))
  # the fit is on centred columns, so it works as well as with a starting value subtracted
  r1 <- regress_data(tm, "Temp", "Time")
  tm0 <- tm; tm0$Time <- tm0$Time - tm0$Time[1]
  expect_equal(r1$coefficients$Estimate[2], regress_data(tm0, "Temp", "Time")$coefficients$Estimate[2])
})

test_that("an exact fit leaves out what cannot be worked out", {
  d <- data.frame(x = c(1, 2, 3, 4, 5), y = c(5, 8, 11, 14, 17))
  r <- regress_data(d, "y", "x")
  expect_true(r$exact)
  expect_equal(r$coefficients$Estimate, c(2, 3))
  expect_true(all(is.na(r$coefficients$SE)) && all(is.na(r$coefficients$p)))
  expect_identical(r$model$R2, 1); expect_true(is.na(r$model$F))
  expect_true(all(grepl("^not available: the model fits every value exactly", r$checks$Verdict)))
  expect_match(reg_text(r), "The equation passes through every point exactly", fixed = TRUE)
  expect_identical(r$equation, "y = 2.000 + 3.000 \u00d7 x")
  expect_null(plot_reg_resid(r)); expect_null(plot_reg_qq(r))
  expect_match(reg_coef_html(r), "standard errors, tests and intervals are not available", fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(plot_reg_main(r)))
})

## ------------------------------------------------- the checks ----

test_that("the checks on the residuals match their definitions", {
  d <- reg_data(4)
  r <- regress_data(d, "Yield", c("Nitrogen", "Rain"))
  f <- lm(Yield ~ Nitrogen + Rain, d); e <- residuals(f); fv <- fitted(f)
  ck <- r$checks
  sw <- shapiro.test(e)
  expect_equal(ck$Statistic[1], unname(sw$statistic))
  bp <- 20 * summary(lm(I(e^2) ~ Nitrogen + Rain, d))$r.squared
  expect_equal(ck$Statistic[2], bp)
  # up to 200 rows both p-values come from normal data on the same predictors
  expect_identical(ck$Simulated, c(TRUE, TRUE, FALSE))
  E <- reg_sim_resid(r$fit)
  expect_equal(ck$p[1], (sum(apply(E, 2, function(z) shapiro.test(z)$statistic) <= sw$statistic + 1e-12) + 1) / 2001)
  bps <- apply(E, 2, function(z) 20 * summary(lm(I(z^2) ~ Nitrogen + Rain, d))$r.squared)
  expect_equal(ck$p[2], (sum(bps >= bp - 1e-9 * max(1, bp)) + 1) / 2001)
  f2 <- lm(Yield ~ Nitrogen + Rain + I(fv^2), d)
  a <- anova(f, f2)
  expect_equal(ck$Statistic[3], a$F[2]); expect_equal(ck$p[3], a$`Pr(>F)`[2])
  expect_identical(ck$df, c("", "2", "1, 16"))
  expect_match(reg_checks_html(r), "come from sets of normal data simulated on these same predictor values (2,000 for Shapiro-Wilk, 2,000 for Breusch-Pagan)", fixed = TRUE)
  expect_identical(ck$Verdict, ifelse(ck$p < 0.05, "significant departure", "no significant departure"))
  # a curve the line misses is found
  x <- rep(1:10, 2); set.seed(5); y <- (x - 5.5)^2 + rnorm(20, 0, 0.5)
  rc <- regress_data(data.frame(x, y), "y", "x")
  expect_lt(rc$checks$p[3], 0.01)
  expect_match(reg_resid_text(rc)$reading, "The check for a curve finds that adding the square of x fits significantly better than the straight line at the 5% level", fixed = TRUE)
  expect_equal(rc$checks$p[3], anova(lm(y ~ x), lm(y ~ x + I(x^2)))$`Pr(>F)`[2])
  # a spread that grows with the fitted value is found
  set.seed(6); x <- runif(80, 1, 10); y <- 2 * x + rnorm(80, 0, 0.3 * x^1.5)
  rs <- regress_data(data.frame(x, y), "y", "x")
  expect_lt(rs$checks$p[2], 0.05)
  expect_match(reg_resid_text(rs)$reading, "The Breusch-Pagan test finds that the spread of the residuals changes with x, significant", fixed = TRUE)
})

test_that("a check that cannot be worked out says why", {
  # two levels of the predictor: no curve can be told from a line
  d <- data.frame(x = rep(c(0, 10), each = 4), y = c(1, 2, 3, 2, 8, 9, 7, 8))
  r <- regress_data(d, "y", "x")
  expect_identical(r$checks$Verdict[3],
                   "not available: x takes only two distinct values, so a curve cannot be told from a straight line")
  expect_match(reg_resid_text(r)$reading, "The curve test is not available: x takes only two distinct values", fixed = TRUE)
  # three rows: one left over for the scatter, none for a curve
  r3 <- regress_data(data.frame(x = 1:3, y = c(1, 3, 2)), "y", "x")
  expect_identical(r3$checks$Verdict[3], "not available: it needs at least 4 rows, one more than the regression itself")
  expect_match(reg_checks_html(r3), "not available: it needs at least 4 rows", fixed = TRUE)
  # one residual degree of freedom: the residuals' pattern is set by the predictors, so no verdict
  for (i in 1:2) expect_identical(r3$checks$Verdict[i],
    "not available: with one residual degree of freedom, the pattern of the residuals is fixed by the predictor values")
  r4 <- regress_data(data.frame(a = 1:4, b = c(2, 1, 4, 3), y = c(3, 1, 4, 1)), "y", c("a", "b"))
  expect_true(all(is.na(r4$checks$p)))
  expect_match(reg_qq_text(r3)$reading, "The Shapiro-Wilk test is not available: with one residual degree of freedom", fixed = TRUE)
})

## ------------------------------------------------- the display ----

test_that("estimates are printed to the precision their standard errors support", {
  expect_identical(reg_dec(0.0456, 0.4521), 4L)
  expect_identical(reg_dec(4.56, 245.6), 2L)
  expect_identical(reg_dec(456, 12345), 0L)
  expect_identical(reg_dec(NA, 0.11405), 4L)
  d <- reg_data()
  r <- regress_data(d, "Yield", "Nitrogen")
  h <- reg_coef_html(r)
  # every number in the equation is the table's
  nums <- regmatches(r$equation, gregexpr("[0-9]+[.][0-9]+", r$equation))[[1]]
  for (z in nums) expect_match(h, paste0("<td>-?", z, "</td>"))
  expect_match(h, sprintf("<td>%s to %s</td>", reg_coef_txt(r, 2, "CI_lower"), reg_coef_txt(r, 2, "CI_upper")), fixed = TRUE)
  expect_match(reg_model_html(r), "Model summary", fixed = TRUE)
  expect_match(reg_anova_html(r), "Analysis of variance of the regression", fixed = TRUE)
  expect_match(reg_checks_html(r), "Checks on the residuals", fixed = TRUE)
  # a negative slope is written with a minus, not "+ -"
  d$Yield <- 100 - d$Yield
  e <- regress_data(d, "Yield", "Nitrogen")$equation
  expect_match(e, " - [0-9.]+ \u00d7 Nitrogen$")
  expect_no_match(e, "+ -", fixed = TRUE)
})

test_that("the reading follows the result at the level chosen", {
  d <- reg_data()
  r <- regress_data(d, "Yield", "Nitrogen")
  txt <- reg_text(r)
  expect_match(txt, sprintf("The fitted equation is %s, from 20 rows.", r$equation), fixed = TRUE)
  expect_match(txt, sprintf("R\u00b2 is %s: the line accounts for %s%% of the variation in Yield about its mean.",
                            fmt(r$model$R2, 3), fmt(100 * r$model$R2, 1)), fixed = TRUE)
  expect_match(txt, "The regression is significant at the 5% level (F = ", fixed = TRUE)
  expect_match(txt, sprintf("For each unit more of Nitrogen, the predicted Yield rises by %s; the slope is significant at the 5%% level",
                            reg_coef_txt(r, 2)), fixed = TRUE)
  expect_match(txt, "but 0 lies outside the observed range of Nitrogen (40 to 160), so the intercept is an extrapolation", fixed = TRUE)
  expect_match(txt, "The equation describes the data only over the observed range (Nitrogen 40 to 160)", fixed = TRUE)
  expect_match(txt, "it can show a cause only when the predictor was set by the experimenter and assigned to the plots at random", fixed = TRUE)
  # not significant: insufficient evidence, at the level chosen
  set.seed(9); n <- data.frame(a = rnorm(15), b = rnorm(15))
  rn <- regress_data(n, "b", "a", alpha = 0.01)
  if (rn$model$p >= 0.01) {
    tn <- reg_text(rn)
    expect_match(tn, "There is insufficient evidence at the 1% level of a straight-line relation between b and a", fixed = TRUE)
    expect_match(tn, "there is insufficient evidence at the 1% level that the slope differs from 0", fixed = TRUE)
    expect_no_match(tn, "5%", fixed = TRUE)
  }
  for (t in c(reg_text(r), reg_text(rn))) {
    expect_no_match(t, "no relationship|no effect|no difference|proves|causes", perl = TRUE)
  }
  # 0 inside the range: no extrapolation sentence
  expect_no_match(reg_text(rn), "extrapolation", fixed = TRUE)
})

test_that("a multiple regression's reading names shared and inflated predictors", {
  set.seed(11)
  x1 <- rnorm(25); x2 <- x1 + rnorm(25, 0, 0.15); y <- x1 + x2 + rnorm(25, 0, 1.5)
  r <- regress_data(data.frame(y, x1, x2), "y", c("x1", "x2"))
  txt <- reg_text(r)
  expect_gte(round(r$coefficients$VIF[2], 2), 5)
  expect_match(txt, sprintf("x1 and x2 have VIFs of 5 or more (%s and %s): they are closely related to the other predictors",
                            fmt(r$coefficients$VIF[2], 2), fmt(r$coefficients$VIF[3], 2)), fixed = TRUE)
  expect_match(txt, "when the other predictors are held at the same values", fixed = TRUE)
  if (r$model$p < 0.05 && all(r$coefficients$p[-1] >= 0.05))
    expect_match(txt, "The predictors are significant together, but neither x1 nor x2 is significant on its own", fixed = TRUE)
  expect_match(txt, "adjusted R\u00b2, which allows for the number of predictors, is", fixed = TRUE)
})

## ------------------------------------------------- the plots ----

test_that("the main plot draws the fitted line, its band and the rows", {
  d <- reg_data()
  r <- regress_data(d, "Yield", "Nitrogen")
  b <- ggplot2::ggplot_build(plot_reg_main(r))$data
  f <- lm(Yield ~ Nitrogen, d)
  g <- data.frame(Nitrogen = b[[1]]$x)
  pr <- predict(f, g, interval = "confidence", level = 0.95)
  expect_equal(b[[1]]$ymin, unname(pr[, 2])); expect_equal(b[[1]]$ymax, unname(pr[, 3]))
  expect_equal(sort(b[[2]]$y), sort(d$Yield))
  expect_equal(b[[3]]$y, unname(pr[, 1]))
  p <- plot_reg_main(r)
  expect_identical(p$labels$subtitle, paste0(r$equation, "; R\u00b2 = ", fmt(r$model$R2, 3)))
  expect_match(p$labels$caption, "95% confidence interval for the average Yield", fixed = TRUE)
  expect_match(reg_main_text(r)$what, "the 95% confidence interval for the average Yield at each value of Nitrogen", fixed = TRUE)
})

test_that("with several predictors the points scatter about the line by exactly the residuals", {
  d <- reg_data(7)
  r <- regress_data(d, "Yield", c("Nitrogen", "Rain"))
  ef <- reg_effect(r, 2)
  f <- lm(Yield ~ Nitrogen + Rain, d)
  line_at <- coef(f)[1] + coef(f)[2] * mean(d$Nitrogen) + coef(f)[3] * d$Rain
  expect_equal(unname(ef$pts$y - line_at), unname(residuals(f)))
  b <- ggplot2::ggplot_build(plot_reg_main(r, "Rain"))$data
  expect_equal(b[[3]]$y, unname(coef(f)[1] + coef(f)[2] * mean(d$Nitrogen) + coef(f)[3] * b[[3]]$x))
  w <- reg_main_text(r, "Rain")$what
  expect_match(w, sprintf("held at their means (Nitrogen %s)", num_text(signif(mean(d$Nitrogen), 6))), fixed = TRUE)
  expect_match(gsub("[[:space:]]+", " ", plot_reg_main(r, "Rain")$labels$title),
               "Yield against Rain, other predictors at their means", fixed = TRUE)
  # a name that is not a predictor falls back on the first
  expect_identical(reg_show(r, "Weight"), 1L)
})

test_that("the residual plot marks what its note names", {
  d <- reg_data(8); d$Yield[5] <- d$Yield[5] + 15
  r <- regress_data(d, "Yield", "Nitrogen")
  s <- rstandard(lm(Yield ~ Nitrogen, d))
  b <- ggplot2::ggplot_build(plot_reg_resid(r))$data
  expect_equal(sort(b[[3]]$y), sort(unname(s)))
  far <- unname(which(abs(s) > 3))
  expect_identical(far, 5L)
  expect_identical(b[[4]]$label, "row 5")
  expect_match(reg_resid_text(r)$reading, "Row 5 lies more than three standard errors from 0", fixed = TRUE)
  r0 <- regress_data(reg_data(), "Yield", "Nitrogen")
  if (all(abs(rstandard(r0$fit)) <= 3)) expect_match(reg_resid_text(r0)$reading, "No point lies beyond -3 or 3.", fixed = TRUE)
})

test_that("the Q-Q and observed-against-predicted notes state the facts", {
  r <- regress_data(reg_data(), "Yield", "Nitrogen")
  q <- reg_qq_text(r)$reading
  expect_match(q, if (r$checks$p[1] < 0.05) "The Shapiro-Wilk test finds a significant departure from normal at the 5% level"
                  else "The Shapiro-Wilk test finds insufficient evidence at the 5% level", fixed = TRUE)
  expect_no_match(q, "values", fixed = TRUE)
  b <- ggplot2::ggplot_build(plot_reg_obs(r))$data
  expect_equal(b[[2]]$x, unname(fitted(r$fit))); expect_equal(b[[2]]$y, r$y)
  o <- reg_obs_text(r)$reading
  expect_match(o, sprintf("R\u00b2 (%s) is the square of the correlation between the observed and the predicted values",
                          fmt(r$model$R2, 3)), fixed = TRUE)
  expect_equal(cor(fitted(r$fit), r$y)^2, r$model$R2)
})

test_that("printing gives the equation and the tables", {
  r <- regress_data(reg_data(), "Yield", "Nitrogen")
  out <- capture.output(print(r))
  expect_identical(out[1], paste(r$equation, ""))
  expect_true(any(grepl("Intercept", out)))
})

## ------------------------------------------------- the app ----

test_that("the regression tab fits, reads and draws", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- reg_data()
    rv$data <- d
    session$flushReact()
    html <- output$regUI$html
    y <- regmatches(html, regexpr('(?s)id="regY".*?</select>', html, perl = TRUE))
    x <- regmatches(html, regexpr('(?s)id="regX".*?</select>', html, perl = TRUE))
    expect_match(y, '<option value="Yield" selected>', fixed = TRUE)      # the last measurement column
    expect_match(x, '<option value="Nitrogen" selected>', fixed = TRUE)
    expect_no_match(x, '<option value="Yield"', fixed = TRUE)             # the response is not offered
    expect_no_match(x, '<option value="Rep" selected>', fixed = TRUE)     # a label column, not chosen
    session$setInputs(regY = "Yield", regX = c("Nitrogen", "Rain"), regAlpha = "0.01")
    expect_match(output$regEquation$html, "Yield = ", fixed = TRUE)
    expect_match(output$regCoef$html, "99% CI", fixed = TRUE)
    expect_match(output$regModel$html, "Model summary", fixed = TRUE)
    expect_match(output$regText$html, "at the 1% level", fixed = TRUE)
    expect_match(output$regAnova$html, "Residual", fixed = TRUE)
    expect_match(output$regChecks$html, "Shapiro-Wilk", fixed = TRUE)
    expect_match(output$regShowUI$html, "Predictor on the main plot", fixed = TRUE)
    session$setInputs(regShow = "Rain")
    expect_match(output$regMain$src, "^data:image/png")
    expect_match(output$regMainText$html, "Yield changes with Rain", fixed = TRUE)
    expect_match(output$regResid$src, "^data:image/png")
    expect_match(output$regQQ$src, "^data:image/png")
    expect_match(output$regObs$src, "^data:image/png")
    expect_identical(reg_res()$predictors, c("Nitrogen", "Rain"))
    # no predictor
    session$setInputs(regX = character(0))
    expect_error(output$regCoef, "Choose at least one predictor.")
    # one numeric column: the tab says so
    rv$data <- data.frame(Treatment = c("A", "B", "C"), Yield = c(1, 2, 3))
    session$flushReact()
    expect_error(output$regCoef, "Regression needs at least two columns of numbers.")
  })
})

## ------------------------------------------------ found in review ----

test_that("a slope of exactly 0 is fitted and its curve is still tested", {
  # a symmetric arch: the least-squares slope is exactly 0
  r <- regress_data(data.frame(x = 0:4, y = c(4, 1, 0, 1, 4)), "y", "x")
  expect_equal(r$coefficients$Estimate[2], 0)
  expect_lt(r$checks$p[3], 1e-6)                          # the square of x fits it exactly
  expect_match(reg_main_text(r)$reading, "Adding the square of x to the model fits significantly better at the 5% level", fixed = TRUE)
  d6 <- data.frame(x = rep(c(0, 60, 120), each = 4), y = c(20, 21, 22, 23, 25, 26, 27, 28, 20, 21, 22, 23))
  r6 <- regress_data(d6, "y", "x")
  expect_equal(r6$checks$p[3], anova(lm(y ~ x, d6), lm(y ~ x + I(x^2), d6))$`Pr(>F)`[2])
  # several predictors and a flat fit: RESET has nothing to square
  d <- data.frame(x1 = rep(1:5, 2), x2 = rep(1:2, each = 5)); d$y <- (d$x1 - 3)^2 + c(0.1, -0.1, 0, 0.1, -0.1)[d$x1]
  d$y <- d$y - c(0.1, -0.1, 0, 0.1, -0.1)[d$x1]           # exactly (x1 - 3)^2, unrelated to x1 and x2 in a line
  rf <- regress_data(d, "y", c("x1", "x2"))
  expect_identical(rf$checks$Verdict[3], "not available: the fitted values are all the same, so their square cannot show a curve")
  expect_match(reg_main_text(rf, "x1")$reading, "Adding the square of x1 to the model fits significantly better", fixed = TRUE)
})

test_that("residuals all of one size show no change of spread", {
  d <- data.frame(x = rep(c(3, 7, 11, 15), each = 2), y = c(2.3, 2.9, 4.1, 4.7, 5.9, 6.5, 7.7, 8.3))
  r <- regress_data(d, "y", "x")
  expect_identical(r$checks$Statistic[2], 0)
  expect_identical(r$checks$Verdict[2], "no significant departure")
})

test_that("the simulated p-values hold their level where the usual ones do not", {
  # two predictors of small whole numbers and five rows: the usual Shapiro-Wilk
  # p-value on the residuals is set largely by the design
  X <- data.frame(a = c(0, 1, 2, 0, 2), b = c(1, 0, 2, 2, 0))
  set.seed(21)
  sim <- replicate(150, { X$y <- rnorm(5); regress_data(X, "y", c("a", "b"))$checks$p[1:2] })
  expect_lt(mean(sim[1, ] < 0.05), 0.12)
  expect_lt(mean(sim[2, ] < 0.05), 0.12)
  # above 200 rows the usual p-values
  set.seed(22); big <- data.frame(x = runif(250)); big$y <- big$x + rnorm(250)
  rb <- regress_data(big, "y", "x")
  e <- residuals(lm(y ~ x, big))
  expect_equal(rb$checks$p[1], shapiro.test(e)$p.value)
  expect_equal(rb$checks$p[2], pchisq(250 * summary(lm(I(e^2) ~ x, big))$r.squared, 1, lower.tail = FALSE))
  expect_identical(rb$checks$Simulated, c(FALSE, FALSE, FALSE))
  expect_no_match(reg_checks_html(rb), "simulated", fixed = TRUE)
})

test_that("rows are named as the data table numbers them", {
  d <- reg_data(12, 21)
  d$Yield[12] <- d$Yield[12] + 40
  d[3, ] <- NA
  d$Rain <- as.character(d$Rain); d$Rain[3] <- "n/a"
  fx <- fix_data(d)$data
  expect_identical(nrow(fx), 20L)
  r <- regress_data(fx, "Yield", "Nitrogen")
  expect_true(12 %in% r$rows)
  expect_match(reg_resid_text(r)$reading, "Row 12 lies more than three standard errors from 0", fixed = TRUE)
  b <- ggplot2::ggplot_build(plot_reg_resid(r))$data
  expect_identical(b[[4]]$label, "row 12")
})

test_that("with nine or fewer residual degrees of freedom the plot cannot flag a stray point, and says so", {
  d <- data.frame(N = seq(0, 180, 20), Y = c(30, 33, 35, 38, 41, 43, 46, 48, 51, 54))
  d$Y[5] <- 410
  r <- regress_data(d, "Y", "N")
  expect_lte(max(abs(rstandard(r$fit))), sqrt(8))
  t <- reg_resid_text(r)$reading
  expect_match(t, "With 8 residual degrees of freedom, no standardised residual can exceed 3, so the dotted lines cannot pick out a stray point here", fixed = TRUE)
  expect_no_match(t, "No point lies beyond", fixed = TRUE)
})

test_that("the notes say only what applies to the fit on screen", {
  # exact fit: no band, no curve check promised
  r <- regress_data(data.frame(x = 1:5, y = c(5, 8, 11, 14, 17)), "y", "x")
  m <- reg_main_text(r)
  expect_no_match(m$what, "band", fixed = TRUE)
  expect_no_match(m$reading, "curve", fixed = TRUE)
  # three rows: the curve cannot be tested, and the note says why
  r3 <- regress_data(data.frame(x = 1:3, y = c(1, 3, 2)), "y", "x")
  expect_match(reg_main_text(r3)$reading, "A curve in x cannot be tested: it needs at least 4 rows", fixed = TRUE)
  expect_no_match(reg_main_text(reg_data() |> regress_data("Yield", "Nitrogen"))$what, "expected", fixed = TRUE)
  # the Q-Q note's reassurance about many rows only with many rows
  q10 <- reg_qq_text(regress_data(reg_data(1, 10), "Yield", "Nitrogen"))$reading
  expect_no_match(q10, "many rows", fixed = TRUE)
  q40 <- reg_qq_text(regress_data(reg_data(1, 40), "Yield", "Nitrogen"))$reading
  expect_match(q40, "With this many rows, the tests and intervals for the coefficients are little affected", fixed = TRUE)
  # a footnote with one degree of freedom
  expect_match(reg_coef_html(r3), "N - k - 1 = 3 - 1 - 1 = 1 degree of freedom", fixed = TRUE)
})

test_that("a model with no straight-line relation but a clear curve says both", {
  x <- rep(1:10, 2); set.seed(5); y <- (x - 5.5)^2 + rnorm(20, 0, 0.5)
  r <- regress_data(data.frame(x, y), "y", "x")
  t <- reg_text(r)
  expect_match(t, "There is insufficient evidence at the 5% level of a straight-line relation between y and x", fixed = TRUE)
  expect_match(t, "The check for a curve below is significant, though (p < 0.0001): the relation bends, which a straight line cannot show",
               fixed = TRUE)
})

test_that("a curve in one of several predictors is tested where it is drawn", {
  set.seed(31); x1 <- runif(40, 0, 20); x2 <- runif(40, 0, 10)
  y <- 5 + 0.5 * x1 + 0.6 * (x2 - 5)^2 + rnorm(40)
  r <- regress_data(data.frame(x1, x2, y), "y", c("x1", "x2"))
  cv <- anova(lm(y ~ x1 + x2), lm(y ~ x1 + x2 + I(x2^2)))$`Pr(>F)`[2]
  expect_lt(cv, 0.01)
  expect_match(reg_main_text(r, "x2")$reading,
               sprintf("Adding the square of x2 to the model fits significantly better at the 5%% level (%s)", reg_p(cv, 0.05)), fixed = TRUE)
  expect_match(reg_checks_html(r), "a curve in one predictor is tested in the main plot's note", fixed = TRUE)
})

test_that("joint and single tests that disagree are explained by what the table shows", {
  r <- regress_data(reg_data(3), "Yield", c("Nitrogen", "Rain"))
  # significant together, none alone, predictors unrelated (VIF about 1)
  a <- r; a$model$p <- 0.01; a$coefficients$p[2:3] <- c(0.08, 0.09); a$coefficients$VIF[2:3] <- c(1.02, 1.02)
  t <- reg_text(a)
  expect_match(t, "but neither Nitrogen nor Rain is significant on its own: each falls short of significance by itself, while together they account for more of the variation", fixed = TRUE)
  expect_no_match(t, "related to each other", fixed = TRUE)
  a$coefficients$VIF[2:3] <- c(3.1, 3.1)
  expect_match(reg_text(a), "are related to each other (see the VIF column)", fixed = TRUE)
  # one slope significant, the model not
  b <- r; b$model$p <- 0.2; b$coefficients$p[2:3] <- c(0.03, 0.4)
  expect_match(reg_text(b), "Nitrogen is significant on its own although the model as a whole is not", fixed = TRUE)
})

test_that("a row where each of two rows alone decides a coefficient is worded for both", {
  d <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(0, 0, 1, 0, 0, 0, 0, 0), b = c(0, 0, 0, 0, 0, 0, 1, 0),
                  y = c(2.1, 3.9, 9, 8.2, 9.9, 12.1, 20, 16.1))
  r <- regress_data(d, "y", c("x", "a", "b"))
  expect_match(reg_resid_text(r)$reading, "Rows 3 and 7 fix the fit on their own (each alone decides a coefficient), so they have no residual to plot.",
               fixed = TRUE)
})

test_that("numbers keep the figures they need and no more", {
  # tiny-scale data: the SE keeps three figures and the interval's ends differ
  set.seed(4); x <- 1:20; y <- (2 + 0.05 * x + rnorm(20, 0, 0.01)) * 1e-8
  r <- regress_data(data.frame(x, y), "y", "x")
  se <- reg_coef_txt(r, 2, "SE")
  expect_gt(as.numeric(se), 0)
  expect_equal(as.numeric(se), r$coefficients$SE[2], tolerance = 0.01)
  expect_false(identical(reg_coef_txt(r, 2, "CI_lower"), reg_coef_txt(r, 2, "CI_upper")))
  # a significant interval never prints an end as 0
  set.seed(8)
  for (i in 1:150) {
    d <- data.frame(x = 1:8, y = 0.3 * (1:8) + rnorm(8, 0, 1.2))
    ri <- regress_data(d, "y", "x")
    if (ri$coefficients$p[2] < 0.05) {
      expect_false(as.numeric(reg_coef_txt(ri, 2, "CI_lower")) == 0)
      expect_false(as.numeric(reg_coef_txt(ri, 2, "CI_upper")) == 0)
    }
  }
  # an exact fit's zero intercept is 0, not rounding residue
  r0 <- regress_data(data.frame(x = 1:5, y = 3 * (1:5)), "y", "x")
  expect_identical(r0$equation, "y = 0.00 + 3.000 \u00d7 x")
  # no negative zero in t or adjusted R-squared
  n0 <- regress_data(reg_data(), "Yield", "Nitrogen")
  n0$coefficients$t[2] <- -0.001; n0$model$Adj_R2 <- -0.0003
  expect_match(reg_coef_html(n0), "<td>0.00</td>", fixed = TRUE)
  expect_no_match(reg_coef_html(n0), "<td>-0.00</td>", fixed = TRUE)
  expect_no_match(reg_model_html(n0), "-0.000", fixed = TRUE)
  # a centred covariate's mean is 0, not rounding residue
  d <- data.frame(Temp = c(0.1, 0.2, -0.3, 0.4, -0.4, 0.25, -0.25, 0.05, -0.05, 0), Rain = c(3, 5, 2, 8, 6, 9, 4, 7, 1, 10))
  d$Y <- 2 + d$Temp + 0.3 * d$Rain + c(0.1, -0.2, 0.15, 0, -0.1, 0.05, -0.05, 0.2, -0.15, 0)
  rc <- regress_data(d, "Y", c("Temp", "Rain"))
  expect_match(reg_main_text(rc, "Rain")$what, "held at their means (Temp 0)", fixed = TRUE)
  # predictor limits held to full precision print to six figures
  set.seed(5); d <- data.frame(x = rnorm(12)); d$y <- d$x + rnorm(12)
  t <- reg_text(regress_data(d, "y", "x"))
  rg <- reg_range(regress_data(d, "y", "x"), 1)
  expect_match(t, sprintf("(x %s)", rg), fixed = TRUE)
  ends <- as.numeric(strsplit(rg, " to ")[[1]])
  expect_lte(ends[1], min(d$x)); expect_gte(ends[2], max(d$x))
  expect_lte(nchar(gsub("[^0-9]", "", strsplit(rg, " to ")[[1]][1])), 6)
})

test_that("an exact fit is judged against the spread of the values, not their size", {
  set.seed(4); d <- data.frame(x = 1:15); d$y <- round(2e9 + 50 * d$x + rnorm(15, 0, 0.6))
  r <- regress_data(d, "y", "x")
  expect_false(r$exact)
  expect_false(is.na(r$coefficients$SE[2]))
  # a true exact fit at that size still is one
  d$y <- 2e9 + 50 * d$x
  expect_true(regress_data(d, "y", "x")$exact)
})

test_that("rows left out for an infinite value say so", {
  d <- data.frame(Dose = c(1, 2, 3, 4, 5, 6, 7, 8), Ratio = c(1.2, 1.9, Inf, 4.1, 5.2, 5.8, 7.1, 8.3))
  t <- reg_text(regress_data(d, "Ratio", "Dose"))
  expect_match(t, "1 of the 8 rows is left out because it has no usable value of one of the two variables. Of these, 1 holds a value that is not a finite number (such as Inf, from a division by zero).",
               fixed = TRUE)
})

test_that("a label column is never chosen as the response", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Treatment = rep(c("A", "B"), 6), Rep = rep(1:6, each = 2), Plot = 1:12,
                          stringsAsFactors = FALSE)
    session$flushReact()
    y <- regmatches(output$regUI$html, regexpr('(?s)id="regY".*?</select>', output$regUI$html, perl = TRUE))
    expect_match(y, '<option value="" selected>Choose a column</option>', fixed = TRUE)
    expect_error(output$regCoef, "Choose a response.")
  })
})

## ------------------------------------------ found in second review ----

test_that("a change of spread that no data could show is not tested", {
  d <- data.frame(Dose = rep(c(0, 50, 100), each = 2), Yield = c(20.1, 20.3, 31.0, 31.4, 30.0, 44.0))
  d$Dose2 <- d$Dose^2
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  expect_match(r$checks$Verdict[2], "^not available: the squared residuals are fitted exactly by the predictors whatever the data")
  r2 <- regress_data(data.frame(x = c(0, 0, 10, 10), y = c(5.0, 5.1, 8, 28)), "y", "x")
  expect_true(is.na(r2$checks$p[2]))
  expect_match(reg_resid_text(r2)$reading, "The Breusch-Pagan test is not available: the squared residuals are fitted exactly", fixed = TRUE)
})

test_that("a simulated p-value close to a threshold is worked out again from more sets", {
  set.seed(14); d <- data.frame(x = 1:12); d$y <- d$x + rexp(12)
  r <- regress_data(d, "y", "x")
  p0 <- r$checks$p[1]
  skip_if(p0 >= 0.5)
  r1 <- regress_data(d, "y", "x", alpha = p0)
  e <- residuals(r$fit); w <- shapiro.test(e)$statistic
  E <- reg_sim_resid(r$fit, REG_SIM_B_CLOSE)
  expect_equal(r1$checks$p[1], (sum(apply(E, 2, function(z) shapiro.test(z)$statistic) <= w + 1e-12) + 1) / (REG_SIM_B_CLOSE + 1))
})

test_that("the simulated p-values do not depend on the kind of random numbers chosen", {
  d <- data.frame(x = c(10, 20, 30, 40, 50, 60, 70, 80), y = c(22.7, 28.4, 27.5, 30.8, 34.5, 37.1, 42.3, 42.3))
  p1 <- regress_data(d, "y", "x")$checks$p
  old <- RNGkind()
  RNGkind("L'Ecuyer-CMRG")
  set.seed(1); s1 <- .Random.seed
  p2 <- regress_data(d, "y", "x")$checks$p
  expect_identical(.Random.seed, s1)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  RNGkind(old[1], old[2], old[3])
  expect_identical(p1, p2)
})

test_that("predictor limits are printed as entered", {
  d <- data.frame(Density = rep(c(1111111, 1250000, 1428571, 1666667), 2), Yield = c(3.1, 3.4, 3.9, 4.0, 3.0, 3.6, 3.8, 4.3))
  t <- reg_text(regress_data(d, "Yield", "Density"))
  expect_match(t, "(Density 1111111 to 1666667)", fixed = TRUE)
  expect_match(t, "observed range of Density (1111111 to 1666667)", fixed = TRUE)
})

test_that("an exact fit draws and describes no band, and calls no slope imprecise", {
  d <- data.frame(a = 1:6, b = c(2, 1, 4, 3, 6, 5)); d$y <- 1 + 2 * d$a + 3 * d$b
  r <- regress_data(d, "y", c("a", "b"))
  expect_true(r$exact)
  expect_no_match(reg_main_text(r)$what, "band", fixed = TRUE)
  p <- plot_reg_main(r)
  expect_null(p$labels$caption)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  d2 <- data.frame(x1 = 1:8, x2 = c(1.1, 2.3, 2.9, 4.2, 5.1, 5.8, 7.2, 7.9)); d2$y <- 3 + 2 * d2$x1 - d2$x2
  r2 <- regress_data(d2, "y", c("x1", "x2"))
  expect_no_match(reg_text(r2), "imprecise", fixed = TRUE)
  # coefficients of the same size keep the same figures
  expect_identical(r2$equation, "y = 3.000 + 2.000 \u00d7 x1 - 1.000 \u00d7 x2")
})

test_that("a residual at the largest size the fit allows is not called beyond it", {
  for (row in c(1, 6, 11)) {
    d <- data.frame(x = 1:11); d$y <- 2 * d$x + 1; d$y[row] <- d$y[row] + 5
    t <- reg_resid_text(regress_data(d, "y", "x"))$reading
    expect_no_match(t, "more than three standard errors", fixed = TRUE)
    expect_match(t, "With 9 residual degrees of freedom, no standardised residual can exceed 3", fixed = TRUE)
  }
})

test_that("the Q-Q note's reassurance needs rows to spare and no dominant row", {
  set.seed(3); X <- as.data.frame(matrix(rnorm(30 * 20), 30)); X$y <- rnorm(30)
  r <- regress_data(X, "y", paste0("V", 1:20))
  expect_no_match(reg_qq_text(r)$reading, "With this many rows", fixed = TRUE)
  # one residual degree of freedom: the plot cannot show a shape
  set.seed(1); X <- as.data.frame(matrix(round(rnorm(35, 10, 3), 1), 7)); X$y <- round(rnorm(7, 50, 5), 1)
  q <- reg_qq_text(regress_data(X, "y", paste0("V", 1:5)))$reading
  expect_match(q, "every standardised residual is +1 or -1, so the plot cannot show the shape of the scatter", fixed = TRUE)
  expect_no_match(q, "S-shape", fixed = TRUE)
})

test_that("RMSE is described as what it is", {
  set.seed(8); X <- as.data.frame(matrix(round(rnorm(54, 10, 3), 1), 9)); X$y <- round(rnorm(9, 50, 5), 1)
  r <- regress_data(X, "y", paste0("V", 1:6), 0.01)
  o <- reg_obs_text(r)$reading
  expect_no_match(o, "typical vertical distance", fixed = TRUE)
  expect_match(o, sprintf("The points lie on average %s from the line, vertically (MAE); RMSE (%s) estimates the standard deviation",
                          sig_fmt(r$model$MAE), sig_fmt(r$model$RMSE)), fixed = TRUE)
  expect_match(reg_model_html(r), "estimates the standard deviation of the scatter about the true equation", fixed = TRUE)
})

test_that("with several predictors a change of spread is looked for along each one", {
  x1 <- rep(1:10, 4); x2 <- rep(1:4, each = 10); set.seed(3); y <- 5 + 2 * x1 + rnorm(40, 0, 0.3 * x2^1.5)
  r <- regress_data(data.frame(x1, x2, y), "y", c("x1", "x2"))
  expect_lt(r$checks$p[2], 0.05)
  expect_match(reg_resid_text(r)$reading, "choose each predictor in turn on the main plot", fixed = TRUE)
  expect_match(reg_checks_html(r), "a band that widens along one of them", fixed = TRUE)
})

test_that("a curve that is already in the model is read as the curve", {
  set.seed(3); x <- 1:12; d <- data.frame(x = x, x2 = x^2, y = 3 + 2 * x - 0.3 * x^2 + rnorm(12))
  m <- reg_main_text(regress_data(d, "y", c("x", "x2")), "x")$reading
  expect_match(m, "The curve is highest at x = ", fixed = TRUE)
  expect_no_match(m, "two distinct values", fixed = TRUE)
})

test_that("a flat fit sends the reader to the main plot for the bend", {
  d <- data.frame(x = rep(0:4, each = 2), y = c(4.1, 3.9, 1.2, 0.8, 0.1, -0.1, 0.9, 1.1, 3.8, 4.2))
  t <- reg_resid_text(regress_data(d, "y", "x"))$reading
  expect_match(t, "look for the bend in the main plot, since every fitted value here is the same", fixed = TRUE)
})

test_that("refusals for constant predictors and many rows beyond 3 are worded for what they are", {
  d <- data.frame(a = rep(5, 6), b = rep(2, 6), c = 1:6, y = c(2, 4, 3, 6, 5, 7))
  expect_error(regress_data(d, "y", c("a", "b", "c")),
               "'a' and 'b' each have the same value in every row used, so their effects cannot be estimated; leave them out.", fixed = TRUE)
  set.seed(5); d <- data.frame(x = round(runif(250, 0, 100), 1)); d$y <- round(10 + 0.3 * d$x + rnorm(250, 0, 3), 2)
  r <- regress_data(d, "y", "x")
  if (any(abs(rstandard(r$fit)) > 3))
    expect_match(reg_resid_text(r)$reading, "Among 250 rows, about 0.7 would lie beyond 3 by chance even if every residual were normal.", fixed = TRUE)
})

## ------------------------------------------- found in third review ----

test_that("a predictor and its square are drawn and read as one curve", {
  set.seed(3)
  d <- data.frame(Dose = rep(seq(0, 120, 30), each = 4))
  d$Dose2 <- d$Dose^2
  d$Yield <- 20 + 0.25 * d$Dose - 0.0012 * d$Dose2 + rnorm(20)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  expect_identical(r$built, data.frame(col = 2L, kind = "power", a = 1L, b = NA_integer_, p = 2L, stringsAsFactors = FALSE))
  # the plot draws the fitted curve through the observed points
  b <- ggplot2::ggplot_build(plot_reg_main(r, "Dose"))$data
  f <- lm(Yield ~ Dose + Dose2, d)
  expect_equal(b[[3]]$y, unname(coef(f)[1] + coef(f)[2] * b[[3]]$x + coef(f)[3] * b[[3]]$x^2))
  expect_equal(sort(b[[2]]$y), sort(d$Yield))
  # choosing the square shows the same curve
  expect_identical(reg_show(r, "Dose2"), 1L)
  expect_match(gsub("[[:space:]]+", " ", plot_reg_main(r)$labels$title), "Yield against Dose (with Dose2, its square)", fixed = TRUE)
  # the reading names the turning point, from the coefficients as fitted
  b1 <- r$coefficients$Estimate[2]; b2 <- r$coefficients$Estimate[3]
  t <- reg_text(r)
  expect_match(t, "Dose2 is the square of Dose, so the two describe one curve and cannot change separately", fixed = TRUE)
  expect_match(t, sprintf("The curve is highest at Dose = %s, where its slope is 0, inside the observed range (0 to 120).",
                          dfmt(-b1 / (2 * b2), 1)), fixed = TRUE)
  expect_no_match(t, "For each unit more of Dose", fixed = TRUE)
  expect_no_match(t, "imprecise", fixed = TRUE)
  expect_match(t, "The large VIFs of Dose and Dose2 come from Dose2 being the square of Dose, and are no warning about the curve", fixed = TRUE)
  expect_match(reg_main_text(r)$what, "The curve is the fitted equation, with Dose2 moving with it as its square", fixed = TRUE)
  expect_match(reg_coef_html(r), "Dose2 is the square of Dose: the two describe one curve", fixed = TRUE)
  # a turning point beyond the data
  d$Yield <- 20 + 0.25 * d$Dose - 0.0003 * d$Dose2 + rnorm(20, 0, 0.3)
  r2 <- regress_data(d, "Yield", c("Dose", "Dose2"))
  t0 <- -r2$coefficients$Estimate[2] / (2 * r2$coefficients$Estimate[3])
  if (t0 > 120) expect_match(reg_text(r2), "outside the observed range (0 to 120), so over the data the curve keeps rising", fixed = TRUE)
})

test_that("a row with no residual does not make the spread look uneven", {
  d <- data.frame(Lime = c(0, 0, 0, 0, 0, 0, 0, 0, 2), Y = c(10.5, 9.5, 10.5, 9.6, 10.4, 9.5, 10.5, 9.5, 14.0))
  r <- regress_data(d, "Y", "Lime")
  expect_match(r$checks$Verdict[2], "^not available: the rows that have a residual all share one setting of the predictors")
  # with a predictor that still varies among the other rows, the test runs on them
  d2 <- data.frame(x = c(1:8, 0), z = c(rep(0, 8), 1), y = c(2.1, 3.9, 6.2, 7.8, 10.1, 12.2, 13.8, 16.1, 30))
  r2 <- regress_data(d2, "y", c("x", "z"))
  keep <- hatvalues(r2$fit) < 1 - 1e-8
  e <- residuals(r2$fit)[keep]
  expect_equal(r2$checks$Statistic[2], sum(keep) * summary(lm(I(e^2) ~ d2$x[keep]))$r.squared)
  expect_identical(r2$checks$df[2], "1")
})

test_that("the simulation note says which tests were simulated and from how many sets", {
  set.seed(11); d <- data.frame(x = 1:15); d$y <- round(d$x + rexp(15), 2)
  r <- regress_data(d, "y", "x")
  h <- reg_checks_html(r)
  expect_match(h, sprintf("(%s for Shapiro-Wilk, %s for Breusch-Pagan)",
                          formatC(r$checks$Sets[1], format = "d", big.mark = ","), formatC(r$checks$Sets[2], format = "d", big.mark = ",")),
               fixed = TRUE)
  if (r$checks$Sets[1] > REG_SIM_B) expect_match(h, "10,000 sets are used when the first estimate from 2,000 falls close to a threshold", fixed = TRUE)
  r2 <- regress_data(data.frame(Dose = c(0, 0, 100, 100), Y = c(10.2, 11.0, 15.1, 16.3)), "Y", "Dose")
  h2 <- reg_checks_html(r2)
  expect_no_match(h2, "Breusch-Pagan come", fixed = TRUE)
  expect_match(h2, "The p-value of Shapiro-Wilk comes from sets of normal data", fixed = TRUE)
})

test_that("small fits get no contradictory notes", {
  # one residual degree of freedom: no 'only 5 residuals' after 'cannot show the shape'
  set.seed(2); X <- as.data.frame(matrix(rnorm(15), 5)); X$y <- rnorm(5)
  q <- reg_qq_text(regress_data(X, "y", c("V1", "V2", "V3")))$reading
  expect_no_match(q, "With only 5 residuals", fixed = TRUE)
  # a negative adjusted R-squared is explained
  set.seed(4); X <- as.data.frame(matrix(rnorm(24), 8)); X$y <- rnorm(8)
  r <- regress_data(X, "y", c("V1", "V2", "V3"))
  if (as.numeric(dfmt(r$model$Adj_R2, 3)) < 0)
    expect_match(reg_text(r), "below 0: the predictors account for less of the variation than chance alone would give", fixed = TRUE)
  expect_match(reg_model_html(r), "it falls below 0 when the predictors account for less of the variation than chance alone would give", fixed = TRUE)
  # k is defined where it is used
  expect_match(reg_coef_html(r), "N - k - 1 = 8 - 3 - 1 = 4 degrees of freedom (N rows used, k predictors)", fixed = TRUE)
})

test_that("the residual plot and its note flag the same rows", {
  for (row in c(1, 5, 7, 9)) {
    d <- data.frame(x = 1:11); d$y <- 2 * d$x + 1; d$y[row] <- d$y[row] + 5
    r <- regress_data(d, "y", "x")
    b <- ggplot2::ggplot_build(plot_reg_resid(r))$data
    expect_length(b, 3)                                    # no label layer
  }
})

test_that("the curve check's reason fits the case", {
  set.seed(2); d <- data.frame(Dose = rep(c(0, 50, 100), each = 4)); d$Dose2 <- d$Dose^2
  d$Yield <- round(20 + 0.3 * d$Dose - 0.002 * d$Dose2 + rnorm(12), 1)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  expect_identical(r$checks$Verdict[3],
    "not available: the square of the fitted values can already be worked out from the predictors in the model, so a curve along them is already allowed for")
  expect_identical(reg_curve(r, 1)$why, "Dose2, the square of Dose, is already in the model, so a curve in Dose is already allowed for")
})

test_that("ranges hold every value used", {
  d <- data.frame(T = c(1728460800123, 1728547200456, 1728633600789, 1728720000012, 1728806400345, 1728892800678),
                  y = c(3.1, 3.5, 3.4, 4.0, 4.2, 4.1))
  expect_match(reg_text(regress_data(d, "y", "T")), "(T 1728460800123 to 1728892800678)", fixed = TRUE)
  d <- data.frame(x = c(12.3456789012, 13.1, 14.2, 15.9876543219), y = c(1, 2, 2.5, 4.1))
  expect_match(reg_text(regress_data(d, "y", "x")), "(x 12.3456 to 15.9877)", fixed = TRUE)
})

test_that("a flat fit's observed-against-predicted note says what the plot shows", {
  d <- data.frame(x = rep(0:4, each = 2), y = c(4.1, 3.9, 1.2, 0.8, 0.1, -0.1, 0.9, 1.1, 3.8, 4.2))
  o <- reg_obs_text(regress_data(d, "y", "x"))$reading
  expect_match(o, "Every predicted value is the same (the fitted equation is flat), so the points stand in one column", fixed = TRUE)
  expect_no_match(o, "correlation", fixed = TRUE)
})

## ------------------------------------------ found in fourth review ----

test_that("a cubic is drawn and read as one curve, not a quadratic and a slope", {
  d <- data.frame(Dose = rep(0:6, each = 3))
  d$Dose2 <- d$Dose^2; d$Dose3 <- d$Dose^3
  d$Yield <- c(9.6, 10.1, 10.8, 13.5, 14, 14.1, 15.3, 14.8, 16, 14.1, 14.3, 14.6, 12.7, 12.4, 13.8, 11.3, 12.9, 12.5, 14.8, 14.5, 15.4)
  r <- regress_data(d, "Yield", c("Dose", "Dose2", "Dose3"))
  expect_identical(reg_group(r, 3), 1:3)
  t <- reg_text(r)
  expect_match(t, "Dose2 and Dose3 are the square and cube of Dose, so with it they describe one curve (a polynomial of degree 3)", fixed = TRUE)
  expect_no_match(t, "For each unit more of Dose", fixed = TRUE)
  expect_no_match(t, "highest at", fixed = TRUE)
  expect_no_match(t, "raise or lower the curve", fixed = TRUE)
  b <- ggplot2::ggplot_build(plot_reg_main(r))$data
  f <- lm(Yield ~ Dose + Dose2 + Dose3, d)
  expect_equal(b[[3]]$y, unname(drop(cbind(1, b[[3]]$x, b[[3]]$x^2, b[[3]]$x^3) %*% coef(f))))
  expect_equal(sort(b[[2]]$y), sort(d$Yield))
  expect_match(reg_main_text(r, "Dose3")$reading, "The curve is the fitted polynomial of degree 3 in Dose.", fixed = TRUE)
})

test_that("a square copied from a spreadsheet at the decimals shown is still a square", {
  d <- data.frame(Dose = rep(c(12.5, 25, 37.5, 50, 62.5), each = 4))
  d$Dose2 <- rep(c(156.3, 625, 1406.3, 2500, 3906.3), each = 4)
  d$Yield <- c(26.8, 26, 27.5, 27.2, 32.9, 31.9, 30, 31, 36, 35.8, 34.6, 34.1, 35.4, 35, 35, 35.2, 35.2, 34, 34, 33.8)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  expect_identical(r$built, data.frame(col = 2L, kind = "power", a = 1L, b = NA_integer_, p = 2L, stringsAsFactors = FALSE))
  expect_match(reg_text(r), "The curve is highest at Dose = ", fixed = TRUE)
  # a column that is not a square is not taken for one
  d$Dose2 <- d$Dose^2 + rep(c(3, -2, 5, 1, -4), each = 4)
  expect_identical(nrow(regress_data(d, "Yield", c("Dose", "Dose2"))$built), 0L)
})

test_that("when the fitted curve misses the data's shape the reading says so, not that a line cannot bend", {
  d <- data.frame(Dose = rep(seq(0, 200, 25), each = 3))
  d$Dose2 <- d$Dose^2
  d$Yield <- c(19.7, 20.9, 21.1, 28.2, 27.5, 28.1, 35, 35.5, 34.8, 43, 42.3, 42.6, 44.3, 43.9, 44.2, 44.4, 44.9, 44.8,
               44.1, 44.5, 43.4, 43.9, 44.1, 43.9, 44.4, 43.6, 44.7)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  expect_lt(r$checks$p[3], 0.05)
  t <- reg_text(r)
  expect_match(t, "The check of the shape below is significant, though", fixed = TRUE)
  expect_no_match(t, "linear equation", fixed = TRUE)
  expect_match(reg_resid_text(r)$reading, "a pattern along the fitted values that the fitted equation does not follow", fixed = TRUE)
  expect_match(reg_checks_html(r), "<td>Shape of the fit</td>", fixed = TRUE)
  # an S-shape: no 'a straight line may do as well' when the shape check finds what the curve misses
  set.seed(1); d <- data.frame(Dose = rep(0:7, each = 3))
  d$Mortality <- round(100 / (1 + exp(-(d$Dose - 3.5) * 2)) + rnorm(24, 0, 3), 1); d$Dose2 <- d$Dose^2
  r2 <- regress_data(d, "Mortality", c("Dose", "Dose2"))
  if (r2$checks$p[3] < 0.05) expect_no_match(reg_text(r2), "a straight line in Dose may do as well", fixed = TRUE)
})

test_that("a curve's VIF raised by another predictor is not excused", {
  d <- data.frame(Dose = rep(c(0, 50, 100, 150, 200), each = 4))
  d$Dose2 <- d$Dose^2
  d$Moist <- c(30, 29.8, 28.6, 29.4, 35.3, 35.4, 33.8, 34.6, 38.4, 39.7, 41.1, 40.8, 44.8, 46, 45.7, 45.1, 49, 49.8, 50.9, 50.5)
  d$Yield <- c(28.1, 25.7, 27.6, 25.6, 36.7, 38.1, 37.1, 37.1, 43.4, 43.5, 41.5, 44.1, 46.9, 46.1, 43.6, 43.4, 43.2, 40.3, 42.8, 42.2)
  r <- regress_data(d, "Yield", c("Dose", "Dose2", "Moist"))
  t <- reg_text(r)
  expect_match(t, sprintf("Dose's VIF (%s) is also raised by the other predictors", fmt(r$coefficients$VIF[2], 2)), fixed = TRUE)
  expect_no_match(t, "The large VIFs of Dose and Dose2", fixed = TRUE)
})

test_that("a single curve's lower powers are not read for joint and single tests", {
  d <- data.frame(Dose = rep(seq(100, 140, 10), each = 3)); d$Dose2 <- d$Dose^2
  d$Yield <- c(24.6, 25.1, 24.5, 26.5, 25.7, 25, 26.3, 26.4, 26.3, 26.3, 27.4, 26.7, 26.6, 25.7, 27.7)
  t <- reg_text(regress_data(d, "Yield", c("Dose", "Dose2")))
  expect_no_match(t, "significant together but none", fixed = TRUE)
  expect_no_match(t, "significant on its own", fixed = TRUE)
  set.seed(47); d <- data.frame(Dose = rep(seq(0, 40, 10), each = 3)); d$Dose2 <- d$Dose^2; d$Yield <- rnorm(15, 30)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  t <- reg_text(r)
  expect_no_match(t, "One slope is significant", fixed = TRUE)
  if (r$model$p >= 0.05) expect_match(t, "that Yield is related to Dose along the fitted curve", fixed = TRUE)
})

test_that("a turning point printed as the end of the range is said to be there", {
  d <- data.frame(Dose = rep(seq(0, 160, 40), each = 3)); d$Dose2 <- d$Dose^2
  d$Yield <- c(21.4, 20.7, 17.3, 27.8, 33.5, 27.3, 43.1, 39.8, 38.1, 43.7, 43.9, 41.1, 43.7, 47.3, 45.3)
  r <- regress_data(d, "Yield", c("Dose", "Dose2"))
  tp <- dfmt(-r$coefficients$Estimate[2] / (2 * r$coefficients$Estimate[3]), 1)
  if (as.numeric(tp) == 160) expect_match(reg_text(r), sprintf("= %s, where its slope is 0, at the upper end of the observed range (0 to 160).", tp), fixed = TRUE)
  expect_no_match(reg_text(r), sprintf("= %s, where its slope is 0, outside", tp), fixed = TRUE)
})

test_that("the Q-Q note names a far residual instead of saying neither end bends", {
  set.seed(1); d <- data.frame(N = rep(c(0, 40, 80, 120, 160, 200), each = 10))
  d$Yield <- round(3 + 0.012 * d$N + rnorm(60, 0, 0.3), 2); d$Yield[25] <- d$Yield[25] + 1.5
  r <- regress_data(d, "Yield", "N")
  q <- reg_qq_text(r)$reading
  expect_match(q, "Row 25 sits far above the line at the top end, as the residual plot flags.", fixed = TRUE)
  expect_no_match(q, "^Neither end")
  if (r$checks$p[1] < 0.05 && grepl("Apart from it", q, fixed = TRUE))
    expect_match(q, "the test weighs every point, row 25 among them", fixed = TRUE)
})

test_that("another curve is held at a setting that can occur", {
  set.seed(21); d <- data.frame(N = rep(c(0, 30, 60, 90, 120, 150), each = 4)); d$Rain <- round(runif(24, 300, 600))
  d$Yield <- round(2 + 0.045 * d$N - 0.00018 * d$N^2 + 0.002 * d$Rain + rnorm(24, 0, 0.25), 2); d$N2 <- d$N^2
  r <- regress_data(d, "Yield", c("N", "N2", "Rain"))
  expect_match(reg_main_text(r, "Rain")$what, "held at their means (N 75, so N2 5625)", fixed = TRUE)
  ef <- reg_effect(r, 3)
  f <- lm(Yield ~ N + N2 + Rain, d)
  expect_equal(ef$band$fit[1], unname(coef(f)[1] + coef(f)[2] * 75 + coef(f)[3] * 75^2 + coef(f)[4] * min(d$Rain)))
})

## ------------------------------------------- found in fifth review ----

test_that("a curve is judged as a whole in the joint and single tests", {
  d <- data.frame(Dose = rep(c(0, 40, 80, 120, 160), each = 3))
  d$Dose2 <- d$Dose^2
  d$Rain <- c(597, 419, 335, 321, 373, 538, 402, 592, 350, 438, 352, 369, 532, 329, 436)
  d$Yield <- c(18.9, 15, 20.2, 21.8, 27.7, 20, 25.3, 25.3, 23.2, 24.1, 29.4, 28.8, 27.8, 26.2, 27.1)
  r <- regress_data(d, "Yield", c("Dose", "Dose2", "Rain"))
  u <- reg_units(r)
  expect_identical(vapply(u, function(x) x$name, ""), c("the curve in Dose", "Rain"))
  full <- lm(Yield ~ Dose + Dose2 + Rain, d)
  expect_equal(u[[1]]$p, anova(lm(Yield ~ Rain, d), full)$`Pr(>F)`[2])
  if (u[[1]]$p < 0.05) expect_no_match(reg_text(r), "significant on its own", fixed = TRUE)
})

test_that("a quadratic in calendar years turns where the fitted curve does", {
  d <- data.frame(Year = 2006:2020); d$Year2 <- d$Year^2
  d$Yield <- c(29.7, 29.9, 32.1, 32, 32.4, 32.2, 34.1, 34.6, 33, 32.8, 33.5, 34.8, 33.8, 34.1, 34)
  r <- regress_data(d, "Yield", c("Year", "Year2"))
  f <- lm(Yield ~ Year + Year2, d)
  t0 <- -coef(f)[2] / (2 * coef(f)[3])
  expect_match(reg_text(r), sprintf("The curve is highest at Year = %s, where its slope is 0, inside the observed range (2006 to 2020).",
                                    dfmt(unname(t0), 2)), fixed = TRUE)
  b <- ggplot2::ggplot_build(plot_reg_main(r))$data[[3]]
  expect_lt(abs(b$x[which.max(b$y)] - t0), 0.2)
})

test_that("a cubic in calendar years is fitted, not refused", {
  set.seed(9); d <- data.frame(Year = 2001:2020); d$Year2 <- d$Year^2; d$Year3 <- d$Year^3
  d$Yield <- 30 + 0.2 * (d$Year - 2010) + rnorm(20)
  r <- regress_data(d, "Yield", c("Year", "Year2", "Year3"))
  op <- lm(Yield ~ poly(Year, 3), d)
  expect_equal(unname(fitted(r$fit)), unname(fitted(op)))
  expect_equal(r$coefficients$p[4], summary(op)$coefficients[4, 4])
})

test_that("the intercept and ranges are judged on the predictors, not their powers", {
  d <- data.frame(Temp = rep(c(-6, -2, 2, 6, 10), each = 3)); d$Temp2 <- d$Temp^2
  d$Survival <- c(41.2, 44, 42.5, 55.3, 57.1, 54.8, 61, 63.2, 60.4, 62.8, 60.9, 61.7, 57, 55.4, 58.3)
  t <- reg_text(regress_data(d, "Survival", c("Temp", "Temp2")))
  expect_no_match(t, "extrapolation", fixed = TRUE)
  expect_match(t, "The equation describes the data only over the observed range (Temp -6 to 10)", fixed = TRUE)
  expect_match(t, "how Survival goes with Temp in these data; it can show a cause only when the predictor was set", fixed = TRUE)
})

test_that("powers are named for what they are", {
  set.seed(4); d <- data.frame(Dose = rep(-2:2, each = 3)); d$Dose3 <- d$Dose^3
  d$Y <- round(10 + 0.5 * d$Dose - 0.3 * d$Dose3 + rnorm(15, 0, 0.8), 1)
  r <- regress_data(d, "Y", c("Dose", "Dose3"))
  t <- reg_text(r)
  expect_match(t, "Dose3 is the cube of Dose, so the two describe one curve", fixed = TRUE)
  expect_no_match(t, "square", fixed = TRUE)
  if (round(r$coefficients$VIF[2], 2) >= 5) expect_match(t, "come from Dose3 being the cube of Dose", fixed = TRUE)
  expect_match(gsub("[[:space:]]+", " ", plot_reg_main(r)$labels$title), "Y against Dose (with Dose3, its cube)", fixed = TRUE)
  expect_match(reg_main_text(r)$what, "with Dose3 moving with it as its cube", fixed = TRUE)
  # two curves: each named for its own predictor
  d <- expand.grid(Dose = c(0, 50, 100, 150), N = c(0, 40, 80, 120)); d$Dose2 <- d$Dose^2; d$N2 <- d$N^2
  set.seed(5); d$Y <- 10 + 0.1 * d$Dose - 0.0004 * d$Dose2 + 0.05 * d$N - 0.0003 * d$N2 + rnorm(16, 0, 0.3)
  t2 <- reg_text(regress_data(d, "Y", c("Dose", "N", "Dose2", "N2")))
  expect_match(t2, "come from Dose2 being the square of Dose", fixed = TRUE)
  expect_match(t2, "come from N2 being the square of N", fixed = TRUE)
  expect_no_match(t2, "powers of one predictor", fixed = TRUE)
})

test_that("a response surface is read as one surface, with its curve drawn at a setting that can occur", {
  set.seed(6)
  d <- expand.grid(N = c(0, 40, 80, 120, 160), P = c(0, 30, 60), rep = 1:2)
  d$N2 <- d$N^2; d$P2 <- d$P^2; d$NP <- d$N * d$P
  d$Yield <- round(2 + 0.03 * d$N + 0.02 * d$P - 0.00015 * d$N2 - 0.0002 * d$P2 + 0.0004 * d$NP + rnorm(30, 0, 0.15), 2)
  r <- regress_data(d, "Yield", c("N", "P", "N2", "P2", "NP"))
  expect_identical(reg_groups(r), list(c(1L, 2L, 3L, 4L, 5L)))
  t <- reg_text(r)
  expect_match(t, "N, P, N2, P2 and NP describe one surface in N and P", fixed = TRUE)
  expect_match(t, "NP is N \u00d7 P", fixed = TRUE)
  expect_match(t, "The coefficient of NP, which measures how the effect of N changes with P, is significant", fixed = TRUE)
  expect_no_match(t, "without changing its shape", fixed = TRUE)
  expect_no_match(t, "For each unit more of", fixed = TRUE)
  m <- reg_main_text(r, "N")
  expect_match(m$what, "with N2 moving with it as its square and NP as N \u00d7 P", fixed = TRUE)
  expect_match(m$what, "held at their means (P 30, so P2 900)", fixed = TRUE)
  f <- lm(Yield ~ N + P + N2 + P2 + NP, d); b <- coef(f)
  t0 <- -(b[["N"]] + b[["NP"]] * 30) / (2 * b[["N2"]])
  expect_match(m$reading, sprintf("With the other predictors at their means, the curve is highest at N = %s, where its slope is 0",
                                  dfmt(t0, 1)), fixed = TRUE)
  expect_match(m$reading, "Its shape in N changes with P (through NP), so at other values it turns elsewhere.", fixed = TRUE)
  bd <- ggplot2::ggplot_build(plot_reg_main(r, "N"))$data[[3]]
  expect_equal(bd$y, unname(b[1] + b[["N"]] * bd$x + b[["P"]] * 30 + b[["N2"]] * bd$x^2 + b[["P2"]] * 900 + b[["NP"]] * bd$x * 30))
})

test_that("the residual plot's note speaks of the fitted equation when a curve is fitted", {
  set.seed(2); d <- data.frame(Dose = rep(seq(0, 200, 25), each = 4)); d$Dose2 <- d$Dose^2
  d$Yield <- 20 + 0.2 * d$Dose - 0.0006 * d$Dose2 + rnorm(36)
  w <- reg_resid_text(regress_data(d, "Yield", c("Dose", "Dose2")))$what
  expect_match(w, "If the fitted equation suits the data", fixed = TRUE)
  expect_no_match(w, "straight line", fixed = TRUE)
})

## ------------------------------------------- found in sixth review ----

test_that("the printed equation reproduces the fitted curve, even for calendar years", {
  set.seed(1); d <- data.frame(Year = 2001:2020); d$Year2 <- d$Year^2
  d$Y <- 50 + 0.5 * (d$Year - 2010) - 0.03 * (d$Year - 2010)^2 + rnorm(20)
  r <- regress_data(d, "Y", c("Year", "Year2"))
  b <- vapply(seq_len(3), function(i) as.numeric(reg_coef_txt(r, i)), 0)
  expect_lt(max(abs(drop(cbind(1, r$X) %*% b) - fitted(r$fit))), 0.01 * r$model$RMSE)
  t <- reg_text(r)
  expect_match(t, "The coefficients carry more figures than their standard errors need", fixed = TRUE)
  expect_match(t, "Subtracting a starting value from Year (such as 2000) before working out its powers", fixed = TRUE)
  # an ordinary fit keeps the figures its standard errors support
  r0 <- regress_data(reg_data(), "Yield", "Nitrogen")
  expect_identical(r0$extra, 0L)
  expect_no_match(reg_text(r0), "more figures", fixed = TRUE)
})

test_that("a quartic in calendar years is fitted as poly() fits it", {
  set.seed(1); d <- data.frame(Year = 2001:2020); d$Year2 <- d$Year^2; d$Year3 <- d$Year^3; d$Year4 <- d$Year^4
  d$Y <- 50 + 0.5 * (d$Year - 2010) - 0.03 * (d$Year - 2010)^2 + rnorm(20)
  r <- regress_data(d, "Y", c("Year", "Year2", "Year3", "Year4"))
  op <- lm(Y ~ poly(Year, 4), d)
  expect_equal(unname(fitted(r$fit)), unname(fitted(op)), tolerance = 1e-6)
  expect_equal(r$coefficients$p[5], summary(op)$coefficients[5, 4], tolerance = 1e-6)
  # the cubic's Breusch-Pagan has a degree of freedom for each predictor
  r3 <- regress_data(d, "Y", c("Year", "Year2", "Year3"))
  expect_identical(r3$checks$df[2], "3")
})

test_that("a square rounded to the decimals shown is recognised for small doses too", {
  d <- data.frame(Dose = rep(c(0, 2.5, 5, 7.5, 10, 12.5), each = 2))
  d$Dose2 <- rep(c(0, 6.3, 25, 56.3, 100, 156.3), each = 2)
  set.seed(3); d$Y <- 10 + 2 * d$Dose - 0.1 * d$Dose^2 + rnorm(12, 0, 0.5)
  expect_identical(nrow(regress_data(d, "Y", c("Dose", "Dose2"))$built), 1L)
  # small whole numbers that happen to lie within rounding of squares are not squares
  d2 <- data.frame(A = c(1.1, 1.6, 2.1, 1.1, 1.6, 2.1), W = c(1, 3, 4, 1, 3, 4), Y = c(2, 3.1, 4.4, 2.2, 2.9, 4.6))
  expect_identical(nrow(regress_data(d2, "Y", c("A", "W"))$built), 0L)
})

test_that("a mean held for the plot is printed so its power can be checked", {
  set.seed(2); d <- data.frame(Year = 2001:2020); d$Year2 <- d$Year^2; d$Rain <- round(runif(20, 300, 600))
  d$Y <- 40 + 0.3 * (d$Year - 2010) - 0.02 * (d$Year - 2010)^2 + 0.01 * d$Rain + rnorm(20)
  r <- regress_data(d, "Y", c("Year", "Year2", "Rain"))
  expect_match(reg_main_text(r, "Rain")$what, "held at their means (Year 2010.5, so Year2 4042110)", fixed = TRUE)
})

test_that("a product without squares is drawn and described as a line", {
  d <- expand.grid(N = c(0, 40, 80, 120), P = c(0, 30, 60), rep = 1:2); d$NP <- d$N * d$P
  set.seed(7); d$Y <- 2 + 0.02 * d$N + 0.03 * d$P + 0.0002 * d$NP + rnorm(24, 0, 0.2)
  r <- regress_data(d, "Y", c("N", "P", "NP"))
  t <- reg_text(r)
  expect_match(t, "The main plot draws the fitted line for N or P", fixed = TRUE)
  m <- reg_main_text(r, "N")
  expect_match(m$reading, "With the other predictors at their means, the slope is", fixed = TRUE)
  expect_match(m$reading, "it changes with P (through NP)", fixed = TRUE)
})
