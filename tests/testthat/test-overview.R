## The Data tab's overview: the examples and their roles, the quick summary
## of the responses, the structure of every column and the screening.

## --------------------------------------------------- the factorial CRD --

test_that("the factorial CRD example is a balanced two-factor experiment with two responses", {
  d <- demo_data("FCRD")
  expect_identical(names(d), c("Variety", "Nitrogen", "Rep", "Yield", "Plant_height"))
  expect_equal(nrow(d), 36)
  # every Variety x Nitrogen cell has the same four replications
  expect_true(all(table(d$Variety, d$Nitrogen) == 4))
  expect_true(is.numeric(d$Yield) && is.numeric(d$Plant_height))
  expect_false(anyNA(d))
  # the same data on every machine
  expect_identical(demo_data("FCRD"), d)
})

test_that("the factorial CRD example analyses as a factorial CRD", {
  dm <- DEMO_MAPS$FCRD
  expect_identical(dm$design, "FCRD")
  rr <- run_all(demo_data("FCRD"), dm$design, dm$map, dm$response)
  expect_identical(names(rr$fits), c("Yield", "Plant_height"))
  y <- rr$fits$Yield$final; h <- rr$fits$Plant_height$final
  an <- y$anova
  # one error term, d.f. as a 3 x 3 factorial with four replications: 2, 2, 4, 27
  expect_equal(an$Df[match(c("Variety", "Nitrogen", "Variety:Nitrogen", "Residuals"), an$Source)], c(2, 2, 4, 27))
  # against an independent fit
  ref <- anova(lm(Yield ~ Variety * Nitrogen, data = demo_data("FCRD")))
  expect_equal(an$F[1:3], ref$`F value`[1:3], tolerance = 1e-10)
  expect_equal(an$p[1:3], ref$`Pr(>F)`[1:3], tolerance = 1e-10)
  # yield: both factors and their interaction; height: both factors, no interaction
  expect_true(all(an$p[1:3] < 0.05))
  hp <- h$anova$p
  expect_true(all(hp[1:2] < 0.05))
  expect_gt(hp[3], 0.05)
  # the means of a main effect average four replications at three levels of the other factor
  e <- y$effects$Variety
  expect_equal(e$means$N, rep(12, 3))
  expect_equal(e$sem, sqrt(y$mse / 12), tolerance = 1e-12)
  expect_equal(e$sed, sqrt(2 * y$mse / 12), tolerance = 1e-12)
  eI <- y$effects[["Variety:Nitrogen"]]
  expect_equal(eI$sed, sqrt(2 * y$mse / 4), tolerance = 1e-12)
})

test_that("the examples are described as demonstration data, not observations", {
  expect_match(DEMO_NOTE, "synthetic", fixed = TRUE)
  expect_match(DEMO_NOTE, "not observations from a real experiment", fixed = TRUE)
  expect_true("FCRD" %in% DEMO_CHOICES)
})

test_that("every example's roles analyse without error", {
  for (key in names(DEMO_MAPS)) {
    dm <- DEMO_MAPS[[key]]
    d <- demo_data(key)
    used <- c(unlist(dm$map), dm$response)
    expect_true(all(used %in% names(d)), info = key)
    rr <- run_all(d, dm$design, dm$map, dm$response)
    expect_identical(names(rr$fits), dm$response, info = key)
  }
})

## ------------------------------------------------------ the quick summary --

test_that("the quick summary matches base R and shows the SE of the mean", {
  d <- demo_data("FCRD")
  d$Yield[c(3, 7)] <- NA
  tab <- quick_describe(d, c("Yield", "Plant_height"))
  y <- d$Yield[!is.na(d$Yield)]
  r <- tab[tab$Response == "Yield", ]
  expect_identical(r$n, length(y))
  expect_identical(r$Missing, 2L)
  expect_equal(r$Mean, mean(y)); expect_equal(r$SD, sd(y))
  expect_equal(r$SE, sd(y) / sqrt(length(y)))
  expect_equal(r$Median, median(y))
  expect_equal(c(r$Q1, r$Q3), unname(quantile(y, c(0.25, 0.75), type = 7)))
  expect_equal(c(r$Min, r$Max), range(y))
  expect_equal(r$CV, 100 * sd(y) / mean(y))
  # the table says what SE is, and that missing values are not zeros
  h <- quick_describe_html(tab, d)
  expect_match(h, "SE of mean", fixed = TRUE)
  expect_match(h, "never treated as zeros", fixed = TRUE)
  expect_match(h, "not the standard error of a treatment mean", fixed = TRUE)
})

test_that("the quick summary gives no CV where it means nothing, and no SE from one value", {
  d <- data.frame(T = c("a", "b", "c", "d"), Change = c(-2, 3, -1, 4), One = c(5, NA, NA, NA))
  tab <- quick_describe(d, c("Change", "One"))
  expect_true(is.na(tab$CV[tab$Response == "Change"]))      # negative values
  expect_true(is.na(tab$SE[tab$Response == "One"]))         # a single value
  expect_match(quick_describe_html(tab, d), "&ndash;", fixed = TRUE)
})

## --------------------------------------------------- the column structure --

test_that("every column is described by its role, its storage and its kind", {
  d <- demo_data("RCBD")
  d$Plot <- seq_len(nrow(d))
  d$Note <- c("n/a", rep("ok", nrow(d) - 1))
  d$Yield[2] <- NA
  map <- list(response = "Yield", block = "Block", treat = "Variety")
  s <- data_structure(d, map, "RCBD")
  expect_identical(s$Variable, names(d))
  row <- function(v) s[s$Variable == v, ]
  expect_identical(row("Yield")$Role, "Response")
  expect_identical(row("Yield")$Type, "Continuous")
  expect_identical(row("Block")$Role, "Block")
  expect_identical(row("Variety")$Role, "Treatment factor")
  expect_identical(row("Variety")$Type, "Categorical (design factor)")
  expect_identical(row("Plot")$Type, "Identifier")
  # counts: totals, missing (an empty cell and a marker such as n/a), distinct
  expect_identical(row("Yield")$Total, nrow(d))
  expect_identical(row("Yield")$Missing, 1L)
  expect_identical(row("Yield")$Observed, nrow(d) - 1L)
  expect_equal(row("Yield")$Missing_pct, 100 / nrow(d))
  expect_identical(row("Note")$Missing, 1L)
  expect_identical(row("Variety")$Distinct, nlevels(factor(d$Variety)))
  h <- data_structure_html(s)
  for (k in c("Variable", "Assigned role", "Storage type", "Analytical type", "Total N", "Non-missing N",
              "Missing N", "Missing (%)", "Distinct values"))
    expect_match(h, k, fixed = TRUE)
})

test_that("the kind of a column follows its role, not how it is stored", {
  d <- data.frame(Rep = rep(1:4, each = 3), Dose = rep(c(0, 40, 80), 4), Count = c(3, 5, 9, 2, 6, 8, 4, 4, 10, 3, 7, 9))
  s <- data_structure(d, list(response = "Count", block = "Rep", treat = "Dose"), "RCBD")
  # numbers that label blocks and doses are categorical once mapped as design factors
  expect_identical(s$Type[s$Variable == "Rep"], "Categorical (design factor)")
  expect_identical(s$Type[s$Variable == "Dose"], "Categorical (design factor)")
  expect_identical(s$Type[s$Variable == "Count"], "Integer / count")
  # the same columns with nothing mapped are not called measurements
  s0 <- data_structure(d)
  expect_identical(s0$Role[s0$Variable == "Rep"], "Not used (named like a design column)")
  expect_identical(s0$Type[s0$Variable == "Count"], "Integer (count or code)")
})

## ---------------------------------------------------------- first guesses --

test_that("the first guess names the design columns by their names", {
  g <- guess_map(demo_data("RCBD"), c("block", "treat"))
  expect_identical(g$resp, "Yield")
  expect_identical(g$block, "Block")
  expect_identical(g$treat, "Variety")
  d <- data.frame(Location = rep(c("L1", "L2"), each = 12), Rep = rep(rep(1:3, each = 4), 2),
                  Variety = rep(paste0("V", 1:4), 6), Yield = round(30 + seq_len(24) / 7, 2))
  g <- guess_map(d, c("env", "rep", "treat"))
  expect_identical(unlist(g[c("resp", "env", "rep", "treat")]),
                   c(resp = "Yield", env = "Location", rep = "Rep", treat = "Variety"))
  # numeric doses with few values are a factor, not the response
  d <- data.frame(Nitrogen = rep(c(0, 40, 80), each = 4), Variety = rep(c("A", "B"), 6),
                  Yield = c(31.2, 33.4, 35.1, 36.0, 38.2, 39.9, 41.5, 40.2, 44.8, 45.1, 47.3, 46.9))
  g <- guess_map(d, c("f1", "f2"))
  expect_identical(g$resp, "Yield")
  expect_setequal(c(g$f1, g$f2), c("Nitrogen", "Variety"))
})

## -------------------------------------------------------------- screening --

test_that("the screening reads only the responses it is given, from the model for the design", {
  d <- demo_data("FCRD")
  map <- DEMO_MAPS$FCRD$map
  bf <- list(Yield = fit_base(d, "FCRD", map, "Yield"))
  s <- screen_rows(bf)
  expect_identical(s$Response, "Yield")
  a <- bf$Yield$asm0
  # the normality check is of the model's residuals, not of the raw values
  expect_equal(a$p_norm, shapiro.test(residuals(lm(Yield ~ Variety * Nitrogen, d)))$p.value)
  h <- screening_html(s, 0.05)
  expect_match(h, "uses the model's residuals, not the raw values", fixed = TRUE)
  expect_match(h, "at the 5% level", fixed = TRUE)
  expect_no_match(h, "Plant_height", fixed = TRUE)
  # a response that cannot be analysed is named with its reason
  bad <- screen_rows(list(Yield = list(resp = "Yield", err = "Only 2 rows can be analysed")))
  expect_false(bad$ok)
  expect_match(screening_html(bad, 0.05), "Only 2 rows can be analysed", fixed = TRUE)
})
