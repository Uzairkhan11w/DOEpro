test_that("CRD gives the textbook degrees of freedom", {
  d <- demo_data("CRD")
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  t <- nlevels(factor(d$Treatment)); n <- nrow(d)
  expect_equal(r$anova$Df[r$anova$Source == "Treatment"], t - 1)
  expect_equal(r$dfe, n - t)
  expect_equal(r$anova$Df[r$anova$Source == "Total"], n - 1)
})

test_that("RCBD partitions the block sum of squares out of the error", {
  d <- demo_data("RCBD")
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  t <- nlevels(factor(d$Variety)); b <- nlevels(factor(d$Block))
  expect_equal(r$anova$Df[r$anova$Source == "Variety"], t - 1)
  expect_equal(r$anova$Df[r$anova$Source == "Block"], b - 1)
  expect_equal(r$dfe, (t - 1) * (b - 1))
  # the ANOVA must agree with a direct aov() fit
  a <- stats::anova(stats::lm(Yield ~ Block + Variety, data = d))
  expect_equal(r$anova$SS[r$anova$Source == "Variety"], a["Variety", "Sum Sq"],
               tolerance = 1e-8)
})

test_that("Latin square removes both row and column effects", {
  d <- demo_data("LSD")
  r <- analyze(d, "LSD", list(response = "Yield", treat = "Treatment",
                              row = "Row", col = "Column"))
  k <- nlevels(factor(d$Treatment))
  expect_equal(r$dfe, (k - 1) * (k - 2))
})

test_that("a factorial partitions into main effects and the interaction", {
  d <- demo_data("FRCBD")
  r <- analyze(d, "FRCBD", list(response = "Yield",
                                factors = c("Nitrogen", "Variety"), block = "Block"))
  a <- nlevels(factor(d$Nitrogen)); b <- nlevels(factor(d$Variety))
  expect_equal(r$anova$Df[r$anova$Source == "Nitrogen"], a - 1)
  expect_equal(r$anova$Df[r$anova$Source == "Variety"], b - 1)
  expect_equal(r$anova$Df[r$anova$Source == "Nitrogen:Variety"], (a - 1) * (b - 1))
  expect_length(r$effects, 3)
})

test_that("a split plot keeps two error strata and four standard errors", {
  d <- demo_data("SPLIT")
  r <- analyze(d, "SPLIT", list(response = "Yield", rep = "Rep",
                                main = "Irrigation", sub = "Variety"))
  expect_true(any(grepl("Error \\(a\\)", r$anova$Source)))
  expect_true(any(grepl("Error \\(b\\)", r$anova$Source)))
  # the sub-plot error must have more degrees of freedom than the main-plot error
  ea <- r$anova$Df[grepl("Error \\(a\\)", r$anova$Source)]
  eb <- r$anova$Df[grepl("Error \\(b\\)", r$anova$Source)]
  expect_gt(eb, ea)
  # the interaction effect carries the four distinct comparisons
  eI <- r$effects[[3]]
  expect_true(length(eI$extra) >= 4)
})

test_that("pooled RCBD tests each term against the right error", {
  d <- demo_data("POOLRCBD")
  r <- analyze(d, "POOLRCBD", list(response = "Yield", env = "Location",
                                   rep = "Rep", treat = "Variety"))
  a <- r$anova
  ms <- stats::setNames(a$MS, a$Source)
  # environment is tested against replications within environment
  f_env <- a$F[grepl("^Environment", a$Source)]
  expect_equal(f_env, unname(ms[grepl("^Environment", names(ms))] /
                             ms["Replication within environment"]),
               tolerance = 1e-8)
  # the treatment is tested against the treatment by environment interaction
  f_trt <- a$F[grepl("^Treatment", a$Source)]
  expect_equal(f_trt, unname(ms[grepl("^Treatment", names(ms))] /
                             ms[grepl(" x ", names(ms))]),
               tolerance = 1e-8)
})

test_that("a pooled factorial tests every effect against its own interaction", {
  d <- demo_data("POOLFACT")
  r <- analyze(d, "POOLFRCBD", list(response = "Yield", env = "Location",
                                    rep = "Rep", factors = c("Nitrogen", "Variety")))
  a <- r$anova
  ms <- stats::setNames(a$MS, a$Source)
  for (trt in c("Nitrogen", "Variety", "Nitrogen x Variety")) {
    inter <- paste("Location x", trt)
    expect_equal(a$F[a$Source == trt], unname(ms[trt] / ms[inter]),
                 tolerance = 1e-8,
                 info = paste(trt, "must be tested against", inter))
  }
  # and every interaction with the environment against the pooled error
  for (inter in c("Location x Nitrogen", "Location x Variety",
                  "Location x Nitrogen x Variety")) {
    expect_equal(a$F[a$Source == inter], unname(ms[inter] / ms["Pooled error"]),
                 tolerance = 1e-8)
  }
})

test_that("the sums of squares add up to the total", {
  for (nm in c("CRD", "RCBD", "FRCBD", "SPLIT")) {
    map <- switch(nm,
      CRD   = list(response = "Yield", treat = "Treatment"),
      RCBD  = list(response = "Yield", treat = "Variety", block = "Block"),
      FRCBD = list(response = "Yield", factors = c("Nitrogen", "Variety"), block = "Block"),
      SPLIT = list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
    d <- demo_data(nm)
    r <- analyze(d, nm, map)
    a <- r$anova
    tot <- a$SS[a$Source == "Total"]
    parts <- sum(a$SS[a$Source != "Total"], na.rm = TRUE)
    expect_equal(parts, tot, tolerance = 1e-6, info = nm)
  }
})
