## The Means & Comparisons tab: what each comparison shows, and that the
## standard errors at the foot of the tables are those of the design.

sup_re <- "<sup>[a-z]+</sup>"
DESIGN_RUNS <- list(
  CRD   = list("CRD", list(treat = "Treatment"), "Yield"),
  RCBD  = list("RCBD", list(treat = "Variety", block = "Block"), "Yield"),
  FCRD  = list("FCRD", list(factors = c("Variety", "Nitrogen")), c("Yield", "Plant_height")),
  SPLIT = list("SPLIT", list(rep = "Rep", main = "Irrigation", sub = "Variety"), "Yield"),
  STRIP = list("STRIP", list(rep = "Rep", main = "Tillage", sub = "Mulch"), "Yield"),
  POOLFACT = list("POOLFRCBD", list(env = "Location", rep = "Rep", factors = c("Nitrogen", "Variety")), "Yield"))
run_demo <- function(key, alpha = 0.05) {
  s <- DESIGN_RUNS[[key]]
  run_all(demo_data(key), s[[1]], s[[2]], s[[3]], alpha = alpha)
}

## ------------------------------------------------- one comparison at a time --

test_that("letters and a C.D. are never shown together", {
  for (key in names(DESIGN_RUNS)) {
    rr <- run_demo(key)
    for (m in c("none", PH_METHODS)) {
      cmp <- comparison(m, effect = rr$facs[1])
      h <- means_section_html(rr, 2, cmp, detailed = TRUE, readings = TRUE)
      has_letters <- grepl(sup_re, h)
      has_cd <- grepl("C.D. (5%)", h, fixed = TRUE)
      expect_false(has_letters && has_cd, info = paste(key, m))
      if (cmp$kind != "letters") expect_false(has_letters, info = paste(key, m))
      if (cmp$kind != "cd") expect_false(has_cd, info = paste(key, m))
      if (cmp$kind == "cd") expect_true(has_cd, info = paste(key, m))
    }
  }
})

test_that("the default shows the means alone: no letters, no C.D., no plus-or-minus beside a mean", {
  rr <- run_demo("CRD")
  h <- sketch_main_html(rr$fits, "Treatment")
  expect_no_match(h, sup_re)
  expect_no_match(h, "C.D.", fixed = TRUE)
  m <- rr$fits$Yield$final$effects$Treatment$means
  for (i in seq_len(nrow(m)))
    expect_match(h, sprintf("<td>%s</td><td>%s</td>", m$Treatment[i], fmt(m$Mean[i], 2)), fixed = TRUE)
  # SE(m) and SE(d) stay at the foot
  expect_match(h, "SE(m) &plusmn;", fixed = TRUE)
  expect_match(h, "SE(d) &plusmn;", fixed = TRUE)
  expect_identical(comparison()$kind, "none")
})

test_that("a letter procedure's letters are its own, and only under a significant F-test", {
  rr <- run_demo("RCBD")
  f <- rr$fits$Yield$final
  for (m in c("Tukey HSD", "Duncan's DMRT", "Student-Newman-Keuls", "Scheffe", "LSD (Bonferroni-adjusted)")) {
    L <- cmp_letters(f, "Variety", comparison(m))
    x <- gate_posthoc(posthoc(f, "Variety", m, f$alpha))
    ## the posthoc table is in order of the means; the letters follow the
    ## table of means
    got <- stats::setNames(L$letters, as.character(f$effects$Variety$means$Variety))
    want <- stats::setNames(x$groups$Group, x$groups$Treatment)
    expect_identical(got[names(want)], want, info = m)
    h <- sketch_main_html(rr$fits, "Variety", 2, comparison(m))
    expect_match(h, sprintf("letters from %s at the 5%% level", CMP_NAME[[m]]), fixed = TRUE, info = m)
  }
  # at 1% the F-test for this example is not significant: no letters, and the note says why
  rr01 <- run_demo("RCBD", 0.01)
  e01 <- rr01$fits$Yield$final$effects$Variety
  if (!effect_sig(e01)) {
    h <- sketch_main_html(rr01$fits, "Variety", 2, comparison("Tukey HSD"))
    expect_no_match(h, sup_re)
    expect_match(h, "No letters: the F-test for Variety is not significant at the 1% level.", fixed = TRUE)
  }
})

test_that("the C.D. comparison prints the critical difference of the design, labelled with the level", {
  for (a in c(0.05, 0.01)) {
    rr <- run_demo("CRD", a)
    e <- rr$fits$Yield$final$effects$Treatment
    h <- sketch_main_html(rr$fits, "Treatment", 2, comparison("LSD (Fisher's protected)"))
    lab <- sprintf("C.D. (%s%%)", pct(a))
    expect_match(h, lab, fixed = TRUE)
    if (effect_sig(e)) {
      expect_equal(e$cd, qt(1 - a / 2, e$df) * e$sed, tolerance = 1e-12)
      expect_match(h, sprintf("<td>%s</td><td>%s</td>", lab, err_text(e, "cd", 2)), fixed = TRUE)
    } else expect_match(h, sprintf("<td>%s</td><td>NS</td>", lab), fixed = TRUE)
  }
})

## --------------------------------------------------- the design's errors --

test_that("the SE(m), SE(d) and C.D. at the foot are the design's own", {
  ms <- function(an, src) an$MS[an$Source == src]
  ## CRD, four replications
  f <- run_demo("CRD")$fits$Yield$final; e <- f$effects$Treatment; r <- 4
  expect_equal(c(e$sem, e$sed), c(sqrt(f$mse / r), sqrt(2 * f$mse / r)), tolerance = 1e-12)
  ## RCBD: one plot of each variety per block
  f <- run_demo("RCBD")$fits$Yield$final; e <- f$effects$Variety; b <- nlevels(f$data$Block)
  expect_equal(c(e$sem, e$sed), c(sqrt(f$mse / b), sqrt(2 * f$mse / b)), tolerance = 1e-12)
  ## factorial CRD: a main effect's mean averages r x (levels of the other factor) plots
  f <- run_demo("FCRD")$fits$Yield$final
  expect_equal(f$effects$Variety$sed, sqrt(2 * f$mse / (4 * 3)), tolerance = 1e-12)
  expect_equal(f$effects[["Variety:Nitrogen"]]$sed, sqrt(2 * f$mse / 4), tolerance = 1e-12)
  ## split plot: four different standard errors of a difference (Gomez and Gomez, 1984)
  f <- run_demo("SPLIT")$fits$Yield$final; an <- f$anova
  Ea <- ms(an, "Error (a)"); Eb <- ms(an, "Error (b)")
  a <- nlevels(f$data$Irrigation); b <- nlevels(f$data$Variety); r <- nlevels(f$data$Rep)
  expect_equal(f$effects$Irrigation$sed, sqrt(2 * Ea / (r * b)), tolerance = 1e-12)
  expect_equal(f$effects$Variety$sed, sqrt(2 * Eb / (r * a)), tolerance = 1e-12)
  eI <- f$effects[["Irrigation:Variety"]]
  expect_equal(eI$sed, sqrt(2 * Eb / r), tolerance = 1e-12)                       # B within the same A
  expect_equal(eI$extra[["SE(d): two main-plot means at the same sub plot"]],
               sqrt(2 * ((b - 1) * Eb + Ea) / (r * b)), tolerance = 1e-12)       # A at the same B
  ## and the two-way table prints them, the second one listed as well
  h <- sketch_twoway_html(list(final = f, trans = "none", header = "Yield"), "Irrigation", "Variety", 3,
                          comparison("LSD (Fisher's protected)"))
  expect_match(h, sprintf("SE(d) &plusmn; &nbsp; %s / %s / %s", fmt(f$effects$Irrigation$sed, 3),
                          fmt(f$effects$Variety$sed, 3), fmt(eI$sed, 3)), fixed = TRUE)
  expect_match(h, "two main-plot means at the same sub plot", fixed = TRUE)
  ## strip plot: each factor against its own error
  f <- run_demo("STRIP")$fits$Yield$final; an <- f$anova
  a <- nlevels(f$data$Tillage); b <- nlevels(f$data$Mulch); r <- nlevels(f$data$Rep)
  expect_equal(f$effects$Tillage$sed, sqrt(2 * ms(an, "Error (a)") / (r * b)), tolerance = 1e-12)
  expect_equal(f$effects$Mulch$sed, sqrt(2 * ms(an, "Error (b)") / (r * a)), tolerance = 1e-12)
})

test_that("with unequal replication each mean keeps its own SE and the foot gives the range", {
  d <- demo_data("CRD")
  d <- d[-c(1, 2, 9), ]
  rr <- run_all(d, "CRD", list(treat = "Treatment"), "Yield")
  f <- rr$fits$Yield$final; e <- f$effects$Treatment
  expect_false(e$equal_rep)
  expect_equal(e$means$SE, sqrt(f$mse / e$means$N), tolerance = 1e-12)
  expect_true(is.na(e$sem) && is.na(e$sed))
  h <- sketch_main_html(rr$fits, "Treatment", 2)
  expect_match(h, sprintf("<td>SE(m) &plusmn;</td><td>%s</td>", err_text(e, "sem", 2)), fixed = TRUE)
  expect_match(err_text(e, "sem", 2), "&ndash;", fixed = TRUE)
  # the CSV gives each mean its own SE
  csv <- means_csv(rr, comparison("LSD (Fisher's protected)"))
  rows <- csv[csv$Effect == "Treatment", ]
  expect_equal(rows$SEm, e$means$SE, tolerance = 1e-12)
  expect_true(all(is.na(rows$SEd)))
})

## ------------------------------------------------------- simple effects --

test_that("an interaction's cell means can be compared within each level of a factor", {
  f <- run_demo("FCRD")$fits$Yield$final
  eI <- f$effects[["Variety:Nitrogen"]]
  x <- posthoc(f, "Variety:Nitrogen", "Tukey HSD", 0.05, by = "Nitrogen")
  # one family per nitrogen level, each comparing the three varieties
  expect_length(x$families, 3)
  expect_identical(vapply(x$families, `[[`, "", "within"), levels(f$data$Nitrogen))
  expect_true(all(vapply(x$families, function(fm) length(fm$mu), 0) == 3))
  # Tukey's range for three means on the error d.f., not for nine
  crit <- unique(round(x$pairs$`Critical value`, 10))
  expect_equal(crit, round(qtukey(0.95, 3, eI$df) / sqrt(2), 10))
  expect_equal(x$pairs$`Critical difference`, x$pairs$`Critical value` * eI$sed, tolerance = 1e-12)
  expect_match(paste(x$note, collapse = " "), "(simple effects), as chosen", fixed = TRUE)
  # all the cells together: Tukey's range for nine means
  x9 <- posthoc(f, "Variety:Nitrogen", "Tukey HSD", 0.05)
  expect_equal(unique(round(x9$pairs$`Critical value`, 10)), round(qtukey(0.95, 9, eI$df) / sqrt(2), 10))
  # a split plot's interaction stays within its main plots whatever is asked
  s <- run_demo("SPLIT")$fits$Yield$final
  xs <- posthoc(s, "Irrigation:Variety", "Tukey HSD", 0.05, by = "Variety")
  expect_identical(xs$by, "Irrigation")
})

test_that("the two-way table's letters say what they compare", {
  rr <- run_demo("FCRD")
  fit <- rr$fits$Yield
  h <- sketch_twoway_html(fit, "Variety", "Nitrogen", 2, comparison("Tukey HSD"))
  expect_match(h, "Letters compare every Variety &times; Nitrogen cell with every other.", fixed = TRUE)
  expect_match(h, sup_re)
  hs <- sketch_twoway_html(run_demo("SPLIT")$fits$Yield, "Irrigation", "Variety", 2, comparison("Tukey HSD"))
  expect_match(hs, "Letters compare Variety means within the same level of Irrigation only", fixed = TRUE)
})

## -------------------------------------------------------------- readings --

test_that("each comparison's reading follows that comparison", {
  rr <- run_demo("RCBD"); fit <- rr$fits$Yield
  none <- cmp_means_text(fit, "Variety", comparison())
  expect_match(none, "Choose a comparison method to see which of them differ.", fixed = TRUE)
  expect_no_match(none, "C.D.", fixed = TRUE)
  cd <- cmp_means_text(fit, "Variety", comparison("LSD (Fisher's protected)"))
  expect_match(cd, "C.D.", fixed = TRUE)
  tk <- cmp_means_text(fit, "Variety", comparison("Tukey HSD"))
  expect_match(tk, "by this test", fixed = TRUE)
  expect_no_match(tk, "C.D.", fixed = TRUE)
  # the interpretation's advice on reporting follows it too
  i_tk <- interpret(fit$final, fit$asm, fit$sug, "None", comparison("Tukey HSD"))
  expect_match(i_tk, "Do not add a C.D. to the same table", fixed = TRUE)
  i_cd <- interpret(fit$final, fit$asm, fit$sug, "None", comparison("LSD (Fisher's protected)"))
  expect_match(i_cd, "Do not add letters from another procedure", fixed = TRUE)
  expect_no_match(interpret(fit$final, fit$asm, fit$sug, "None", comparison()), "Means followed by a common letter", fixed = TRUE)
})

## ---------------------------------------------------------------- exports --

test_that("the CSV of the means carries the comparison's own column only", {
  rr <- run_demo("FCRD")
  n <- means_csv(rr, comparison())
  expect_false(any(c("CD", "Letters") %in% names(n)))
  c1 <- means_csv(rr, comparison("LSD (Fisher's protected)"))
  expect_true("CD" %in% names(c1)); expect_false("Letters" %in% names(c1))
  t1 <- means_csv(rr, comparison("Tukey HSD"))
  expect_true("Letters" %in% names(t1)); expect_false("CD" %in% names(t1))
  expect_identical(unique(t1$Comparison), "Tukey's HSD")
  # SD (of the observations) and SEm (of the mean) are separate columns
  e <- rr$fits$Yield$final$effects$Variety
  r1 <- t1[t1$Response == "Yield" & t1$Effect == "Variety", ]
  expect_equal(r1$SD, e$means$SD); expect_equal(r1$SEm, e$means$SE)
  expect_true(all(r1$SD > r1$SEm))
})

test_that("comparison() refuses a procedure it does not know", {
  expect_error(comparison("Fisher"), "Unknown comparison")
  expect_identical(comparison("Dunnett", control = "T1")$kind, "dunnett")
})
