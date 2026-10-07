## Regression tests for problems found in review of the unequal-replication and
## significance-level changes. Each test names the problem it guards against.

test_that("letters for a constant C.D. are the same from the sweep and the general algorithm", {
  # the sweep is the fast path; Piepho's algorithm must agree with it wherever
  # one critical difference applies to every pair
  set.seed(11)
  for (i in 1:300) {
    k  <- sample(2:15, 1)
    mu <- round(stats::rnorm(k, 20, 4), sample(0:2, 1))
    cd <- stats::runif(1, 0.5, 6)
    sig <- abs(outer(mu, mu, "-")) > cd
    diag(sig) <- FALSE
    expect_identical(cld_sweep(mu, sig), cld_from_sig(mu, sig))
  }
})

test_that("many means are analysed quickly", {
  # a 120-cell factorial once took minutes because every letter display went
  # through a double loop
  skip_on_cran()
  d <- expand.grid(Rep = 1:3, A = paste0("a", 1:5), B = paste0("b", 1:4),
                   C = paste0("c", 1:3), D = paste0("d", 1:2))
  d$Y <- 50 + as.integer(d$A) + 0.5 * as.integer(d$B) + sin(seq_len(nrow(d)))
  t0 <- proc.time()[["elapsed"]]
  r <- analyze(d, "FCRD", list(response = "Y", factors = c("A", "B", "C", "D")))
  expect_lt(proc.time()[["elapsed"]] - t0, 15)
  expect_length(r$effects, 15)
})

test_that("a design factor named like a column of the means table is refused plainly", {
  d <- demo_data("FRCBD")
  names(d)[names(d) == "Nitrogen"] <- "N"
  expect_error(analyze(d, "FRCBD", list(response = "Yield", factors = c("N", "Variety"),
                                         block = "Block")),
               "called 'N'.*rename", fixed = FALSE)
})

test_that("level names containing ' : ' do not merge two cells", {
  d <- data.frame(A = rep(c("p", "p : q"), each = 6),
                  B = rep(c("q : r", "r", "q : r", "r"), c(2, 4, 3, 3)))
  d$Y <- c(12.5, 12.8, 10, 10.4, 9.8, 10.2, 13, 13.3, 12.6, 11.9, 12.1, 12.4)
  r <- analyze(d, "FCRD", list(response = "Y", factors = c("A", "B")))
  e <- r$effects[["A:B"]]
  cells <- stats::aggregate(d$Y, d[c("A", "B")], mean)
  expect_equal(sort(e$means$Mean), sort(cells$x), tolerance = 1e-10)
  # each cell keeps its own SE, sqrt(MSE / n)
  n <- table(interaction(d$A, d$B, drop = TRUE))
  expect_equal(sort(e$means$SE), sort(as.vector(sqrt(r$mse / n))), tolerance = 1e-10)
  expect_true(all(e$sed_mat[row(e$sed_mat) != col(e$sed_mat)] > 0))
})

test_that("post-hoc letters and verdicts are withheld when the F-test is not significant", {
  d <- data.frame(Treatment = rep(c("A", "B", "C"), each = 4),
                  Yield = c(10, 12, 11, 13, 11, 13, 12, 10, 12, 11, 13, 12))
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_false(effect_sig(r$effects$Treatment))
  for (m in PH_METHODS) {
    x <- gate_posthoc(posthoc(r, "Treatment", m))
    expect_true(all(x$groups$Group == ""), info = m)
    expect_true(all(startsWith(x$pairs$Significant, "Not declared")), info = m)
    expect_match(x$note[1], "not significant at the 5% level", fixed = TRUE)
  }
})

test_that("each further C.D. of a pooled interaction is gated by its own F-test", {
  d <- demo_data("POOLRCBD")
  r <- analyze(d, "POOLRCBD", list(response = "Yield", env = "Location", rep = "Rep",
                                   treat = "Variety"), alpha = 0.01)
  e <- r$effects[["Location:Variety"]]
  xt <- extra_text(e)
  p_trt <- r$effects$Variety$p
  over <- grep("over all environments", names(xt))
  cd_over <- over[startsWith(names(xt)[over], "C.D.")]
  expect_identical(unname(xt[cd_over] == "NS"), !(p_trt < 0.01))
  # an SE(d) is always shown
  expect_false(any(xt[startsWith(names(xt), "SE(d)")] == "NS"))
})

test_that("an unbalanced one-way CRD is not described as adjusted", {
  d <- demo_data("CRD")[-c(1, 2), ]
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_false(r$balanced)
  expect_false(isTRUE(r$adjusted_ss))
  a <- r$anova
  expect_equal(sum(a$SS[a$Source != "Total"]), a$SS[a$Source == "Total"], tolerance = 1e-8)
  expect_null(r$effects$Treatment$means$Raw_mean)
})

test_that("the interpretation reports a significant environment x factor interaction", {
  d <- demo_data("POOLFACT")
  bump <- ifelse(d$Nitrogen == "N1" & d$Location == "Zampathan", -4, 0)
  d$Yield <- d$Yield + bump
  rr <- run_all(d, "POOLFRCBD", list(env = "Location", rep = "Rep",
                                     factors = c("Nitrogen", "Variety")), "Yield")
  f <- rr$fits$Yield
  row <- f$final$anova[f$final$anova$Source == "Location x Nitrogen", ]
  expect_lt(row$p, 0.05)
  txt <- interpret(f$final, f$asm, f$sug)
  expect_match(txt, "Location x Nitrogen</b> is significant at the 5% level", fixed = TRUE)
  expect_false(grepl("No interaction was significant", txt, fixed = TRUE))
})

test_that("a non-significant interaction is worded as an interaction", {
  d <- demo_data("FRCBD")
  r <- run_all(d, "FRCBD", list(factors = c("Nitrogen", "Variety"), block = "Block"),
               "Yield", alpha = 0.001)$fits$Yield
  e <- r$final$effects[["Nitrogen:Variety"]]
  skip_if(effect_sig(e))
  txt <- interpret(r$final, r$asm, r$sug)
  expect_match(txt, "insufficient evidence at the 0.1% level that the effect of Nitrogen depends on the level of Variety",
               fixed = TRUE)
})

test_that("range tests on one error degree of freedom stop with a plain message", {
  d <- data.frame(Block = rep(c("B1", "B2"), each = 2), Variety = rep(c("V1", "V2"), 2),
                  Yield = c(10, 12, 11, 14))
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  expect_equal(r$dfe, 1)
  for (m in c("Tukey HSD", "Student-Newman-Keuls", "Duncan's DMRT"))
    expect_error(posthoc(r, "Variety", m), "at least 2 error degrees of freedom", fixed = TRUE)
  expect_silent(posthoc(r, "Variety", "Scheffe"))
})

test_that("a factor with a single level is refused plainly", {
  d <- data.frame(T = "A", Y = c(1, 2, 3, 4))
  expect_error(analyze(d, "CRD", list(response = "Y", treat = "T")), "only one level")
})

test_that("Duncan's critical ranges never shrink as more means are spanned", {
  d <- data.frame(Treatment = factor(rep(sprintf("T%02d", 1:16), each = 2)))
  d$Yield <- 30 + rep(seq(0, 7.5, by = 0.5), each = 2) + rep(c(-0.4, 0.4), 16)
  d <- d[-(1:2), ][1:22, ]                              # few error df, many means
  r <- analyze(droplevels(d), "CRD", list(response = "Yield", treat = "Treatment"))
  skip_if(!isTRUE(r$effects$Treatment$equal_rep))
  cr <- posthoc(r, "Treatment", "Duncan's DMRT")$ranges[["Critical range"]]
  expect_true(all(diff(cr) >= -1e-12))
})

test_that("p-values in text are never in scientific notation", {
  expect_identical(p_text(c(0.5, 0.0373, 2e-7, NA)), c("0.5", "0.0373", "< 0.0001", "-"))
  expect_identical(p_eq(2e-7), "p &lt; 0.0001")
  expect_identical(p_lab(0.001 / 5), "0.0002")
})

## ------------------------------------------- level order and the X-axis ----

test_that("signed numbers, thousands separators and controls sort as people expect", {
  expect_identical(natural_levels(c("25 C", "4 C", "-4 C", "-20 C")),
                   c("-20 C", "-4 C", "4 C", "25 C"))
  expect_identical(natural_levels(c("-5C", "+5C", "0C", "-10C", "10C")),
                   c("-10C", "-5C", "0C", "+5C", "10C"))
  # a hyphen inside a code or a range is not a minus sign
  expect_identical(natural_levels(c("SR-10", "SR-2", "SR-1")), c("SR-1", "SR-2", "SR-10"))
  expect_identical(natural_levels(c("15-30 cm", "0-15 cm", "30-45 cm")),
                   c("0-15 cm", "15-30 cm", "30-45 cm"))
  expect_identical(natural_levels(c("1,000 ppm", "500 ppm", "250 ppm", "2,000 ppm")),
                   c("250 ppm", "500 ppm", "1,000 ppm", "2,000 ppm"))
  expect_identical(natural_levels(c("ID1152921504606846976", "ID900000000000000000", "ID5")),
                   c("ID5", "ID900000000000000000", "ID1152921504606846976"))
  # the unnumbered control or starting point comes first
  expect_identical(natural_levels(c("90 days", "Fresh", "30 days", "60 days")),
                   c("Fresh", "30 days", "60 days", "90 days"))
  expect_identical(natural_levels(c("T2", "T1", "Control")), c("Control", "T1", "T2"))
  # wholly worded labels stay alphabetical
  expect_identical(natural_levels(c("Vegetative", "Flowering", "Maturity")),
                   c("Flowering", "Maturity", "Vegetative"))
})

test_that("codes that look like times or doses are not mistaken for them", {
  dx <- function(...) {
    cols <- list(...)
    d <- lapply(cols, function(lv) factor(lv, levels = natural_levels(lv)))
    default_x(list(vars = names(cols)), d)
  }
  nit <- c("N0", "N40", "N80", "N120")
  expect_identical(dx(Hybrid = c("H1", "H2", "H3", "H4"), Nitrogen = nit), "Nitrogen")
  expect_identical(dx(Treatment = c("D1", "D2", "D3"), Nitrogen = nit), "Nitrogen")
  expect_identical(dx(Molybdenum = c("Mo0", "Mo1", "Mo2"), Phosphorus = c("P0", "P30", "P60")),
                   "Phosphorus")
  expect_identical(dx(Days = c("D0", "D30", "D60"), DAP = c("0", "50", "100", "150")), "Days")
  # real times with a one-letter stem still count
  expect_identical(dx(Variety = c("V1", "V2"), Obs = c("H6", "H12", "H24")), "Obs")
  # a control beside numbered rates, and a fresh sample beside storage times
  expect_identical(dx(Variety = c("V1", "V2", "V3"), Nitrogen = c("Control", "N40", "N80")),
                   "Nitrogen")
  expect_identical(dx(Variety = c("V1", "V2"), Storage = c("Fresh", "30 days", "60 days")),
                   "Storage")
  # a dropped entry in numbered codes does not make them quantities
  expect_identical(dx(Treatment = c("T1", "T2", "T3"), Variety = c("V1", "V2", "V4")),
                   "Treatment")
  # worded labels earn nothing, whatever the column is called
  expect_identical(dx(Packaging = c("P1", "P2", "P3"),
                      Storage_condition = c("Ambient", "Refrigerated")), "Packaging")
  expect_identical(dx(Variety = c("V1", "V2"), Month = c("January", "February", "March")),
                   "Variety")
})

test_that("tied means give the same range-test result in any level order", {
  mk <- function(lv) {
    d <- data.frame(Treatment = factor(rep(names(lv), each = 3), levels = names(lv)))
    d$Yield <- rep(unname(lv), each = 3) + rep(c(-0.4, 0, 0.4), length(lv))
    analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  }
  a <- mk(c(T1 = 10, T2 = 12.1, T3 = 16, T10 = 12.1))
  b <- mk(c(T1 = 10, T10 = 12.1, T2 = 12.1, T3 = 16))
  for (m in c("Student-Newman-Keuls", "Duncan's DMRT")) {
    pa <- posthoc(a, "Treatment", m)$pairs
    pb <- posthoc(b, "Treatment", m)$pairs
    key <- function(p) vapply(strsplit(p$Comparison, " vs "), function(z)
      paste(sort(z), collapse = "|"), character(1))
    pb <- pb[match(key(pa), key(pb)), ]
    expect_equal(pa[["Critical value"]], pb[["Critical value"]], info = m)
    expect_identical(pa$Significant, pb$Significant, info = m)
  }
})

test_that("an x_var that is not a single factor name falls back to the default", {
  r <- analyze(demo_data("FRCBD"), "FRCBD",
               list(response = "Yield", block = "Block", factors = c("Variety", "Nitrogen")))
  for (xv in list(c("Variety", "Nitrogen"), character(0), NA, "", "Block")) {
    p <- plot_main(r, "Variety:Nitrogen", "line", x_var = xv)
    expect_identical(rlang::as_label(p$mapping$x), "Nitrogen")
  }
})

test_that("a stale X-axis choice from another effect is not used", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- expand.grid(Rep = 1:3, Variety = c("V1", "V2"), Nitrogen = c("N0", "N60"),
                     Days = c("D0", "D60"), stringsAsFactors = FALSE)
    d$Yield <- 20 + 3 * (d$Nitrogen == "N60") - 2 * (d$Days == "D60") +
      (d$Variety == "V2") + sin(seq_len(nrow(d)))
    rv$data <- d
    session$setInputs(design = "FCRD", nfac = 3, alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", f1 = "Variety", f2 = "Nitrogen", f3 = "Days")
    session$setInputs(tr_1 = "none", run = 1)
    session$setInputs(plEff = "Variety:Nitrogen", plType = "line", plLetters = TRUE)
    session$setInputs(plX = "Variety")
    expect_identical(rlang::as_label(mp()$mapping$x), "Variety")
    # a new effect: until its own selector reports back, its default is drawn
    session$setInputs(plEff = "Variety:Days")
    expect_identical(rlang::as_label(mp()$mapping$x), "Days")
  })
})
