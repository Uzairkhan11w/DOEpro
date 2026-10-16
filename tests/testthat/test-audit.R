## The accuracy audit of October 2026: each test reproduces a defect a user
## could have been misled by, and checks the result against an independent
## computation or the rule the readings follow.

strip_html <- function(h) gsub("[ ]+", " ", gsub("<[^>]+>", " ", h))

## ------------------------------------------- pooled factorial means ----

test_that("a pooled factorial's interaction means carry the environment's interactions", {
  d <- demo_data("POOLFACT")
  r <- analyze(d, "POOLFRCBD", list(response = "Yield", env = "Location", rep = "Rep",
                                    factors = c("Nitrogen", "Variety")))
  ms <- stats::setNames(r$anova$MS, r$anova$Source)
  e <- r$effects[["Nitrogen:Variety"]]
  a <- 2; b <- 3; ne <- 3; rr <- 3
  x <- unlist(e$extra)
  ## the expected-mean-square formulas, as in a strip plot with the
  ## environment as the replication
  expect_equal(unname(x["SE(d): two Nitrogen means at the same Variety"]),
               unname(sqrt(2 * ((b - 1) * ms["Location x Nitrogen x Variety"] + ms["Location x Nitrogen"]) / (ne * rr * b))))
  expect_equal(unname(x["SE(d): two Variety means at the same Nitrogen"]),
               unname(sqrt(2 * ((a - 1) * ms["Location x Nitrogen x Variety"] + ms["Location x Variety"]) / (ne * rr * a))))
  ## and, with no formula at all, from how the difference varies between
  ## the environments
  m <- stats::aggregate(Yield ~ Location + Nitrogen + Variety, d, mean)
  w <- stats::reshape(m, idvar = c("Location", "Variety"), timevar = "Nitrogen", direction = "wide")
  dd <- w[[4]] - w[[3]]
  expect_equal(unname(x["SE(d): two Nitrogen means at the same Variety"]),
               sqrt(mean(tapply(dd, w$Variety, stats::var)) / ne))
  ## the C.D. of each kind uses the weighted t of its mean squares
  wt <- c(ms["Location x Nitrogen"], (b - 1) * ms["Location x Nitrogen x Variety"])
  tw <- sum(wt * stats::qt(0.975, c(2, 4))) / sum(wt)
  expect_equal(unname(x["C.D. (5%): two Nitrogen means at the same Variety"]),
               unname(tw * x["SE(d): two Nitrogen means at the same Variety"]))
  ## the letters follow each pair's own C.D.
  k <- level_key(e$means, e$vars)
  expect_equal(e$cd_mat[match("N0 : V1", k), match("N1 : V1", k)],
               unname(x["C.D. (5%): two Nitrogen means at the same Variety"]))
  ## a main effect keeps its own environment interaction
  expect_equal(r$effects$Nitrogen$sed, unname(sqrt(2 * ms["Location x Nitrogen"] / (ne * rr * b))))
  ## post-hoc tests give each pair its own degrees of freedom; Dunnett's is refused
  ph <- posthoc(r, "Nitrogen:Variety", "Tukey HSD")
  expect_match(ph$stats$Value[4], "Satterthwaite", fixed = TRUE)
  ## the pairs differ by error structure, not by replication
  expect_no_match(paste(ph$note, collapse = " "), "Replication is unequal", fixed = TRUE)
  expect_error(posthoc(r, "Nitrogen:Variety", "Dunnett"), "not available for the Nitrogen x Variety means")
  expect_no_error(posthoc(r, "Nitrogen", "Dunnett"))
})

test_that("three-factor pooled interactions match the per-environment estimate", {
  set.seed(1)
  g <- expand.grid(Env = paste0("E", 1:4), Rep = paste0("R", 1:2), A = paste0("A", 1:2),
                   B = paste0("B", 1:3), C = paste0("C", 1:2), stringsAsFactors = FALSE)
  g$y <- 50 + stats::rnorm(nrow(g)) + stats::rnorm(4, 0, 2)[as.integer(factor(g$Env))] +
    stats::rnorm(8)[as.integer(interaction(g$Env, g$A))] + stats::rnorm(12)[as.integer(interaction(g$Env, g$B))] +
    stats::rnorm(24)[as.integer(interaction(g$Env, g$A, g$B))]
  r <- analyze(g, "POOLFRCBD", list(response = "y", env = "Env", rep = "Rep", factors = c("A", "B", "C")))
  cm <- stats::aggregate(y ~ Env + A + B + C, g, mean)
  w1 <- cm[cm$A == "A1", ]; w2 <- cm[cm$A == "A2", ]
  w1 <- w1[order(w1$Env, w1$B, w1$C), ]; w2 <- w2[order(w2$Env, w2$B, w2$C), ]
  expect_equal(unname(unlist(r$effects[["A:B:C"]]$extra)[1]),
               sqrt(mean(tapply(w2$y - w1$y, interaction(w1$B, w1$C), stats::var)) / 4))
  ## A x B, averaged over C
  c2 <- stats::aggregate(y ~ Env + A + B, g, mean); a1 <- c2[c2$A == "A1", ]; a2 <- c2[c2$A == "A2", ]
  a1 <- a1[order(a1$Env, a1$B), ]; a2 <- a2[order(a2$Env, a2$B), ]
  expect_equal(unname(unlist(r$effects[["A:B"]]$extra)[1]),
               sqrt(mean(tapply(a2$y - a1$y, a1$B, stats::var)) / 4))
  ## the weights are never negative, for any number of factors and levels
  for (n in list(c(A = 2, B = 2, C = 2), c(A = 3, B = 4, C = 2, D = 2)))
    for (Sp in list(names(n)[1:2], names(n)))
      for (D in list(Sp[1], Sp)) expect_true(all(pool_weights(n, Sp, D) >= 0))
})

test_that("the pooled interaction C.D. holds its level when the environment interacts", {
  set.seed(2026)
  g <- expand.grid(Rep = paste0("R", 1:3), A = c("A1", "A2"), B = c("B1", "B2", "B3"), Env = paste0("E", 1:4),
                   stringsAsFactors = FALSE)
  hit <- 0; n <- 0
  for (it in 1:120) {
    ea <- matrix(stats::rnorm(8, 0, 2), 4, 2, dimnames = list(paste0("E", 1:4), c("A1", "A2")))
    g$y <- 50 + c(B1 = 0, B2 = 2, B3 = 4)[g$B] + ea[cbind(g$Env, g$A)] + stats::rnorm(nrow(g))
    r <- analyze(g, "POOLFRCBD", list(response = "y", env = "Env", rep = "Rep", factors = c("A", "B")))
    e <- r$effects[["A:B"]]; k <- level_key(e$means, c("A", "B"))
    for (j in c("B1", "B2", "B3")) {
      i1 <- match(paste("A1 :", j), k); i2 <- match(paste("A2 :", j), k)
      hit <- hit + (abs(e$means$Mean[i1] - e$means$Mean[i2]) > e$cd_mat[i1, i2]); n <- n + 1
    }
  }
  ## a single MS(E x A x B) gave about 42% here
  expect_lt(hit / n, 0.10)
})

test_that("the pooled factorial note names a significant environment interaction", {
  r <- analyze(demo_data("POOLFACT"), "POOLFRCBD",
               list(response = "Yield", factors = c("Nitrogen", "Variety"), env = "Location", rep = "Rep"))
  expect_lt(r$anova$p[r$anova$Source == "Location x Nitrogen"], 0.05)
  nt <- r$effects[["Location:Nitrogen:Variety"]]$notes
  expect_no_match(nt, "differences between treatment combinations change", fixed = TRUE)
  expect_match(nt, "Location x Nitrogen is significant at the 5% level, however", fixed = TRUE)
  expect_match(nt, "the pooled means of Nitrogen average over environments that behave differently", fixed = TRUE)
})

test_that("a combined analysis without a pooled error says so plainly", {
  d <- demo_data("POOLCRD"); d <- d[!duplicated(d[c("Season", "Treatment")]), ]
  expect_error(analyze(d, "POOLCRD", list(response = "Yield", treat = "Treatment", env = "Season")),
               "needs at least two observations in every environment x treatment cell")
})

## ------------------------------------------------ levels and values ----

test_that("a gap in a design column keeps its levels in order and as written", {
  d <- demo_data("RCBD")
  d$Density <- c(0, 50000, 100000, 150000, 200000, 250000)[as.integer(factor(d$Variety))]
  d$Density[24] <- NA
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Density", block = "Block"))
  expect_identical(as.character(r$effects$Density$means$Density),
                   c("0", "50000", "100000", "150000", "200000", "250000"))
  d <- demo_data("CRD"); lv <- c("Untreated", "Fungicide B", "Fungicide A", "Neem oil", "Bio-agent")
  d$Treatment <- factor(lv[as.integer(factor(d$Treatment))], levels = lv); d$Treatment[3] <- NA
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_identical(levels(r$data$Treatment), lv)
  expect_match(posthoc(r, "Treatment", "Dunnett")$pairs$Comparison[1], "vs Untreated$")
})

test_that("adjusted means are kept at any scale of the response", {
  d <- demo_data("RCBD"); d$CFU <- round(d$Yield * 1e6); d$CFU[5] <- NA
  e <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), "CFU",
               trans = list(CFU = "reciprocal"))$fits$CFU$final$effects$Variety
  dd <- stats::na.omit(d); dd$z <- 1 / dd$CFU; fit <- stats::lm(z ~ Block + Variety, dd)
  gr <- expand.grid(Block = unique(dd$Block), Variety = unique(dd$Variety))
  expect_equal(e$means$Mean, as.vector(tapply(stats::predict(fit, gr), gr$Variety, mean)[as.character(e$means$Variety)]))
  expect_true("Raw_mean" %in% names(e$means))
})

test_that("Inf and NaN are flagged and left out, not analysed or crashed on", {
  d <- demo_data("RCBD"); d$Yield[4] <- Inf; d$Yield[7] <- NaN
  msg <- vapply(check_data(d)$issues, `[[`, "", "message")
  expect_true(any(grepl("rows 4 and 7 have entries that are not numbers ('Inf' and 'NaN')", msg, fixed = TRUE)))
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  expect_match(excluded_text(r$excluded), "not a number in rows 4 and 7 ('Inf' and 'NaN')", fixed = TRUE)
  expect_equal(nrow(r$data), 22)
  ## read from text they stay text, as does R's hexadecimal
  p <- read_pasted(paste(c("Trt\tYield", "T1\t5.6", "T1\tInf", "T2\t0x10", "T2\t7"), collapse = "\n"), "\t", TRUE)
  expect_identical(p$Yield, c("5.6", "Inf", "0x10", "7"))
  expect_identical(read_pasted(paste(c("A\tB", "1e3\t-2.5", "+4\t.5"), collapse = "\n"), "\t", TRUE)$A, c(1000, 4))
})

test_that("pasted text whose lines run past the header is refused", {
  txt <- paste(c("Plot,Trt,Rep,Yield", "1,T1,1,5,6", "2,T1,2,6", "3,T1,3,5,9"), collapse = "\n")
  expect_error(read_pasted(txt, ",", TRUE), "more values than the header has names")
  ## row names on every line, as R writes them, are kept as a column
  p <- read_pasted(paste(c("Trt Yield", "1 T1 5.6", "2 T1 6.1", "3 T2 7.0"), collapse = "\n"), " ", TRUE)
  expect_identical(names(p), c("Label", "Trt", "Yield"))
})

test_that("a % sign typed into one cell of a number column is refused", {
  skip_on_cran()
  d3 <- data.frame(Treatment = rep(paste0("T", 1:3), each = 3),
                   Germination = c(0.45, 0.50, 0.48, 0.60, 0.58, 0.66, 0.71, 0.69, 0.75))
  txt <- paste(c(paste(names(d3), collapse = "\t"), apply(d3, 1, paste, collapse = "\t")), collapse = "\n")
  shiny::testServer(doepro_server, {
    session$setInputs(paste = txt, sep = "\t", header = TRUE, load = 1)
    session$setInputs(tbl_cell_edit = list(row = 4, col = 2, value = "62%"))
    expect_equal(rv$data$Germination[4], 0.60)
  })
})

test_that("dates, fractions and doses are ordered as people mean them", {
  expect_identical(natural_levels(c("25 Oct", "10 Nov", "25 Nov")), c("25 Oct", "10 Nov", "25 Nov"))
  expect_identical(natural_levels(c("10 Nov", "1 Jan", "25 Oct", "15 Dec")), c("25 Oct", "10 Nov", "15 Dec", "1 Jan"))
  expect_identical(natural_levels(c("15/06/2023", "01/07/2023", "01/06/2023")), c("01/06/2023", "15/06/2023", "01/07/2023"))
  expect_identical(natural_levels(c("1/4 RDF", "1/2 RDF", "3/4 RDF")), c("1/4 RDF", "1/2 RDF", "3/4 RDF"))
  expect_identical(natural_levels(c("120/60/40", "60/30/20", "90/45/30")), c("60/30/20", "90/45/30", "120/60/40"))
  expect_identical(natural_levels(c("Mayuri", "Kufri", "Jyoti")), c("Jyoti", "Kufri", "Mayuri"))
})

## ---------------------------------------------------- tables of means ----

test_that("a level one response lacks stays in the other responses' columns", {
  d <- demo_data("CRD")
  d$Disease <- round(10 + as.integer(factor(d$Treatment)) * 0.3 + demo_noise(20, 0, 0.2), 2)
  d$Disease[d$Treatment == "T1"] <- NA
  rr <- run_all(d, "CRD", list(treat = "Treatment"), c("Disease", "Yield"))
  x <- strip_html(sketch_main_html(rr$fits, "Treatment"))
  expect_match(x, "T1 - 29.05", fixed = TRUE)
  card <- strip_html(means_summary_card(rr))
  expect_match(card, "16 observations", fixed = TRUE); expect_match(card, "20 observations", fixed = TRUE)
})

test_that("each factor's table gives the C.V. of its own error", {
  rs <- run_all(demo_data("SPLIT"), "SPLIT", list(rep = "Rep", main = "Irrigation", sub = "Variety"), "Yield")
  cv <- rs$fits$Yield$final$cv
  expect_match(strip_html(sketch_main_html(rs$fits, "Irrigation")), sprintf("C.V. (%%) %s (a)", fmt(cv[[1]], 2)), fixed = TRUE)
  expect_match(strip_html(sketch_main_html(rs$fits, "Variety")), sprintf("C.V. (%%) %s (b)", fmt(cv[[2]], 2)), fixed = TRUE)
})

test_that("a split-plot note quotes no C.D. for a non-significant interaction", {
  d <- demo_data("SPLIT")
  fit <- stats::lm(Yield ~ Rep + Irrigation + Variety + Rep:Irrigation, data = d)
  d$Yield2 <- round(stats::fitted(fit) + stats::residuals(stats::lm(Yield ~ Rep * Irrigation + Variety * Irrigation, d)), 2)
  r <- analyze(d, "SPLIT", list(response = "Yield2", rep = "Rep", main = "Irrigation", sub = "Variety"))
  expect_gt(r$effects[["Irrigation:Variety"]]$p, 0.05)
  expect_no_match(strip_html(as.character(integrated_means_html(r))), "Letters 'within MP'", fixed = TRUE)
})

test_that("means that print alike are never called different from a mean they share a letter with", {
  d <- demo_data("RCBD"); d$Oil <- round(d$Yield / 1000, 4)
  e <- analyze(d, "RCBD", list(response = "Oil", treat = "Variety", block = "Block"))$effects$Variety
  txt <- means_text(e, 2)
  expect_match(txt, "differ significantly among themselves", fixed = TRUE)
  expect_match(txt, "V1 (0.04) does not differ significantly from V6", fixed = TRUE)
  ## a single SE or C.D. never prints as zero
  expect_identical(err_text(e, "cd", 2), "0.003")
  dev6 <- c(-1.2, -0.7, -0.2, 0.2, 0.7, 1.2)
  d <- data.frame(Treatment = c(rep("T1", 6), rep("T2", 2), rep("T3", 6), rep("T4", 6)),
                  Yield = c(10 + dev6, 15 + c(-1, 1), 15 + dev6, 13.6 + dev6))
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_match(means_text(r$effects$Treatment, 2), "T4 (13.60) does not differ significantly from T2 but is significantly lower than T3", fixed = TRUE)
  x <- gate_posthoc(posthoc(r, "Treatment", "LSD (Fisher's protected)"))
  expect_match(posthoc_groups_text(x, r$effects$Treatment), "does not differ significantly from T2 but is significantly lower than T3", fixed = TRUE)
})

## ----------------------------------------------------- the adviser ----

test_that("one stray value does not make the adviser order a transformation", {
  d <- data.frame(Block = rep(paste0("B", 1:4), 6), Variety = rep(paste0("V", 1:6), each = 4))
  d$Yield <- c(46.6, 39.6, 40.6, 41.2, 45.1, 45.1, 48.5, 46.8, 39.3, 43.4, 39.7, 44.4,
               36.6, 55.6, 58.8, 55.9, 48.2, 49.4, 50, 52, 45.7, 45.4, 46.6, 41.2)
  r <- analyze(d, "RCBD", list(treat = "Variety", block = "Block", response = "Yield"))
  s <- suggest_transform(r, check_assumptions(r))
  expect_identical(s$method, "none")
  expect_match(s$why, "the possible outlier (row 13) may be the cause", fixed = TRUE)
})

test_that("a response between 0 and 1 is a proportion only when named as one", {
  d <- demo_data("RCBD")
  d$Height_m <- c(0.61, 0.62, 0.62, 0.61, 0.69, 0.71, 0.91, 0.68, 0.51, 0.53, 0.53, 0.58,
                  0.79, 0.8, 0.72, 0.79, 0.67, 0.73, 0.69, 0.67, 0.64, 0.64, 0.68, 0.64)
  r <- analyze(d, "RCBD", list(response = "Height_m", treat = "Variety", block = "Block"))
  expect_false(identical(suggest_transform(r, check_assumptions(r))$method, "arcsine01"))
  names(d)[names(d) == "Height_m"] <- "Germination_prop"
  r <- analyze(d, "RCBD", list(response = "Germination_prop", treat = "Variety", block = "Block"))
  expect_identical(suggest_transform(r, check_assumptions(r))$method, "arcsine01")
})

test_that("no transformation is suggested that the data cannot take", {
  dc <- data.frame(Block = rep(paste0("B", 1:4), 5), Trt = rep(paste0("T", 1:5), each = 4),
                   Count = c(0, 0, 0, 0, 2, 2, 3, 3, 14, 15, 9, 1, 3, 57, 7, 25, 146, 13, 28, 86))
  m <- list(treat = "Trt", block = "Block")
  r <- analyze(dc, "RCBD", c(m, response = "Count")); s <- suggest_transform(r, check_assumptions(r))
  expect_true(trans_defined(s$method, dc$Count))
  ## Box-Cox needs positive values and an optimum inside the range searched
  expect_error(run_all(dc, "RCBD", m, "Count", trans = list(Count = "boxcox")), "needs every value of 'Count' to be positive")
  d2 <- data.frame(Block = rep(paste0("B", 1:4), 6), Variety = rep(paste0("V", 1:6), each = 4))
  d2$Yield <- c(50.2, 62.2, 51.2, 49.4, 50.3, 51.3, 51.6, 54.7, 55.7, 54.4, 53.2, 54.1,
                53.8, 53, 51.6, 47.7, 47.6, 49.5, 53.8, 51.5, 54.4, 57.6, 56.6, 50.2)
  expect_error(run_all(d2, "RCBD", list(treat = "Variety", block = "Block"), "Yield", trans = list(Yield = "boxcox")),
               "still rising at lambda = -2")
  a <- check_assumptions(analyze(d2, "RCBD", list(response = "Yield", treat = "Variety", block = "Block")))
  expect_match(assum_table_html(a), "beyond -2.00", fixed = TRUE)
})

## ------------------------------------------- interpretation and report ----

test_that("the interpretation judges the CV as the readings do", {
  d <- demo_data("RCBD")
  d$Count <- round(exp(log(rep(c(300, 450, 250, 600, 500, 350), each = 4)) + demo_noise(24, 0, 0.4)))
  f <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), "Count", trans = list(Count = "log"))$fits$Count
  it <- interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)
  expect_no_match(it, "precisely conducted", fixed = TRUE)
  expect_match(it, "transformed scale, so the usual guide", fixed = TRUE)
  d$Change <- d$Yield - 50
  f <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), "Change")$fits$Change
  expect_match(interpret(f$final, f$asm, f$sug), "grand mean is not positive", fixed = TRUE)
})

test_that("the interpretation names the best treatment as the reader knows it", {
  f <- run_all(demo_data("RCBD"), "RCBD", list(treat = "Variety", block = "Block"), "Yield",
               trans = list(Yield = "reciprocal"))$fits$Yield
  expect_match(strip_html(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)),
               "The highest mean (back-transformed), 54.194, was recorded for V4", fixed = TRUE)
  expect_match(paste(posthoc(f$final, "Variety", "Tukey HSD")$note, collapse = " "), "reverse of Yield as entered", fixed = TRUE)
  d <- data.frame(Treatment = rep(c("T1", "T2", "T3", "T4"), each = 3),
                  Yield = c(10, 10.5, 11, 14.5, 15, 15.5, 14.5, 15, 15.5, 12, 12.5, 13))
  f <- run_all(d, "CRD", list(treat = "Treatment"), "Yield")$fits$Yield
  expect_match(strip_html(interpret(f$final, f$asm, f$sug)), "was recorded for T2 and T3", fixed = TRUE)
})

test_that("the interpretation of an exact fit claims no significance", {
  d <- expand.grid(Block = paste0("B", 1:3), Variety = paste0("V", 1:4), stringsAsFactors = FALSE)
  d$Yield <- 40 + c(B1 = 0, B2 = 1.5, B3 = -0.5)[d$Block] + c(V1 = 0, V2 = 3, V3 = 5.2, V4 = 1.1)[d$Variety]
  f <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), "Yield")$fits$Yield
  it <- interpret(f$final, f$asm, f$sug)
  expect_no_match(it, "highly significant", fixed = TRUE)
  expect_no_match(it, "precisely conducted", fixed = TRUE)
  expect_match(it, "fits every value exactly", fixed = TRUE)
})

test_that("a three-way interaction is read as what its F-test asks", {
  set.seed(3)
  d <- expand.grid(Block = paste0("R", 1:3), Nitrogen = c("N0", "N60", "N120"), Variety = c("V1", "V2"),
                   Spray = c("S0", "S1"), stringsAsFactors = FALSE)
  nN <- c(N0 = 0, N60 = 1, N120 = 2)[d$Nitrogen]
  d$Yield <- round(30 + 3 * nN + 2 * (d$Variety == "V2") + 3 * nN * (d$Spray == "S1") + stats::rnorm(nrow(d)), 2)
  f <- run_all(d, "FRCBD", list(factors = c("Nitrogen", "Variety", "Spray"), block = "Block"), "Yield")$fits$Yield
  expect_match(interpret(f$final, f$asm, f$sug), "the interaction of Nitrogen and Variety differs between the levels of Spray", fixed = TRUE)
})

test_that("the recommendation says which values it was judged on", {
  set.seed(1); d <- data.frame(Treatment = rep(paste0("T", 1:5), each = 6))
  d$Yield <- round(rep(c(6, 10, 20, 40, 80), each = 6) + stats::rnorm(30, 0, 2), 2)
  f <- run_all(d, "CRD", list(treat = "Treatment"), "Yield", trans = list(Yield = "log"))$fits$Yield
  expect_match(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab),
               "judged on the values as entered (the checks above are on the log(y) scale)", fixed = TRUE)
})

test_that("tables and report follow each response, the slope test and the chosen decimals", {
  d <- demo_data("RCBD"); d$Height <- round(d$Yield * 2 + rep(c(1, 3, 2, 5), 6), 1); d$Height[5] <- NA
  expect_null(combined_anova_html(run_all(d, "RCBD", list(treat = "Variety", block = "Block"), c("Yield", "Height"))))
  set.seed(1); d <- data.frame(Treatment = rep(paste0("T", 1:5), each = 4))
  d$Yield <- round(stats::rnorm(20, rep(c(20, 25, 30, 35, 40), each = 4), 2), 2)
  a <- check_assumptions(analyze(d, "CRD", list(response = "Yield", treat = "Treatment")))
  expect_gt(a$slope_p, 0.05)
  expect_match(assum_table_html(a), "insufficient evidence that the variance changes with the mean", fixed = TRUE)
  d <- demo_data("RCBD"); d$SeedWt <- round(d$Yield / 1000, 5)
  rr <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), "SeedWt")
  expect_match(strip_html(build_report(rr, digits = 4)), "C.D. (P&le;0.05) &nbsp; Variety 0.0028", fixed = TRUE)
})

test_that("promax factor scores are regression scores of correlated factors", {
  set.seed(1); n <- 300
  f1 <- stats::rnorm(n); f2 <- 0.6 * f1 + 0.8 * stats::rnorm(n)
  d <- data.frame(A = f1 + stats::rnorm(n, 0, .5), B = f1 + stats::rnorm(n, 0, .5), C = f1 + stats::rnorm(n, 0, .5),
                  D = f2 + stats::rnorm(n, 0, .5), E = f2 + stats::rnorm(n, 0, .5), F = f2 + stats::rnorm(n, 0, .5))
  for (rot in c("promax", "varimax", "none")) {
    fa <- fa_data(d, names(d), factors = 2, rotation = rot)
    expect_equal(unname(fa$scores), unname(scale(as.matrix(d)) %*% solve(stats::cor(d)) %*% fa$loadings %*% fa$phi),
                 info = rot)
    expect_true(all(apply(fa$scores, 2, stats::sd) <= 1))
  }
})
