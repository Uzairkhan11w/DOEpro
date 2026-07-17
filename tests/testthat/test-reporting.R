test_that("run_all analyses every response", {
  d <- demo_data("FRCBD")
  d$Score <- d$Yield * 0.8 + 1
  rr <- run_all(d, "FRCBD", list(factors = c("Nitrogen", "Variety"), block = "Block"),
                responses = c("Yield", "Score"))
  expect_length(rr$fits, 2)
  expect_named(rr$fits, c("Yield", "Score"))
  expect_s3_class(rr$fits[["Yield"]]$final$anova, "data.frame")
})

test_that("build_report returns one complete HTML document", {
  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  h <- build_report(rr)
  expect_type(h, "character")
  expect_length(h, 1)
  expect_true(startsWith(h, "<!DOCTYPE html>"))
  expect_match(h, "</html>\\s*$")
  expect_match(h, "Analysis of variance")
})

test_that("the report carries the citation and the DOI", {
  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  h <- build_report(rr)
  expect_match(h, "Please cite", fixed = TRUE)
  expect_match(h, "10.5281/zenodo", fixed = TRUE)
})

test_that("grouping letters are shown only when the F-test is significant", {
  d <- demo_data("POOLFACT")
  r <- analyze(d, "POOLFRCBD", list(response = "Yield", env = "Location",
                                    rep = "Rep", factors = c("Nitrogen", "Variety")))
  for (nm in names(r$effects)) {
    e <- r$effects[[nm]]
    m <- gate_letters(e)
    cols <- intersect(LETTER_COLS, names(m))
    if (!length(cols)) next
    any_letter <- any(nzchar(unlist(m[cols])))
    if (!is.na(e$p) && e$p < 0.05) {
      expect_true(any_letter, info = paste(e$label, "is significant; letters expected"))
    } else {
      expect_false(any_letter, info = paste(e$label, "is NS; letters must be suppressed"))
    }
  }
})

test_that("a significant environment interaction is broken down by environment", {
  set.seed(1)
  d <- expand.grid(Rep = paste0("R", 1:3), Variety = c("V1", "V2", "V3"),
                   Location = c("L1", "L2", "L3"))
  base <- c(L1 = 20, L2 = 25, L3 = 30)[as.integer(d$Location)]
  # a deliberate crossover: V1 best at L1, V3 best at L3
  m <- matrix(c(8, 4, 0, 4, 4, 4, 0, 4, 8), 3, 3, byrow = TRUE,
              dimnames = list(c("L1", "L2", "L3"), c("V1", "V2", "V3")))
  inter <- m[cbind(as.character(d$Location), as.character(d$Variety))]
  d$Yield <- round(base + inter + rnorm(nrow(d), 0, 0.8), 2)

  rr <- run_all(d, "POOLRCBD", list(env = "Location", rep = "Rep", treat = "Variety"), "Yield")
  gxe <- rr$fits[[1]]$final$effects[["Location:Variety"]]
  expect_lt(gxe$p, 0.05)                       # the interaction is real
  html <- means_section_html(rr, detailed = TRUE)
  expect_match(html, "ms-env-wrap")            # the per-environment tables appear
})

test_that("demo_data returns a usable frame for every design", {
  for (nm in c("CRD", "RCBD", "LSD", "FRCBD", "SPLIT", "STRIP",
               "POOLRCBD", "POOLCRD", "POOLFACT", "POOLFACTC")) {
    d <- demo_data(nm)
    # expect_s3_class() takes no info argument; inherits() keeps the design name
    # in the failure message
    expect_true(inherits(d, "data.frame"), info = nm)
    expect_true(nrow(d) > 0, info = nm)
    expect_false(anyNA(d), info = nm)
  }
})

test_that("DESIGNS and TRANS are the documented shape", {
  expect_type(DESIGNS, "character")
  expect_length(DESIGNS, 11)
  expect_true(all(nzchar(names(DESIGNS))))
  expect_type(TRANS, "list")
  expect_length(TRANS, 9)
  expect_true(all(c("none", "log", "sqrt", "arcsine", "boxcox") %in% names(TRANS)))
})
