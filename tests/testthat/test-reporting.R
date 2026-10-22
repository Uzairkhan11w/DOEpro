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
    if (!is.na(e$p) && e$p < e$alpha) {
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

## ------------------------------------------------- the report, to print --

test_that("each response's ANOVA has its own heading and no table of mean squares stands beside them", {
  d <- demo_data("RCBD"); d$Height <- round(d$Yield * 2 + rep(c(1, 3, 2, 5), length.out = nrow(d)), 1)
  rr <- run_all(d, "RCBD", list(treat = "Variety", block = "Block"), c("Yield", "Height"))
  h <- build_report(rr)
  expect_match(h, "Analysis of Variance (Yield)", fixed = TRUE)
  expect_match(h, "Analysis of Variance (Height)", fixed = TRUE)
  expect_no_match(h, "mean squares</caption>", fixed = TRUE)
  # the significance column says the level it is judged at
  expect_match(h, "<th>Significance (5%)</th>", fixed = TRUE)
})

test_that("the report is branded, dated and ready to print on A4", {
  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  h <- build_report(rr, comparison("Tukey HSD"))
  expect_match(h, "Generated with StatLabX", fixed = TRUE)
  expect_match(h, sprintf("DOEpro v%s", APP_VERSION), fixed = TRUE)
  expect_match(h, format(Sys.Date(), "%Y"), fixed = TRUE)
  expect_match(h, "@page{size:A4 portrait", fixed = TRUE)
  expect_match(h, "@page wide{size:A4 landscape", fixed = TRUE)
  expect_match(h, "@media print", fixed = TRUE)
  expect_match(h, "break-inside:avoid", fixed = TRUE)
  expect_match(h, "class='print-foot'", fixed = TRUE)
  expect_match(h, "Comparison of means: <b>Tukey's HSD</b>", fixed = TRUE)
  # the table of means carries the procedure's letters and no C.D.
  expect_match(h, "<sup>[a-z]+</sup>")
  expect_no_match(h, "C.D. (5%)", fixed = TRUE)
  # an older call that passed letters_on still runs
  expect_true(startsWith(build_report(rr, TRUE), "<!DOCTYPE html>"))
  expect_true(startsWith(build_report(rr, letters_on = TRUE), "<!DOCTYPE html>"))
})

test_that("Dunnett's comparisons with the control are in the report, open to print", {
  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  h <- build_report(rr, comparison("Dunnett", control = "T1", effect = "Treatment"))
  expect_match(h, "Dunnett's comparisons with the control", fixed = TRUE)
  expect_match(h, "<details class='slx-fold' open>", fixed = TRUE)
  expect_no_match(h, "<sup>[a-z]")
})

test_that("a table with many columns is marked to print on a landscape page", {
  expect_match(raw_table(LETTERS[1:9], list(letters[1:9])), "class='doe wide'", fixed = TRUE)
  expect_no_match(raw_table(LETTERS[1:4], list(letters[1:4])), "wide", fixed = TRUE)
})

## ---------------------------------------------------------------- authors --

test_that("Dr. Pradeep Mishra is listed last among the developers, and not cited", {
  nm <- vapply(AUTHORS, `[[`, "", "name")
  # the cited authors keep their places, then the Chief Scientist, then him
  expect_identical(nm, c("Dr. Immad A. Shah", "Mr. Uzair Javid Khan", "Dr. M. Iqbal Jeelani",
                         "Dr. Imran Khan", "Dr. Pradeep Mishra"))
  pm <- AUTHORS[[5]]
  expect_identical(pm$role, "Assistant Professor")
  expect_identical(pm$aff, "JNKVV, Madhya Pradesh")
  # nothing was made up for him
  expect_true(is.na(pm$email)); expect_true(is.na(pm$orcid))
  # the credit lines and the citation name the cited authors only
  for (txt in c(CREDIT_SHORT, CREDIT_LONG, CITE_AUTHORS)) {
    expect_no_match(txt, "Mishra", fixed = TRUE)
    expect_no_match(txt, "Imran", fixed = TRUE)
  }
  expect_match(CITE_AUTHORS, "Shah, I. A., Khan, U. J. and Jeelani, M. I.", fixed = TRUE)
  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  h <- build_report(rr)
  expect_match(h, "5. Dr. Pradeep Mishra &mdash; Assistant Professor, JNKVV, Madhya Pradesh", fixed = TRUE)
  expect_no_match(gsub("<[^>]+>", " ", sub("^.*Please cite:", "", h)), "Mishra", fixed = TRUE)
  desc <- read.dcf(system.file("DESCRIPTION", package = "DOEpro"))
  expect_match(desc[1, "Authors@R"], 'person("Pradeep", "Mishra", role = "ctb")', fixed = TRUE)
  cit <- utils::readCitationFile(system.file("CITATION", package = "DOEpro"))
  fam <- unlist(lapply(cit$author, function(p) p$family))
  expect_identical(fam, c("Shah", "Khan", "Jeelani"))
})
