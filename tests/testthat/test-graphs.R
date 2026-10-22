## Graphs: the download menu on every graph, the files it saves, and the
## interaction plots.

## the width and height in pixels a PNG file says it has
png_size <- function(f) {
  b <- readBin(f, "raw", 24)
  int <- function(x) sum(as.integer(x) * 256^(3:0))
  c(int(b[17:20]), int(b[21:24]))
}

test_that("bytes are encoded as base64 exactly", {
  enc <- function(s) b64_raw(charToRaw(s))
  expect_identical(enc("Man"), "TWFu")
  expect_identical(enc("Ma"), "TWE=")
  expect_identical(enc("M"), "TQ==")
  expect_identical(enc(""), "")
  expect_identical(enc("StatLabX graphs"), "U3RhdExhYlggZ3JhcGhz")
  # against a byte-by-byte encoder built from bit shifts (RFC 4648)
  ref <- function(r) {
    al <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "")[[1]]
    b <- as.integer(r); out <- character(0)
    for (i in seq(1, length(b), by = 3)) {
      x <- b[i:min(i + 2, length(b))]; k <- length(x); x <- c(x, rep(0L, 3 - k))
      n <- bitwOr(bitwOr(bitwShiftL(x[1], 16L), bitwShiftL(x[2], 8L)), x[3])
      q <- c(bitwShiftR(n, 18L), bitwAnd(bitwShiftR(n, 12L), 63L), bitwAnd(bitwShiftR(n, 6L), 63L), bitwAnd(n, 63L))
      out <- c(out, al[q[seq_len(k + 1)] + 1L], rep("=", 3 - k))
    }
    paste(out, collapse = "")
  }
  set.seed(9)
  for (n in c(1001, 1000, 999)) {
    r <- as.raw(sample(0:255, n, replace = TRUE))
    expect_identical(b64_raw(r), ref(r))
  }
})

test_that("a saved graph is named for what it shows and carries the platform's name", {
  expect_identical(plot_file("residuals-vs-fitted", "Yield"), "StatLabX_residuals-vs-fitted_Yield.png")
  expect_identical(plot_file("means-bar", "Plant height (cm)", "svg"), "StatLabX_means-bar_Plant-height-cm.svg")
  expect_identical(plot_file("correlation-heat-map"), "StatLabX_correlation-heat-map.png")
  r <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  p <- plot_main(r, "Treatment", "bar")
  cap <- plot_brand(p)$labels$caption
  expect_match(cap, "Generated with StatLabX", fixed = TRUE)
  # the plot's own caption is kept above it
  expect_true(startsWith(cap, p$labels$caption))
})

test_that("a graph is saved as a 300 dpi PNG of the size asked for", {
  r <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  f <- tempfile(fileext = ".png"); on.exit(unlink(f))
  save_plot_file(plot_brand(plot_main(r, "Treatment", "bar")), f, "png", 8, 5)
  expect_identical(readBin(f, "raw", 8), as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))
  expect_equal(png_size(f), c(2400, 1500))
  if (isTRUE(capabilities("cairo"))) {
    s <- tempfile(fileext = ".svg"); on.exit(unlink(s), add = TRUE)
    save_plot_file(plot_main(r, "Treatment", "bar"), s, "svg", 8, 5)
    expect_match(paste(readLines(s, 3, warn = FALSE), collapse = " "), "<svg|<\\?xml")
  }
})

test_that("every graph in the app has a download menu, and every one can be redrawn for it", {
  ui <- as.character(doepro_ui())
  tags <- regmatches(ui, gregexpr('<div[^>]*shiny-plot-output[^>]*>', ui))[[1]]
  ids <- sub('.*id="([A-Za-z]+)".*', "\\1", tags)
  expect_gt(length(ids), 25)
  for (id in ids) {
    expect_match(ui, sprintf('data-plot="%s" data-fmt="png"', id), fixed = TRUE, info = id)
    expect_match(ui, sprintf('data-plot="%s" data-fmt="svg"', id), fixed = TRUE, info = id)
  }
  skip_on_cran()
  shiny::testServer(doepro_server, {
    expect_setequal(ls(plots), ids)
  })
})

## ------------------------------------------------------ interaction plots --

test_that("an interaction plot draws each cell mean's confidence interval from its own SE", {
  r <- analyze(demo_data("FCRD"), "FCRD", list(response = "Yield", factors = c("Variety", "Nitrogen")))
  e <- r$effects[["Variety:Nitrogen"]]
  p <- plot_interaction(r, "Variety:Nitrogen", "Nitrogen", ci = TRUE)
  b <- ggplot_build(p)
  bars <- b$data[[1]]
  expect_true(all(c("ymin", "ymax") %in% names(bars)))
  half <- qt(0.975, e$df) * e$means$SE
  expect_equal(sort(bars$ymax - bars$ymin), sort(2 * half), tolerance = 1e-10)
  expect_match(p$labels$caption, "95% confidence interval", fixed = TRUE)
  # the level follows the analysis
  r01 <- analyze(demo_data("FCRD"), "FCRD", list(response = "Yield", factors = c("Variety", "Nitrogen")), alpha = 0.01)
  expect_match(plot_interaction(r01, "Variety:Nitrogen")$labels$caption, "99% confidence interval", fixed = TRUE)
  expect_identical(p$labels$x, "Nitrogen")
})

test_that("no confidence intervals where a cell mean has no standard error of its own", {
  s <- analyze(demo_data("SPLIT"), "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
  p <- plot_interaction(s, "Irrigation:Variety", ci = TRUE)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1))))
  expect_match(gsub("\\s+", " ", p$labels$caption), "No confidence intervals", fixed = TRUE)
  t <- interaction_text(s, "Irrigation:Variety")
  expect_match(t$what, "No confidence intervals are drawn", fixed = TRUE)
  pf <- analyze(demo_data("POOLFACT"), "POOLFRCBD", list(response = "Yield", env = "Location", rep = "Rep",
                                                          factors = c("Nitrogen", "Variety")))
  expect_false(ci_ok(pf$effects[["Nitrogen:Variety"]]))
})

test_that("the interaction plot's reading follows the F-test, not the lines alone", {
  r <- analyze(demo_data("FCRD"), "FCRD", list(response = "Plant_height", factors = c("Variety", "Nitrogen")))
  e <- r$effects[["Variety:Nitrogen"]]
  expect_gt(e$p, 0.05)
  t <- interaction_text(r, "Variety:Nitrogen", "Nitrogen")
  expect_match(t$reading, "insufficient evidence", fixed = TRUE)
  expect_match(t$reading, "the F-test for the interaction says whether the lines depart from parallel by more than chance", fixed = TRUE)
  ry <- analyze(demo_data("FCRD"), "FCRD", list(response = "Yield", factors = c("Variety", "Nitrogen")))
  expect_match(interaction_text(ry, "Variety:Nitrogen", "Nitrogen")$reading, "significant at the", fixed = TRUE)
})

test_that("letters on a plot are those of the comparison chosen, beside their own means", {
  r <- analyze(demo_data("RCBD"), "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  L <- cmp_letters(r, "Variety", comparison("Tukey HSD"))
  p <- plot_main(r, "Variety", "bar", L$letters, method = "Tukey's HSD")
  txt <- ggplot_build(p)$data[[3]]
  m <- r$effects$Variety$means
  expect_identical(txt$label, L$letters[match(levels(m$Variety)[txt$x], as.character(m$Variety))])
  expect_match(gsub("\\s+", " ", p$labels$caption), "by Tukey's HSD at the 5% level", fixed = TRUE)
  # no letters asked for, none drawn
  p0 <- plot_main(r, "Variety", "bar", NULL)
  expect_false(any(vapply(p0$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})
