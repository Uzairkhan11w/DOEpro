## Graphs: every plot of the app as the chart the browser draws, the
## download menu on every graph, and the interaction plots.

## every plot the app draws, from the example data
app_plots <- function() {
  d <- demo_data("FCRD")
  r <- analyze(d, "FCRD", list(response = "Yield", factors = c("Variety", "Nitrogen")))
  e <- r$effects[["Variety:Nitrogen"]]
  out <- list()
  for (ty in c("bar", "line", "heat", "box")) {
    out[[paste0("main_", ty)]] <- plot_main(r, "Variety:Nitrogen", ty, e$means$Letter)
    out[[paste0("main1_", ty)]] <- plot_main(r, "Variety", ty, r$effects$Variety$means$Letter)
  }
  d3 <- expand.grid(Rep = 1:3, V = c("V1", "V2"), Nit = c("N0", "N60"), Day = c("D0", "D60"))
  d3$Y <- 20 + 3 * (d3$Nit == "N60") + seq_len(nrow(d3)) %% 5
  r3 <- analyze(d3, "FCRD", list(response = "Y", factors = c("V", "Nit", "Day")))
  out$main_facet <- plot_main(r3, "V:Nit:Day", "line")
  out$inter <- plot_interaction(r, "Variety:Nitrogen")
  out$inter_facet <- plot_interaction(r3, "V:Nit:Day")
  dg <- plot_diag(r); names(dg) <- paste0("diag", 1:4); out <- c(out, dg)
  out$cooks <- plot_cooks(r)
  a <- check_assumptions(r)
  out$boxcox <- plot_boxcox(a); out$meanvar <- plot_meanvar(a)
  out$hist <- plot_hist(d, "Yield"); out$dens <- plot_density(d, "Yield")
  out$box <- plot_box(d, "Yield", "Variety"); out$box1 <- plot_box(d, "Yield"); out$qq <- plot_qq(d, "Yield")
  ct <- correlate_data(d, c("Yield", "Plant_height", "Rep"), "pearson", 0.05)
  out$corheat <- plot_cor_heat(ct, c("Yield", "Plant_height", "Rep"))
  out$corscatter <- plot_cor_scatter(d, "Yield", "Plant_height", "pearson", 0.05)
  rg <- regress_data(d, "Yield", c("Plant_height", "Rep"), 0.05)
  out$regmain <- plot_reg_main(rg, "Plant_height"); out$regresid <- plot_reg_resid(rg)
  out$regqq <- plot_reg_qq(rg); out$regobs <- plot_reg_obs(rg)
  pc <- pca_data(d, c("Yield", "Plant_height", "Rep"))
  out$pcascree <- plot_pca_scree(pc); out$pcascores <- plot_pca_scores(pc, c(1, 2), factor(d$Variety))
  out$pcaload <- plot_pca_loadings(pc, c(1, 2)); out$pcabi <- plot_pca_biplot(pc, c(1, 2), factor(d$Variety))
  cl <- cluster_data(d, c("Yield", "Plant_height"), group = "Variety")
  out$cldendro <- plot_cl_dendro(cl); out$clpcs <- plot_cl_pcs(cl); out$clsil <- plot_cl_silhouette(cl)
  set.seed(1); n <- 120; f1 <- rnorm(n); f2 <- rnorm(n)
  fd <- data.frame(A = f1 + rnorm(n, 0, .5), B = f1 + rnorm(n, 0, .5), C = f1 + rnorm(n, 0, .5),
                   D = f2 + rnorm(n, 0, .5), E = f2 + rnorm(n, 0, .5), F = f2 + rnorm(n, 0, .5))
  fa <- fa_data(fd, names(fd), factors = 2)
  out$fascree <- plot_fa_scree(fa); out$faheat <- plot_fa_heat(fa); out$faload <- plot_fa_loadings(fa, c(1, 2))
  out
}

test_that("every plot of the app becomes a chart the browser can draw, with nothing left out", {
  plots <- app_plots()
  for (nm in names(plots)) {
    p <- plots[[nm]]
    s <- chart_spec(p, nm, "Yield")
    kinds <- vapply(s$layers, `[[`, "", "kind")
    # each layer of the ggplot is drawn: no geom the browser does not know
    expect_identical(length(kinds), length(p$layers), info = nm)
    expect_true(all(kinds %in% c(unname(CHART_GEOMS), "hist")), info = nm)
    expect_true(length(s$panels) >= 1, info = nm)
    for (pn in s$panels) {
      expect_length(pn$x$range, 2); expect_length(pn$y$range, 2)
      expect_identical(length(pn$x$breaks), length(pn$x$labels), info = nm)
    }
    expect_identical(s$title, p$labels$title, info = nm)
    expect_identical(s$file, sub("[.]$", "", plot_file(nm, "Yield", "")), info = nm)
    # colours reach the browser as #RRGGBB
    cols <- unlist(lapply(s$layers, function(l) c(l$data$colour, l$data$fill)))
    expect_true(all(is.na(cols) | grepl("^#[0-9A-F]{6}$", cols)), info = nm)
  }
})

test_that("the chart of a table of means draws its bars, intervals and letters where ggplot2 puts them", {
  r <- analyze(demo_data("RCBD"), "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  L <- cmp_letters(r, "Variety", comparison("Tukey HSD"))
  s <- chart_spec(plot_main(r, "Variety", "bar", L$letters, method = "Tukey's HSD"), "means-bar", "Yield")
  m <- r$effects$Variety$means
  bars <- s$layers[[1]]$data; err <- s$layers[[2]]$data; txt <- s$layers[[3]]$data
  expect_identical(s$layers[[1]]$kind, "bar")
  ## the panel's levels in their natural order, at 1, 2, 3, ...
  expect_identical(s$panels[[1]]$x$labels, levels(m$Variety))
  expect_true(s$panels[[1]]$x$discrete)
  ord <- match(s$panels[[1]]$x$labels[round(bars$x)], as.character(m$Variety))
  expect_equal(bars$ymax, m$Mean[ord], tolerance = 1e-12)
  expect_equal(err$ymax - err$ymin, 2 * m$SE[ord], tolerance = 1e-12)
  expect_identical(txt$label, L$letters[ord])
  expect_match(s$caption, "by Tukey's HSD", fixed = TRUE)
})

test_that("histograms, legends, colour bars, fixed aspects and hidden axes reach the browser", {
  plots <- app_plots()
  ## a histogram's bars, edge to edge, as ggplot2 bins them
  h <- chart_spec(plots$hist, "histogram", "Yield")
  expect_identical(h$layers[[1]]$kind, "hist")
  b <- ggplot_build(plots$hist)$data[[1]]
  expect_equal(h$layers[[1]]$data$xmin, b$xmin); expect_equal(h$layers[[1]]$data$ymax, b$ymax)
  ## the mean and median lines carry their own legend, by line type
  lg <- h$legends[[1]]
  expect_identical(lg$items$label, c("Mean", "Median"))
  expect_identical(lg$items$linetype, c("dashed", "dotted"))
  ## an interaction plot: one legend for the colours and shapes of one factor
  i <- chart_spec(plots$inter)
  expect_length(i$legends, 1)
  expect_identical(i$legends[[1]]$items$label, c("V1", "V2", "V3"))
  expect_true(all(c("colour", "shape") %in% names(i$legends[[1]]$items)))
  ## a heat map: a colour bar, running from the lowest value to the highest
  hm <- chart_spec(plots$corheat)
  expect_identical(hm$legends[[1]]$type, "bar")
  expect_equal(range(hm$legends[[1]]$stops$at), c(0, 1))
  ## the loading plots keep their circle round
  expect_equal(chart_spec(plots$pcaload)$ratio, 1)
  expect_null(chart_spec(plots$main_bar)$ratio)
  ## a single box hides its one axis label, as the plot does
  expect_true(chart_spec(plots$box1)$hide$x)
  expect_false(chart_spec(plots$box)$hide$x)
  ## panels of a third factor, each with its strip (Day, the time factor, is on X)
  f <- chart_spec(plots$main_facet)
  expect_length(f$panels, 2)
  expect_identical(f$xlab, "Day")
  expect_identical(vapply(f$panels, `[[`, "", "strip"), c("N0", "N60"))
  ## segments that are arrows say so
  expect_true(!is.null(chart_spec(plots$pcaload)$layers[[4]]$arrow))
})

test_that("legend keys are the marks they stand for, and only grouping letters become pills", {
  plots <- app_plots()
  keys <- function(nm) chart_spec(plots[[nm]])$legends[[1]]$keys
  ## bars and boxes: a swatch; points: a point; lines through points: both;
  ## the mean and median lines, and the branches of a tree: a line
  expect_true("bar" %in% keys("main_bar"))
  expect_true("box" %in% keys("main_box"))
  expect_true(all(c("line", "point") %in% keys("inter")))
  expect_true(all(c("line", "point") %in% keys("main_line")))
  expect_identical(keys("pcascores"), "point")
  expect_identical(keys("hist"), "vline")
  expect_identical(keys("cldendro"), "segment")
  ## a layer that leaves the legend out adds nothing to it
  expect_false("text" %in% keys("clpcs"))
  ## the letters of a plot of means are marked as such; other text is not
  lets <- function(nm) vapply(chart_spec(plots[[nm]])$layers, function(l) isTRUE(l$letters), logical(1))
  expect_identical(lets("main1_bar"), c(FALSE, FALSE, TRUE))
  expect_true(any(lets("main_bar")))
  for (nm in c("main_heat", "pcaload", "faload", "faheat", "corheat", "clpcs", "diag1"))
    expect_false(any(lets(nm)), info = nm)
  ## the heat maps of coefficients and loadings share one scale, red to blue
  for (nm in c("corheat", "faheat")) {
    st <- chart_spec(plots[[nm]])$legends[[1]]$stops
    expect_identical(st$colour[c(1, nrow(st))], SLX_DIVERGE[c(1, 6)], info = nm)
  }
})

test_that("colours, line types and justification are given to the browser in one form", {
  expect_identical(hex_col(c("white", "#3b7dd8", "grey30", NA)), c("#FFFFFF", "#3B7DD8", "#4D4D4D", NA))
  expect_identical(lty_name(c("1", "2", "3", "dashed", "solid")), c("solid", "dashed", "dotted", "dashed", "solid"))
  expect_identical(just_num(c("left", "center", "right", "0.3")), c(0, 0.5, 1, 0.3))
})

test_that("a saved graph is named for what it shows", {
  expect_identical(plot_file("residuals-vs-fitted", "Yield"), "StatLabX_residuals-vs-fitted_Yield.png")
  expect_identical(plot_file("means-bar", "Plant height (cm)", "svg"), "StatLabX_means-bar_Plant-height-cm.svg")
  expect_identical(plot_file("correlation-heat-map"), "StatLabX_correlation-heat-map.png")
})

test_that("every graph in the app is a chart with a download menu, and every one is drawn by the server", {
  ui <- as.character(doepro_ui())
  tags <- regmatches(ui, gregexpr('<div[^>]*class="slx-chart"[^>]*>', ui))[[1]]
  ids <- sub('.*id="([A-Za-z]+)".*', "\\1", tags)
  expect_gt(length(ids), 25)
  expect_no_match(ui, "shiny-plot-output", fixed = TRUE)
  for (id in ids) {
    expect_match(ui, sprintf('data-plot="%s" data-fmt="png"', id), fixed = TRUE, info = id)
    expect_match(ui, sprintf('data-plot="%s" data-fmt="svg"', id), fixed = TRUE, info = id)
  }
  ## the browser's half, in a script of the page's head: it draws the charts,
  ## saves them, and names the platform on what it saves
  scripts <- function(x) {
    if (inherits(x, "shiny.tag"))
      return(c(if (identical(x$name, "script")) paste(vapply(x$children, as.character, ""), collapse = ""),
               unlist(lapply(x$children, scripts))))
    if (is.list(x)) unlist(lapply(x, scripts)) else character(0)
  }
  js <- scripts(doepro_ui())
  expect_true(APP_CHART_JS %in% js)
  for (k in c("statlabx.chart", "SLXChart", "Generated with StatLabX", "pHYs", "image/svg+xml"))
    expect_match(APP_CHART_JS, k, fixed = TRUE, info = k)
  ## and nothing in it but ASCII, as R code must be (other characters as \u escapes)
  expect_false(grepl("[^\\x01-\\x7F]", APP_CHART_JS, perl = TRUE))
  skip_on_cran()
  shiny::testServer(doepro_server, {
    expect_setequal(plots, ids)
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
