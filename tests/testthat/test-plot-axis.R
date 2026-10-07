## Interaction plots and the order of factor levels (known bug 3). Time should
## run along the X-axis in the order people mean (D0, D60, D120, D180, not the
## alphabetical D0, D120, D180, D60), the other factor should become the lines,
## and the user should be able to choose the X-axis factor. Every expected order
## below is written out by hand and every expected mean comes from base R, so a
## mistake in the package's own sorting cannot be copied into a test.

## ------------------------------------------------------------------ helpers --

AX_DAYS <- c("D0", "D60", "D120", "D180")
AX_VAR <- c("V1", "V2", "V3")
AX_NIT <- c("N0", "N60", "N120")

## Reorder rows without random numbers: 37 is invertible modulo the prime 101,
## so this is a permutation of up to 100 rows and the first appearance of each
## label is no longer its natural position.
ax_shuffle <- function(d) d[order((seq_len(nrow(d)) * 37) %% 101), , drop = FALSE]

## Variety x Days factorial in three blocks, every factor stored as character.
## The labels are listed out of order, and Days and Variety interact (V3 jumps
## at D180), so the interaction is significant and its letters are shown.
ax_vd <- function() {
  d <- expand.grid(Block = c("B2", "B3", "B1"),
                   Days = c("D120", "D0", "D180", "D60"),
                   Variety = c("V3", "V1", "V2"),
                   stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  di <- match(d$Days, AX_DAYS); vi <- match(d$Variety, AX_VAR)
  bi <- match(d$Block, c("B1", "B2", "B3"))
  d$Yield <- 20 + c(0, 1.2, 4.1, 6.3)[di] + c(0, 1.5, 3)[vi] + c(-0.4, 0, 0.4)[bi] +
             ifelse(vi == 3 & di == 4, 3, 0) + 0.5 * sin(seq_len(nrow(d)))
  ax_shuffle(d)
}

ax_vd_map <- function(factors = c("Variety", "Days"))
  list(response = "Yield", factors = factors, block = "Block")

## the same data with every factor given explicitly ordered levels
ax_as_factors <- function(d, lv) {
  for (v in names(lv)) d[[v]] <- factor(d[[v]], levels = lv[[v]])
  d
}

## Variety x Nitrogen x Days in two blocks, again as shuffled character data.
ax_vnd <- function() {
  d <- expand.grid(Block = c("B2", "B1"),
                   Days = c("D180", "D0", "D120", "D60"),
                   Nitrogen = c("N120", "N0", "N60"),
                   Variety = c("V2", "V1"),
                   stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  di <- match(d$Days, AX_DAYS); ni <- match(d$Nitrogen, AX_NIT)
  vi <- match(d$Variety, AX_VAR)
  d$Yield <- 25 + c(0, 1.5, 3.5, 5)[di] + c(0, 2, 3)[ni] + c(0, 1)[vi] +
             ifelse(ni == 3 & di == 4, 2, 0) + 0.4 * cos(seq_len(nrow(d)))
  ax_shuffle(d)
}

ax_vnd_fit <- function()
  analyze(ax_vnd(), "FRCBD", list(response = "Yield", block = "Block",
                                  factors = c("Variety", "Nitrogen", "Days")))

## a factor and its levels, the minimal "effect" and "data" default_x() reads
ax_effect <- function(...) {
  cols <- list(...)
  d <- as.data.frame(lapply(cols, function(lv) factor(lv, levels = lv)),
                     stringsAsFactors = FALSE)
  ## columns of different lengths: pad by recycling, only the levels matter
  list(e = list(vars = names(cols)), d = lapply(cols, function(lv) factor(lv, levels = lv)))
}

## build a plot quietly: ggplot2 4 announces labels that a geom does not use
ax_build <- function(p) suppressMessages(ggplot2::ggplot_build(p))

## the labels along the X- and Y-axes of the first panel, as drawn
ax_xlab <- function(b) b$layout$panel_params[[1]]$x$get_labels()
ax_ylab <- function(b) b$layout$panel_params[[1]]$y$get_labels()

## the levels a colour or fill scale shows in its legend
ax_legend <- function(b, aes) b$plot$scales$get_scales(aes)$get_limits()

## the plot's axis and legend titles, in ggplot2 3.x and 4.x alike
ax_labs <- function(p) {
  gl <- get0("get_labs", envir = asNamespace("ggplot2"), inherits = FALSE)
  if (is.function(gl)) gl(p) else p$labels
}

## ------------------------------------------------------- natural_levels() --

test_that("time labels sort chronologically, not alphabetically", {
  expect_identical(natural_levels(c("D120", "D0", "D180", "D60", "D0", "D120")), AX_DAYS)
  expect_identical(natural_levels(c("90 DAS", "30 DAS", "120 DAS", "60 DAS")),
                   c("30 DAS", "60 DAS", "90 DAS", "120 DAS"))
  wk <- paste0("Week", 1:12)
  expect_identical(natural_levels(rev(wk)), wk)
  expect_identical(natural_levels(wk[c(10, 2, 12, 1, 11, 3, 9, 4, 8, 5, 7, 6)]), wk)
  # alphabetical order would have put Week10 straight after Week1
  expect_false(identical(sort(wk), wk))
})

test_that("numbered treatments run T1, T2, ..., T12", {
  tr <- paste0("T", 1:12)
  expect_identical(natural_levels(tr[c(12, 1, 10, 2, 11, 3, 9, 4, 8, 5, 7, 6)]), tr)
  expect_identical(natural_levels(c("T10", "T9", "T1")), c("T1", "T9", "T10"))
  # the text before the number still sorts alphabetically
  expect_identical(natural_levels(c("B2", "A10", "A2")), c("A2", "A10", "B2"))
})

test_that("decimal quantities compare by value", {
  expect_identical(natural_levels(c("10 kg", "0.5 kg", "2 kg", "1.5 kg")),
                   c("0.5 kg", "1.5 kg", "2 kg", "10 kg"))
  expect_identical(natural_levels(c("10", "9.5", "100", "0.25")),
                   c("0.25", "9.5", "10", "100"))
})

test_that("a column that is already a factor keeps its own order", {
  # even an order that is neither natural nor alphabetical is the user's choice
  f <- factor(c("D60", "D0", "D120"), levels = c("D120", "D60", "D0"))
  expect_identical(natural_levels(f), c("D120", "D60", "D0"))
  f2 <- factor(c("b", "a", "c"), levels = c("c", "a", "b"))
  expect_identical(natural_levels(f2), c("c", "a", "b"))
})

test_that("a numeric column sorts numerically", {
  expect_identical(natural_levels(c(120, 0, 180, 60, 0)), c("0", "60", "120", "180"))
  expect_identical(natural_levels(c(10, 2, 1, 100)), c("1", "2", "10", "100"))
  expect_identical(natural_levels(c(0.5, 10, 1.5)), c("0.5", "1.5", "10"))
  expect_type(natural_levels(c(3, 1, 2)), "character")
})

test_that("missing values never become a level", {
  expect_identical(natural_levels(c("D60", NA, "D0", NA)), c("D0", "D60"))
  expect_identical(natural_levels(c(60, NA, 0)), c("0", "60"))
  expect_identical(natural_levels(factor(c("D60", NA, "D0"), levels = c("D0", "D60"))),
                   c("D0", "D60"))
})

## ------------------------------------------------------------- analyze() --

test_that("analyze() on character data gives tables of means in natural order", {
  d <- ax_vd()
  r <- analyze(d, "FRCBD", ax_vd_map())
  expect_identical(levels(r$data$Days), AX_DAYS)
  expect_identical(levels(r$data$Variety), AX_VAR)
  expect_identical(levels(r$data$Block), c("B1", "B2", "B3"))

  # the Days table runs D0, D60, D120, D180 with the means base R gives
  e <- r$effects[["Days"]]
  expect_identical(as.character(e$means$Days), AX_DAYS)
  expect_equal(e$means$Mean, unname(as.vector(tapply(d$Yield, d$Days, mean)[AX_DAYS])),
               tolerance = 1e-10)
  fit <- stats::lm(Yield ~ Block + Variety * Days, data = d)
  mse <- sum(stats::residuals(fit)^2) / stats::df.residual(fit)
  expect_equal(e$means$SE, rep(sqrt(mse / 9), 4), tolerance = 1e-10)

  # in the two-factor table the days run in order within every variety
  m <- r$effects[["Variety:Days"]]$means
  for (v in AX_VAR)
    expect_identical(as.character(m$Days[m$Variety == v]), AX_DAYS)
  cell <- tapply(d$Yield, list(d$Variety, d$Days), mean)
  expect_equal(m$Mean, unname(cell[cbind(as.character(m$Variety), as.character(m$Days))]),
               tolerance = 1e-10)

  # the two-way table of means has its columns in the same order
  tw <- two_way(r$data, "Yield", "Variety", "Days")
  expect_identical(names(tw), c("Variety", AX_DAYS, "Mean"))
  expect_identical(tw$Variety, c(AX_VAR, "Mean"))
})

test_that("a numeric time column and numbered treatments keep their natural order", {
  d <- ax_vd()
  d$Days <- as.numeric(sub("D", "", d$Days))
  r <- analyze(d, "FRCBD", ax_vd_map())
  expect_identical(levels(r$data$Days), c("0", "60", "120", "180"))
  expect_identical(as.character(r$effects[["Days"]]$means$Days), c("0", "60", "120", "180"))
  expect_equal(r$effects[["Days"]]$means$Mean,
               unname(as.vector(tapply(d$Yield, d$Days, mean)[c("0", "60", "120", "180")])),
               tolerance = 1e-10)

  tr <- paste0("T", 1:10)
  dc <- ax_shuffle(data.frame(Treatment = rep(tr, each = 3),
                              Yield = rep(10 + seq_along(tr), each = 3) +
                                      rep(c(-0.3, 0.1, 0.2), 10),
                              stringsAsFactors = FALSE))
  rc <- analyze(dc, "CRD", list(response = "Yield", treat = "Treatment"))
  expect_identical(as.character(rc$effects[["Treatment"]]$means$Treatment), tr)
})

test_that("character data and explicitly ordered factors give identical statistics", {
  d <- ax_vd()
  lv <- list(Days = AX_DAYS, Variety = AX_VAR, Block = c("B1", "B2", "B3"))
  r_chr <- analyze(d, "FRCBD", ax_vd_map())
  r_fac <- analyze(ax_as_factors(d, lv), "FRCBD", ax_vd_map())
  expect_equal(r_chr$anova, r_fac$anova)
  expect_identical(names(r_chr$effects), names(r_fac$effects))
  for (k in names(r_fac$effects)) {
    a <- r_chr$effects[[k]]; b <- r_fac$effects[[k]]
    expect_equal(a$means, b$means)            # means, N, SD, SE, letters, row order
    expect_equal(a$sed_mat, b$sed_mat)
    expect_equal(a$cd_mat, b$cd_mat)
    expect_equal(a$p, b$p)
  }
})

test_that("the natural order changes no statistic, only the order of the rows", {
  # the same data as factors in alphabetical order (D0, D120, D180, D60): each
  # level must keep its mean, SE and letter, whichever row it sits in
  d <- ax_vd()
  r_nat <- analyze(d, "FRCBD", ax_vd_map())
  r_alp <- analyze(ax_as_factors(d, list(Days = sort(AX_DAYS))), "FRCBD", ax_vd_map())
  expect_identical(levels(r_alp$data$Days), c("D0", "D120", "D180", "D60"))
  expect_equal(r_nat$anova, r_alp$anova, tolerance = 1e-10)
  for (k in c("Days", "Variety:Days")) {
    a <- r_nat$effects[[k]]$means; b <- r_alp$effects[[k]]$means
    ka <- paste(a$Variety, a$Days); kb <- paste(b$Variety, b$Days)
    b <- b[match(ka, kb), ]
    expect_equal(a$Mean, b$Mean, tolerance = 1e-10)
    expect_equal(a$SE, b$SE, tolerance = 1e-10)
    expect_identical(a$Letter, b$Letter)
  }
})

test_that("an unbalanced factorial gives the same adjusted means either way", {
  d <- ax_vd()
  d <- d[-c(5, 17), ]                       # two plots lost
  lv <- list(Days = AX_DAYS, Variety = AX_VAR, Block = c("B1", "B2", "B3"))
  r_chr <- analyze(d, "FRCBD", ax_vd_map())
  r_fac <- analyze(ax_as_factors(d, lv), "FRCBD", ax_vd_map())
  expect_false(r_chr$balanced)
  for (k in names(r_fac$effects)) {
    expect_identical(as.character(r_chr$effects[[k]]$means$Days),
                     as.character(r_fac$effects[[k]]$means$Days))
    expect_equal(r_chr$effects[[k]]$means, r_fac$effects[[k]]$means, tolerance = 1e-10)
  }
})

## ------------------------------------------------------------ default_x() --

## default_x() needs only the effect's factor names and the data's levels
ax_dx <- function(...) {
  cols <- list(...)
  d <- lapply(cols, function(lv) factor(lv, levels = lv))
  default_x(list(vars = names(cols)), d)
}

test_that("the time factor goes on the X-axis whatever the column order", {
  d <- ax_vd()
  for (fs in list(c("Variety", "Days"), c("Days", "Variety"))) {
    r <- analyze(d, "FRCBD", ax_vd_map(fs))
    k <- paste(fs, collapse = ":")
    expect_identical(default_x(r$effects[[k]], r$data), "Days")
  }
  # recognised from the labels alone, under a name that says nothing of time
  expect_identical(ax_dx(Treatment = paste0("T", 1:5),
                         Sampling = c("30 DAS", "60 DAS", "90 DAS")), "Sampling")
  expect_identical(ax_dx(Variety = AX_VAR, Obs = c("D0", "D30", "D60")), "Obs")
  # Week1, Week2, ... count from one but are still time, and time beats a dose
  expect_identical(ax_dx(Dose = c("0.5 kg", "1.5 kg", "10 kg"),
                         Obs = paste0("Week", 1:4)), "Obs")
  # recognised from the column name when the labels are numbered
  expect_identical(ax_dx(Nitrogen = AX_NIT, Interval = c("I1", "I2", "I3")), "Interval")
  # but not with worded labels: they sort alphabetically, so a line through
  # them would run Flowering, Maturity, Vegetative
  expect_identical(ax_dx(Variety = AX_VAR,
                         Stage = c("Vegetative", "Flowering", "Maturity")), "Variety")
  expect_identical(ax_dx(Variety = AX_VAR, Storage_period = c("P1", "P2")),
                   "Storage_period")
})

test_that("a quantitative factor beats one that is merely numbered", {
  expect_identical(ax_dx(Variety = AX_VAR, Nitrogen = AX_NIT), "Nitrogen")
  expect_identical(ax_dx(Nitrogen = AX_NIT, Variety = AX_VAR), "Nitrogen")
  expect_identical(ax_dx(Treatment = paste0("T", 1:4), Rate = c("0", "25", "50")), "Rate")
  expect_identical(ax_dx(Variety = AX_VAR, Dose = c("0.5 kg", "1.5 kg", "10 kg")), "Dose")
  # on the demo factorial the variety comes first but nitrogen goes on X
  r <- analyze(demo_data("FRCBD"), "FRCBD",
               list(response = "Yield", block = "Block", factors = c("Variety", "Nitrogen")))
  expect_identical(default_x(r$effects[["Variety:Nitrogen"]], r$data), "Nitrogen")
})

test_that("two time factors tie, and the one with more levels wins", {
  # Year (2021, 2022) and Days (D0 to D180) are both time and both quantities
  expect_identical(ax_dx(Year = c("2021", "2022"), Days = AX_DAYS), "Days")
  expect_identical(ax_dx(Days = AX_DAYS, Year = c("2021", "2022")), "Days")
  expect_identical(ax_dx(Year = c("2019", "2020", "2021", "2022", "2023"), Days = AX_DAYS),
                   "Year")
})

test_that("with nothing to go on, the effect's first factor stays on X", {
  # numbering that runs 1, 2, 3 or 0, 1, 2 says nothing about quantity
  expect_identical(ax_dx(Irrigation = c("I1", "I2", "I3"), Variety = paste0("V", 1:4)),
                   "Irrigation")
  expect_identical(ax_dx(Variety = paste0("V", 1:4), Irrigation = c("I1", "I2", "I3")),
                   "Variety")
  expect_identical(ax_dx(Tillage = c("Conventional", "Minimum", "Zero"),
                         Mulch = c("M0", "M1", "M2", "M3")), "Tillage")
  expect_identical(ax_dx(Location = c("Shalimar", "Wadura"), Variety = AX_VAR), "Location")
  # and the demo designs whose factors are all plain labels keep their order
  r <- analyze(demo_data("SPLIT"), "SPLIT",
               list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
  expect_identical(default_x(r$effects[["Irrigation:Variety"]], r$data), "Irrigation")
})

## ------------------------------------------------------------ plot_main() --

test_that("by default the time factor is on X and the other factor is the lines", {
  d <- ax_vd()
  for (fs in list(c("Variety", "Days"), c("Days", "Variety"))) {
    r <- analyze(d, "FRCBD", ax_vd_map(fs))
    k <- paste(fs, collapse = ":")
    p <- plot_main(r, k, "line")
    b <- ax_build(p)
    expect_identical(ax_xlab(b), AX_DAYS)
    expect_identical(ax_legend(b, "colour"), AX_VAR)
    expect_identical(ax_labs(p)$x, "Days")
    expect_identical(ax_labs(p)$colour, "Variety")
  }
})

test_that("each line joins one variety's means from D0 through to D180", {
  d <- ax_vd()
  r <- analyze(d, "FRCBD", ax_vd_map())
  b <- ax_build(plot_main(r, "Variety:Days", "line"))
  ln <- b$data[[1]]                         # the geom_line layer
  cell <- tapply(d$Yield, list(d$Variety, d$Days), mean)
  for (j in seq_along(AX_VAR)) {
    g <- ln[ln$group == j, ]
    g <- g[order(g$x), ]
    expect_equal(g$x, 1:4)
    expect_equal(g$y, unname(cell[AX_VAR[j], AX_DAYS]), tolerance = 1e-10)
  }
})

test_that("plot_main() honours the X-axis factor the user chooses", {
  r <- analyze(ax_vd(), "FRCBD", ax_vd_map())
  for (ty in c("bar", "line")) {
    p <- plot_main(r, "Variety:Days", ty, x_var = "Variety")
    b <- ax_build(p)
    expect_identical(ax_xlab(b), AX_VAR)
    expect_identical(ax_labs(p)$x, "Variety")
    expect_identical(ax_labs(p)$colour, "Days")
    # the days become the legend, still in chronological order
    expect_identical(ax_legend(b, if (ty == "bar") "fill" else "colour"), AX_DAYS)
  }
  # the heat map puts the chosen factor on X and the other on Y
  b <- ax_build(plot_main(r, "Variety:Days", "heat", x_var = "Variety"))
  expect_identical(ax_xlab(b), AX_VAR)
  expect_identical(ax_ylab(b), AX_DAYS)
  b <- ax_build(plot_main(r, "Variety:Days", "heat"))
  expect_identical(ax_xlab(b), AX_DAYS)
  expect_identical(ax_ylab(b), AX_VAR)
  # the box plot draws the raw data on the chosen axis
  b <- ax_build(plot_main(r, "Variety:Days", "box", x_var = "Variety"))
  expect_identical(ax_xlab(b), AX_VAR)
  expect_identical(ax_legend(b, "fill"), AX_DAYS)
})

test_that("a name that is not one of the effect's factors falls back to the default", {
  r <- analyze(ax_vd(), "FRCBD", ax_vd_map())
  for (xv in list(NULL, "Block", "Nonsense", "Yield")) {
    p <- plot_main(r, "Variety:Days", "bar", x_var = xv)
    expect_identical(ax_labs(p)$x, "Days")
    expect_identical(ax_xlab(ax_build(p)), AX_DAYS)
  }
  # a single-factor effect has only one possible axis
  p <- plot_main(r, "Days", "bar", x_var = "Variety")
  expect_identical(ax_labs(p)$x, "Days")
  expect_identical(ax_xlab(ax_build(p)), AX_DAYS)
})

test_that("the letters stay with their own bars after the axes are swapped", {
  d <- ax_vd()
  r <- analyze(d, "FRCBD", ax_vd_map())
  e <- r$effects[["Variety:Days"]]
  expect_lt(e$p, e$alpha)                   # significant, so letters are drawn
  m <- e$means
  for (xv in c("Days", "Variety")) {
    other <- setdiff(c("Variety", "Days"), xv)
    lx <- if (xv == "Days") AX_DAYS else AX_VAR
    lg <- if (xv == "Days") AX_VAR else AX_DAYS
    b <- ax_build(plot_main(r, "Variety:Days", "bar", show_letters = TRUE, x_var = xv))
    txt <- b$data[[3]]                      # the geom_text layer
    expect_equal(nrow(txt), 12)
    at_x <- lx[round(txt$x)]; at_g <- lg[txt$group]
    row <- match(paste(at_x, at_g), paste(m[[xv]], m[[other]]))
    expect_false(anyNA(row))
    expect_identical(txt$label, m$Letter[row])
    expect_equal(txt$y, m$Mean[row] + m$SE[row], tolerance = 1e-10)
  }
})

test_that("a third factor becomes panels, in natural order", {
  r <- ax_vnd_fit()
  k <- "Variety:Nitrogen:Days"
  expect_identical(default_x(r$effects[[k]], r$data), "Days")

  # default: Days on X, Variety (the next factor) as colour, Nitrogen as panels
  p <- plot_main(r, k, "line")
  b <- ax_build(p)
  expect_identical(ax_xlab(b), AX_DAYS)
  expect_identical(ax_legend(b, "colour"), c("V1", "V2"))
  expect_identical(ax_labs(p)$colour, "Variety")
  lay <- b$layout$layout
  expect_identical(as.character(lay$Nitrogen[order(lay$PANEL)]), AX_NIT)

  # Nitrogen chosen: Variety stays the colour, Days become the panels
  p <- plot_main(r, k, "bar", x_var = "Nitrogen")
  b <- ax_build(p)
  expect_identical(ax_xlab(b), AX_NIT)
  expect_identical(ax_legend(b, "fill"), c("V1", "V2"))
  lay <- b$layout$layout
  expect_identical(as.character(lay$Days[order(lay$PANEL)]), AX_DAYS)

  # Variety chosen: Nitrogen becomes the colour and Days the panels
  p <- plot_main(r, k, "line", x_var = "Variety")
  b <- ax_build(p)
  expect_identical(ax_xlab(b), c("V1", "V2"))
  expect_identical(ax_legend(b, "colour"), AX_NIT)
  expect_identical(ax_labs(p)$colour, "Nitrogen")
  lay <- b$layout$layout
  expect_identical(as.character(lay$Days[order(lay$PANEL)]), AX_DAYS)
})

test_that("every plot type builds for two- and three-factor effects", {
  r2 <- analyze(ax_vd(), "FRCBD", ax_vd_map())
  r3 <- ax_vnd_fit()
  for (ty in c("bar", "line", "heat", "box")) {
    for (xv in list(NULL, "Variety")) {
      b <- expect_no_error(ax_build(plot_main(r2, "Variety:Days", ty, x_var = xv)))
      expect_identical(ax_xlab(b), if (is.null(xv)) AX_DAYS else AX_VAR)
      b <- expect_no_error(ax_build(plot_main(r3, "Variety:Nitrogen:Days", ty, x_var = xv)))
      expect_identical(ax_xlab(b), if (is.null(xv)) AX_DAYS else c("V1", "V2"))
      # by default Days is the X-axis and Nitrogen the panels (3 rates); with
      # Variety on X, Nitrogen becomes the colour and Days the panels (4 dates)
      expect_identical(nrow(b$layout$layout), if (is.null(xv)) 3L else 4L)
    }
  }
})

test_that("every plot type builds for the split, strip and pooled interactions", {
  specs <- list(
    SPLIT = list("SPLIT", list(rep = "Rep", main = "Irrigation", sub = "Variety"),
                 "Irrigation:Variety"),
    STRIP = list("STRIP", list(rep = "Rep", main = "Tillage", sub = "Mulch"),
                 "Tillage:Mulch"),
    POOLRCBD = list("POOLRCBD", list(env = "Location", rep = "Rep", treat = "Variety"),
                    "Location:Variety"),
    POOLCRD = list("POOLCRD", list(env = "Season", treat = "Treatment"),
                   "Season:Treatment"),
    POOLFACT = list("POOLFRCBD", list(env = "Location", rep = "Rep",
                                      factors = c("Nitrogen", "Variety")),
                    "Location:Nitrogen:Variety"),
    POOLFACTC = list("POOLFCRD", list(env = "Season", factors = c("Spacing", "Variety")),
                     "Season:Spacing:Variety"))
  for (nm in names(specs)) {
    s <- specs[[nm]]
    r <- analyze(demo_data(nm), s[[1]], c(list(response = "Yield"), s[[2]]))
    k <- s[[3]]
    v <- strsplit(k, ":")[[1]]
    expect_true(k %in% names(r$effects), info = nm)
    # nothing in these demos is time or a quantity, so the first factor stays
    expect_identical(default_x(r$effects[[k]], r$data), v[1], info = nm)
    for (ty in c("bar", "line", "heat", "box")) {
      for (xv in v) {
        b <- expect_no_error(ax_build(plot_main(r, k, ty, x_var = xv)))
        expect_identical(ax_xlab(b), levels(r$data[[xv]]), info = paste(nm, ty, xv))
        expect_identical(nrow(b$layout$layout),
                         if (length(v) > 2) nlevels(r$data[[setdiff(v, xv)[2]]]) else 1L,
                         info = paste(nm, ty, xv))
      }
    }
  }
})

test_that("the caption keeps describing the letters whichever factor is on X", {
  # a split-plot interaction's letters compare means within one main plot; the
  # caption must still say so when the sub-plot factor is put on the X-axis
  r <- analyze(demo_data("SPLIT"), "SPLIT",
               list(response = "Yield", rep = "Rep", main = "Irrigation", sub = "Variety"))
  e <- r$effects[["Irrigation:Variety"]]
  for (xv in c("Irrigation", "Variety")) {
    cap <- ax_labs(plot_main(r, "Irrigation:Variety", "bar", x_var = xv))$caption
    expect_match(cap, "same level of Irrigation", fixed = TRUE)
  }
})

## -------------------------------------------------------------------- app --

test_that("the Plots tab offers an X-axis selector", {
  ui <- as.character(doepro_ui())
  expect_match(ui, 'id="plXUI"', fixed = TRUE)
})

test_that("the app suggests the X-axis factor and passes the choice to the plot", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    session$setInputs(demo = "FRCBD", loaddemo = 1)
    session$setInputs(design = "FRCBD", nfac = 2, alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", block = "Block", f1 = "Variety", f2 = "Nitrogen")
    session$setInputs(tr_1 = "none", run = 1)
    session$setInputs(plEff = "Variety:Nitrogen", plType = "line", plLetters = TRUE)
    # the selector lists both factors and suggests Nitrogen (a quantity)
    ui <- output$plXUI$html
    expect_match(ui, 'id="plX"', fixed = TRUE)
    expect_match(ui, '<option value="Nitrogen" selected>', fixed = TRUE)
    expect_match(ui, '<option value="Variety">', fixed = TRUE)
    expect_identical(ax_labs(mp())$x, "Nitrogen")
    # the user's choice reaches the plot
    session$setInputs(plX = "Variety")
    expect_identical(ax_labs(mp())$x, "Variety")
    # a main effect has a single axis and no selector
    session$setInputs(plEff = "Nitrogen")
    expect_null(output$plXUI)
    expect_identical(ax_labs(mp())$x, "Nitrogen")
  })
})
