## Descriptive statistics: the summary table, its reading, the four
## distribution plots and their readings, and the Explore tab of the app.
##
## Expectations come from base R and from the definitions, not from the
## code's own output:
##   * quartiles are calculated as Excel's QUARTILE.INC does, which is
##     quantile(type = 7);
##   * CV = 100 * SD / mean, and only for values that cannot be negative with
##     a positive mean;
##   * unusual values are those beyond Tukey's fences (1.5 times the
##     interquartile range beyond the quartiles), which are exactly the points
##     a ggplot2 box plot draws on their own;
##   * the readings describe the observations; they never claim that groups or
##     treatments differ, which is for the analysis of variance to judge;
##   * skewness is Excel's SKEW (the adjusted Fisher-Pearson coefficient),
##     read with Bulmer's bands (0.5 and 1);
##   * every sentence is true of the numbers it describes, and each plot's
##     note is worked out from the rows that plot draws.

## every sentence the descriptives can write about a variable
all_text <- function(d, v, group = NULL) {
  c(if (is.null(group)) describe_text(v, d[[v]], row_ids(d))
    else describe_group_text(v, d[[v]], group_labels(d, group), row_ids(d), group),
    unlist(lapply(c("hist", "density", "box", "qq"),
                  function(k) unlist(plot_text(k, d, v, group)))))
}

## ---------------------------------------------------------------- table ----

test_that("the summary of a variable matches base R", {
  d <- demo_data("RCBD"); y <- d$Yield
  tab <- describe_data(d, "Yield")
  expect_identical(names(tab), c("Variable", "N", "Missing", "Mean", "SD", "Min",
                                 "Q1", "Median", "Q3", "Max", "CV", "Skewness"))
  expect_identical(tab$Variable, "Yield")
  expect_equal(tab$N, 24); expect_equal(tab$Missing, 0)
  expect_equal(tab$Mean, mean(y)); expect_equal(tab$SD, sd(y))
  expect_equal(c(tab$Min, tab$Max), range(y))
  expect_equal(c(tab$Q1, tab$Median, tab$Q3),
               unname(quantile(y, c(0.25, 0.5, 0.75), type = 7)))
  expect_equal(tab$Median, median(y))
  expect_equal(tab$CV, 100 * sd(y) / mean(y))
  # Excel's SKEW: n / ((n - 1)(n - 2)) * sum(((y - mean) / sd)^3)
  n <- length(y)
  expect_equal(tab$Skewness, n / ((n - 1) * (n - 2)) * sum(((y - mean(y)) / sd(y))^3))
})

test_that("quartiles are Excel's QUARTILE.INC", {
  # QUARTILE.INC(1..10, 1) = 3.25 and QUARTILE.INC(1..10, 3) = 7.75
  tab <- describe_data(data.frame(x = 1:10), "x")
  expect_equal(c(tab$Q1, tab$Median, tab$Q3), c(3.25, 5.5, 7.75))
  # with five values the quartiles are the second and fourth
  tab <- describe_data(data.frame(x = c(9, 1, 7, 3, 5)), "x")
  expect_equal(c(tab$Min, tab$Q1, tab$Median, tab$Q3, tab$Max), c(1, 3, 5, 7, 9))
})

test_that("several variables give one row each, in the order asked", {
  d <- data.frame(a = c(1, 2, 3, 4), b = c(10, 20, 30, 50))
  tab <- describe_data(d, c("b", "a"))
  expect_identical(tab$Variable, c("b", "a"))
  expect_equal(tab$Mean, c(27.5, 2.5))
  expect_identical(rownames(tab), c("1", "2"))
  expect_error(describe_data(d, character(0)), "at least one variable")
})

test_that("a grouping column gives one row per level, in natural order", {
  d <- data.frame(Trt = c("T10", "T2", "T1", "T2", "T10", "T1", "T2"),
                  y = c(5, 7, 1, 9, 6, 3, 8))
  tab <- describe_data(d, "y", group = "Trt")
  expect_identical(names(tab)[1:3], c("Variable", "Group", "N"))
  expect_identical(tab$Group, c("T1", "T2", "T10"))
  expect_equal(tab$N, c(2, 3, 2))
  expect_equal(tab$Mean, c(2, 8, 5.5))
  expect_equal(tab$SD, c(sd(c(1, 3)), 1, sd(c(5, 6))))

  # an empty group name means no grouping
  expect_false("Group" %in% names(describe_data(d, "y", group = "")))

  # every group agrees with tapply
  d <- demo_data("FRCBD")
  tab <- describe_data(d, "Yield", "Nitrogen")
  expect_identical(tab$Group, levels(d$Nitrogen))
  expect_equal(tab$Mean, as.vector(tapply(d$Yield, d$Nitrogen, mean)))
  expect_equal(tab$SD, as.vector(tapply(d$Yield, d$Nitrogen, sd)))
  expect_equal(tab$Q3, as.vector(tapply(d$Yield, d$Nitrogen, quantile, 0.75, type = 7)))
})

test_that("rows without a group label are left out of the groups", {
  d <- data.frame(G = c("A", "A", NA, "B", "B", "B"), y = c(1, 2, 100, 4, 5, 6))
  tab <- describe_data(d, "y", "G")
  expect_identical(tab$Group, c("A", "B"))
  expect_equal(tab$N, c(2, 3))
  expect_equal(sum(tab$N), 5)
  # the box plot leaves the same row out rather than drawing an NA box
  b <- ggplot2::ggplot_build(plot_box(d, "y", "G"))$data[[1]]
  expect_equal(nrow(b), 2)
  expect_equal(b$ymax, c(2, 6))
})

test_that("entries that are not plainly numbers count as missing", {
  # 5,6 (decimal comma), blank, NA, 12a, n/a and Inf are not used; three are
  d <- data.frame(y = c("5.6", "5,6", "", NA, "12a", "7", "n/a", "8.0"),
                  stringsAsFactors = FALSE)
  tab <- describe_data(d, "y")
  expect_equal(tab$N, 3); expect_equal(tab$Missing, 5)
  expect_equal(tab$Mean, mean(c(5.6, 7, 8)))
  expect_match(describe_text("y", response_values(d$y), 1:8),
               "5 of the 8 rows have no usable value and are left out", fixed = TRUE)

  tab <- describe_data(data.frame(y = c(1, 2, Inf, 4, NA)), "y")
  expect_equal(tab$N, 3); expect_equal(tab$Missing, 2)
  expect_equal(tab$Max, 4)
  expect_match(describe_text("y", c(1, 2, Inf, 4, NA), 1:5),
               "2 of the 5 rows have no usable value", fixed = TRUE)
  expect_match(describe_text("y", c(1, 2, 3, NA), 1:4),
               "1 of the 4 rows has no usable value and is left out", fixed = TRUE)
})

test_that("CV is given only for values that cannot be negative", {
  cv <- function(x) describe_data(data.frame(x = x), "x")$CV
  expect_equal(cv(c(2, 4, 6)), 100 * sd(c(2, 4, 6)) / 4)
  expect_equal(cv(c(0, 0, 3)), 100 * sd(c(0, 0, 3)) / 1)   # zeros are allowed
  expect_true(is.na(cv(c(-3, -1, 2, 5, 1))))               # crosses zero
  expect_true(is.na(cv(c(-5, -4, -6))))                    # all negative
  expect_true(is.na(cv(c(0, 0, 0))))                       # mean zero
  expect_true(is.na(cv(7)))                                # one value
  txt <- describe_text("x", c(-3, -1, 2, 5, 1), 1:5)
  expect_match(txt, "No coefficient of variation is given: it needs values that cannot be negative, and the smallest value here is -3.", fixed = TRUE)
  expect_no_match(txt, "The CV of", fixed = TRUE)
})

test_that("degenerate variables are described without an error", {
  tab <- describe_data(data.frame(x = c(NA, NA)), "x")
  expect_equal(tab$N, 0); expect_equal(tab$Missing, 2)
  expect_true(all(is.na(unlist(tab[c("Mean", "SD", "Min", "Q1", "Median", "Q3", "Max", "CV", "Skewness")]))))
  expect_match(describe_text("x", c(NA, NA), 1:2), "no values that can be read as numbers", fixed = TRUE)

  tab <- describe_data(data.frame(x = c(NA, 4)), "x")
  expect_equal(tab$N, 1); expect_true(is.na(tab$SD)); expect_true(is.na(tab$CV))
  expect_match(describe_text("x", c(NA, 4), 1:2), "has a single value, 4", fixed = TRUE)

  expect_match(describe_text("x", c(5, 5, 5), 1:3), "Every value of 'x' is 5", fixed = TRUE)
  expect_match(describe_text("x", c(5, 5, 5), 1:3), "no variation to describe", fixed = TRUE)
})

test_that("decimals follow the size of the values", {
  expect_identical(var_digits(c(46.2, 55.13)), 2L)       # about three figures
  expect_identical(var_digits(c(0.0123, 0.05)), 5L)      # small units keep their figures
  expect_identical(var_digits(c(12345.6, 2.1)), 0L)      # at least none
  expect_identical(var_digits(c(3, 5, 7)), 2L)           # whole numbers: two at most
  expect_identical(var_digits(c(0, 0)), 2L)
  expect_identical(var_digits(c(NA, Inf, 5.5)), 3L)      # infinite values ignored
  # a small spread around a large value is not rounded away
  expect_identical(var_digits(c(1000.1, 1000.2, 1000.3)), 3L)
  # very small values keep about three significant figures
  tiny <- c(0.000021, 0.000034, 0.000018, 0.000047, 0.000029, 0.000052)
  html <- describe_html(describe_data(data.frame(Conc = tiny), "Conc"), data.frame(Conc = tiny))
  expect_no_match(html, ">0.0000<", fixed = TRUE)
  expect_match(html, sprintf(">%s<", formatC(mean(tiny), digits = var_digits(tiny), format = "f")), fixed = TRUE)
  expect_identical(signif(as.numeric(fmt(sd(tiny), var_digits(tiny))), 2), signif(sd(tiny), 2))
})

test_that("figures that round to zero carry no minus sign", {
  expect_identical(dfmt(-0.0004, 3), "0.000")
  expect_identical(dfmt(-0.4, 0), "0")
  expect_identical(dfmt(-0.6, 0), "-1")
  expect_identical(dfmt(c(-0.00001, -2.5, NA), 2), c("0.00", "-2.50", "-"))
  x <- c(-2, 1, 3, -1.5, -0.502)
  expect_match(describe_text("Change", x, 1:5), "average 0.000 (median -0.502)", fixed = TRUE)
})

test_that("the table is HTML-safe and has a group column only when grouped", {
  d <- data.frame(G = c("<a>", "<a>", "b&c", "b&c"), `<y>` = c(1, 2, 3, 4), check.names = FALSE)
  html <- describe_html(describe_data(d, "<y>", "G"), d)
  expect_match(html, "&lt;y&gt;", fixed = TRUE)
  expect_match(html, "&lt;a&gt;", fixed = TRUE)
  expect_match(html, "b&amp;c", fixed = TRUE)
  expect_no_match(html, "<y>", fixed = TRUE)
  expect_match(html, "<th>Group</th>", fixed = TRUE)
  expect_match(html, "CV (%)", fixed = TRUE)
  expect_no_match(describe_html(describe_data(d, "<y>"), d), "<th>Group</th>", fixed = TRUE)
  # whole numbers: two decimals; CV always one
  html <- describe_html(describe_data(data.frame(x = c(3, 5, 7)), "x"), data.frame(x = c(3, 5, 7)))
  expect_match(html, ">5.00<", fixed = TRUE)
  expect_match(html, ">40.0<", fixed = TRUE)
})

## -------------------------------------------------------------- reading ----

test_that("the reading states the centre, range, quartiles and CV", {
  y <- demo_data("RCBD")$Yield
  txt <- describe_text("Yield", y, seq_along(y))
  expect_match(txt, sprintf("The 24 values of 'Yield' average %s (median %s)",
                            fmt(mean(y), 2), fmt(median(y), 2)), fixed = TRUE)
  expect_match(txt, sprintf("run from %s to %s", fmt(min(y), 2), fmt(max(y), 2)), fixed = TRUE)
  expect_match(txt, sprintf("The CV of %s%%", fmt(100 * sd(y) / mean(y), 1)), fixed = TRUE)
  # the CV of raw data is not the CV of an analysis of variance
  expect_match(txt, "not the experimental CV of an analysis of variance", fixed = TRUE)
})

test_that("the CV is put into words at the stated thresholds", {
  expect_identical(cv_words(5), "little")
  expect_identical(cv_words(10), "moderately")
  expect_identical(cv_words(19.9), "moderately")
  expect_identical(cv_words(20), "considerably")
  expect_identical(cv_words(30), "a great deal")
  expect_identical(cv_words(NA), "")
  # the word follows the CV as printed: 9.97 prints as 10.0
  expect_identical(cv_words(9.97), "moderately")
  expect_identical(cv_words(9.94), "little")
})

test_that("the shape is read from the skewness with Bulmer's bands", {
  r <- c(1, 1, 2, 2, 2, 3, 3, 4, 5, 8, 12, 20)
  shape <- function(x) shape_of(desc_one(x))
  expect_identical(shape(r)$kind, "right")
  expect_identical(shape(r)$strength, "strongly")                  # G1 over 1
  expect_identical(shape(30 - r)$kind, "left")
  expect_identical(shape(1:20)$kind, "symmetric")
  expect_identical(shape(c(1, 2, 9, 10))$kind, "unclear")          # fewer than five
  expect_identical(shape(c(5, 5, 5, 5))$kind, "constant")
  expect_true(shape(c(rep(0, 16), 2, 5, 9, 20))$heaped)
  # the bands: 0.5 and 1
  g <- function(x) skew_of(x)
  m <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 7)
  expect_gt(g(m), 0.5); expect_lt(g(m), 1)
  expect_identical(shape(m)$strength, "moderately")
  # a skewness inside two standard errors is flagged as possibly chance
  expect_false(shape(m)$clear)
  expect_true(shape(c(r, r, r))$clear)

  # the direction is stated in figures the table shows: min, median, max
  expect_match(describe_text("x", r, seq_along(r)), sprintf(
    "The skewness of %s means the values are strongly skewed to the right: they reach further above the median (up to 20.00) than below it (down to 1.00).",
    fmt(g(r), 2)), fixed = TRUE)
  expect_match(describe_text("x", 30 - r, seq_along(r)),
    "skewed to the left: they reach further below the median (down to 10.00) than above it (up to 29.00).", fixed = TRUE)
  expect_match(describe_text("x", 1:20, 1:20), "is close to zero, so by this measure the values are spread roughly evenly about their mean", fixed = TRUE)
  expect_match(describe_text("x", m, 1:10), "can also arise by chance in values from a symmetric distribution", fixed = TRUE)
  expect_match(describe_text("x", c(1, 2, 9, 10), 1:4), "too few values to say much about the shape", fixed = TRUE)
  # with fewer than five values the quartiles are not put into words
  expect_no_match(describe_text("y", c(20, 40), 1:2), "middle half", fixed = TRUE)
  expect_no_match(describe_text("y", c(20, 40), 1:2), "rough guide", fixed = TRUE)
  txt <- describe_text("Pests", c(rep(0, 16), 2, 5, 9, 20), 1:20)
  expect_match(txt, "At least half the values are exactly 0. The skewness of", fixed = TRUE)
})

test_that("count data with ties are not called skewed the wrong way", {
  # pests per plant: a peak at 2 and a run of higher counts (found in review:
  # the quartile measure called this left skew)
  x <- rep(0:6, c(6, 7, 11, 2, 1, 2, 1))
  expect_gt(skew_of(x), 0.5)
  expect_identical(shape_of(desc_one(x))$kind, "right")
  d <- data.frame(Pests = x)
  expect_match(describe_text("Pests", x, seq_along(x)), "skewed to the right: they reach further above the median (up to 6.00) than below it (down to 0.00)", fixed = TRUE)
  expect_match(plot_text("hist", d, "Pests")$reading, "The bars reach further above the median than below it (the skewness is 1.06).", fixed = TRUE)
  # counts in steps wider than the smoothing: the note warns that bumps at
  # the counts come from the steps
  expect_gt(1, 2 * bw.nrd0(x))
  expect_match(plot_text("density", d, "Pests")$reading,
               "The values are recorded in steps of 1, wide beside the smoothing, so the curve may rise and fall at each recorded value", fixed = TRUE)
  # the median sits on the top edge of the box, which the box note says as it is
  expect_match(plot_text("box", d, "Pests")$reading, "The median is on the top edge of the box: it and Q3 are both 2", fixed = TRUE)
  # the mean is below the median here; the note states it without a reason
  expect_match(plot_text("hist", d, "Pests")$reading, "The mean line lies to the left of the median line.", fixed = TRUE)
  for (k in c("hist", "density"))
    expect_no_match(plot_text(k, d, "Pests")$reading, "low values", fixed = TRUE)
})

test_that("unusual values are those beyond Tukey's fences, named by row", {
  x <- c(10, 11, 12, 13, 14, 15, 16, 50)
  # Q1 = 11.75, Q3 = 15.25, so the fences are 6.5 and 20.5
  expect_identical(unusual(x), 8L)
  expect_identical(unusual(c(x[-8], 20.5)), integer(0))      # on the fence is not beyond it
  expect_identical(unusual(c(NA, x)), 9L)

  big <- c(44000, 45000, 46000, 47000, 48000, 49000, 50000, 51000, 400000)
  expect_match(describe_text("Plants", big, 1:9), "400000 in row 9", fixed = TRUE)
  expect_identical(num_text(c(4e5, 1e-05, 2987654.3)), c("400000", "0.00001", "2987654.3"))

  txt <- describe_text("x", x, c(101:108))
  expect_match(txt, "1 value lies outside the usual range", fixed = TRUE)
  expect_match(txt, "A value from a treatment that really differs can look unusual too", fixed = TRUE)
  expect_match(txt, "50 in row 108", fixed = TRUE)            # as entered, not 50.00
  expect_match(txt, "do not remove it only because it is unusual", fixed = TRUE)

  y <- c(x, -40)
  txt <- describe_text("y", y, 1:9)
  expect_match(txt, "2 values lie outside the usual range", fixed = TRUE)
  expect_match(txt, "50 in row 8 and -40 in row 9", fixed = TRUE)
  expect_match(txt, "do not remove them only because they are unusual", fixed = TRUE)

  # the same points a box plot draws on their own
  set.seed(11)
  z <- c(rexp(40, 0.3), 25, 31)
  out <- ggplot2::ggplot_build(plot_box(data.frame(z = z), "z"))$data[[1]]$outliers[[1]]
  expect_equal(sort(out), sort(z[unusual(z)]))
})

test_that("a small sample is flagged", {
  expect_match(describe_text("x", c(1, 4, 2, 8, 5, 7, 3), 1:7), "only a rough guide", fixed = TRUE)
  expect_no_match(describe_text("x", c(1, 4, 2, 8, 5, 7, 3, 6), 1:8), "only a rough guide", fixed = TRUE)
})

## ---------------------------------------------------------------- plots ----

test_that("the four plots draw the data they describe", {
  set.seed(4)
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 10), y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 9)))
  d$y[3] <- NA
  y <- d$y[!is.na(d$y)]

  h <- ggplot2::ggplot_build(plot_hist(d, "y"))$data
  expect_equal(sum(h[[1]]$count), length(y))
  expect_equal(h[[2]]$xintercept, c(mean(y), median(y)))   # the mean and median lines

  dn <- ggplot2::ggplot_build(plot_density(d, "y"))$data
  expect_equal(nrow(dn[[2]]), length(y))                   # one rug tick per value
  expect_equal(dn[[3]]$xintercept, c(mean(y), median(y)))

  b <- ggplot2::ggplot_build(plot_box(d, "y"))$data
  expect_equal(b[[1]]$middle, median(y))
  expect_equal(c(b[[1]]$lower, b[[1]]$upper), unname(quantile(y, c(0.25, 0.75))))
  expect_equal(b[[2]]$y, mean(y))                          # the diamond

  bg <- ggplot2::ggplot_build(plot_box(d, "y", "Trt"))$data
  expect_equal(bg[[1]]$middle, as.vector(tapply(d$y, d$Trt, median, na.rm = TRUE)))
  expect_equal(bg[[2]]$y, as.vector(tapply(d$y, d$Trt, mean, na.rm = TRUE)))

  q <- ggplot2::ggplot_build(plot_qq(d, "y"))$data[[1]]
  expect_equal(sort(q$sample), sort(y))
})

test_that("plots that cannot be drawn are not drawn", {
  one <- data.frame(y = c(4, NA))
  expect_null(plot_hist(one, "y")); expect_null(plot_box(one, "y"))
  flat <- data.frame(y = c(5, 5, 5, 5))
  expect_null(plot_density(flat, "y")); expect_null(plot_qq(flat, "y"))
  expect_s3_class(plot_hist(flat, "y"), "ggplot")
  for (k in c("density", "qq")) {
    t <- plot_text(k, data.frame(y = c(1, NA, 2)), "y")
    expect_identical(t$what, "")
    expect_match(t$reading, "too few values", fixed = TRUE)
  }
  # two values: the histogram and box plot are drawn, so their notes describe them
  two <- data.frame(y = c(4.2, 5.1, NA, NA))
  expect_s3_class(plot_hist(two, "y"), "ggplot"); expect_s3_class(plot_box(two, "y"), "ggplot")
  for (k in c("hist", "box")) {
    t <- plot_text(k, two, "y")
    expect_true(nzchar(t$what))
    expect_no_match(t$reading, "draw this plot", fixed = TRUE)
  }
  # a constant variable has plenty of values, only no spread
  flat <- data.frame(y = rep(4.2, 10))
  for (k in c("density", "qq")) {
    t <- plot_text(k, flat, "y")
    expect_identical(t$what, "")
    expect_identical(t$reading, "Every value is 4.2, so there is no spread to draw.")
  }
  expect_match(plot_text("hist", flat, "y")$reading, "Every value is 4.2, so all the observations fall in one bar.", fixed = TRUE)
  expect_match(plot_text("box", flat, "y")$reading, "Every value is 4.2, so the box is a single line.", fixed = TRUE)
  expect_match(describe_text("y", flat$y, 1:10), "Every value of 'y' is 4.2, so there is no variation", fixed = TRUE)
})

test_that("each plot says what it shows and what it shows here", {
  r <- c(1, 1, 2, 2, 2, 3, 3, 4, 5, 8, 12, 20)
  d <- data.frame(x = r)
  for (k in c("hist", "density", "box", "qq")) {
    t <- plot_text(k, d, "x")
    expect_true(nzchar(t$what)); expect_true(nzchar(t$reading))
  }
  expect_match(plot_text("hist", d, "x")$reading, sprintf(
    "The bars reach further above the median than below it (the skewness is %s).", dfmt(skew_of(r), 2)), fixed = TRUE)
  expect_match(plot_text("hist", d, "x")$reading, "The mean line lies to the right of the median line, as it usually does with right skew.", fixed = TRUE)
  expect_match(plot_text("density", d, "x")$reading, "The curve reaches further above the median than below it", fixed = TRUE)
  expect_no_match(plot_text("density", d, "x")$reading, "bars", fixed = TRUE)
  expect_no_match(plot_text("hist", d, "x")$reading, "curve", fixed = TRUE)
  expect_match(plot_text("box", d, "x")$reading, "median sits towards the bottom of the box", fixed = TRUE)
  # Q1 = 2 and Q3 = 5.75, so the upper fence is 11.375: 12 and 20 lie beyond it
  expect_match(plot_text("box", d, "x")$reading, "2 values lie beyond the whiskers (rows 11 and 12)", fixed = TRUE)
  expect_match(plot_text("hist", d, "x")$reading, "The box plot marks rows 11 and 12 as possibly unusual; they are at the right-hand end of the histogram.", fixed = TRUE)
  expect_match(plot_text("qq", d, "x")$reading, "The values the box plot marks as possibly unusual (rows 11 and 12) are the points at the top end.", fixed = TRUE)
  expect_match(plot_text("qq", d, "x")$reading, "With only 12 values, a Q-Q plot can show only a large departure", fixed = TRUE)
  # raw values are not what an analysis of variance assumes to be normal
  expect_match(plot_text("qq", d, "x")$reading, "it is the residuals that should look normal", fixed = TRUE)

  s <- data.frame(x = 1:20)
  expect_match(plot_text("hist", s, "x")$reading, "lines nearly coincide", fixed = TRUE)
  expect_match(plot_text("box", s, "x")$reading, "No value lies beyond the whiskers", fixed = TRUE)
  expect_match(plot_text("box", s, "x")$reading, "The median sits near the middle of the box", fixed = TRUE)
  sg <- data.frame(G = rep(c("a", "b"), 10), x = 1:20)
  expect_match(plot_text("hist", sg, "x")$reading, "choose a column under 'Summarise separately for each level of'", fixed = TRUE)

  # the box reading is read off the box as drawn, whatever the mean does
  m <- data.frame(x = c(0, 6, 11, 12, 12, 13, 13, 16, 18, 18, 19, 20))
  b <- ggplot2::ggplot_build(plot_box(m, "x"))$data[[1]]
  expect_gt((b$upper - b$middle) / (b$upper - b$lower), 0.6)       # median low in the box
  expect_match(plot_text("box", m, "x")$reading, "The median sits towards the bottom of the box", fixed = TRUE)
  z <- data.frame(x = c(rep(0, 16), 2, 5, 9, 20))
  expect_match(plot_text("box", z, "x")$reading, "The box is a single line: at least half the values are exactly 0.", fixed = TRUE)

  # the caption says what the red points are, in the words of the note
  expect_identical(plot_box(d, "x")$labels$caption,
    "Diamond: mean. Line in the box: median. Red points: values more than 1.5 box lengths beyond the box.")
})

test_that("the grouped box plot is read as a description, not a test", {
  d <- data.frame(G = rep(c("Low", "High", "Mid"), each = 4),
                  y = c(1, 2, 3, 4, 20, 22, 24, 40, 10, 11, 12, 13))
  txt <- plot_text("box", d, "y", "G")$reading
  expect_match(txt, "The median is highest in G High (23.00) and lowest in G Low (2.50)", fixed = TRUE)
  expect_match(txt, "widest in G High", fixed = TRUE)
  expect_match(txt, "whether the groups really differ is for the analysis of variance to judge", fixed = TRUE)
})

test_that("nothing in the readings claims a difference or a significance", {
  set.seed(7)
  sets <- list(
    list(demo_data("RCBD"), "Yield", NULL),
    list(demo_data("RCBD"), "Yield", "Variety"),
    list(demo_data("SPLIT"), "Yield", "Irrigation"),
    list(data.frame(x = c(rexp(30, 0.2), 60, 75)), "x", NULL),
    list(data.frame(x = c(-3, -1, 2, 5, 1, 0, 4)), "x", NULL))
  for (s in sets) {
    txt <- all_text(s[[1]], s[[2]], s[[3]])
    expect_no_match(txt, "significan", ignore.case = TRUE)
    expect_no_match(txt, "no difference", ignore.case = TRUE)
    expect_no_match(txt, "proves|caused|because of the treatment", ignore.case = TRUE)
  }
})

## ------------------------------------------------------------------ app ----

test_that("the Explore tab summarises, reads and plots the data", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    # CRD: Rep is a column of numbers that labels replications
    rv$data <- demo_data("CRD")
    session$flushReact()
    html <- output$descUI$html
    expect_match(html, '<option value="Yield" selected>', fixed = TRUE)
    expect_match(html, '<option value="Rep">', fixed = TRUE)
    vars <- regmatches(html, regexpr('(?s)id="descVars".*?</select>', html, perl = TRUE))
    expect_length(vars, 1)
    expect_no_match(vars, '<option value="Treatment"', fixed = TRUE)   # not a number
    expect_match(html, '<option value="Treatment">', fixed = TRUE)     # but a grouping

    session$setInputs(descVars = "Yield", descGroup = "", descPlotVar = "Yield")
    tab <- output$descTab$html
    expect_match(tab, "Descriptive statistics", fixed = TRUE)
    expect_match(tab, "QUARTILE.INC", fixed = TRUE)
    txt <- output$descText$html
    expect_match(txt, "The 20 values of &#39;Yield&#39;|The 20 values of 'Yield'")
    expect_identical(output$descPlotTitle, "Distribution of Yield")
    for (o in c("descHist", "descDens", "descBox", "descQQ"))
      expect_match(output[[o]]$src, "^data:image/png")
    for (o in c("descHistText", "descDensText", "descBoxText", "descQQText")) {
      expect_match(output[[o]]$html, "What it shows.", fixed = TRUE)
      expect_match(output[[o]]$html, "Here.", fixed = TRUE)
    }

    session$setInputs(descGroup = "Treatment")
    expect_match(output$descTab$html, "<th>Group</th>", fixed = TRUE)
    expect_match(output$descBoxText$html, "The median is highest in", fixed = TRUE)
    expect_match(output$descText$html, "The group means of &#39;Yield&#39; run from|The group means of 'Yield' run from")
    expect_no_match(output$descText$html, "The 20 values of", fixed = TRUE)
    expect_equal(nrow(desc_tab()), nlevels(demo_data("CRD")$Treatment))
  })
})

test_that("the Explore tab leaves design factors and label columns unselected", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- data.frame(Block = rep(1:3, each = 4), Nitrogen = rep(c(0, 40, 80, 120), 3),
                    Yield = c(30, 34, 39, 41, 29, 35, 38, 43, 31, 33, 40, 42),
                    Height = c(80, 84, 90, 95, 79, 86, 91, 96, 81, 85, 89, 97))
    rv$data <- d
    session$setInputs(design = "RCBD")
    session$setInputs(resp = "Yield", block = "Block", treat = "Nitrogen")
    html <- output$descUI$html
    expect_match(html, '<option value="Yield" selected>', fixed = TRUE)
    expect_match(html, '<option value="Height" selected>', fixed = TRUE)
    vars <- regmatches(html, regexpr('(?s)id="descVars".*?</select>', html, perl = TRUE))
    expect_length(vars, 1)
    expect_match(vars, '<option value="Nitrogen">', fixed = TRUE)
    expect_match(vars, '<option value="Block">', fixed = TRUE)
  })
})

test_that("a variable with too few values says so instead of failing", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = c("A", "B", "C"), y = c(4, NA, NA))
    session$setInputs(descVars = "y", descGroup = "", descPlotVar = "y")
    expect_match(output$descText$html, "has a single value", fixed = TRUE)
    expect_error(output$descHist, "too few values to draw this plot")
    expect_match(output$descQQText$html, "too few values", fixed = TRUE)
    expect_no_match(output$descQQText$html, "What it shows", fixed = TRUE)
  })
})

## -------------------------------------------------- assumptions plots ----

test_that("the Assumptions tab shows all four residual plots", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("RCBD")
    session$setInputs(design = "RCBD")
    session$setInputs(resp = "Yield", block = "Block", treat = "Variety", alpha = "0.05", run = 1)
    p <- diag_plots()
    expect_length(p, 4)
    titles <- vapply(p, function(g) g$labels$title, character(1))
    expect_identical(titles, c("Residuals vs fitted", "Normal Q-Q plot",
                               "Histogram of residuals", "Scale-location"))
    for (o in c("diagFit", "diagQQ", "diagHist", "diagScale"))
      expect_match(output[[o]]$src, "^data:image/png")
  })
})

## ------------------------------------------------- found in review ----

test_that("a misspelt column or a grouping column with no labels gives a plain error", {
  d <- demo_data("RCBD")
  expect_error(describe_data(d, "Yeild"), "'Yeild' is not a column of the data", fixed = TRUE)
  expect_error(describe_data(d, "Yield", "Varity"), "'Varity' is not a column of the data", fixed = TRUE)
  d$Remarks <- NA
  expect_error(describe_data(d, "Yield", "Remarks"), "'Remarks' has no labels to group the rows by.", fixed = TRUE)
  d$Remarks <- c("", " ", rep(NA, 22))
  expect_error(describe_data(d, "Yield", "Remarks"), "has no labels", fixed = TRUE)
})

test_that("grouping is offered for columns with repeated labels only", {
  d <- data.frame(Block = rep(1:3, each = 4), Nitrogen = rep(c(0, 40, 80, 120), 3),
                  Yield = c(30, 34, 39, 41, 29, 35, 38, 43, 31, 33, 40, 42.5),
                  Plot = 1:12, Remarks = NA, Note = c("late", rep("", 11)))
  expect_identical(group_choices(d), c("Block", "Nitrogen"))
})

test_that("blank group labels are missing, not a group", {
  d <- data.frame(G = c("A", "A", " ", "B", "B", ""), y = c(1, 2, 3, 4, 5, 6))
  expect_identical(levels(group_labels(d, "G")), c("A", "B"))
  expect_identical(describe_data(d, "y", "G")$Group, c("A", "B"))
})

test_that("with a grouping, each plot's note describes the rows that plot draws", {
  d <- data.frame(G = c(rep("A", 5), rep("B", 5), NA), y = c(10:14, 11:15, 60))
  # the histogram, density and Q-Q plots draw all eleven values, 60 included
  expect_equal(sum(ggplot2::ggplot_build(plot_hist(d, "y"))$data[[1]]$count), 11)
  for (k in c("hist", "qq"))
    expect_match(plot_text(k, d, "y", "G")$reading, "row 11", fixed = TRUE)
  # with a grouping the flags over all rows are described as such, since the
  # only box plot on screen is the one by group
  for (k in c("hist", "qq")) {
    txt <- plot_text(k, d, "y", "G")$reading
    expect_match(txt, "Taken over all rows together", fixed = TRUE)
    expect_match(txt, "Row 11 has no label in 'G', so the box plot by group does not show it.", fixed = TRUE)
    expect_no_match(txt, "The box plot marks", fixed = TRUE)
    expect_no_match(txt, "the box plot marks", fixed = TRUE)
  }
  # the box plot by group cannot place row 11, and says so
  expect_match(plot_text("box", d, "y", "G")$reading, "Row 11 has no label in 'G' and is not drawn.", fixed = TRUE)
  # and so does the reading of the grouped table
  txt <- describe_group_text("y", d$y, group_labels(d, "G"), row_ids(d), "G")
  expect_match(txt, "Row 11 has no label in 'G' and is left out of the groups.", fixed = TRUE)
})

test_that("the grouped table is read group by group", {
  d <- data.frame(Trt = rep(c("T1", "T2", "T3", "T4", "T5"), each = 4),
                  y = c(10, 11, 12, 13, 10.5, 11.5, 12.5, 13.6, 10.2, 11.1, 12.3, 13.1,
                        10.4, 11.2, 12.2, 13.3, 40.1, 41.3, 42, 40.6))
  g <- group_labels(d, "Trt")
  txt <- describe_group_text("y", d$y, g, row_ids(d), "Trt")
  mu <- tapply(d$y, d$Trt, mean)
  expect_match(txt, sprintf("The group means of 'y' run from %s (Trt T1) to %s (Trt T5).",
                            fmt(min(mu), var_digits(d$y)), fmt(max(mu), var_digits(d$y))), fixed = TRUE)
  cv <- round(100 * tapply(d$y, d$Trt, sd) / mu, 1)
  expect_match(txt, sprintf("The CV within a group runs from %s%%", fmt(min(cv), 1)), fixed = TRUE)
  expect_match(txt, "differences between groups are not part of it", fixed = TRUE)
  # judged within each group, as the grouped box plot draws them: none
  expect_match(txt, "No value lies outside the usual range of its own group.", fixed = TRUE)
  expect_equal(sum(lengths(ggplot2::ggplot_build(plot_box(d, "y", "Trt"))$data[[1]]$outliers)), 0)
  expect_match(txt, "whether the groups really differ is for the analysis of variance to judge", fixed = TRUE)
  expect_match(txt, "Groups with fewer than five values", fixed = TRUE)
  # the pooled plots still mark T5, and say it is ordinary within its group,
  # in the Q-Q note as in the histogram note
  expect_match(plot_text("qq", d, "y", "Trt")$reading, "Within their own group of Trt, rows 17, 18, 19 and 20 are not unusual", fixed = TRUE)
  h <- plot_text("hist", d, "y", "Trt")$reading
  expect_match(h, "Within their own group of Trt, rows 17, 18, 19 and 20 are not unusual", fixed = TRUE)
  expect_match(h, "may reflect a difference between groups rather than a recording error", fixed = TRUE)
  # each unusual value is named with its own group
  r <- demo_data("RCBD")
  rt <- describe_group_text("Yield", r$Yield, group_labels(r, "Variety"), row_ids(r), "Variety")
  u <- unusual_within(r$Yield, group_labels(r, "Variety"))
  expect_gt(length(u), 0)
  for (i in u)
    expect_match(rt, sprintf("%s in row %d (Variety %s)", num_text(r$Yield[i]), i, r$Variety[i]), fixed = TRUE)
  expect_no_match(plot_text("hist", d, "y")$reading, "Within their own group", fixed = TRUE)
})

test_that("ties between groups are named as ties", {
  same <- data.frame(G = rep(c("V1", "V2", "V3"), each = 3), y = rep(c(4, 5, 6), 3))
  txt <- plot_text("box", same, "y", "G")$reading
  expect_match(txt, "The median is the same in every group, 5.00.", fixed = TRUE)
  expect_match(txt, "The middle half is equally spread in every group.", fixed = TRUE)
  expect_no_match(txt, "highest", fixed = TRUE)
  two <- data.frame(G = rep(c("V1", "V2", "V3"), each = 3), y = c(1, 2, 3, 7, 8, 9, 7, 8, 9))
  txt <- plot_text("box", two, "y", "G")$reading
  expect_match(txt, "The median is highest in G V2 and G V3 (8.00) and lowest in G V1 (2.00).", fixed = TRUE)
  one <- data.frame(G = c("V1", "V2", "V3"), y = c(30, 42, 35))
  txt <- plot_text("box", one, "y", "G")$reading
  expect_match(txt, "Every box is a single line", fixed = TRUE)
  gtxt <- describe_group_text("y", rep(c(4, 5, 6), 3), group_labels(same, "G"), 1:9, "G")
  expect_match(gtxt, "Every group of G has the same mean of 'y', 5.00.", fixed = TRUE)
  expect_match(gtxt, "The CV within every group is 20.0%.", fixed = TRUE)
})

test_that("numbers waiting for a correction are pointed to the Data tab", {
  raw <- c("5,6", "6,1", "5,4", "6,3", "5,2", "6,0")
  expect_match(describe_text("Yield", raw, 1:6),
    "'Yield' has no values that can be read as numbers. 6 entries are numbers written in a form the data check has flagged (such as '5,6'); correct them on the Data tab to include them here.",
    fixed = TRUE)
  raw <- c("5.6", "6.1", "45%", "6.3", "5.2", "6.0")
  expect_match(describe_text("Yield", raw, 1:6),
    "1 entry is a number written in a form the data check has flagged (such as '45%'); correct it on the Data tab to include it here.",
    fixed = TRUE)
  expect_match(describe_text("Yield", raw, 1:6), "such as '45%'", fixed = TRUE)
  expect_no_match(describe_text("Yield", c("5.6", "12a", "6"), 1:3), "data check has flagged", fixed = TRUE)
  expect_null(held_text(c(1, 2, NA)))
})

test_that("whole-number scores get one bar per score", {
  sc <- rep(1:5, c(6, 14, 20, 13, 7))
  b <- ggplot2::ggplot_build(plot_hist(data.frame(s = sc), "s"))$data[[1]]
  expect_equal(b$x, 1:5)
  expect_equal(b$count, c(6, 14, 20, 13, 7))
  expect_true(all(b$count > 0))
  # a wider range of whole numbers: bars of whole widths, none splitting a value
  w <- c(0, 3, 7, 12, 18, 25, 33, 41, 52, 60, 61, 75, 80, 88, 99, 120)
  b <- ggplot2::ggplot_build(plot_hist(data.frame(w = w), "w"))$data[[1]]
  expect_true(all((b$xmin + 0.5) == round(b$xmin + 0.5)))
  expect_equal(sum(b$count), length(w))
})

## --------------------------------------------------------------- Q-Q ----

test_that("the Q-Q limits are those of normal samples", {
  # a fresh simulation: normal samples are read as bending about once in a
  # hundred, at sizes in the table and between its entries
  set.seed(99)
  for (n in c(8, 19, 27, 45, 64, 120, 350)) {
    bent <- mean(replicate(3000, qq_pattern(rnorm(n)) != "ok ok"))
    expect_gt(bent, 0.002); expect_lt(bent, 0.025)
  }
  expect_false(anyNA(QQ_LIMIT$out)); expect_false(anyNA(QQ_LIMIT$inward))
  expect_false(is.unsorted(QQ_LIMIT$n))
})

test_that("qq_ends measures the plot as ggplot2 draws it", {
  set.seed(5)
  x <- rexp(30)
  b <- ggplot2::ggplot_build(plot_qq(data.frame(x = x), "x"))$data
  pts <- b[[1]]; ln <- b[[2]]
  slope <- diff(range(ln$y)) / diff(range(ln$x))
  dev <- pts$sample - (ln$y[1] + slope * (pts$theoretical - ln$x[1]))
  e <- qq_ends(x)
  expect_equal(unname(e[c("low", "high")]), c(mean(dev[1:3]), mean(dev[28:30])))
  expect_equal(e[["slope"]], slope)
  expect_equal(e[["sd"]], sd(x))
})

test_that("the Q-Q reading follows the bend of the points", {
  set.seed(42)
  qq <- function(x) plot_text("qq", data.frame(x = x), "x")$reading
  expect_match(qq(rnorm(200, 50, 5)), "gives no clear sign that the values are not normal", fixed = TRUE)
  expect_match(qq(rlnorm(200)), "a longer tail of high values", fixed = TRUE)
  expect_match(qq(-rlnorm(200)), "a longer tail of low values", fixed = TRUE)
  expect_match(qq(rexp(400)), "a longer tail of high values (right skew)", fixed = TRUE)
  expect_match(qq(rt(200, 2)), "(heavy tails)", fixed = TRUE)
  expect_match(qq(c(rnorm(100, 8, 1), rnorm(100, 22, 1))), "(an S-shape)", fixed = TRUE)
  expect_match(qq(c(rep(0, 16), 2, 5, 9, 20)), "many points sit in one flat run", fixed = TRUE)
  # the two clusters are not called normal, nor 'following the line'
  two <- qq(c(rnorm(20, 8, 1), rnorm(20, 22, 1)))
  expect_no_match(two, "no clear sign", fixed = TRUE)
  # heavy tails are not said to follow the line and then to sit off it
  expect_no_match(qq(rt(200, 2)), "follow", fixed = TRUE)
})

## ---------------------------------------------- found in second review ----

test_that("a group without a CV is given its own reason", {
  d <- data.frame(Trt = rep(c("Control", "Neem", "Imidacloprid", "Oil"), each = 4),
                  Aphids = c(12, 15, 9, 14, 6, 8, 5, 7, 0, 0, 0, 0, -1, 2, 3, 1))
  d$Aphids[16] <- NA; d$Aphids[14:15] <- NA
  txt <- describe_group_text("Aphids", d$Aphids, group_labels(d, "Trt"), row_ids(d), "Trt")
  expect_match(txt, "No CV is given for Trt Imidacloprid, where every value is zero.", fixed = TRUE)
  expect_match(txt, "No CV is given for Trt Oil, which has only one value.", fixed = TRUE)
  expect_no_match(txt, "negative values or only one value", fixed = TRUE)
  neg <- data.frame(G = rep(c("A", "B"), each = 3), x = c(1, 2, 3, -1, 2, 4))
  expect_match(describe_group_text("x", neg$x, group_labels(neg, "G"), 1:6, "G"),
               "No CV is given for G B: a CV needs values that cannot be negative.", fixed = TRUE)
})

test_that("'every group' is said only of every group", {
  d <- data.frame(Trt = rep(c("T1", "T2", "T3"), each = 3), y = c(2, 4, 6, 3, 6, 9, 5, NA, NA))
  txt <- describe_group_text("y", d$y, group_labels(d, "Trt"), 1:9, "Trt")
  expect_no_match(txt, "every group", fixed = TRUE)
  expect_match(txt, "The CV within Trt T1 and Trt T2 is 50.0%.", fixed = TRUE)
  one <- data.frame(G = rep(c("A", "B"), c(3, 1)), y = c(1, 2, 3, 9))
  expect_match(describe_group_text("y", one$y, group_labels(one, "G"), 1:4, "G"),
               "The CV within G A is 50.0%. It covers only", fixed = TRUE)
  kh <- data.frame(Season = rep("Kharif", 4), y = 1:4)
  expect_match(describe_group_text("y", kh$y, group_labels(kh, "Season"), 1:4, "Season"),
               "Every row with a label in 'Season' has the same one, Kharif, so there are no groups to compare.", fixed = TRUE)
})

test_that("the readings print the figures the table prints", {
  # V1's mean is 16.395, which formats and rounds differently
  d <- data.frame(Variety = rep(c("V1", "V2", "V3"), each = 4),
                  Height = c(16.39, 16.40, 16.38, 16.41, 16.80, 16.79, 16.81, 16.85,
                             16.57, 16.58, 16.56, 16.59))
  g <- group_labels(d, "Variety")
  tab <- describe_data(d, "Height", "Variety")
  dg <- var_digits(d$Height, g)
  html <- describe_html(tab, d, "Variety")
  txt <- describe_group_text("Height", d$Height, g, 1:12, "Variety")
  lo <- dfmt(min(tab$Mean), dg)
  expect_match(html, sprintf(">%s<", lo), fixed = TRUE)
  expect_match(txt, sprintf("run from %s (Variety V1)", lo), fixed = TRUE)
  box <- plot_text("box", d, "Height", "Variety")$reading
  for (m in dfmt(tab$Median[c(which.max(tab$Median), which.min(tab$Median))], dg)) {
    expect_match(box, sprintf("(%s)", m), fixed = TRUE)
    expect_match(html, sprintf(">%s<", m), fixed = TRUE)
  }
  # a row without a group label does not change the box note's decimals
  d2 <- rbind(d, data.frame(Variety = NA, Height = 125.4))
  tab2 <- describe_data(d2, "Height", "Variety")
  dg2 <- var_digits(response_values(d2$Height), group_labels(d2, "Variety"))
  box2 <- plot_text("box", d2, "Height", "Variety")$reading
  expect_match(box2, sprintf("(%s)", dfmt(max(tab2$Median), dg2)), fixed = TRUE)
  expect_match(describe_html(tab2, d2, "Variety"), sprintf(">%s<", dfmt(max(tab2$Median), dg2)), fixed = TRUE)
  # and the CV word follows the CV as printed
  expect_identical(cv_words(29.949999999999999), "considerably")
})

test_that("one wild value does not set the decimals for everything else", {
  w <- c(1.18, 1.24, 1.31, 1.42, 1.25, 1.36, 1.29, 1250)
  expect_identical(var_digits(w), 3L)
  expect_match(describe_text("Wt", w, 1:8), "the middle half lie between", fixed = TRUE)
  expect_no_match(describe_text("Wt", w, 1:8), "between 1 and 1", fixed = TRUE)
  d <- data.frame(G = c("A", "A", "A", "A", "B", "B", "B", "B", NA),
                  Wt = c(1.10, 1.15, 1.20, 1.25, 1.35, 1.40, 1.42, 1.43, 1250))
  txt <- describe_group_text("Wt", d$Wt, group_labels(d, "G"), 1:9, "G")
  expect_no_match(txt, "the same mean", fixed = TRUE)
  html <- describe_html(describe_data(d, "Wt", "G"), d, "G")
  # each group's SD keeps two significant figures
  for (z in c(sd(d$Wt[1:4]), sd(d$Wt[5:8])))
    expect_match(html, sprintf(">%s<", fmt(z, var_digits(d$Wt, group_labels(d, "G")))), fixed = TRUE)
  expect_identical(signif(sd(d$Wt[5:8]), 2), signif(as.numeric(fmt(sd(d$Wt[5:8]), var_digits(d$Wt, group_labels(d, "G")))), 2))
})

test_that("numeric and factor groupings keep their order and are written out", {
  d <- data.frame(Density = rep(c(50000, 100000, 150000), each = 2), Yield = c(32, 33, 41, 42, 37, 38))
  g <- group_labels(d, "Density")
  expect_identical(levels(g), c("50000", "100000", "150000"))
  expect_identical(describe_data(d, "Yield", "Density")$Group, c("50000", "100000", "150000"))
  expect_match(describe_group_text("Yield", d$Yield, g, 1:6, "Density"), "(Density 100000)", fixed = TRUE)
  f <- data.frame(Stage = factor(rep(c("Vegetative", "Flowering", "Maturity"), 2),
                                 levels = c("Vegetative", "Flowering", "Maturity")), y = 1:6)
  expect_identical(describe_data(f, "y", "Stage")$Group, c("Vegetative", "Flowering", "Maturity"))
})

test_that("a column is offered for grouping only when its labels repeat", {
  expect_false("Yield" %in% group_choices(demo_data("SPLIT")))
  expect_identical(group_choices(demo_data("SPLIT")), c("Rep", "Irrigation", "Variety"))
  germ <- data.frame(Accession = paste0("IC", 1:12), Days = c(65, 67, 65, 70, 72, 67, 66, 69, 71, 64, 68, 70))
  expect_identical(group_choices(germ), character(0))
  expect_identical(group_choices(data.frame(Season = rep("Kharif", 4), y = 1:4)), character(0))
})

test_that("label columns are recognised however their names are written", {
  for (nm in c("Rep", "Rep.", "Replication.No", "Block", "Block.No", "Plot.No", "Plot", "S.No",
               "S..No.", "Sl..No", "Sr..No.", "Serial.No", "ID", "R"))
    expect_true(is_id_name(nm), info = nm)
  for (nm in c("Yield", "Plant.height", "Days.to.flowering", "Nitrogen", "Rows.per.plot"))
    expect_false(is_id_name(nm), info = nm)
})

test_that("plots of a column waiting for corrections point to the Data tab", {
  d <- data.frame(Yield = c("5,6", "6,1", "5,4", "6,3", "5,2", "6,0", "n/a"), stringsAsFactors = FALSE)
  for (k in c("hist", "density", "box", "qq"))
    expect_match(plot_text(k, d, "Yield")$reading, "correct them on the Data tab to include them here", fixed = TRUE)
})

test_that("the histogram suggests a grouping only when one can be chosen", {
  d <- data.frame(Trt = c("A", "B", "C", "D", "E"), y = c(5.2, 6.1, 5.8, 7.0, 6.4))
  txt <- plot_text("hist", d, "y")$reading
  expect_no_match(txt, "Summarise separately", fixed = TRUE)
  expect_match(txt, "rather than a tail.", fixed = TRUE)
})

test_that("a group of three or fewer cannot clear a flagged value", {
  d <- data.frame(Variety = rep(paste0("V", 1:6), each = 3),
                  Yield = c(42.1, 43.0, 42.6, 47.2, 48.1, 47.7, 37.5, 38.2, 37.9,
                            54.3, 55.1, 54.8, 50.2, 49.6, 50.9, 45.0, 44.2, 45.3))
  d$Yield[11] <- 450
  g <- group_labels(d, "Variety")
  expect_true(all(table(g) == 3))
  h <- plot_text("hist", d, "Yield", "Variety")$reading
  expect_match(h, "Row 11 is in a group of three or fewer values, too few for a value to stand out within its group, so check it all the same.", fixed = TRUE)
  expect_no_match(h, "may reflect a difference between groups", fixed = TRUE)
  txt <- describe_group_text("Yield", d$Yield, g, row_ids(d), "Variety")
  expect_match(txt, "Every group has three or fewer values, too few for any value to stand out within its group.", fixed = TRUE)
  expect_no_match(txt, "No value lies outside the usual range", fixed = TRUE)
})

test_that("values recorded in coarse steps are not read as heavy tails or clusters", {
  qq <- function(x) plot_text("qq", data.frame(x = x), "x")$reading
  days <- round(qnorm(ppoints(200), 65, 2))
  expect_match(qq(days), "recorded in steps of 1", fixed = TRUE)
  expect_match(qq(days), "cannot be read reliably", fixed = TRUE)
  expect_no_match(qq(days), "heavy tails", fixed = TRUE)
  expect_no_match(qq(round(qnorm(ppoints(100), 65, 1))), "two clusters", fixed = TRUE)
  # normal values rounded to these steps are read as bending rarely
  set.seed(8)
  for (n in c(20, 50, 100, 200)) for (s in c(2, 4, 8)) {
    bent <- mean(replicate(400, {
      x <- round(rnorm(n, 65, s))
      sh <- shape_of(desc_one(x))
      !sh$heaped && !qq_coarse(x) && qq_pattern(x) != "ok ok" }))
    expect_lt(bent, 0.035)
  }
  # finely recorded values are read as usual
  expect_false(qq_coarse(round(rnorm(100, 50, 5), 1)))
})

test_that("the Explore choices survive a change of design or mapping", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("RCBD")
    session$flushReact()
    invisible(output$descUI)
    session$setInputs(descVars = "Yield", descGroup = "Variety", descPlotVar = "Yield")
    session$setInputs(design = "CRD")
    html <- output$descUI$html
    expect_match(html, '<option value="Variety" selected>', fixed = TRUE)
    session$setInputs(design = "RCBD", resp = "Yield", block = "Block", treat = "Variety")
    expect_match(output$descUI$html, '<option value="Variety" selected>', fixed = TRUE)
  })
})

test_that("the scale-location trend stays among the points with few fitted values", {
  res <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  p <- plot_diag(res)[[4]]
  b <- ggplot2::ggplot_build(p)$data
  expect_lte(max(b[[2]]$y), max(b[[1]]$y) + 1e-9)
  expect_gte(min(b[[2]]$y), min(b[[1]]$y) - 1e-9)
})

## ----------------------------------------------- found in third review ----

test_that("only values on a grid, with ties, are said to be recorded in steps", {
  expect_false(qq_coarse(c(32.4, 35.1, 38.7, 41.2, 44.9)))        # no ties
  expect_false(qq_coarse(c(-3, -1, 2, 5, 1, 0, 4)))                # whole, but no ties
  expect_no_match(plot_text("qq", data.frame(x = c(32.4, 35.1, 38.7, 41.2, 44.9)), "x")$reading,
                  "steps", fixed = TRUE)
  ph <- c(6.1, 6.3, 6.2, 6.5, 6.4, 6.2, 6.3, 6.6, 6.4, 6.3)
  expect_identical(qq_step(ph), 0.1)
  expect_true(qq_coarse(ph))
  expect_match(plot_text("qq", data.frame(pH = ph), "pH")$reading, "recorded in steps of 0.1,", fixed = TRUE)
})

test_that("the skewness band follows the skewness as printed", {
  A <- c(7, 16, 1, 11, 17, 2, 22, 17, 6, 3, 27, 2)
  B <- c(3, 14, 1, 2, 15, 20, 25, 1, 28, 10, 13, 4)
  expect_lt(skew_of(A), 0.5); expect_gt(skew_of(B), 0.5)
  expect_identical(dfmt(skew_of(A), 2), "0.50")
  sa <- skew_text(shape_of(desc_one(A)), desc_one(A), 2); sb <- skew_text(shape_of(desc_one(B)), desc_one(B), 2)
  expect_identical(sub("^The skewness of 0.50 (means|counts as) ", "", sa) == sa, sub("^The skewness of 0.50 (means|counts as) ", "", sb) == sb)
  expect_identical(shape_of(desc_one(A))$kind, shape_of(desc_one(B))$kind)
})

test_that("the notes give the reader the rule for humps and claim no tail on the plots", {
  # two clusters: the notes describe the skewness by its definition, which
  # is true of clusters too, and leave the humps to the reader's eye
  set.seed(2)
  bi <- data.frame(Yield = round(c(rnorm(30, 20, 1), rnorm(10, 40, 1)), 1))
  for (k in c("hist", "density")) {
    txt <- plot_text(k, bi, "Yield")$reading
    expect_no_match(txt, "trail", fixed = TRUE)
    expect_no_match(txt, "tail of", fixed = TRUE)
  }
  expect_match(plot_text("hist", bi, "Yield")$reading,
    "If the bars form two or more separate humps rather than one, the values fall into groups that lie apart", fixed = TRUE)
  expect_match(plot_text("density", bi, "Yield")$reading,
    "Two or more separate peaks would mean the values gather in more than one place", fixed = TRUE)
  # the high values do lie further from the centre: positive third moment
  y <- bi$Yield
  expect_gt(sum((y[y > mean(y)] - mean(y))^3), sum((mean(y) - y[y < mean(y)])^3))
  # the Q-Q note does not call clustered values normal
  expect_no_match(plot_text("qq", bi, "Yield")$reading, "no clear sign", fixed = TRUE)
})

test_that("a tail the skewness does not show is explained, not left to contradict", {
  set.seed(6)
  x <- round(rt(300, 5) * 3 + 50, 2)
  expect_identical(shape_of(desc_one(x))$kind, "symmetric")
  expect_identical(qq_pattern(x), "ok out")
  d <- data.frame(x = x)
  expect_match(plot_text("qq", d, "x")$reading, "The skewness in the table is small, so this tail shows only at the very end of the plot", fixed = TRUE)
  # the histogram makes no claim about tails it was not checked for
  expect_no_match(plot_text("hist", d, "x")$reading, "no long tail", fixed = TRUE)
  expect_identical(qq_vs_skew("ok out", list(kind = "left", clear = TRUE)),
                   " The skewness in the table points the other way: the two measures disagree, so read the histogram as well.")
  expect_identical(qq_vs_skew("out out", list(kind = "symmetric", clear = FALSE)), "")
  # a clear skewness that neither end of the plot shows beyond chance
  set.seed(3)
  ln <- round(rlnorm(20, 2, 1), 2)
  expect_identical(qq_pattern(ln), "ok ok")
  expect_true(shape_of(desc_one(ln))$clear)
  expect_match(plot_text("qq", data.frame(x = ln), "x")$reading,
               "The skewness in the table is larger than chance would usually give, though, even if neither end of this plot bends", fixed = TRUE)
  expect_identical(qq_vs_skew("ok ok", list(kind = "right", clear = FALSE)), "")
})

test_that("'every group' is not said when a group has no values", {
  d <- data.frame(Trt = rep(c("T1", "T2", "T3"), each = 4), y = c(4, 5, 5, 6, NA, NA, NA, NA, 4, 5, 5, 6))
  txt <- describe_group_text("y", d$y, group_labels(d, "Trt"), 1:12, "Trt")
  expect_match(txt, "Trt T2 has no usable values.", fixed = TRUE)
  expect_match(txt, "Trt T1 and Trt T3 have the same mean of 'y', 5.00.", fixed = TRUE)
  expect_no_match(txt, "every group", fixed = TRUE)
  expect_no_match(txt, "Every group", fixed = TRUE)
})

test_that("the grouped box note claims equal values only when they are equal", {
  fb <- data.frame(G = rep(c("A", "B"), each = 6),
                   Wt = c(10.001, 10.001, 10.002, 10.002, 10.003, 18.5, 30.001, 30.001, 30.002, 30.002, 30.003, 38.5))
  txt <- plot_text("box", fb, "Wt", "G")$reading
  expect_no_match(txt, "at least half the values are equal", fixed = TRUE)
  dg <- var_digits(fb$Wt, group_labels(fb, "G"))
  if (all(as.numeric(dfmt(c(0.0015, 0.0015), dg)) == 0))
    expect_match(txt, "narrower than the last decimal shown", fixed = TRUE)
})

test_that("a small negative value is named where the CV is refused", {
  ng <- c(-0.02, 120, 150, 180, 200, 220, 250)
  expect_match(describe_text("Uptake", ng, 1:7),
               "No coefficient of variation is given: it needs values that cannot be negative, and the smallest value here is -0.02.", fixed = TRUE)
})

test_that("one flagged value is spoken of in the singular, and group words follow the groups", {
  d <- data.frame(x = c(10:16, 50))
  expect_match(plot_text("qq", d, "x")$reading,
               "The value the box plot marks as possibly unusual (row 8) is the point at the top end.", fixed = TRUE)
  pf <- data.frame(G = rep(c("A", "B", "C"), c(3, 6, 6)), y = c(1000, 1100, 5, 4:15))
  h <- plot_text("hist", pf, "y", "G")$reading
  expect_match(h, "Rows 1 and 2 are in a group of three or fewer values, too few for a value to stand out within their group", fixed = TRUE)
  lc <- data.frame(variety = rep(c("V1", "V2", "V3", "V4"), c(5, 4, 2, 5)),
                   yield = c(1:5, NA, NA, NA, NA, 6, 7, 8:12))
  txt <- describe_group_text("yield", lc$yield, group_labels(lc, "variety"), 1:16, "variety")
  expect_match(txt, "Variety V2 has no usable values.", fixed = TRUE)
  expect_match(txt, "Variety V3 has three or fewer values", fixed = TRUE)
  z <- data.frame(G = rep(c("A", "B"), each = 3), y = c(1, 2, 3, 0, 0, 0))
  expect_no_warning(describe_group_text("y", z$y, group_labels(z, "G"), 1:6, "G"))
})

test_that("the Explore outputs follow the sidebar, not choices it no longer offers", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = rep(c("A", "B"), each = 6), Height = rep(c(80, 85, 90), 4),
                          Yield = c(5.1, 5.4, 5.2, 5.6, 5.3, 5.5, 6.1, 6.4, 6.2, 6.6, 6.3, 6.5))
    session$flushReact()
    session$setInputs(descVars = "Yield", descGroup = "Height", descPlotVar = "Yield")
    expect_identical(desc_group(), "Height")
    # new data in which Height is measured, not a grouping
    set.seed(1)
    rv$data <- data.frame(Trt = rep(c("A", "B"), each = 20), Height = round(rnorm(40, 85, 4), 2),
                          Yield = round(rnorm(40, 6, 0.5), 2))
    session$flushReact()
    expect_false("Height" %in% group_choices(rv$data))
    expect_identical(desc_group(), "")
    expect_false("Group" %in% names(desc_tab()))
  })
})

## ----------------------------------------------- found in fourth review ----

test_that("values recorded in steps get one bar per step, with no empty bars between", {
  germ <- data.frame(G = rep(c(70, 75, 80, 85, 90, 95), c(1, 3, 8, 18, 14, 4)))
  b <- ggplot2::ggplot_build(plot_hist(germ, "G"))$data[[1]]
  expect_equal(b$count, c(1, 3, 8, 18, 14, 4))
  expect_equal((b$xmin + b$xmax) / 2, c(70, 75, 80, 85, 90, 95))
  ph <- data.frame(pH = rep(c(6.3, 6.4, 6.5, 6.6, 6.7), c(6, 12, 14, 11, 5)))
  b <- ggplot2::ggplot_build(plot_hist(ph, "pH"))$data[[1]]
  expect_equal(b$count, c(6, 12, 14, 11, 5))
  expect_true(all(b$count > 0))
  # gaps of 0.2 and 0.3 still find the step of 0.1, and no value sits on a bar edge
  p2 <- data.frame(pH = c(6, 6.2, 6.5, 6.5, 6.8))
  expect_identical(qq_step(p2$pH), 0.1)
  b <- ggplot2::ggplot_build(plot_hist(p2, "pH"))$data[[1]]
  mid <- (b$xmin + b$xmax) / 2
  expect_equal(sum(b$count), 5)
  expect_true(all(abs(outer(p2$pH, c(b$xmin, b$xmax), "-")) > 1e-9))
  # no ties: pH to 0.1 still gets bars on the grid
  b <- ggplot2::ggplot_build(plot_hist(data.frame(pH = c(6.0, 6.1, 6.2, 6.3)), "pH"))$data[[1]]
  expect_equal(b$count, c(1, 1, 1, 1))
  # values kept to full precision have no grid
  set.seed(1)
  expect_true(is.na(qq_step(rnorm(30))))
  # over many grids, no value on a bar edge and no empty bar between values one step apart
  set.seed(9)
  for (i in 1:60) {
    st <- sample(c(0.1, 0.5, 1, 5, 0.01), 1)
    v <- round(rnorm(sample(6:60, 1), 50, sample(c(0.3, 1, 3, 10), 1)) / st) * st
    if (length(unique(v)) < 2) next
    b <- ggplot2::ggplot_build(plot_hist(data.frame(v = v), "v"))$data[[1]]
    expect_equal(sum(b$count), length(v))
    expect_true(all(abs(outer(v, c(b$xmin, b$xmax), "-")) > 1e-9 * max(1, abs(v))))
  }
})

test_that("the density note mentions steps only when they are wider than the smoothing", {
  # pH to 0.1 in a bell of 48 values: the step is narrower than the smoothing
  ph <- data.frame(pH = rep(round(seq(6.1, 6.9, by = 0.1), 1), c(1, 3, 6, 9, 10, 9, 6, 3, 1)))
  expect_lt(qq_step(ph$pH), 2 * bw.nrd0(ph$pH))
  expect_no_match(plot_text("density", ph, "pH")$reading, "steps", fixed = TRUE)
  # a mistyped value on a fine grid: the step is far narrower than the smoothing
  y <- c(4.12, 4.35, 4.21, 4.48, 5.02, 4.87, 5.10, 4.95, 4.66, 4.71, 4.59, 4.80, 5.31, 5.18, 5.44, 5.25, 4.35, 4.52, 4.47, 46.1)
  expect_no_match(plot_text("density", data.frame(y = y), "y")$reading, "steps", fixed = TRUE)
  # counts a step apart, wider than the smoothing
  pests <- data.frame(Pests = rep(0:6, c(6, 7, 11, 2, 1, 2, 1)))
  txt <- plot_text("density", pests, "Pests")$reading
  expect_match(txt, "bumps like these come from the steps, not from the shape", fixed = TRUE)
  expect_match(txt, "read it together with the histogram", fixed = TRUE)
  # spread-out counts whose step is narrower than the smoothing
  expect_no_match(plot_text("density", data.frame(P = c(7, 10, 14, 14, 14, 16, 17, 24)), "P")$reading, "steps", fixed = TRUE)
})

test_that("box notes by group say 'every group' only of every group", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 4), Y = c(NA, NA, NA, NA, 5, 6, 7, 6, 6, 5, 7, 6))
  txt <- plot_text("box", d, "Y", "Trt")$reading
  expect_match(txt, "The median is the same in every group with values, 6.00.", fixed = TRUE)
  expect_match(txt, "The middle half is equally spread in every group with values.", fixed = TRUE)
})

test_that("a mixed-case column name keeps its case at the start of a sentence", {
  expect_identical(cap1(c("pH 5.5", "kgN 40", "variety V2", "row 3")), c("pH 5.5", "kgN 40", "Variety V2", "Row 3"))
  d <- data.frame(pH = rep(c(5.5, 6.5, 7.5), each = 4), Yield = c(NA, NA, NA, NA, 3.2, 3.4, 3.3, 3.3, 4.1, 4.0, 4.1, 3.95))
  expect_match(describe_group_text("Yield", d$Yield, group_labels(d, "pH"), 1:12, "pH"),
               "pH 5.5 has no usable values.", fixed = TRUE)
})

## ------------------------------------------------ found in sixth review ----

test_that("a skew the minimum and maximum do not show is said to be so", {
  # skewness 0.50, yet the lowest value is further from the median than the highest
  x1 <- c(0, 16, 16, 17, 17, 17, 17, 19, 19, 25, 25, 25, 25, 25, 26, 26, 45, 45, 45, 45)
  expect_identical(shape_of(desc_one(x1))$kind, "right")
  expect_gt(median(x1) - min(x1), max(x1) - median(x1))
  txt <- describe_text("x", x1, seq_along(x1))
  expect_match(txt, "The skewness of 0.50 counts as a moderate right skew, but the lowest value lies further from the median than the highest", fixed = TRUE)
  expect_no_match(txt, "reach further above", fixed = TRUE)
  d <- data.frame(x = x1)
  expect_match(plot_text("hist", d, "x")$reading,
               "The skewness is 0.50, which counts as right skew, but the bars reach further below the median than above it.", fixed = TRUE)
  expect_match(plot_text("density", d, "x")$reading,
               "but the curve reaches further below the median than above it.", fixed = TRUE)
  # extremes equally far out
  e <- c(0, 9, 10, 10, 10, 10, 11, 12, 14, 20)
  if (shape_of(desc_one(e))$kind == "right")
    expect_match(plot_text("hist", data.frame(e = e), "e")$reading, "as far below the median as above it", fixed = TRUE)
  expect_identical(reach_of(desc_one(e), 2), "even")
})

test_that("one value off the grid does not turn the histogram into a comb", {
  sc <- c(rep(0:10, c(1, 1, 7, 7, 7, 6, 7, 7, 7, 2, 3)), 5.5)
  expect_identical(qq_step(sc), 0.5)
  expect_identical(main_step(sc), 1)
  b <- ggplot2::ggplot_build(plot_hist(data.frame(s = sc), "s"))$data[[1]]
  # every bar holds the same number of whole-number values, so flat data look flat
  held <- vapply(seq_len(nrow(b)), function(i) sum(0:10 > b$xmin[i] & 0:10 < b$xmax[i]), numeric(1))
  expect_true(all(held[-c(1, length(held))] == held[2]))
  expect_true(all(abs(outer(sc, c(b$xmin, b$xmax), "-")) > 1e-9))
  expect_equal(sum(b$count), length(sc))
  # a 0-1-3-5-7-9 scale: no empty bars between the scores
  ses <- rep(c(0, 1, 3, 5, 7, 9), c(10, 30, 70, 100, 60, 30))
  b <- ggplot2::ggplot_build(plot_hist(data.frame(s = ses), "s"))$data[[1]]
  expect_true(all(b$count > 0))
})

test_that("the steps note survives one value between the steps", {
  g <- data.frame(G = rep(c(75, 80, 85, 87.5, 90, 95), c(1, 4, 21, 1, 22, 5)))
  expect_identical(main_step(g$G), 5)
  expect_match(plot_text("density", g, "G")$reading, "The values are recorded mostly in steps of 5, wide beside the smoothing", fixed = TRUE)
  expect_identical(step_words(c(70, 75, 75, 80)), "in steps of 5")
})
