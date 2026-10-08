###############################################################################
##  DESCRIPTIVE STATISTICS
###############################################################################
## A summary of each numeric variable and four plots of its distribution, each
## followed by a plain-language reading of what it shows. Everything here
## describes the observations as they are; whether differences between
## treatments are real is a question for the analysis of variance, and the
## text says so rather than drawing conclusions the numbers cannot support.
## Every sentence states only what the numbers checked for it show: where two
## measures disagree, the text says so instead of picking one.

## How far the ends of a normal Q-Q plot bend in samples that really are
## normal, by sample size: the 99.5th percentile, over 20,000 normal samples
## of each size, of an end reaching out beyond the line (`out`, in units of
## the line's slope) and of an end stopping short of it (`inward`, in
## standard deviations); see qq_ends(). Each kind of bend is measured on the
## scale its cause does not inflate: heavy tails inflate the SD, two clusters
## inflate the quartiles. With both limits, normal values of that size are
## read as bending about once in a hundred. Computed once with
## set.seed(20261008); tests/testthat/test-descriptives.R checks a fresh
## simulation against them. The table is dense where the number of points
## averaged at each end changes (every ten values).
QQ_LIMIT <- data.frame(
  n = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
        43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
        69, 70, 79, 80, 89, 90, 99, 100, 125, 150, 200, 250, 300, 400, 500, 750,
        1000, 2000, 5000),
  out = c(25.872, 11.243, 8.038, 7.042, 8.848, 6.643, 5.482, 5.102, 5.886, 5.084,
          4.611, 4.214, 4.617, 4.174, 3.935, 2.869, 3.060, 2.945, 2.726, 2.692,
          2.931, 2.703, 2.537, 2.492, 2.657, 2.163, 2.038, 2.027, 2.186, 2.065,
          1.955, 1.990, 1.994, 1.920, 1.894, 1.678, 1.699, 1.661, 1.618, 1.613,
          1.645, 1.639, 1.585, 1.582, 1.582, 1.481, 1.423, 1.394, 1.417, 1.417,
          1.384, 1.364, 1.393, 1.346, 1.389, 1.260, 1.207, 1.130, 1.073, 1.041,
          0.996, 0.941, 0.949, 0.891, 0.788, 0.684, 0.563, 0.506, 0.463, 0.376,
          0.329, 0.272, 0.236, 0.160, 0.098),
  inward = c(0.684, 0.701, 0.823, 0.951, 1.085, 1.042, 1.204, 1.296, 1.352, 1.309,
             1.323, 1.354, 1.422, 1.367, 1.355, 1.089, 1.123, 1.079, 1.103, 1.104,
             1.147, 1.108, 1.120, 1.136, 1.167, 0.957, 0.960, 0.988, 0.999, 0.960,
             0.968, 0.979, 0.988, 0.969, 0.977, 0.871, 0.894, 0.866, 0.883, 0.876,
             0.895, 0.865, 0.883, 0.877, 0.883, 0.812, 0.795, 0.808, 0.832, 0.807,
             0.802, 0.804, 0.818, 0.821, 0.803, 0.746, 0.766, 0.702, 0.710, 0.665,
             0.676, 0.637, 0.630, 0.605, 0.564, 0.504, 0.449, 0.406, 0.366, 0.326,
             0.292, 0.239, 0.206, 0.147, 0.093))


## fmt() without a minus sign on a figure that rounds to zero
dfmt <- function(x, d) sub("^-(0[.]?0*)$", "\\1", fmt(x, d))

## "row 3" with a capital, to start a sentence; a name written in mixed case
## (pH, kgN) is left as it is
cap1 <- function(s)
  ifelse(grepl("^[a-z]+[A-Z]", s), s, paste0(toupper(substr(s, 1, 1)), substring(s, 2)))

## the order of magnitude of a positive number, rounded first so that an SD
## of 0.0999999999 counts as the 0.1 it is
mag <- function(z) floor(log10(signif(z, 6)))

## a typical spread: the interquartile range on the scale of an SD, which one
## wild value cannot inflate; the SD itself when the quartiles coincide
spread_of <- function(z) {
  z <- z[is.finite(z)]
  if (length(z) < 2) return(NA_real_)
  s <- stats::IQR(z) / 1.349
  if (s > 0) s else if (stats::sd(z) > 0) stats::sd(z) else NA_real_
}

## Decimals for a variable: about three significant figures of its typical
## (median) size and two of its typical spread, so neither a small unit nor a
## small spread rounds to zero, and one wild value (1250 typed for 1.25) does
## not set the precision of everything else. Given the groups `g`, each
## group's SD keeps two significant figures too, so a grouped table never
## shows a group's SD as 0. Whole numbers need no more than two decimals.
var_digits <- function(x, g = NULL) {
  ok <- is.finite(x)
  v <- x[ok]
  nz <- abs(v[v != 0])
  if (!length(nz)) return(2L)
  dg <- 3 - mag(stats::median(nz))
  sp <- spread_of(v)
  if (!is.null(g)) {
    gs <- vapply(split(v, g[ok]), function(z) if (length(z) > 1) stats::sd(z) else NA_real_, numeric(1))
    sp <- c(sp, gs[is.finite(gs) & gs > 0])
  }
  sp <- sp[is.finite(sp)]
  if (length(sp)) dg <- max(dg, 1 - mag(min(sp)))
  dg <- as.integer(min(10, max(0, dg)))
  if (all(v == round(v))) min(dg, 2L) else dg
}

## Excel's SKEW: the adjusted Fisher-Pearson coefficient of skewness, about 0
## for a symmetric spread, positive when the high values trail further out
skew_of <- function(v) {
  n <- length(v)
  if (n < 3) return(NA_real_)
  s <- stats::sd(v)
  if (s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((v - mean(v)) / s)^3)
}

## its standard error for n values from a normal distribution
skew_se <- function(n) sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))

## the numbers of one variable (one group), and everything said about them
desc_one <- function(x) {
  ## an infinite value is not a measurement, so it counts as missing
  v <- x[is.finite(x)]
  n <- length(v)
  q <- if (n) stats::quantile(v, c(0.25, 0.5, 0.75), type = 7, names = FALSE) else rep(NA_real_, 3)
  mu <- if (n) mean(v) else NA_real_
  s <- if (n > 1) stats::sd(v) else NA_real_
  list(N = n, Missing = sum(!is.finite(x)), Mean = mu, SD = s,
       Min = if (n) min(v) else NA_real_, Q1 = q[1], Median = q[2], Q3 = q[3],
       Max = if (n) max(v) else NA_real_,
       ## a CV compares the spread with the mean, which only makes sense for
       ## values that cannot be negative and are not all zero
       CV = if (n > 1 && min(v) >= 0 && is.finite(mu) && mu > 0) 100 * s / mu else NA_real_,
       Skewness = skew_of(v))
}

## A grouping column as a factor. Numbers are labelled as written (100000,
## not 1e+05) and kept in numeric order, a factor keeps its own order, and
## text takes its natural order; blank labels count as missing, so a row with
## no label is never a group of its own.
group_labels <- function(d, group) {
  x <- d[[group]]
  if (is.numeric(x)) {
    ok <- is.finite(x)
    lab <- rep(NA_character_, length(x))
    lab[ok] <- num_text(x[ok])
    return(factor(lab, levels = num_text(sort(unique(x[ok])))))
  }
  g <- trimws(as.character(x))
  g[!is.na(g) & g == ""] <- NA
  lv <- if (is.factor(x)) unique(trimws(levels(x))) else natural_levels(g)
  factor(g, levels = lv[lv %in% g])
}

#' Descriptive statistics for numeric variables
#'
#' For each variable: the number of values (N), the number missing, the mean,
#' standard deviation (SD), minimum, first quartile (Q1), median, third
#' quartile (Q3), maximum, coefficient of variation (CV, per cent) and
#' skewness, optionally for each level of a grouping column. Quartiles are
#' calculated as Excel's QUARTILE.INC does (\code{quantile(type = 7)}) and
#' skewness as Excel's SKEW does (the adjusted Fisher-Pearson coefficient).
#' Text entries are read as the data check reads them: an entry that is not
#' plainly a number (such as 5,6 or 12a) counts as missing; see
#' \code{\link{check_data}}.
#'
#' @param d A data frame.
#' @param vars The names of the numeric columns to describe.
#' @param group Optionally, the name of a column whose levels split each
#'   variable into groups. Rows without a label in it are left out.
#'
#' @return A data frame with one row per variable (and group): \code{Variable},
#'   \code{Group} (when grouped), \code{N}, \code{Missing}, \code{Mean},
#'   \code{SD}, \code{Min}, \code{Q1}, \code{Median}, \code{Q3}, \code{Max},
#'   \code{CV} and \code{Skewness}. CV is \code{NA} when any value is negative
#'   or the mean is not positive, where it has no useful meaning; skewness
#'   needs three values that are not all equal. Missing counts empty cells,
#'   entries that are not numbers and infinite values.
#'
#' @examples
#' describe_data(demo_data("RCBD"), "Yield")
#' describe_data(demo_data("RCBD"), "Yield", group = "Variety")
#'
#' @export
describe_data <- function(d, vars, group = NULL) {
  if (!length(vars)) stop("Choose at least one variable to describe.")
  if (!is.null(group) && !nzchar(group)) group <- NULL
  gone <- setdiff(c(vars, group), names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(sprintf("'%s'", gone)),
                 pl(length(gone), "is not a column", "are not columns")))
  if (!is.null(group)) {
    g <- group_labels(d, group)
    if (!nlevels(g)) stop(sprintf("'%s' has no labels to group the rows by.", group))
  }
  out <- lapply(vars, function(v) {
    x <- response_values(d[[v]])
    if (is.null(group)) return(data.frame(Variable = v, desc_one(x), check.names = FALSE))
    do.call(rbind, lapply(levels(g), function(lv)
      data.frame(Variable = v, Group = lv, desc_one(x[!is.na(g) & g == lv]), check.names = FALSE)))
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

## values beyond Tukey's fences (more than 1.5 times the interquartile range
## beyond the quartiles): the points a box plot draws on their own
unusual <- function(x) {
  q <- stats::quantile(x[is.finite(x)], c(0.25, 0.75), type = 7, names = FALSE)
  w <- 1.5 * (q[2] - q[1])
  which(is.finite(x) & (x < q[1] - w | x > q[2] + w))
}

## the same, judged within each group, as a box plot by group draws them
unusual_within <- function(x, g)
  sort(unlist(lapply(split(seq_along(x), g), function(i) i[unusual(x[i])]), use.names = FALSE))

## The shape of a distribution, from the skewness in the table read with
## Bulmer's (1979) bands: under 0.5 either way roughly symmetric, 0.5 to 1
## moderately skewed, over 1 strongly. `clear` is FALSE when a skewness that
## size could easily arise by chance in symmetric data of this size (within
## two standard errors), which the text then says.
shape_of <- function(s) {
  out <- list(kind = "unclear", g1 = s$Skewness, strength = "", clear = FALSE,
              heaped = FALSE)
  if (s$N == 0) return(out)
  if (s$N > 1 && s$SD == 0) { out$kind <- "constant"; return(out) }
  if (s$N < 5) return(out)
  out$heaped <- s$Q3 == s$Q1
  ## judged as printed, so two skewnesses that both print as 0.50 read alike
  g <- as.numeric(dfmt(s$Skewness, 2))
  out$kind <- if (g >= 0.5) "right" else if (g <= -0.5) "left" else "symmetric"
  out$strength <- if (abs(g) > 1) "strongly" else "moderately"
  out$clear <- abs(g) > 2 * skew_se(s$N)
  out
}

## how much the values vary relative to their mean, in words, judged on the
## CV as it is printed so that the word and the number never disagree
cv_words <- function(cv) {
  if (is.na(cv)) return("")
  cv <- as.numeric(fmt(cv, 1))
  if (cv < 10) "little" else if (cv < 20) "moderately" else if (cv < 30) "considerably" else "a great deal"
}

## Entries left out only because they are written in a form the data check
## flags (5,6; 1,250; 45%; 12 kg): numbers the user can bring in by correcting
## them on the Data tab, which the text should say rather than leave a column
## looking empty.
held_text <- function(raw) {
  if (is.numeric(raw)) return(NULL)
  k <- which(read_entries(raw)$kind %in% setdiff(NUMBER_KINDS, "number"))
  if (!length(k)) return(NULL)
  sprintf("%d %s written in a form the data check has flagged (such as '%s'); correct %s on the Data tab to include %s here.",
          length(k), pl(length(k), "entry is a number", "entries are numbers"), trimws(as.character(raw[k[1]])),
          pl(length(k), "it", "them"), pl(length(k), "it", "them"))
}

## "Variety V2", "Variety V2 and Variety V5", at most six named
group_names <- function(group, lv) join_and(head_more(sprintf("%s %s", group, lv), 6))

## the groups whose printed figure is the highest or the lowest: figures are
## compared as they are printed, so a tie in the table is a tie in the text
at_extreme <- function(txt, fun) which(as.numeric(txt) == fun(as.numeric(txt)))

## Which way the values reach further from the median, judged on the
## minimum, median and maximum as the table prints them: "up", "down" or
## "even". The skewness weighs the values furthest out most heavily but not
## only them, so the two usually agree and sometimes do not; when they do
## not, the text says so rather than state a direction the table contradicts.
reach_of <- function(s, digits) {
  p <- function(z) as.numeric(dfmt(z, digits))
  up <- p(s$Max) - p(s$Median); down <- p(s$Median) - p(s$Min)
  if (abs(up - down) < 1e-9 * max(1, abs(up), abs(down))) "even" else if (up > down) "up" else "down"
}

## The sentence on skewness, for the table reading. `s` is desc_one() of the
## values and `digits` the decimals the table prints them to.
skew_text <- function(sh, s, digits) {
  f <- function(z) dfmt(z, digits)
  g <- dfmt(sh$g1, 2)
  if (!sh$kind %in% c("right", "left"))
    return(switch(sh$kind,
      symmetric = sprintf("The skewness of %s is close to zero, so by this measure the values are spread roughly evenly about their mean.", g),
      "There are too few values to say much about the shape of the distribution."))
  side <- if (sh$kind == "right") "up" else "down"
  r <- reach_of(s, digits)
  out <- if (r == side)
    sprintf("The skewness of %s means the values are %s skewed to the %s: they reach further %s the median (%s %s) than %s it (%s %s).",
            g, sh$strength, sh$kind, if (side == "up") "above" else "below",
            if (side == "up") "up to" else "down to", f(if (side == "up") s$Max else s$Min),
            if (side == "up") "below" else "above",
            if (side == "up") "down to" else "up to", f(if (side == "up") s$Min else s$Max))
  else sprintf(paste("The skewness of %s counts as %s %s skew, but the %s value lies %s from the median than the %s,",
                     "so the skew does not come from a long tail of %s values; the histogram shows where the values lie."),
               g, if (sh$strength == "strongly") "a strong" else "a moderate", sh$kind,
               if (side == "up") "lowest" else "highest", if (r == "even") "as far" else "further",
               if (side == "up") "highest" else "lowest", if (side == "up") "high" else "low")
  if (!sh$clear)
    out <- paste(out, sprintf("With %d values, a skewness this size can also arise by chance in values from a symmetric distribution.", s$N))
  out
}

## The reading of the summary table for one variable. `raw` is the column as
## it is in the data (numbers or text); `rows` the row numbers of the data,
## for naming unusual values.
describe_text <- function(v, raw, rows, digits = NULL) {
  x <- response_values(raw)
  if (is.null(digits)) digits <- var_digits(x)
  s <- desc_one(x); sh <- shape_of(s)
  f <- function(z) dfmt(z, digits)
  if (s$N == 0)
    return(paste(c(sprintf("'%s' has no values that can be read as numbers.", v), held_text(raw)), collapse = " "))
  out <- character(0)
  if (s$Missing)
    out <- c(out, sprintf("%d of the %d rows %s no usable value and %s left out of these figures.",
      s$Missing, s$N + s$Missing, pl(s$Missing, "has", "have"), pl(s$Missing, "is", "are")), held_text(raw))
  if (s$N == 1) return(paste(c(out, sprintf("'%s' has a single value, %s.", v, num_text(x[is.finite(x)]))), collapse = " "))
  if (sh$kind == "constant")
    return(paste(c(out, sprintf("Every value of '%s' is %s, so there is no variation to describe.", v,
                                num_text(s$Mean))), collapse = " "))
  out <- c(out, if (s$N >= 5)
    sprintf("The %d values of '%s' average %s (median %s) and run from %s to %s; the middle half lie between %s and %s.",
            s$N, v, f(s$Mean), f(s$Median), f(s$Min), f(s$Max), f(s$Q1), f(s$Q3))
  else sprintf("The %d values of '%s' average %s (median %s) and run from %s to %s.",
               s$N, v, f(s$Mean), f(s$Median), f(s$Min), f(s$Max)))
  out <- c(out, if (!is.na(s$CV))
    sprintf(paste0("The CV of %s%% means the values vary %s relative to their mean. This CV ",
      "covers all the observations, differences between treatments included, so it is not the ",
      "experimental CV of an analysis of variance, which measures only the unexplained variation."),
      fmt(s$CV, 1), cv_words(s$CV))
  else sprintf("No coefficient of variation is given: it needs values that cannot be negative, and the smallest value here is %s.",
               num_text(s$Min)))
  if (sh$heaped) out <- c(out, sprintf("At least half the values are exactly %s.", num_text(s$Median)))
  out <- c(out, skew_text(sh, s, digits))
  u <- unusual(x)
  if (length(u))
    out <- c(out, sprintf(paste0("%d %s outside the usual range for this variable (more than 1.5 times the ",
      "spread of the middle half beyond the quartiles): %s. Check %s for recording errors. %s from a ",
      "treatment that really differs can look unusual too, so do not remove %s only because %s unusual."),
      length(u), pl(length(u), "value lies", "values lie"),
      join_and(head_more(sprintf("%s in row %s", num_text(x[u]), rows[u]), 6)),
      pl(length(u), "it", "them"), pl(length(u), "A value", "Values"),
      pl(length(u), "it", "them"), pl(length(u), "it is", "they are")))
  if (s$N >= 5 && s$N < 8)
    out <- c(out, "With so few values, the quartiles and the shape are only a rough guide.")
  paste(out, collapse = " ")
}

## The reading of the table when it is split by a grouping column: what the
## groups' figures in the table show, judged group by group. Nothing here is
## a test; whether the groups differ is for the analysis of variance.
describe_group_text <- function(v, raw, g, rows, group, digits = NULL) {
  x <- response_values(raw)
  if (is.null(digits)) digits <- var_digits(x, g)
  lab <- !is.na(g)
  st <- lapply(split(x[lab], g[lab]), desc_one)
  n <- vapply(st, function(s) as.numeric(s$N), numeric(1))
  has <- names(st)[n > 0]
  out <- character(0)
  if (any(!lab))
    out <- c(out, sprintf("%s %s no label in '%s' and %s left out of the groups.",
      cap1(rows_text(rows[!lab])), pl(sum(!lab), "has", "have"), group, pl(sum(!lab), "is", "are")))
  if (nlevels(g) == 1)
    return(paste(c(out, sprintf("Every row with a label in '%s' has the same one, %s, so there are no groups to compare.",
                                group, levels(g))), collapse = " "))
  miss <- sum(vapply(st, function(s) as.numeric(s$Missing), numeric(1)))
  if (miss)
    out <- c(out, sprintf("Within the groups, %d %s no usable value and %s left out of the figures.",
      miss, pl(miss, "row has", "rows have"), pl(miss, "is", "are")), held_text(raw[lab]))
  if (!length(has))
    return(paste(c(out, sprintf("No group of %s has a value of '%s' that can be read as a number.", group, v)),
                 collapse = " "))
  if (any(n == 0))
    out <- c(out, sprintf("%s %s no usable values.", cap1(group_names(group, names(st)[n == 0])),
                          pl(sum(n == 0), "has", "have")))
  if (length(has) == 1)
    return(paste(c(out, sprintf("Only %s has values, so there is nothing to compare between groups.",
                                group_names(group, has))), collapse = " "))
  mu <- dfmt(vapply(st[has], function(s) s$Mean, numeric(1)), digits)
  every <- length(has) == nlevels(g)
  out <- c(out, if (length(unique(mu)) == 1)
    (if (every) sprintf("Every group of %s has the same mean of '%s', %s.", group, v, mu[1])
     else sprintf("%s have the same mean of '%s', %s.", cap1(group_names(group, has)), v, mu[1]))
  else sprintf("The group means of '%s' run from %s (%s) to %s (%s).", v,
               mu[at_extreme(mu, min)[1]], group_names(group, has[at_extreme(mu, min)]),
               mu[at_extreme(mu, max)[1]], group_names(group, has[at_extreme(mu, max)])))
  cv <- vapply(st[has], function(s) s$CV, numeric(1))
  ok <- !is.na(cv)
  if (any(ok)) {
    cvo <- fmt(cv[ok], 1); nm <- has[ok]
    out <- c(out, if (length(cvo) == 1)
      sprintf("The CV within %s is %s%%.", group_names(group, nm), cvo)
    else if (length(unique(cvo)) == 1)
      (if (all(ok) && every) sprintf("The CV within every group is %s%%.", cvo[1])
       else sprintf("The CV within %s is %s%%.", group_names(group, nm), cvo[1]))
    else sprintf("The CV within a group runs from %s%% (%s) to %s%% (%s).",
                 cvo[at_extreme(cvo, min)[1]], group_names(group, nm[at_extreme(cvo, min)]),
                 cvo[at_extreme(cvo, max)[1]], group_names(group, nm[at_extreme(cvo, max)])),
      sprintf(paste("%s only the observations within one group, so differences between groups are not part",
                    "of it; it is not the experimental CV of an analysis of variance either."),
              if (length(cvo) == 1) "It covers" else "Each covers"))
  }
  if (any(!ok)) {
    ## why each group without a CV has none, from its own figures
    why <- vapply(st[has[!ok]], function(s) if (s$N < 2) "one" else if (s$Min < 0) "negative" else "zero", "")
    nm <- has[!ok]
    for (w in c("one", "negative", "zero")) if (any(why == w)) {
      k <- sum(why == w); who <- group_names(group, nm[why == w])
      out <- c(out, switch(w,
        one = sprintf("No CV is given for %s, which %s only one value.", who, pl(k, "has", "have")),
        negative = sprintf("No CV is given for %s: a CV needs values that cannot be negative.", who),
        zero = sprintf("No CV is given for %s, where every value is zero.", who)))
    }
  }
  ## Tukey's fences cannot mark any of three values or fewer, so only groups
  ## of four or more can show an unusual value within themselves
  small <- has[n[has] <= 3]
  u <- unusual_within(x, g)
  if (length(u))
    out <- c(out, sprintf(paste0("Judged within %s own group, %d %s outside the usual range (more than 1.5 times the ",
      "spread of the group's middle half beyond its quartiles): %s. Check %s for recording errors; ",
      "do not remove %s only because %s unusual."),
      pl(length(u), "its", "their"), length(u), pl(length(u), "value lies", "values lie"),
      join_and(head_more(sprintf("%s in row %s (%s %s)", num_text(x[u]), rows[u], group,
                                 as.character(g[u])), 6)),
      pl(length(u), "it", "them"), pl(length(u), "it", "them"), pl(length(u), "it is", "they are")))
  else if (length(small) < length(has))
    out <- c(out, if (length(small)) "No value lies outside the usual range of its own group, in the groups with four or more values."
                  else "No value lies outside the usual range of its own group.")
  if (length(small))
    out <- c(out, if (length(small) == length(has))
      "Every group has three or fewer values, too few for any value to stand out within its group."
    else sprintf("%s %s three or fewer values, too few for any value to stand out within %s.",
                 cap1(group_names(group, small)), pl(length(small), "has", "have"), pl(length(small), "it", "them")))
  if (any(n[n > 0] < 5))
    out <- c(out, "Groups with fewer than five values give only a rough guide to their quartiles.")
  out <- c(out, "These figures describe the observations; whether the groups really differ is for the analysis of variance to judge.")
  paste(out, collapse = " ")
}

## ------------------------------------------------------------------ plots ----

## The data of one variable as a frame for plotting, with the row numbers and,
## when a grouping column is given, the group. Rows without a group label are
## dropped only when `drop` is TRUE (the box plot by group, which cannot place
## them); the other plots draw every row.
desc_frame <- function(d, v, group = NULL, drop = TRUE) {
  x <- response_values(d[[v]])
  out <- data.frame(value = x, row = row_ids(d))
  if (!is.null(group) && nzchar(group)) {
    out$group <- group_labels(d, group)
    if (drop) out <- out[!is.na(out$group), , drop = FALSE]
  }
  out[is.finite(out$value), , drop = FALSE]
}

## The fewest values each plot is drawn from. A plot is not drawn below this,
## nor (for the density and Q-Q plots) when every value is the same; the
## reason is what the app shows in its place.
PLOT_MIN <- c(hist = 2, box = 2, density = 3, qq = 3)
plot_refusal <- function(kind, x) {
  if (length(x) < PLOT_MIN[[kind]]) return("There are too few values to draw this plot.")
  if (kind %in% c("density", "qq") && stats::sd(x) == 0)
    return(sprintf("Every value is %s, so there is no spread to draw.", num_text(x[1])))
  NULL
}

## lines for the mean and the median, with a key, for the histogram and density
centre_lines <- function(x) {
  data.frame(what = factor(c("Mean", "Median"), levels = c("Mean", "Median")),
             at = c(mean(x), stats::median(x)))
}

plot_hist <- function(d, v) {
  df <- desc_frame(d, v)
  if (!is.null(plot_refusal("hist", df$value))) return(NULL)
  x <- df$value
  bins <- max(5, min(30, ceiling(log2(length(x)) + 1)))          # Sturges' rule
  ## Values recorded in steps (counts, scores, germination in steps of 5, pH
  ## to 0.1) get bars a whole number of the usual spacing wide, so each bar
  ## holds the same number of the usual recorded values and none is left
  ## empty between values a step apart; the edges sit half the finest step
  ## off the grid, so no value lies on an edge.
  st <- qq_step(x)
  bars <- if (is.finite(st)) {
    ms <- main_step(x)
    bw <- ms * max(1, ceiling(diff(range(x)) / (bins * ms)))
    geom_histogram(binwidth = bw, boundary = min(x) - st / 2, fill = "#BBD3F2", colour = "white")
  } else geom_histogram(bins = bins, fill = "#BBD3F2", colour = "white")
  ggplot(df, aes(x = .data$value)) + bars +
    geom_vline(data = centre_lines(x),
               aes(xintercept = .data$at, linetype = .data$what), colour = "#173F7D", linewidth = 0.8) +
    scale_linetype_manual(values = c(Mean = "dashed", Median = "dotted"), name = NULL) +
    labs(title = paste("Histogram of", v), x = v, y = "Number of observations") +
    theme_doe()
}

plot_density <- function(d, v) {
  df <- desc_frame(d, v)
  if (!is.null(plot_refusal("density", df$value))) return(NULL)
  ggplot(df, aes(x = .data$value)) +
    geom_density(fill = "#BBD3F2", colour = "#3B7DD8", alpha = 0.7) +
    geom_rug(colour = "grey40", alpha = 0.6) +
    geom_vline(data = centre_lines(df$value),
               aes(xintercept = .data$at, linetype = .data$what), colour = "#173F7D", linewidth = 0.8) +
    scale_linetype_manual(values = c(Mean = "dashed", Median = "dotted"), name = NULL) +
    labs(title = paste("Density of", v), x = v, y = "Density") +
    theme_doe()
}

plot_box <- function(d, v, group = NULL) {
  df <- desc_frame(d, v, group)
  if (!is.null(plot_refusal("box", df$value))) return(NULL)
  grouped <- "group" %in% names(df)
  if (!grouped) df$group <- factor(v)
  ggplot(df, aes(x = .data$group, y = .data$value)) +
    geom_boxplot(fill = "#BBD3F2", colour = "#173F7D", outlier.colour = "#C0392B", width = 0.55) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", colour = "#173F7D") +
    labs(title = paste("Box plot of", v, if (grouped) paste("by", group) else ""),
         x = if (grouped) group else NULL, y = v,
         caption = "Diamond: mean. Line in the box: median. Red points: values more than 1.5 box lengths beyond the box.") +
    theme_doe() + (if (!grouped) theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) else NULL)
}

plot_qq <- function(d, v) {
  df <- desc_frame(d, v)
  if (!is.null(plot_refusal("qq", df$value))) return(NULL)
  ggplot(df, aes(sample = .data$value)) +
    stat_qq(colour = "#3B7DD8") + stat_qq_line(colour = "#C0392B") +
    labs(title = paste("Normal Q-Q plot of", v),
         x = "Where the values would be if they were normal (theoretical quantiles)",
         y = paste(v, "(sample quantiles)")) +
    theme_doe()
}

## How far the two ends of a normal Q-Q plot lie from its line, as drawn:
## the points are the sorted values against qnorm(ppoints(n)) and the line
## passes through the quartiles (stat_qq and stat_qq_line). Each end is the
## mean distance of its outer tenth of the points (at least one) from the
## line, positive above it; `slope` and `sd` are the two scales they are
## judged on.
qq_ends <- function(x) {
  x <- sort(x[is.finite(x)]); n <- length(x)
  t <- stats::qnorm(stats::ppoints(n))
  q <- stats::quantile(x, c(0.25, 0.75), type = 7, names = FALSE)
  b <- (q[2] - q[1]) / diff(stats::qnorm(c(0.25, 0.75)))
  r <- x - (q[1] + b * (t - stats::qnorm(0.25)))
  k <- max(1, floor(0.1 * n))
  c(low = mean(r[seq_len(k)]), high = mean(r[(n - k + 1):n]), slope = b, sd = stats::sd(x))
}

## the bend beyond which a sample of n normal values would rarely go
qq_limit <- function(n, which)
  stats::approx(log(QQ_LIMIT$n), QQ_LIMIT[[which]], log(n), rule = 2)$y

## Values recorded in steps that are large beside their spread (whole days,
## counts) sit in flat runs, and the quartiles the line passes through jump
## from step to step, so the ends of the plot bend for that reason alone.
## By simulation, normal values rounded to steps up to this fraction of
## their SD are still read as bending at most about 2% of the time.
qq_step_limit <- function(n) min(0.33, 0.125 * sqrt(200 / n))
## The recording step: the largest step that every gap between distinct
## values is a whole number of, looked for among the smallest gap divided by
## 1 to 100 (pH to 0.1 with gaps of 0.2 and 0.3 has a step of 0.1), cleared
## of the residue of subtracting decimals (0.0999999999999996 is 0.1). NA
## when the values sit on no such grid, as values kept to full precision do.
qq_step <- function(x) {
  u <- sort(unique(x[is.finite(x)]))
  if (length(u) < 2) return(NA_real_)
  gap <- diff(u)
  for (k in 1:100) {
    r <- gap / (min(gap) / k)
    if (all(abs(r - round(r)) < 1e-6)) return(signif(min(gap) / k, 10))
  }
  NA_real_
}
## The spacing most of the values sit at: the commonest gap between
## neighbouring distinct values. One entry off the grid (5.5 among whole
## numbers, 87.5 among percentages in steps of 5) makes the common step finer
## but leaves this alone.
main_step <- function(x) {
  st <- qq_step(x)
  if (!is.finite(st)) return(NA_real_)
  k <- round(diff(sort(unique(x[is.finite(x)]))) / st)
  signif(st * as.numeric(names(which.max(table(k)))), 10)
}
## "in steps of 5", or "mostly in steps of 5" when a few values sit between
step_words <- function(x)
  paste0(if (isTRUE(all.equal(main_step(x), qq_step(x)))) "" else "mostly ", "in steps of ", num_text(main_step(x)))

## Values recorded in steps: they sit on a grid and repeat. A few values far
## apart are not "recorded in steps" however wide their gaps.
on_grid <- function(x) {
  x <- x[is.finite(x)]
  is.finite(qq_step(x)) && anyDuplicated(x) > 0
}
qq_coarse <- function(x) {
  x <- x[is.finite(x)]
  on_grid(x) && main_step(x) / stats::sd(x) > qq_step_limit(length(x))
}



## Each end as reaching out beyond the line ("out"), stopping short of it
## ("in") or neither ("ok"), against what normal samples of the same size
## show; for example "in out" is the curve of a longer tail of high values.
## The quartiles must differ (the caller deals with heaped values).
qq_pattern <- function(x) {
  n <- sum(is.finite(x))
  e <- qq_ends(x); lo_out <- qq_limit(n, "out"); lo_in <- qq_limit(n, "inward")
  low <- if (-e[["low"]] / e[["slope"]] > lo_out) "out" else if (e[["low"]] / e[["sd"]] > lo_in) "in" else "ok"
  high <- if (e[["high"]] / e[["slope"]] > lo_out) "out" else if (-e[["high"]] / e[["sd"]] > lo_in) "in" else "ok"
  paste(low, high)
}

## When the Q-Q plot shows a tail on one side that the skewness in the table
## does not, the note says so, rather than leave the histogram's "roughly
## even" and the Q-Q plot's "longer tail" side by side unexplained.
qq_vs_skew <- function(pattern, sh) {
  kind <- sh$kind
  ## a clear skewness while neither end passes its limit: the values do lean
  ## to one side, though no end of this plot shows it beyond chance
  if (pattern == "ok ok" && kind %in% c("right", "left") && isTRUE(sh$clear))
    return(paste(" The skewness in the table is larger than chance would usually give, though, even if neither",
                 "end of this plot bends beyond what chance allows for this many values."))
  tail <- if (pattern %in% c("ok out", "in out")) "right" else if (pattern %in% c("out ok", "out in")) "left" else ""
  if (!nzchar(tail) || !kind %in% c("right", "left", "symmetric")) return("")
  if (kind == "symmetric")
    " The skewness in the table is small, so this tail shows only at the very end of the plot, not as a lopsided spread of the values."
  else if (kind != tail)
    " The skewness in the table points the other way: the two measures disagree, so read the histogram as well."
  else ""
}

## The reading of a Q-Q plot from the bend at each end. When neither end
## bends beyond what normal values can show by chance, the text claims no
## more than "no clear sign" of a departure, since a small sample can hide one.
qq_reading <- function(x) {
  switch(qq_pattern(x),
    "ok ok" = "Neither end of the plot bends away from the line more than values drawn from a normal distribution can by chance, so the plot gives no clear sign that the values are not normal.",
    "ok out" = "The points rise above the line at the right-hand end: the highest values reach further than a normal distribution would put them, a longer tail of high values.",
    "in out" = "The points curve upwards along the plot, above the line at both ends: the low values stop short and the high values reach further than a normal distribution would put them, a longer tail of high values (right skew).",
    "in ok" = "The points lie above the line at the left-hand end: the lowest values stop short of where a normal distribution would put them.",
    "out ok" = "The points fall below the line at the left-hand end: the lowest values reach further than a normal distribution would put them, a longer tail of low values.",
    "out in" = "The points curve downwards along the plot, below the line at both ends: the high values stop short and the low values reach further than a normal distribution would put them, a longer tail of low values (left skew).",
    "ok in" = "The points lie below the line at the right-hand end: the highest values stop short of where a normal distribution would put them.",
    "out out" = "Both ends bend away from the line, down at the left and up at the right: there are more very low and very high values than a normal distribution would have (heavy tails).",
    "in in" = "The points flatten out at both ends, above the line at the left and below it at the right (an S-shape): the values stop short of where a normal distribution would reach at both ends, as happens when they form two clusters or are bounded at both ends.")
}

## which end of the plot the unusual values are at
end_words <- function(x, u, low, high, both) {
  m <- stats::median(x)
  if (all(x[u] > m)) high else if (all(x[u] < m)) low else both
}

## With a grouping chosen, the only box plot on screen is the one by group,
## so values flagged over all rows together are described for what they
## are, and each is put in its group's light: ordinary within a group of
## four or more (perhaps a treatment that differs), in a group too small for
## anything to stand out (check it anyway), or without a group label.
pooled_flags <- function(df, u, group) {
  rows <- function(i) rows_text(df$row[i])
  g <- df$group
  size <- table(g)
  lab <- u[!is.na(g[u])]
  unl <- u[is.na(g[u])]
  small <- lab[as.numeric(size[as.character(g[lab])]) <= 3]
  ok_in_group <- setdiff(setdiff(lab, small), unusual_within(df$value, g))
  out <- character(0)
  if (length(ok_in_group))
    out <- c(out, sprintf(paste0("Within %s own group of %s, %s not unusual (see the box plot by group), ",
      "so %s may reflect a difference between groups rather than a recording error."),
      pl(length(ok_in_group), "its", "their"), group,
      paste(rows(ok_in_group), pl(length(ok_in_group), "is", "are")), pl(length(ok_in_group), "it", "they")))
  if (length(small)) {
    ng <- length(unique(as.character(g[small])))
    out <- c(out, sprintf("%s %s in %s of three or fewer values, too few for a value to stand out within %s, so check %s all the same.",
      cap1(rows(small)), pl(length(small), "is", "are"), pl(ng, "a group", "groups"),
      pl(ng, pl(length(small), "its group", "their group"), "their groups"), pl(length(small), "it", "them")))
  }
  if (length(unl))
    out <- c(out, sprintf("%s %s no label in '%s', so the box plot by group does not show %s.",
      cap1(rows(unl)), pl(length(unl), "has", "have"), group, pl(length(unl), "it", "them")))
  out
}

## What each plot shows, and what it shows for these data. The readings use
## only the numbers the plot is drawn from, so they say nothing the plot
## does not; the box plot by group is read group by group.
plot_text <- function(kind, d, v, group = NULL) {
  grouped <- !is.null(group) && nzchar(group)
  refuse <- function(no) list(what = "", reading = paste(c(no, held_text(d[[v]])), collapse = " "))
  if (kind == "box" && grouped) {
    df <- desc_frame(d, v, group)
    no <- plot_refusal("box", df$value)
    if (!is.null(no)) return(refuse(no))
    all <- desc_frame(d, v, group, drop = FALSE)
    ## the same decimals as the grouped table
    dg <- var_digits(response_values(d[[v]]), group_labels(d, group))
    return(list(what = box_what(), reading = box_group_text(df, v, group, all$row[is.na(all$group)], dg)))
  }
  df <- desc_frame(d, v, if (grouped) group, drop = FALSE)
  x <- df$value
  no <- plot_refusal(kind, x)
  if (!is.null(no)) return(refuse(no))
  s <- desc_one(x); sh <- shape_of(s); u <- unusual(x)
  if (sh$kind == "constant")
    return(list(what = switch(kind, hist = hist_what(), box = box_what(), ""),
                reading = sprintf(switch(kind, hist = "Every value is %s, so all the observations fall in one bar.",
                                               box = "Every value is %s, so the box is a single line."),
                                  num_text(s$Mean))))
  rows <- function(i) rows_text(df$row[i])
  own <- if (grouped && length(u)) paste0(" ", pooled_flags(df, u, group), collapse = "") else ""
  flagged <- function(where) if (!length(u)) "" else if (grouped)
    sprintf(" Taken over all rows together, %s would be marked as possibly unusual (more than 1.5 box lengths beyond the box of all the values); %s at the %s.",
            rows(u), pl(length(u), "it is", "they are"), where)
  else sprintf(" The box plot marks %s as possibly unusual; %s at the %s.", rows(u), pl(length(u), "it is", "they are"), where)
  heap <- function(what) if (sh$heaped) sprintf("At least half the values are exactly %s, so %s. ", num_text(s$Median), what) else ""
  g1 <- dfmt(s$Skewness, 2)
  ## which way the drawn values reach further from the median, as the table
  ## prints them, and whether that is the way the skewness points
  r <- reach_of(s, var_digits(response_values(d[[v]])))
  side <- if (sh$kind == "right") "up" else if (sh$kind == "left") "down" else ""
  lean <- function(thing) {
    verb <- if (thing == "bars") "reach" else "reaches"
    hi <- if (side == "up") "above" else "below"; lo <- if (side == "up") "below" else "above"
    if (r == side) sprintf("The %s %s further %s the median than %s it (the skewness is %s).", thing, verb, hi, lo, g1)
    else if (r == "even") sprintf("The skewness is %s, which counts as %s skew, but the %s %s as far %s the median as %s it.",
                                  g1, sh$kind, thing, verb, lo, hi)
    else sprintf("The skewness is %s, which counts as %s skew, but the %s %s further %s the median than %s it.",
                 g1, sh$kind, thing, verb, lo, hi)
  }
  switch(kind,
    hist = list(what = hist_what(), reading = paste0(
      heap("one bar holds at least half the observations"),
      switch(sh$kind,
        right = , left = lean("bars"),
        symmetric = sprintf("The skewness is %s, close to zero, so by this measure the bars are spread roughly evenly about the mean.", g1),
        unclear = "There are too few values to judge the shape."),
      flagged(paste(end_words(x, u, "left-hand end", "right-hand end", "two ends"), "of the histogram")),
      own,
      if (abs(s$Mean - s$Median) <= 0.1 * s$SD) " The mean and median lines nearly coincide."
      else if (s$Mean > s$Median) paste0(" The mean line lies to the right of the median line",
        if (sh$kind == "right") ", as it usually does with right skew." else ".")
      else paste0(" The mean line lies to the left of the median line",
        if (sh$kind == "left") ", as it usually does with left skew." else "."),
      paste(" If the bars form two or more separate humps rather than one, the values fall into groups that lie apart,",
            "often treatments or seasons that differ, and the skewness then reflects how large the groups are rather than a tail"),
      if (grouped) "; the box plot by group shows each group separately."
      else if (length(group_choices(d))) "; choose a column under 'Summarise separately for each level of' to see a box plot for each group."
      else ".")),
    density = list(
      what = "A density curve is a smoothed histogram: the higher the curve, the more observations lie near that value. The ticks along the bottom are the individual observations.",
      reading = paste0(
        heap("the curve has a sharp peak there"),
        switch(sh$kind,
          right = , left = lean("curve"),
          symmetric = sprintf("The skewness is %s, close to zero, so by this measure the curve is spread roughly evenly about the mean.", g1),
          unclear = "There are too few values to judge the shape."),
        " Two or more separate peaks would mean the values gather in more than one place, which often happens when they come from groups that differ.",
        ## The curve is a sum of bell curves as wide as the smoothing
        ## (ggplot2's bandwidth, bw.nrd0), one on each value; two equal bells
        ## stay one hump unless they are more than two widths apart, so only
        ## steps wider than that can give the curve a bump at each value.
        if (on_grid(x) && main_step(x) > 2 * stats::bw.nrd0(x))
          sprintf(paste(" The values are recorded %s, wide beside the smoothing, so the curve may rise and fall",
                        "at each recorded value; bumps like these come from the steps, not from the shape."), step_words(x)),
        " The smoothing can round off sharp features, so read it together with the histogram.")),
    box = list(what = box_what(), reading = paste0(
      if (length(u)) sprintf("%d %s beyond the whiskers (%s); check %s for recording errors.", length(u),
        pl(length(u), "value lies", "values lie"), rows(u), pl(length(u), "it", "them"))
      else "No value lies beyond the whiskers.",
      " ", box_shape(s),
      if (s$N < 5) " With so few values the box is only a rough guide." else "")),
    qq = list(
      what = "Each point is one observation, placed against where it would fall if the values followed a normal distribution. Points close to the straight line mean the values look normal.",
      reading = paste0(
        if (s$N < 5) "There are too few points to judge."
        else if (sh$heaped) sprintf(paste0("At least half the values are exactly %s, so many points sit ",
          "in one flat run; values like these do not follow a normal distribution."), num_text(s$Median))
        else if (qq_coarse(x)) sprintf(paste0("The values are recorded %s, which is large beside their ",
          "spread, so the points sit in flat runs and the line through the quartiles jumps with the steps: ",
          "the ends of this plot cannot be read reliably. The histogram shows the shape better."), step_words(x))
        else paste0(qq_reading(x), qq_vs_skew(qq_pattern(x), sh)),
        if (!length(u)) ""
        else sprintf(" %s %s (%s) %s at the %s.",
          if (grouped) "Taken over all rows together, the" else "The",
          if (grouped) pl(length(u), "value that would be marked as possibly unusual", "values that would be marked as possibly unusual")
          else pl(length(u), "value the box plot marks as possibly unusual", "values the box plot marks as possibly unusual"),
          rows(u), pl(length(u), "is the point", "are the points"),
          end_words(x, u, "bottom end", "top end", "two ends")),
        own,
        if (s$N >= 5 && s$N < 20) sprintf(" With only %d values, a Q-Q plot can show only a large departure from normal.", s$N) else "",
        " For an analysis of variance it is the residuals that should look normal, not the raw values, which mix all the treatments; the Assumptions tab checks the residuals.")))
}

hist_what <- function()
  "A histogram counts how many observations fall in each range of values. The dashed line marks the mean and the dotted line the median."
box_what <- function() paste0(
  "The box covers the middle half of the values (from Q1 to Q3), the line inside it is the median and the diamond the mean. ",
  "The whiskers reach the furthest values within 1.5 box lengths of the box; points beyond them are drawn on their own as possibly unusual values.")

## where the median sits in the box, read straight from the quartiles drawn
box_shape <- function(s) {
  iqr <- s$Q3 - s$Q1
  if (iqr == 0) return(sprintf("The box is a single line: at least half the values are exactly %s.", num_text(s$Median)))
  if (s$Median == s$Q3) return(sprintf("The median is on the top edge of the box: it and Q3 are both %s, as happens when many values are equal.", num_text(s$Median)))
  if (s$Median == s$Q1) return(sprintf("The median is on the bottom edge of the box: it and Q1 are both %s, as happens when many values are equal.", num_text(s$Median)))
  b <- (s$Q3 + s$Q1 - 2 * s$Median) / iqr
  if (b > 0.1) "The median sits towards the bottom of the box, so the middle half is more spread out above the median than below it."
  else if (b < -0.1) "The median sits towards the top of the box, so the middle half is more spread out below the median than above it."
  else "The median sits near the middle of the box, so the middle half is spread about evenly either side of it."
}

## The reading of a box plot by group: which groups have the highest and the
## lowest median and the widest spread, compared as the figures are printed
## (to `digits`, the grouped table's decimals) so that a tie is named as a
## tie, and the values each group's own whiskers leave out. Facts about these
## data only.
box_group_text <- function(df, v, group, unlabelled = integer(0), digits = var_digits(df$value, df$group)) {
  st <- lapply(split(df$value, df$group), function(z) if (length(z)) desc_one(z))
  st <- st[!vapply(st, is.null, logical(1))]
  out <- character(0)
  if (length(unlabelled))
    out <- c(out, sprintf("%s %s no label in '%s' and %s not drawn.", cap1(rows_text(unlabelled)),
                          pl(length(unlabelled), "has", "have"), group, pl(length(unlabelled), "is", "are")))
  if (length(st) < 2)
    return(paste(c(out, sprintf("Only one group of %s has values, so there is nothing to compare.", group)), collapse = " "))
  nm <- names(st)
  ## 'every group' only when every level has values
  every <- if (length(st) == nlevels(df$group)) "every group" else "every group with values"
  med <- dfmt(vapply(st, function(s) s$Median, numeric(1)), digits)
  iqr <- dfmt(vapply(st, function(s) s$Q3 - s$Q1, numeric(1)), digits)
  out <- c(out, if (length(unique(med)) == 1)
    sprintf("The median is the same in %s, %s.", every, med[1])
  else sprintf("The median is highest in %s (%s) and lowest in %s (%s).",
               group_names(group, nm[at_extreme(med, max)]), med[at_extreme(med, max)[1]],
               group_names(group, nm[at_extreme(med, min)]), med[at_extreme(med, min)[1]]))
  raw_iqr <- vapply(st, function(s) s$Q3 - s$Q1, numeric(1))
  out <- c(out, if (all(raw_iqr == 0)) sprintf("Every box is a single line: in %s at least half the values are equal.", every)
  else if (all(as.numeric(iqr) == 0)) sprintf("In %s the middle half is narrower than the last decimal shown, so each box looks like a single line.", every)
  else if (length(unique(iqr)) == 1) sprintf("The middle half is equally spread in %s.", every)
  else sprintf("The spread of the middle half is widest in %s.", group_names(group, nm[at_extreme(iqr, max)])))
  u <- unusual_within(df$value, df$group)
  if (length(u))
    out <- c(out, sprintf("%d %s beyond the whiskers of %s own group (%s).", length(u),
      pl(length(u), "value lies", "values lie"), pl(length(u), "its", "their"), rows_text(df$row[u])))
  n <- vapply(st, function(s) as.numeric(s$N), numeric(1))
  if (any(n <= 3))
    out <- c(out, if (all(n <= 3)) "Every group has three or fewer values, too few for any value to stand out within its group."
                  else sprintf("%s %s three or fewer values, too few for any value to stand out within %s.",
                               cap1(group_names(group, nm[n <= 3])), pl(sum(n <= 3), "has", "have"), pl(sum(n <= 3), "it", "them")))
  if (any(n < 5))
    out <- c(out, "Groups with fewer than five values give only a rough box.")
  paste(c(out, "These are descriptions of the observations; whether the groups really differ is for the analysis of variance to judge."),
        collapse = " ")
}

## the summary table as HTML, numbers right-aligned, with each variable's
## figures to a sensible number of decimals (those of its groups, when the
## table is split by `group`)
describe_html <- function(tab, d, group = NULL) {
  g <- if (!is.null(group) && nzchar(group)) group_labels(d, group)
  digits <- vapply(tab$Variable, function(v) var_digits(response_values(d[[v]]), g), integer(1))
  num <- c("Mean", "SD", "Min", "Q1", "Median", "Q3", "Max")
  body <- lapply(seq_len(nrow(tab)), function(i) c(
    esc(tab$Variable[i]),
    if ("Group" %in% names(tab)) esc(tab$Group[i]),
    as.character(tab$N[i]), as.character(tab$Missing[i]),
    vapply(num, function(k) dfmt(tab[[k]][i], digits[i]), ""),
    fmt(tab$CV[i], 1), dfmt(tab$Skewness[i], 2)))
  head_ <- c("Variable", if ("Group" %in% names(tab)) "Group", "N", "Missing", num, "CV (%)", "Skewness")
  raw_table(head_, body, caption = "Descriptive statistics")
}

## Columns offered for grouping: two or more labels, each used about twice
## on average at least (an identifier or a measurement has a new value in
## nearly every row; a grouping repeats its labels). So a column of numbers
## such as 0, 40, 80, 120 kg N is offered, a measured yield with the odd tie
## is not.
group_choices <- function(d) {
  names(d)[vapply(d, function(z) {
    g <- trimws(as.character(z)); g <- g[!is.na(g) & g != ""]
    k <- length(unique(g))
    k >= 2 && k <= length(g) / 2
  }, logical(1))]
}

## Column names that usually label rows rather than measure anything (Rep,
## Block, Plot No, S. No.), however they are spelt or punctuated, left out of
## the default selection of variables.
ID_KEYS <- c("rep", "reps", "replication", "replicate", "replicationno", "repno", "r",
             "block", "blocks", "blockno", "blk", "b", "row", "rowno", "col", "column",
             "colno", "columnno", "plot", "plotno", "plotnumber", "sno", "slno", "srno",
             "serialno", "id", "no", "number")
is_id_name <- function(nm) gsub("[^a-z]", "", tolower(nm)) %in% ID_KEYS
