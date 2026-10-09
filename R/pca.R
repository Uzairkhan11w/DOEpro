###############################################################################
##  PRINCIPAL COMPONENT ANALYSIS
###############################################################################
## Principal components of the chosen numeric columns, from the rows that
## have every value: the eigenvalues with the share of the variation each
## component carries, the loadings (each variable's correlation with each
## component), the scores, and the scree, score, loading and biplot plots.
## As on the other Explore tabs, every sentence is one the tables or plots on
## screen bear out: loadings are judged on their values as printed, the
## number of components worth keeping comes from stated rules (Kaiser's and
## Horn's parallel analysis), and nothing is said about what a component
## "means" beyond which variables rise and fall with it.

## random data sets for Horn's parallel analysis: fewer when each set is
## large (whose eigenvalues vary little from set to set), so the analysis
## stays quick in the browser
PCA_PA_B <- 1000
PCA_PA_MIN <- 100
PCA_PA_CELLS <- 1e8
## a loading of this size or more, as printed, counts as strong
PCA_STRONG <- 0.5

#' Principal component analysis of numeric variables
#'
#' Principal components of the chosen columns, from the rows with a value of
#' every one (\code{\link[stats]{prcomp}}). By default each variable is
#' standardised first, so the analysis works on their correlations and no
#' variable dominates through its units; with \code{scale = FALSE} it works
#' on their covariances, which suits variables measured in the same units.
#' Text entries are read as the data check reads them: an entry that is not
#' plainly a number counts as missing; see \code{\link{check_data}}.
#'
#' Each component's sign is arbitrary (reversing it changes nothing); here it
#' is chosen so that the variable with the largest loading on it loads
#' positively. The loadings are the correlations between each variable and
#' each component. Components are kept by Horn's parallel analysis: those,
#' from the first, whose eigenvalue exceeds the 95th percentile of the
#' eigenvalues of random normal data of the same size (with the same variances
#' for a covariance analysis), simulated with a fixed seed. Kaiser's rule
#' (an eigenvalue above the average, which is 1 for correlations) is shown
#' beside it.
#'
#' @param d A data frame.
#' @param vars The names of two or more numeric columns.
#' @param scale \code{TRUE} (the default) to standardise each variable,
#'   \code{FALSE} to analyse them as measured.
#'
#' @return A list of class \code{doepro_pca}: \code{eigen} (a data frame:
#'   \code{Component}, \code{Eigenvalue}, \code{Proportion},
#'   \code{Cumulative}, \code{PA_threshold}), \code{loadings} (variables by
#'   components, correlations), \code{vectors} (the eigenvectors),
#'   \code{scores} (rows by components), \code{rows} (the rows used),
#'   \code{kaiser} and \code{retained} (the number of components each rule
#'   keeps), and the settings.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(Height = rnorm(30, 100, 10))
#' d$Tillers <- 0.1 * d$Height + rnorm(30)
#' d$Yield <- 0.3 * d$Height + 0.5 * d$Tillers + rnorm(30, 0, 3)
#' d$Protein <- rnorm(30, 12, 1)
#' p <- pca_data(d, c("Height", "Tillers", "Yield", "Protein"))
#' p$eigen
#' round(p$loadings, 2)
#'
#' @export
pca_data <- function(d, vars, scale = TRUE) {
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  if (length(vars) < 2) stop("Choose at least two variables for a principal component analysis.")
  gone <- setdiff(vars, names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(sprintf("'%s'", gone)), pl(length(gone), "is not a column", "are not columns")))
  p <- length(vars)
  vals <- lapply(stats::setNames(vars, vars), function(v) response_values(d[[v]]))
  held <- lapply(stats::setNames(vars, vars), function(v) held_flags(d[[v]]))
  ok <- Reduce(`&`, lapply(vals, is.finite))
  would <- Reduce(`&`, Map(function(v, h) is.finite(v) | h, vals, held))
  held_rows <- which(would & !ok)
  held_eg <- if (length(held_rows)) {
    i <- held_rows[1]; v <- vars[vapply(held, function(h) h[i], logical(1))][1]
    trimws(as.character(d[[v]][i]))
  } else ""
  n <- sum(ok)
  if (n < 3)
    stop(sprintf("Only %d %s a value of every variable chosen; principal components need at least 3.%s",
                 n, pl(n, "row has", "rows have"),
                 if (length(held_rows)) sprintf(" %d more %s an entry the data check has flagged (such as '%s'), which will count once corrected on the Data tab.",
                                                length(held_rows), pl(length(held_rows), "row has", "rows have"), held_eg) else ""))
  X <- do.call(cbind, lapply(vals, function(v) v[ok]))
  colnames(X) <- vars
  flat <- vars[apply(X, 2, function(z) length(unique(z)) < 2)]
  if (length(flat))
    stop(sprintf("%s %s the same value in every row used, so %s nothing to the components; leave %s out.",
                 join_and(sprintf("'%s'", flat)), pl(length(flat), "has", "each have"),
                 pl(length(flat), "it adds", "they add"), pl(length(flat), "it", "them")))

  pc <- stats::prcomp(X, center = TRUE, scale. = scale)
  ev <- pc$sdev^2
  ## components with no variance (more variables than rows, or a variable
  ## that is an exact combination of others) are left out
  m <- sum(ev > 1e-10 * sum(ev))
  ev_all <- ev
  V <- pc$rotation[, seq_len(m), drop = FALSE]
  S <- pc$x[, seq_len(m), drop = FALSE]
  sdv <- apply(X, 2, stats::sd)
  L <- sweep(V, 2, sqrt(ev[seq_len(m)]), "*")
  if (!scale) L <- sweep(L, 1, sdv, "/")
  ## each component's sign chosen so its largest loading is positive
  for (k in seq_len(m)) {
    if (L[which.max(abs(round(L[, k], 10))), k] < 0) { V[, k] <- -V[, k]; S[, k] <- -S[, k]; L[, k] <- -L[, k] }
  }
  comp <- paste0("PC", seq_len(m))
  colnames(V) <- colnames(S) <- colnames(L) <- comp
  rownames(V) <- rownames(L) <- vars

  ## Horn's parallel analysis: eigenvalues of random normal data of the same
  ## size (with the same variances for a covariance analysis), 95th
  ## percentile at each position
  B <- as.integer(max(PCA_PA_MIN, min(PCA_PA_B, floor(PCA_PA_CELLS / (n * p^2)))))
  pa <- with_fixed_seed(COR_PERM_SEED + 11, {
    sims <- vapply(seq_len(B), function(b) {
      Z <- matrix(stats::rnorm(n * p), n)
      if (!scale) Z <- sweep(Z, 2, sdv, "*")
      eigen(if (scale) stats::cor(Z) else stats::cov(Z), symmetric = TRUE, only.values = TRUE)$values
    }, numeric(p))
    apply(matrix(sims, nrow = p), 1, stats::quantile, probs = 0.95, names = FALSE)
  })
  above <- ev[seq_len(m)] > pa[seq_len(m)]
  retained <- if (all(above)) m else which(!above)[1] - 1L
  avg <- sum(ev_all) / p
  kaiser <- sum(ev[seq_len(m)] > avg)

  eig <- data.frame(Component = comp, Eigenvalue = ev[seq_len(m)], Proportion = ev[seq_len(m)] / sum(ev_all),
                    Cumulative = cumsum(ev[seq_len(m)]) / sum(ev_all), PA_threshold = pa[seq_len(m)],
                    stringsAsFactors = FALSE)
  inf_rows <- sum(!ok & Reduce(`|`, lapply(vals, is.infinite)))
  structure(list(vars = vars, scale = scale, n = n, p = p, m = m, X = X, eigen = eig, loadings = L, vectors = V,
                 scores = S, sdev = sqrt(ev[seq_len(m)]), average = avg, kaiser = kaiser, retained = retained,
                 pa_sets = B, rows = row_ids(d)[ok], pos = which(ok), nrow = nrow(d), held_rows = length(held_rows), held_eg = held_eg,
                 inf_rows = inf_rows, zero = p - m),
            class = "doepro_pca")
}

#' @rdname pca_data
#' @param x A result of \code{pca_data()}.
#' @param ... Not used.
#' @export
print.doepro_pca <- function(x, ...) {
  print(x$eigen, row.names = FALSE)
  cat("\nLoadings (correlations with the components):\n")
  print(round(x$loadings, 3))
  invisible(x)
}

## ------------------------------------------------------------ tables ----

## how many components the tables and readings go through: those kept, and
## at least the first two
pca_shown <- function(p) min(p$m, max(2L, p$retained))

pca_eigen_html <- function(p) {
  e <- p$eigen
  body <- lapply(seq_len(nrow(e)), function(i) c(
    e$Component[i], fmt(e$Eigenvalue[i], 3), fmt(100 * e$Proportion[i], 1), fmt(100 * e$Cumulative[i], 1),
    fmt(e$PA_threshold[i], 3),
    if (i <= p$retained) "kept" else ""))
  notes <- c(
    sprintf(paste("Eigenvalue: the variance carried by the component%s; the eigenvalues add up to %s, the total",
                  "variance of %s."), if (p$scale) " (each variable standardised, so each counts 1)" else "",
            fmt(sum(e$Eigenvalue) + 0, 3), if (p$scale) "the standardised variables" else "the variables as measured"),
    sprintf(paste("Parallel analysis: the 95th percentile of the same eigenvalue in %s sets of random normal data of",
                  "this size%s; a component is kept while its eigenvalue is larger."),
            formatC(p$pa_sets, format = "d", big.mark = ","), if (p$scale) "" else " with the same variances"),
    if (p$zero) sprintf("%d more %s an eigenvalue of 0 and %s left out: %s.", p$zero, pl(p$zero, "component has", "components have"),
                        pl(p$zero, "is", "are"),
                        if (p$n <= p$p) "there are no more rows than variables" else "some variable can be worked out exactly from the others"))
  foot <- sprintf("<tr><td colspan='6' class='cdrow'>%s</td></tr>", esc(paste(notes, collapse = " ")))
  raw_table(c("Component", "Eigenvalue", "% of variance", "Cumulative %", "Parallel analysis", ""), body, foot = foot,
            caption = "Eigenvalues")
}

pca_loadings_html <- function(p) {
  k <- pca_shown(p); L <- p$loadings[, seq_len(k), drop = FALSE]
  body <- lapply(seq_len(nrow(L)), function(i) c(esc(rownames(L)[i]), vapply(seq_len(k), function(j) {
    s <- dfmt(L[i, j], 2)
    if (abs(as.numeric(s)) >= PCA_STRONG) paste0("<b>", s, "</b>") else s
  }, ""), fmt(sum(L[i, ]^2), 2)))
  notes <- c(sprintf(paste("Each loading is the correlation between the variable and the component. In bold: %s or more",
                           "either way, taken here as strong."), dfmt(PCA_STRONG, 2)),
             sprintf("Communality: the share of each variable's variance that %s carry together.",
                     if (k == 2) "these two components" else sprintf("these %d components", k)),
             "A component's sign is arbitrary: reversing every loading and score of a component changes nothing.")
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", k + 2, esc(paste(notes, collapse = " ")))
  raw_table(c("Variable", colnames(L), "Communality"), body, foot = foot, caption = "Loadings")
}

pca_scores_html <- function(p, show = 10) {
  k <- pca_shown(p); S <- p$scores[, seq_len(k), drop = FALSE]
  i <- seq_len(min(show, nrow(S)))
  body <- lapply(i, function(r) c(as.character(p$rows[r]), vapply(seq_len(k), function(j) dfmt(S[r, j], 2), "")))
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", k + 1,
                  esc(paste0("A row's score is its position along the component: 0 is the average row.",
                             if (nrow(S) > show) sprintf(" The first %d of %d rows; the CSV holds them all.", show, nrow(S)) else "")))
  raw_table(c("Row", colnames(S)), body, foot = foot, caption = "Scores")
}

## ------------------------------------------------------------ readings ----

## the strong loadings of a component, judged on their printed values,
## largest first
pca_strong <- function(p, k) {
  l <- p$loadings[, k]; pr <- as.numeric(dfmt(l, 2))
  i <- which(abs(pr) >= PCA_STRONG)
  i[order(-abs(pr[i]))]
}

## one component in a sentence: what share it carries and which variables
## rise and fall with it
pca_component_text <- function(p, k) {
  nm <- p$vars; l <- p$loadings[, k]
  i <- pca_strong(p, k)
  head_ <- sprintf("PC%d (%s%% of the variation)", k, fmt(100 * p$eigen$Proportion[k], 1))
  if (!length(i))
    return(sprintf("%s: no variable is strongly related to it (every loading lies between %s and %s).", head_,
                   dfmt(-PCA_STRONG, 2), dfmt(PCA_STRONG, 2)))
  pos <- i[l[i] > 0]; neg <- i[l[i] < 0]
  part <- function(j, word) sprintf("%s %s (%s %s)", word, join_and(nm[j]), pl(length(j), "loading", "loadings"),
                                    join_and(dfmt(l[j], 2)))
  paste0(if (length(pos) && length(neg))
           sprintf("%s: rows with high scores have %s and %s; it contrasts %s with %s.", head_, part(pos, "high"), part(neg, "low"),
                   join_and(nm[pos]), join_and(nm[neg]))
         else sprintf("%s: rows with high scores have %s.", head_, part(if (length(pos)) pos else neg, if (length(pos)) "high" else "low")),
         if (k > p$retained) " Parallel analysis does not keep this component, so the pattern may be no more than chance." else "")
}

## The reading of the analysis as a whole.
pca_text <- function(p) {
  e <- p$eigen; k2 <- min(2, p$m)
  out <- sprintf("The analysis uses %d %s and %d variables, %s.", p$n, pl(p$n, "row", "rows"), p$p,
                 if (p$scale) "each standardised, so they count equally whatever their units"
                 else "as measured, so a variable with a larger variance counts for more")
  out <- c(out, sprintf("The first component carries %s%% of the variation%s.", fmt(100 * e$Proportion[1], 1),
                        if (p$m >= 2) sprintf(", and the first two together %s%%", fmt(100 * e$Cumulative[2], 1)) else ""))
  avg <- if (p$scale) "1" else sprintf("%s, the average", fmt(p$average, 3))
  out <- c(out, if (p$retained == 0)
    sprintf(paste("No component has a larger eigenvalue than random data of this size would give (parallel analysis),",
                  "so none stands out from chance; the plots still show the first two."))
    else sprintf("Parallel analysis keeps %s: %s eigenvalue%s larger than random data of this size would give.",
                 if (p$retained == 1) "the first component" else sprintf("the first %d components", p$retained),
                 if (p$retained == 1) "its" else "their", if (p$retained == 1) " is" else "s are"))
  out <- c(out, sprintf("%s keeps %d (%s above %s)%s.", if (p$scale) "Kaiser's rule" else "The rule of an eigenvalue above the average",
                        p$kaiser, pl(p$kaiser, "an eigenvalue", "eigenvalues"), avg,
                        if (p$kaiser != p$retained)
                          "; the two rules disagree, and parallel analysis, which allows for chance, is generally the more reliable"
                        else ""))
  ## with covariances a variable with a large variance makes a component of
  ## its own, which parallel analysis (same variances, no correlation) expects
  if (!p$scale)
    out <- c(out, paste("With covariances, a variable with a much larger variance than the others makes a component almost",
                        "on its own, whether or not it is related to them; parallel analysis allows for that, so a component",
                        "can carry a large share of the variation and still not stand out from chance."))
  out <- c(out, vapply(seq_len(pca_shown(p)), function(k) pca_component_text(p, k), ""))
  weak <- which(as.numeric(fmt(rowSums(p$loadings[, seq_len(pca_shown(p)), drop = FALSE]^2), 2)) < 0.5)
  if (length(weak))
    out <- c(out, sprintf("%s %s less than half of %s variance carried by these components (communality below 0.50), so %s little part in them.",
                          join_and(p$vars[weak]), pl(length(weak), "has", "have"), pl(length(weak), "its", "their"),
                          pl(length(weak), "it plays", "they play")))
  out <- c(out, paste("A component's sign is arbitrary, so high and low can be swapped throughout. The components",
                      "describe how the variables vary together in these rows; they do not show what causes it."))
  left <- p$nrow - p$n
  if (left > 0) {
    out <- c(out, sprintf("%d of the %d rows %s left out because %s no usable value of some variable.", left, p$nrow,
                          pl(left, "is", "are"), pl(left, "it has", "they have")))
    if (p$held_rows > 0)
      out <- c(out, sprintf("Of these, %d %s an entry the data check has flagged (such as '%s') and will count once corrected on the Data tab.",
                            p$held_rows, pl(p$held_rows, "has", "have"), p$held_eg))
  }
  paste(out, collapse = " ")
}

## ------------------------------------------------------------ plots ----

## the components to plot: the two asked for when they exist and differ,
## else the first two
pca_axes <- function(p, a, b) {
  okk <- function(v) length(v) == 1 && is.numeric(v) && !is.na(v) && v >= 1 && v <= p$m
  a <- if (okk(a)) as.integer(a) else 1L
  b <- if (okk(b) && b != a) as.integer(b) else if (a == 1L) min(2L, p$m) else 1L
  c(a, b)
}

plot_pca_scree <- function(p) {
  e <- p$eigen
  df <- data.frame(k = seq_len(nrow(e)), ev = e$Eigenvalue, pa = e$PA_threshold)
  ggplot(df, aes(x = .data$k)) +
    geom_hline(yintercept = p$average, linetype = 3, colour = "grey45") +
    geom_line(aes(y = .data$pa), linetype = 2, colour = "#C0392B") +
    geom_point(aes(y = .data$pa), shape = 4, colour = "#C0392B", size = 2.4) +
    geom_line(aes(y = .data$ev), colour = "#173F7D", linewidth = 0.9) +
    geom_point(aes(y = .data$ev), colour = "#3B7DD8", size = 2.6) +
    scale_x_continuous(breaks = df$k, labels = e$Component) +
    labs(title = "Scree plot", x = "Component", y = "Eigenvalue",
         caption = paste(strwrap(sprintf("Blue: eigenvalues. Red crosses: parallel analysis. Dotted line: %s.",
                                         if (p$scale) "an eigenvalue of 1 (Kaiser)" else "the average eigenvalue"), 70),
                         collapse = "\n")) +
    theme_doe()
}

## rows whose score on either plotted component lies more than three of that
## component's standard deviations from 0
pca_far <- function(p, ax) which(apply(abs(sweep(p$scores[, ax, drop = FALSE], 2, p$sdev[ax], "/")) > 3 + 1e-8, 1, any))

plot_pca_scores <- function(p, ax = c(1, 2), group = NULL) {
  df <- data.frame(x = p$scores[, ax[1]], y = p$scores[, ax[2]], row = p$rows)
  if (!is.null(group)) df$group <- group
  far <- df[pca_far(p, ax), , drop = FALSE]
  g <- ggplot(df, aes(x = .data$x, y = .data$y)) +
    geom_hline(yintercept = 0, colour = "grey70") + geom_vline(xintercept = 0, colour = "grey70") +
    (if (is.null(group)) geom_point(colour = "#3B7DD8", size = 2.4, alpha = 0.85)
     else geom_point(aes(colour = .data$group), size = 2.4, alpha = 0.85)) +
    (if (nrow(far)) geom_text(data = far, aes(label = paste("row", .data$row)), vjust = -0.8, size = 3.3, colour = "#C0392B")) +
    labs(title = "Score plot", colour = NULL,
         x = sprintf("PC%d (%s%%)", ax[1], fmt(100 * p$eigen$Proportion[ax[1]], 1)),
         y = sprintf("PC%d (%s%%)", ax[2], fmt(100 * p$eigen$Proportion[ax[2]], 1))) +
    theme_doe()
  g
}

plot_pca_loadings <- function(p, ax = c(1, 2)) {
  df <- data.frame(x = p$loadings[, ax[1]], y = p$loadings[, ax[2]], v = p$vars)
  circ <- data.frame(t = seq(0, 2 * pi, length.out = 200))
  ggplot(df) +
    geom_path(data = circ, aes(x = cos(.data$t), y = sin(.data$t)), colour = "grey70") +
    geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
    geom_segment(aes(x = 0, y = 0, xend = .data$x, yend = .data$y), colour = "#173F7D",
                 arrow = arrow(length = unit(0.18, "cm"))) +
    geom_text(aes(x = .data$x * 1.12, y = .data$y * 1.12, label = .data$v), size = 3.4) +
    coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    labs(title = "Loading plot", caption = "Circle: a correlation of 1 with the two components together.",
         x = sprintf("PC%d", ax[1]), y = sprintf("PC%d", ax[2])) +
    theme_doe()
}

## the factor that stretches the loadings to the scale of the scores
pca_stretch <- function(p, ax) {
  0.8 * max(abs(p$scores[, ax])) / max(sqrt(rowSums(p$loadings[, ax, drop = FALSE]^2)))
}

plot_pca_biplot <- function(p, ax = c(1, 2), group = NULL) {
  f <- pca_stretch(p, ax)
  sc <- data.frame(x = p$scores[, ax[1]], y = p$scores[, ax[2]])
  if (!is.null(group)) sc$group <- group
  ld <- data.frame(x = f * p$loadings[, ax[1]], y = f * p$loadings[, ax[2]], v = p$vars)
  ggplot() +
    geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
    (if (is.null(group)) geom_point(data = sc, aes(x = .data$x, y = .data$y), colour = "#8FB3E8", size = 2)
     else geom_point(data = sc, aes(x = .data$x, y = .data$y, colour = .data$group), size = 2, alpha = 0.8)) +
    geom_segment(data = ld, aes(x = 0, y = 0, xend = .data$x, yend = .data$y), colour = "#C0392B",
                 arrow = arrow(length = unit(0.18, "cm"))) +
    geom_text(data = ld, aes(x = .data$x * 1.1, y = .data$y * 1.1, label = .data$v), size = 3.4, colour = "#7B241C") +
    labs(title = "Biplot", colour = NULL,
         x = sprintf("PC%d (%s%%)", ax[1], fmt(100 * p$eigen$Proportion[ax[1]], 1)),
         y = sprintf("PC%d (%s%%)", ax[2], fmt(100 * p$eigen$Proportion[ax[2]], 1)),
         caption = paste(strwrap(sprintf(paste("Points: the scores. Arrows: the loadings, stretched %s times to the scale",
                                               "of the points; read their directions and relative lengths."), sig_fmt(f, 3)), 70),
                         collapse = "\n")) +
    theme_doe()
}

## ------------------------------------------------------------ plot notes ----

pca_scree_text <- function(p) {
  what <- paste("The eigenvalues in order, each the variance its component carries. The red crosses show what",
                "random data of this size would give; a component whose eigenvalue stands above its cross carries more",
                "than chance alone would.", if (p$scale) "The dotted line marks an eigenvalue of 1, Kaiser's rule."
                else "The dotted line marks the average eigenvalue.")
  above <- which(p$eigen$Eigenvalue > p$eigen$PA_threshold)
  later <- setdiff(above, seq_len(p$retained))
  reading <- paste(c(
    if (p$retained == 0 && !length(above)) "No eigenvalue lies above its cross."
    else if (p$retained == 0) "The first eigenvalue lies below its cross, so parallel analysis keeps no component."
    else if (p$retained == 1) paste0("The first eigenvalue lies above its cross", if (p$m > 1) ", the second below." else ".")
    else paste0(sprintf("The first %d eigenvalues lie above their crosses", p$retained),
                if (p$retained < p$m) ", the next below." else "."),
    if (length(later)) sprintf("%s also %s above %s cross, but parallel analysis stops at the first component that falls short.",
                               join_and(p$eigen$Component[later]), pl(length(later), "lies", "lie"),
                               pl(length(later), "its", "their")),
    "If the line drops steeply and then levels off, the components after the bend (the elbow) add little."), collapse = " ")
  list(what = what, reading = reading)
}

pca_scores_text <- function(p, ax = c(1, 2), group = NULL, gname = NULL) {
  what <- sprintf(paste("Each point is a row, placed by its scores on PC%d (across) and PC%d (up). Rows close together",
                        "have similar values of the variables that load strongly on these components; rows near 0 are",
                        "close to the average row.%s"), ax[1], ax[2],
                  if (!is.null(group)) sprintf(" Colours show %s.", gname) else "")
  far <- pca_far(p, ax)
  out <- c(if (length(far))
             paste0(sprintf("%s %s more than three standard deviations from 0 on PC%d or PC%d; check %s for a recording error, or for a treatment that differs.",
                            cap1(rows_text(p$rows[far])), pl(length(far), "lies", "lie"), ax[1], ax[2], pl(length(far), "it", "them")),
                    if (p$n >= 100) sprintf(" Among %d rows, about %s would lie that far by chance on one or other component even if the scores were normal.",
                                            p$n, fmt(p$n * (1 - (1 - 2 * stats::pnorm(-3))^2), 1)) else "")
           else sprintf("No row lies more than three standard deviations from 0 on PC%d or PC%d.", ax[1], ax[2]))
  if (!is.null(group)) {
    mm <- stats::aggregate(p$scores[, ax], list(g = group), mean)
    ## the highest and lowest average score on each axis, ties named as ties
    txt <- function(col) {
      v <- dfmt(mm[[col + 1]], 2); hi <- which(as.numeric(v) == max(as.numeric(v))); lo <- which(as.numeric(v) == min(as.numeric(v)))
      sprintf("on PC%d %s the highest average score (%s) and %s the lowest (%s)", ax[col],
              paste(join_and(as.character(mm$g[hi])), pl(length(hi), "has", "share")), v[hi[1]],
              paste(join_and(as.character(mm$g[lo])), pl(length(lo), "has", "share")), v[lo[1]])
    }
    out <- c(out, sprintf("Among the levels of %s, %s; %s.", gname, txt(1), txt(2)),
             "If the colours form separate clouds, the groups differ in the variables these components carry; if they overlap, the components do not tell them apart.")
  }
  list(what = what, reading = paste(out, collapse = " "))
}

pca_loadings_text <- function(p, ax = c(1, 2)) {
  what <- sprintf(paste("Each arrow is a variable, ending at its loadings on PC%d (across) and PC%d (up). An arrow near the",
                        "circle is well carried by these two components; a short one is not. Arrows pointing the same way",
                        "belong to variables that rise together; opposite arrows, to variables where one rises as the other",
                        "falls; arrows at right angles, to variables these components show as unrelated."), ax[1], ax[2])
  cm <- rowSums(p$loadings[, ax, drop = FALSE]^2)
  shown <- as.numeric(dfmt(cm, 2))
  weak <- which(shown < 0.5)
  top <- which(shown == max(shown))
  reading <- c(sprintf("The longest %s to %s (%s of %s variance on these two components).",
                       pl(length(top), "arrow belongs", "arrows belong"), join_and(p$vars[top]), dfmt(max(cm), 2),
                       pl(length(top), "its", "each one's")),
               if (length(weak)) sprintf("%s %s less than half of %s variance on these two, so %s %s little.",
                                         join_and(p$vars[weak]), pl(length(weak), "has", "have"), pl(length(weak), "its", "their"),
                                         pl(length(weak), "its arrow", "their arrows"), pl(length(weak), "says", "say")))
  list(what = what, reading = paste(reading, collapse = " "))
}

pca_biplot_text <- function(p, ax = c(1, 2)) {
  what <- sprintf(paste("The score plot and the loading plot together: points are rows, arrows are variables. A row lying",
                        "far out in an arrow's direction tends to have a high value of that variable, and one on the opposite",
                        "side a low value; this is only approximate, as the plot shows two components of %d. The arrows are",
                        "stretched %s times so they can be seen beside the points."), p$m, sig_fmt(pca_stretch(p, ax), 3))
  reading <- sprintf(paste("PC%d and PC%d together carry %s%% of the variation, so the plot shows that share of how the",
                           "rows differ; the rest lies on other components."), ax[1], ax[2],
                     fmt(100 * sum(p$eigen$Proportion[ax]), 1))
  list(what = what, reading = reading)
}
