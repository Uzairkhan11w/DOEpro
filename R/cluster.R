###############################################################################
##  CLUSTER ANALYSIS
###############################################################################
## Hierarchical clustering of rows, or of the means of each level of a
## grouping column (varieties by their trait means, as in plant breeding), on
## the chosen numeric columns. As on the other Explore tabs, every sentence is
## one the tables or plots on screen bear out: the number of clusters comes
## from a stated rule (the average silhouette width), clusters are described
## by their means as printed, and nothing is offered as a test, since
## clustering forms clusters even in data with no groups in them.

CL_LINKS <- c(ward = "ward.D2", complete = "complete", average = "average", single = "single")
CL_LINK_NAMES <- c(ward = "Ward's method", complete = "complete linkage", average = "average linkage (UPGMA)",
                   single = "single linkage")
CL_DIST_NAMES <- c(euclidean = "Euclidean", manhattan = "Manhattan")
## the most clusters the silhouette widths are worked out for
CL_KMAX <- 10
## more items than this: the tree is drawn without their labels
CL_LABELS_MAX <- 60
## more items than this would be slow in the browser
CL_ITEMS_MAX <- 1000

#' Hierarchical cluster analysis
#'
#' Clusters the rows, or the means of each level of a grouping column, on the
#' chosen numeric columns (\code{\link[stats]{hclust}}), from the rows with a
#' value of every one. By default each variable is standardised first, so no
#' variable dominates through its units. Text entries are read as the data
#' check reads them; see \code{\link{check_data}}.
#'
#' The number of clusters, unless given, is the one from 2 to 10 (or one
#' fewer than the items) with the largest average silhouette width (Rousseeuw,
#' 1987): for each item, how much closer it lies to the other items of its own
#' cluster than to those of the nearest other cluster, from -1 to 1. The
#' cophenetic correlation measures how faithfully the tree keeps the
#' distances between the items. Clusters are numbered in the order they
#' appear in the tree from left to right.
#'
#' @param d A data frame.
#' @param vars The names of two or more numeric columns.
#' @param group Optional: the name of a column whose levels are clustered by
#'   their means, instead of clustering the rows.
#' @param k The number of clusters, or \code{NULL} for the suggested number.
#' @param distance \code{"euclidean"} or \code{"manhattan"}.
#' @param linkage \code{"ward"} (Ward's method, which needs Euclidean
#'   distances), \code{"complete"}, \code{"average"} or \code{"single"}.
#' @param scale \code{TRUE} (the default) to standardise each variable.
#'
#' @return A list of class \code{doepro_cluster}: \code{items} (the labels of
#'   the items clustered), \code{X} (their values), \code{cluster} (each
#'   item's cluster), \code{sizes}, \code{means} (each cluster's mean of each
#'   variable), \code{silhouette} (each item's width), \code{by_k} (the
#'   average silhouette width for each number of clusters tried),
#'   \code{suggested}, \code{k}, \code{cophenetic}, \code{tree} (the
#'   \code{hclust} result), and the settings.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(Variety = rep(paste0("V", 1:9), each = 3))
#' d$Yield <- rep(c(30, 31, 29, 40, 42, 41, 35, 36, 34), each = 3) + rnorm(27)
#' d$Height <- rep(c(80, 82, 79, 95, 97, 96, 88, 87, 90), each = 3) + rnorm(27)
#' cl <- cluster_data(d, c("Yield", "Height"), group = "Variety")
#' cl$k
#' cl$cluster
#'
#' @export
cluster_data <- function(d, vars, group = NULL, k = NULL, distance = c("euclidean", "manhattan"),
                         linkage = c("ward", "complete", "average", "single"), scale = TRUE) {
  distance <- match.arg(distance); linkage <- match.arg(linkage)
  if (linkage == "ward" && distance != "euclidean")
    stop("Ward's method needs Euclidean distances; choose Euclidean, or another linkage.")
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  if (length(vars) < 2) stop("Choose at least two variables to cluster on.")
  if (!is.null(group) && (is.na(group) || !nzchar(group))) group <- NULL
  gone <- setdiff(c(vars, group), names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(sprintf("'%s'", gone)), pl(length(gone), "is not a column", "are not columns")))
  if (!is.null(group) && group %in% vars) stop(sprintf("'%s' is the grouping column, so it cannot also be a variable.", group))
  vals <- lapply(stats::setNames(vars, vars), function(v) response_values(d[[v]]))
  held <- lapply(stats::setNames(vars, vars), function(v) held_flags(d[[v]]))
  ok <- Reduce(`&`, lapply(vals, is.finite))
  would <- Reduce(`&`, Map(function(v, h) is.finite(v) | h, vals, held))
  held_rows <- which(would & !ok)
  held_eg <- if (length(held_rows)) {
    i <- held_rows[1]; v <- vars[vapply(held, function(h) h[i], logical(1))][1]
    trimws(as.character(d[[v]][i]))
  } else ""
  lab <- if (!is.null(group)) trimws(as.character(d[[group]])) else NULL
  unlabelled <- if (!is.null(group)) sum(ok & (is.na(lab) | lab == "")) else 0L
  if (!is.null(group)) ok <- ok & !is.na(lab) & lab != ""
  R <- do.call(cbind, lapply(vals, function(v) v[ok])); colnames(R) <- vars
  if (is.null(group)) {
    X <- R; items <- as.character(row_ids(d)[ok]); dropped <- character(0); per <- rep(1L, nrow(X))
  } else {
    g <- lab[ok]
    lv_all <- natural_levels(lab[!is.na(lab) & lab != ""])
    items <- intersect(lv_all, unique(g))
    dropped <- setdiff(lv_all, items)
    X <- do.call(rbind, lapply(items, function(l) colMeans(R[g == l, , drop = FALSE])))
    colnames(X) <- vars
    per <- as.integer(table(factor(g, levels = items)))
  }
  n <- nrow(X)
  what <- if (is.null(group)) "rows" else sprintf("levels of %s", group)
  if (n < 3)
    stop(sprintf("Only %d %s %s a value of every variable chosen; clustering needs at least 3.", n,
                 if (is.null(group)) pl(n, "row", "rows") else pl(n, sprintf("level of %s", group), sprintf("levels of %s", group)),
                 pl(n, "has", "have")))
  if (n > CL_ITEMS_MAX)
    stop(sprintf(paste("Clustering %d rows one by one would be slow in the browser; cluster the means of a grouping column",
                       "instead, or choose at most %d rows."), n, CL_ITEMS_MAX))
  flat <- vars[apply(X, 2, function(z) length(unique(z)) < 2)]
  if (length(flat))
    stop(sprintf("%s %s the same value for every %s, so %s nothing to tell the %s apart; leave %s out.",
                 join_and(sprintf("'%s'", flat)), pl(length(flat), "has", "each have"),
                 if (is.null(group)) "row used" else sprintf("level of %s", group),
                 pl(length(flat), "it adds", "they add"), if (is.null(group)) "rows" else sprintf("levels of %s", group),
                 pl(length(flat), "it", "them")))
  Z <- if (scale) base::scale(X) else X
  D <- stats::dist(Z, method = distance)
  if (all(D == 0)) stop(sprintf("Every one of the %s has the same values, so there is nothing to cluster.", what))
  tree <- stats::hclust(D, method = CL_LINKS[[linkage]])
  ## the average silhouette width for each number of clusters tried; the
  ## suggestion is the first of the largest
  K <- min(CL_KMAX, n - 1L)
  M <- as.matrix(D)
  by_k <- vapply(2:K, function(kk) mean(cl_silhouette(stats::cutree(tree, kk), M)), 0)
  suggested <- (2:K)[which.max(by_k)]
  if (is.null(k) || length(k) != 1 || is.na(k)) k <- suggested
  k <- as.integer(k)
  if (k < 2 || k > n - 1)
    stop(sprintf("The number of clusters must be from 2 to %d (one fewer than the %d %s).", n - 1L, n, what))
  ## clusters numbered in the order they appear in the tree, left to right
  c0 <- stats::cutree(tree, k)
  cl <- match(c0, unique(c0[tree$order]))
  sil <- cl_silhouette(cl, M)
  means <- do.call(rbind, lapply(seq_len(k), function(c) colMeans(X[cl == c, , drop = FALSE])))
  rownames(means) <- paste("Cluster", seq_len(k))
  inf_rows <- sum(!Reduce(`&`, lapply(vals, is.finite)) & Reduce(`|`, lapply(vals, is.infinite)))
  structure(list(vars = vars, group = group, items = items, X = X, Z = Z, per = per, n = n, k = k, suggested = suggested,
                 by_k = data.frame(k = 2:K, Silhouette = by_k), cluster = cl, sizes = tabulate(cl, k), means = means,
                 silhouette = sil, cophenetic = stats::cor(as.vector(D), as.vector(stats::cophenetic(tree))),
                 tree = tree, distance = distance, linkage = linkage, scale = scale, dropped = dropped,
                 unlabelled = unlabelled, rows_used = sum(ok), nrow = nrow(d), held_rows = length(held_rows),
                 held_eg = held_eg, inf_rows = inf_rows, digits = vapply(vars, function(v) var_digits(vals[[v]][ok]), 1L)),
            class = "doepro_cluster")
}

#' @rdname cluster_data
#' @param x A result of \code{cluster_data()}.
#' @param ... Not used.
#' @export
print.doepro_cluster <- function(x, ...) {
  cat(sprintf("%d clusters of %d items (suggested: %d); cophenetic correlation %.3f\n\n", x$k, x$n, x$suggested, x$cophenetic))
  print(data.frame(Item = x$items, Cluster = x$cluster, Silhouette = round(x$silhouette, 3)), row.names = FALSE)
  cat("\n")
  print(x$means)
  invisible(x)
}

## Each item's silhouette width (Rousseeuw, 1987): a is its mean distance to
## the other items of its own cluster, b the smallest mean distance to the
## items of another cluster, and the width (b - a) / max(a, b); an item alone
## in its cluster has 0. Worked out for all items at once, from the
## distances as a "dist" object or a full matrix.
cl_silhouette <- function(cl, D) {
  M <- if (inherits(D, "dist")) as.matrix(D) else D
  n <- nrow(M); k <- max(cl)
  ind <- outer(cl, seq_len(k), "==") + 0
  S <- M %*% ind
  sz <- colSums(ind)
  a <- S[cbind(seq_len(n), cl)] / pmax(sz[cl] - 1, 1)
  other <- sweep(S, 2, sz, "/")
  other[cbind(seq_len(n), cl)] <- Inf
  b <- apply(other, 1, min)
  s <- (b - a) / pmax(a, b)
  s[sz[cl] == 1 | pmax(a, b) == 0] <- 0
  unname(s)
}

## Kaufman and Rousseeuw's (1990) reading of an average silhouette width,
## judged on the value as printed
cl_band <- function(w) {
  v <- as.numeric(fmt(w, 2))
  if (v > 0.70) "a strong structure" else if (v > 0.50) "a reasonable structure"
  else if (v > 0.25) "a weak structure that could be artificial" else "no substantial structure"
}

## how the items are named in the text
cl_item_word <- function(cl, n) {
  if (is.null(cl$group)) pl(n, "row", "rows") else pl(n, sprintf("level of %s", cl$group), sprintf("levels of %s", cl$group))
}
cl_item_names <- function(cl, i, most = 12) {
  nm <- if (is.null(cl$group)) paste("row", cl$items[i]) else cl$items[i]
  if (is.null(cl$group) && length(i) > 1) nm <- c(paste("rows", cl$items[i[1]]), cl$items[i[-1]])
  join_and(head_more(nm, most))
}

## ------------------------------------------------------------ tables ----

cl_members_html <- function(cl) {
  body <- lapply(seq_len(cl$k), function(c) {
    i <- which(cl$cluster == c)
    c(as.character(c), as.character(length(i)), esc(cl_item_names(cl, i, 20)),
      fmt(mean(cl$silhouette[i]), 2))
  })
  foot <- sprintf("<tr><td colspan='4' class='cdrow'>%s</td></tr>", esc(paste(
    "Silhouette: the average, over the cluster's items, of how much closer each lies to its own cluster than to the",
    "nearest other (1 well apart, 0 on a boundary, below 0 closer to another cluster). Clusters are numbered as they",
    "appear in the tree from left to right; the numbers are only labels.")))
  raw_table(c("Cluster", "Size", "Members", "Silhouette"), body, foot = foot, caption = "Clusters")
}

cl_means_html <- function(cl) {
  body <- lapply(seq_along(cl$vars), function(j) c(esc(cl$vars[j]),
    vapply(seq_len(cl$k), function(c) dfmt(cl$means[c, j], cl$digits[j]), ""),
    dfmt(mean(cl$X[, j]), cl$digits[j])))
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", cl$k + 2, esc(paste0(
    "The mean of each variable in each cluster, in the units of the data",
    if (!is.null(cl$group)) sprintf(", worked out from the means of the levels of %s in the cluster", cl$group) else "",
    ". All: the mean over every ", cl_item_word(cl, 1), " clustered.")))
  raw_table(c("Variable", paste("Cluster", seq_len(cl$k)), "All"), body, foot = foot, caption = "Cluster means")
}

cl_items_html <- function(cl) {
  body <- lapply(seq_len(cl$n), function(i) c(esc(cl$items[i]), as.character(cl$cluster[i]), dfmt(cl$silhouette[i], 2),
    if (!is.null(cl$group)) as.character(cl$per[i])))
  raw_table(c(if (is.null(cl$group)) "Row" else esc(cl$group), "Cluster", "Silhouette", if (!is.null(cl$group)) "Rows averaged"),
            body, caption = "Each item's cluster")
}

## ------------------------------------------------------------ readings ----

## a cluster in a sentence: its members, and the variables on which its mean,
## as printed, is the highest or the lowest of the clusters (ties named)
cl_cluster_text <- function(cl, c) {
  i <- which(cl$cluster == c)
  pm <- sapply(seq_along(cl$vars), function(j) as.numeric(dfmt(cl$means[, j], cl$digits[j])))
  pm <- matrix(pm, nrow = cl$k)
  hi <- which(pm[c, ] == apply(pm, 2, max) & apply(pm, 2, function(z) length(unique(z)) > 1))
  lo <- which(pm[c, ] == apply(pm, 2, min) & apply(pm, 2, function(z) length(unique(z)) > 1))
  shared <- function(j) vapply(j, function(v) sum(pm[, v] == pm[c, v]) > 1, logical(1))
  part <- function(j, word) sprintf("the %s %s %s", word, pl(length(j), "mean of", "means of"),
                                    join_and(paste0(cl$vars[j], ifelse(shared(j), " (shared)", ""))))
  head_ <- sprintf("Cluster %d (%s: %s)", c, if (length(i) == 1) paste("1", cl_item_word(cl, 1)) else paste(length(i), cl_item_word(cl, 2)),
                   cl_item_names(cl, i))
  body <- if (length(hi) && length(lo)) sprintf("has %s and %s", part(hi, "highest"), part(lo, "lowest"))
          else if (length(hi)) sprintf("has %s", part(hi, "highest"))
          else if (length(lo)) sprintf("has %s", part(lo, "lowest"))
          else "is neither the highest nor the lowest in any variable"
  sprintf("%s %s.", head_, body)
}

## The reading of the clustering as a whole.
cl_text <- function(cl) {
  w <- mean(cl$silhouette)
  out <- sprintf("The tree joins %d %s by %s, using %s distances between their %s values of %d variables.",
                 cl$n, if (is.null(cl$group)) "rows" else sprintf("levels of %s (each the mean of its rows)", cl$group),
                 CL_LINK_NAMES[[cl$linkage]], CL_DIST_NAMES[[cl$distance]],
                 if (cl$scale) "standardised" else "measured", length(cl$vars))
  sk <- cl$by_k$Silhouette[cl$by_k$k == cl$suggested]
  out <- c(out, sprintf("Of %d to %d clusters, %d %s the largest average silhouette width (%s)%s.",
                        min(cl$by_k$k), max(cl$by_k$k), cl$suggested, "gives", fmt(sk, 2),
                        if (cl$k == cl$suggested) ", and the tree is cut there"
                        else sprintf("; the tree is cut into %d, as chosen, where the average width is %s", cl$k, fmt(w, 2))))
  out <- c(out, sprintf("An average width of %s points to %s (Kaufman and Rousseeuw's bands: above 0.70 strong, 0.51 to 0.70 reasonable, 0.26 to 0.50 weak, 0.25 or less none).",
                        fmt(w, 2), cl_band(w)))
  out <- c(out, sprintf(paste("The cophenetic correlation is %s: how faithfully the heights at which the tree joins the items",
                              "keep the distances between them (1 would be perfect)."), fmt(cl$cophenetic, 2)))
  out <- c(out, vapply(seq_len(cl$k), function(c) cl_cluster_text(cl, c), ""))
  neg <- which(as.numeric(fmt(cl$silhouette, 2)) < 0)
  if (length(neg))
    out <- c(out, sprintf("%s %s a negative silhouette width: on average %s closer to the items of another cluster than to %s own, so %s place is doubtful.",
                          cap1(cl_item_names(cl, neg)), pl(length(neg), "has", "have"), pl(length(neg), "it lies", "they lie"),
                          pl(length(neg), "its", "their"), pl(length(neg), "its", "their")))
  out <- c(out, paste("Clustering always forms clusters, even in data with no groups in them, and gives no test of significance:",
                      "the silhouette widths show how clearly these ones separate. The clusters group items alike in these",
                      "variables; they do not show why they are alike."))
  left <- cl$nrow - cl$rows_used
  if (left > 0) {
    out <- c(out, sprintf("%d of the %d rows %s left out because %s no usable value of some variable%s.", left, cl$nrow,
                          pl(left, "is", "are"), pl(left, "it has", "they have"),
                          if (cl$unlabelled) sprintf(" or no label in %s", cl$group) else ""))
    if (cl$held_rows > 0)
      out <- c(out, sprintf("Of these, %d %s an entry the data check has flagged (such as '%s') and will count once corrected on the Data tab.",
                            cl$held_rows, pl(cl$held_rows, "has", "have"), cl$held_eg))
  }
  if (length(cl$dropped))
    out <- c(out, sprintf("%s %s no row with every value, so %s not clustered.", join_and(head_more(cl$dropped, 8)),
                          pl(length(cl$dropped), "has", "have"), pl(length(cl$dropped), "it is", "they are")))
  paste(out, collapse = " ")
}

## ------------------------------------------------------------ plots ----

## The tree as segments: each join a horizontal at its height and a vertical
## up from each of the two things it joins. A segment takes the colour of the
## cluster below it, or none when it joins two clusters.
cl_dendro <- function(cl) {
  h <- cl$tree; n <- length(h$order)
  xpos <- numeric(n); xpos[h$order] <- seq_len(n)
  nx <- numeric(n - 1); memb <- vector("list", n - 1)
  pos <- function(j) if (j < 0) xpos[-j] else nx[j]
  hgt <- function(j) if (j < 0) 0 else h$height[j]
  mem <- function(j) if (j < 0) -j else memb[[j]]
  colour <- function(m) { u <- unique(cl$cluster[m]); if (length(u) == 1) u else NA_integer_ }
  seg <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    a <- h$merge[i, 1]; b <- h$merge[i, 2]
    nx[i] <- (pos(a) + pos(b)) / 2; memb[[i]] <- c(mem(a), mem(b))
    seg[[i]] <- data.frame(x = c(pos(a), pos(b), pos(a)), y = c(hgt(a), hgt(b), h$height[i]),
                           xend = c(pos(a), pos(b), pos(b)), yend = c(h$height[i], h$height[i], h$height[i]),
                           cluster = c(colour(mem(a)), colour(mem(b)), colour(memb[[i]])))
  }
  out <- do.call(rbind, seg)
  out$cluster <- factor(out$cluster, levels = seq_len(cl$k))
  list(seg = out, leaves = data.frame(x = seq_len(n), label = cl$items[h$order], cluster = factor(cl$cluster[h$order], levels = seq_len(cl$k))),
       cut = mean(sort(h$height)[c(n - cl$k, n - cl$k + 1)]))
}

plot_cl_dendro <- function(cl) {
  dd <- cl_dendro(cl)
  g <- ggplot() +
    geom_segment(data = dd$seg, aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend, colour = .data$cluster),
                 linewidth = 0.7, na.rm = TRUE) +
    geom_hline(yintercept = dd$cut, linetype = 2, colour = "grey45") +
    scale_colour_discrete(breaks = as.character(seq_len(cl$k)), labels = paste("Cluster", seq_len(cl$k)),
                          na.value = "grey55", drop = FALSE) +
    labs(title = "Dendrogram", subtitle = sprintf("%s, %s distances", cap1(CL_LINK_NAMES[[cl$linkage]]), CL_DIST_NAMES[[cl$distance]]),
         x = NULL, y = "Height", colour = NULL,
         caption = sprintf("Dashed line: the cut into %d clusters.", cl$k)) +
    theme_doe() + theme(panel.grid.major.x = element_blank())
  if (cl$n <= CL_LABELS_MAX)
    g <- g + scale_x_continuous(breaks = dd$leaves$x, labels = dd$leaves$label, expand = expansion(add = 0.6)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = if (cl$n > 30) 7 else 9))
  else g <- g + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  g
}

## the items on their first two principal components, coloured by cluster
cl_pcs <- function(cl) {
  pc <- stats::prcomp(cl$X, center = TRUE, scale. = cl$scale)
  list(scores = pc$x[, 1:2, drop = FALSE], share = pc$sdev[1:2]^2 / sum(pc$sdev^2))
}

plot_cl_pcs <- function(cl) {
  pc <- cl_pcs(cl)
  df <- data.frame(x = pc$scores[, 1], y = pc$scores[, 2], cluster = factor(cl$cluster, levels = seq_len(cl$k)), label = cl$items)
  g <- ggplot(df, aes(x = .data$x, y = .data$y, colour = .data$cluster)) +
    geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
    geom_point(size = 2.6) +
    scale_colour_discrete(labels = paste("Cluster", seq_len(cl$k))) +
    labs(title = "Clusters on the first two principal components", colour = NULL,
         x = sprintf("PC1 (%s%%)", fmt(100 * pc$share[1], 1)), y = sprintf("PC2 (%s%%)", fmt(100 * pc$share[2], 1))) +
    theme_doe()
  if (cl$n <= 40) g <- g + geom_text(aes(label = .data$label), vjust = -0.9, size = 3, show.legend = FALSE)
  g
}

plot_cl_silhouette <- function(cl) {
  b <- cl$by_k
  ggplot(b, aes(x = .data$k, y = .data$Silhouette)) +
    geom_hline(yintercept = c(0.25, 0.5, 0.7), linetype = 3, colour = "grey70") +
    geom_line(colour = "#173F7D") + geom_point(colour = "#3B7DD8", size = 2.4) +
    geom_point(data = b[b$k == cl$k, , drop = FALSE], shape = 21, size = 5, colour = "#C0392B", stroke = 1.2) +
    scale_x_continuous(breaks = b$k) +
    labs(title = "Average silhouette width by number of clusters", x = "Number of clusters", y = "Average silhouette width",
         caption = paste(strwrap("Red ring: the number used. Dotted lines: 0.25, 0.50 and 0.70, Kaufman and Rousseeuw's bands.", 55),
                         collapse = "\n")) +
    theme_doe()
}

## ------------------------------------------------------------ plot notes ----

cl_dendro_text <- function(cl) {
  what <- paste("The tree joins the items step by step, the most alike first. The height of each join is how far apart",
                "the two groups it joins are, by the linkage chosen, so a long vertical line below a join means well",
                sprintf("separated groups. The dashed line cuts the tree into %d clusters, coloured.", cl$k))
  hs <- sort(cl$tree$height, decreasing = TRUE)
  reading <- c(sprintf("The cut lies between joins at heights %s and %s.", sig_fmt(hs[cl$k], 3), sig_fmt(hs[cl$k - 1], 3)),
               if (cl$n > CL_LABELS_MAX) sprintf("With %d items the labels are left off; the table of each item's cluster names them.", cl$n))
  list(what = what, reading = paste(reading, collapse = " "))
}

cl_pcs_text <- function(cl) {
  pc <- cl_pcs(cl)
  what <- sprintf(paste("Each point is an item, placed by its scores on the first two principal components of the same",
                        "%s values and coloured by cluster. These two components carry %s%% of the variation, so clusters",
                        "that overlap here may still lie apart on other components."),
                  if (cl$scale) "standardised" else "measured", fmt(100 * sum(pc$share), 1))
  single <- which(cl$sizes == 1)
  reading <- c(if (length(single) == 1) sprintf("Cluster %d holds a single item.", single)
               else if (length(single)) sprintf("Clusters %s each hold a single item.", join_and(single)),
               "If the colours form separate clouds, the clusters are well apart in these variables; if they mix, the split rests on differences these two components do not show.")
  list(what = what, reading = paste(reading, collapse = " "))
}

cl_silhouette_text <- function(cl) {
  what <- paste("For each number of clusters, the average silhouette width: how much closer, on average, each item lies",
                "to its own cluster than to the nearest other (1 well apart, 0 on a boundary, below 0 closer to another",
                "cluster). The highest point is the suggested number of clusters.")
  best <- cl$by_k$Silhouette[cl$by_k$k == cl$suggested]
  reading <- sprintf("The highest is at %d clusters (%s)%s.", cl$suggested, fmt(best, 2),
                     if (cl$k == cl$suggested) ", the number used"
                     else sprintf("; %d are used, as chosen (%s)", cl$k, fmt(mean(cl$silhouette), 2)))
  list(what = what, reading = reading)
}
