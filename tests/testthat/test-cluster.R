## Cluster analysis: the tree against hclust(), the silhouette widths against
## their definition, the readings against the tables as printed, the plots
## against what their notes say, and the Explore tab.
##
## Expectations come from base R and from the definitions:
##   * the items are the rows with every value, or the means of each level of
##     a grouping column over those rows;
##   * each variable is standardised over the items unless asked otherwise;
##   * the suggested number of clusters is the first with the largest average
##     silhouette width from 2 to 10 (or one fewer than the items);
##   * clusters are numbered as they appear in the tree from left to right.

cl_df <- function(seed = 1) {
  set.seed(seed)
  d <- data.frame(Variety = rep(paste0("V", 1:12), each = 3))
  base <- rep(c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3), each = 3)
  d$Yield <- c(30, 40, 36)[base] + rnorm(36, 0, 1.5)
  d$Height <- c(80, 96, 88)[base] + rnorm(36, 0, 2)
  d$Protein <- c(12, 10, 11)[base] + rnorm(36, 0, 0.3)
  d$Moisture <- rnorm(36, 14, 0.5)
  d
}
CL_V <- c("Yield", "Height", "Protein", "Moisture")

## ------------------------------------------------- the numbers ----

test_that("the tree is hclust() on the standardised means, numbered from the left", {
  d <- cl_df()
  cl <- cluster_data(d, CL_V, group = "Variety")
  M <- do.call(rbind, lapply(cl$items, function(l) colMeans(d[d$Variety == l, CL_V])))
  expect_equal(unname(cl$X), unname(M))
  h <- hclust(dist(scale(M)), "ward.D2")
  expect_equal(cl$tree$height, h$height)
  expect_identical(cl$tree$order, h$order)
  # the same partition as cutree(), relabelled in the order of the tree
  ct <- cutree(h, cl$k)
  expect_identical(cl$cluster, match(ct, unique(ct[h$order])))
  expect_identical(unique(cl$cluster[h$order]), seq_len(cl$k))
  expect_identical(cl$sizes, tabulate(cl$cluster, cl$k))
  expect_equal(unname(cl$means), unname(do.call(rbind, lapply(seq_len(cl$k), function(c) colMeans(M[cl$cluster == c, , drop = FALSE])))))
  expect_equal(cl$cophenetic, cor(as.vector(dist(scale(M))), as.vector(cophenetic(h))))
  # other linkages and distances, and the variables as measured
  r <- cluster_data(d, CL_V, group = "Variety", linkage = "average", distance = "manhattan", scale = FALSE)
  expect_equal(r$tree$height, hclust(dist(M, "manhattan"), "average")$height)
})

test_that("silhouette widths follow their definition, and the suggestion is the largest average", {
  d <- cl_df()
  cl <- cluster_data(d, CL_V, group = "Variety")
  D <- as.matrix(dist(cl$Z))
  s <- vapply(seq_len(cl$n), function(i) {
    own <- cl$cluster == cl$cluster[i]
    if (sum(own) == 1) return(0)
    a <- sum(D[i, own]) / (sum(own) - 1)
    b <- min(vapply(setdiff(unique(cl$cluster), cl$cluster[i]), function(c) mean(D[i, cl$cluster == c]), 0))
    (b - a) / max(a, b)
  }, 0)
  expect_equal(cl$silhouette, s)
  avg <- vapply(2:10, function(k) mean(cl_silhouette(cutree(cl$tree, k), dist(cl$Z))), 0)
  expect_equal(cl$by_k$Silhouette, avg)
  expect_identical(cl$suggested, (2:10)[which.max(avg)])
  expect_identical(cl$k, cl$suggested)
  # a number chosen by hand
  r <- cluster_data(d, CL_V, group = "Variety", k = 2)
  expect_identical(r$k, 2L); expect_identical(r$suggested, cl$suggested)
})

test_that("rows are clustered one by one when no grouping is chosen", {
  d <- cl_df(); d$Yield[4] <- NA
  d$Height <- as.character(round(d$Height, 1)); d$Height[8] <- "88,4"
  r <- cluster_data(d, CL_V, k = 3)
  expect_identical(r$n, 34L)
  expect_identical(r$items, as.character(setdiff(1:36, c(4, 8))))
  t <- cl_text(r)
  expect_match(t, "2 of the 36 rows are left out because they have no usable value of some variable.", fixed = TRUE)
  expect_match(t, "Of these, 1 has an entry the data check has flagged (such as '88,4')", fixed = TRUE)
  expect_match(t, "The tree joins 34 rows by Ward's method", fixed = TRUE)
})

test_that("a level with no complete row is named, and rows without a label are counted", {
  d <- cl_df(); d$Yield[d$Variety == "V5"] <- NA; d$Variety[1] <- ""
  cl <- cluster_data(d, CL_V, group = "Variety")
  expect_false("V5" %in% cl$items)
  t <- cl_text(cl)
  expect_match(t, "V5 has no row with every value, so it is not clustered.", fixed = TRUE)
  expect_match(t, "or no label in Variety", fixed = TRUE)
})

test_that("what cannot be clustered is refused in plain words", {
  d <- cl_df()
  expect_error(cluster_data(d, CL_V, distance = "manhattan"), "Ward's method needs Euclidean distances", fixed = TRUE)
  expect_error(cluster_data(d, "Yield"), "Choose at least two variables to cluster on.", fixed = TRUE)
  expect_error(cluster_data(d, c("Yield", "Weight")), "'Weight' is not a column of the data.", fixed = TRUE)
  expect_error(cluster_data(d[d$Variety %in% c("V1", "V2"), ], CL_V, group = "Variety"),
               "Only 2 levels of Variety have a value of every variable chosen; clustering needs at least 3.", fixed = TRUE)
  expect_error(cluster_data(d, CL_V, group = "Variety", k = 12),
               "The number of clusters must be from 2 to 11 (one fewer than the 12 levels of Variety).", fixed = TRUE)
  d$Const <- 5
  expect_error(cluster_data(d, c("Yield", "Const"), group = "Variety"), "'Const' has the same value for every level of Variety", fixed = TRUE)
  expect_error(cluster_data(transform(d, Yield = 1, Height = 2), c("Yield", "Height")),
               "'Yield' and 'Height' each have the same value for every row used, so they add nothing to tell the rows apart", fixed = TRUE)
})

## ------------------------------------------------- the readings ----

test_that("each cluster is read from its means as printed, ties named as shared", {
  cl <- cluster_data(cl_df(), CL_V, group = "Variety")
  for (c in seq_len(cl$k)) {
    t <- cl_cluster_text(cl, c)
    pm <- sapply(seq_along(CL_V), function(j) as.numeric(dfmt(cl$means[, j], cl$digits[j])))
    hi <- CL_V[pm[c, ] == apply(pm, 2, max)]
    for (v in hi) expect_match(t, v, fixed = TRUE)
  }
  q <- cl; q$means[1:2, 1] <- c(40.04, 40.03); q$digits[1] <- 1L        # both print as 40.0
  expect_match(cl_cluster_text(q, 1), "Yield (shared)", fixed = TRUE)
})

test_that("the silhouette bands are judged on the printed width", {
  expect_identical(cl_band(0.7049), "a reasonable structure")
  expect_identical(cl_band(0.7051), "a strong structure")
  expect_identical(cl_band(0.5049), "a weak structure that could be artificial")
  expect_identical(cl_band(0.2549), "no substantial structure")
  expect_identical(cl_band(0.2551), "a weak structure that could be artificial")
})

test_that("the reading names the choice of k, doubtful items and the limits of clustering", {
  d <- cl_df()
  cl <- cluster_data(d, CL_V, group = "Variety")
  t <- cl_text(cl)
  expect_match(t, sprintf("%d gives the largest average silhouette width (%s), and the tree is cut there", cl$suggested,
                          fmt(max(cl$by_k$Silhouette), 2)), fixed = TRUE)
  expect_match(t, "gives no test of significance", fixed = TRUE)
  expect_match(t, sprintf("The cophenetic correlation is %s", fmt(cl$cophenetic, 2)), fixed = TRUE)
  r <- cluster_data(d, CL_V, group = "Variety", k = 2)
  expect_match(cl_text(r), sprintf("the tree is cut into 2, as chosen, where the average width is %s", fmt(mean(r$silhouette), 2)), fixed = TRUE)
  neg <- which(as.numeric(fmt(r$silhouette, 2)) < 0)
  if (length(neg)) expect_match(cl_text(r), "negative silhouette width", fixed = TRUE)
  expect_no_match(t, "significant at|p =", perl = TRUE)
})

## ------------------------------------------------- the plots ----

test_that("the dendrogram draws the tree and colours each cluster's branches", {
  cl <- cluster_data(cl_df(), CL_V, group = "Variety")
  dd <- cl_dendro(cl)
  expect_identical(nrow(dd$seg), 3L * (cl$n - 1L))
  expect_equal(sort(unique(dd$seg$yend[dd$seg$y != dd$seg$yend | dd$seg$x != dd$seg$xend])), sort(unique(cl$tree$height)))
  # the joins above the cut join clusters and are grey; below it they take a cluster's colour
  above <- dd$seg$y == dd$seg$yend & dd$seg$y > dd$cut
  expect_true(all(is.na(dd$seg$cluster[above])))
  expect_false(anyNA(dd$seg$cluster[dd$seg$y == dd$seg$yend & dd$seg$y < dd$cut]))
  hs <- sort(cl$tree$height)
  expect_gt(dd$cut, hs[cl$n - cl$k]); expect_lt(dd$cut, hs[cl$n - cl$k + 1])
  expect_identical(dd$leaves$label, cl$items[cl$tree$order])
  expect_match(cl_dendro_text(cl)$reading, sprintf("between joins at heights %s and %s",
                                                   sig_fmt(rev(hs)[cl$k], 3), sig_fmt(rev(hs)[cl$k - 1], 3)), fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(plot_cl_dendro(cl)))
  # many rows: no labels, and the note says so
  set.seed(2); big <- data.frame(a = rnorm(80), b = rnorm(80))
  rb <- cluster_data(big, c("a", "b"), k = 3)
  expect_match(cl_dendro_text(rb)$reading, "With 80 items the labels are left off", fixed = TRUE)
})

test_that("the cluster plot and the silhouette plot draw what their notes say", {
  cl <- cluster_data(cl_df(), CL_V, group = "Variety")
  b <- ggplot2::ggplot_build(plot_cl_pcs(cl))$data
  pc <- prcomp(cl$X, scale. = TRUE)
  expect_equal(abs(b[[3]]$x), abs(unname(pc$x[, 1]))); expect_equal(abs(b[[3]]$y), abs(unname(pc$x[, 2])))
  expect_match(cl_pcs_text(cl)$what, sprintf("These two components carry %s%% of the variation",
                                             fmt(100 * sum(pc$sdev[1:2]^2) / sum(pc$sdev^2), 1)), fixed = TRUE)
  s <- ggplot2::ggplot_build(plot_cl_silhouette(cl))$data
  expect_equal(s[[3]]$y, cl$by_k$Silhouette)
  expect_match(cl_silhouette_text(cl)$reading, sprintf("The highest is at %d clusters", cl$suggested), fixed = TRUE)
  one <- cl; one$sizes[2] <- 1L
  expect_match(cl_pcs_text(one)$reading, "Cluster 2 holds a single item.", fixed = TRUE)
})

test_that("printing gives the clusters and their means", {
  out <- capture.output(print(cluster_data(cl_df(), CL_V, group = "Variety")))
  expect_match(out[1], "clusters of 12 items", fixed = TRUE)
})

## ------------------------------------------------- the app ----

test_that("the cluster tab clusters, reads and draws", {
  skip_on_cran()
  d <- cl_df(); d$Rep <- rep(1:3, 12)
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$flushReact()
    g <- output$clGroupUI$html
    expect_match(g, '<option value="" selected>Each row</option>', fixed = TRUE)    # nothing mapped yet
    session$setInputs(clVars = CL_V, clGroup = "Variety", clScale = TRUE, clLink = "ward", clDist = "manhattan")
    expect_match(output$clKUI$html, "Suggested (", fixed = TRUE)
    expect_match(output$clText$html, "by Ward's method, using Euclidean distances", fixed = TRUE)
    expect_match(output$clMembers$html, "Clusters", fixed = TRUE)
    expect_match(output$clMeans$html, "Cluster means", fixed = TRUE)
    expect_match(output$clDendro$src, "^data:image/png")
    expect_match(output$clPcs$src, "^data:image/png")
    expect_match(output$clSil$src, "^data:image/png")
    expect_match(output$clItems$html, "Each item", fixed = TRUE)
    session$setInputs(clK = "3", clLink = "average")
    expect_identical(cl_res()$k, 3L)
    expect_identical(cl_res()$distance, "manhattan")
    session$setInputs(clVars = "Yield")
    expect_error(output$clText, "Only 'Yield' is chosen; choose at least two variables.")
  })
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$setInputs(design = "CRD", resp = "Yield", treat = "Variety")
    expect_match(output$clGroupUI$html, '<option value="Variety" selected>', fixed = TRUE)
  })
})
