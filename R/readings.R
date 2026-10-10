###############################################################################
##  READINGS OF THE ANALYSIS OF VARIANCE
###############################################################################
## A reading after each table and plot of the analysis of variance: what the
## table or plot shows, then what it shows for these data. Every sentence can
## be checked against the figures or the plot on screen, at the significance
## level the analysis was run at. A result that is not significant is
## insufficient evidence of a difference, never evidence that there is none,
## and nothing is said about the crop or the treatments that the numbers
## cannot support. The readings are plain text, escaped where they are shown.

## "p = 0.0216" or "p < 0.0001", as the tables print it
rd_p <- function(p, alpha) gsub("&lt;", "<", p_eq(p, alpha), fixed = TRUE)

## "5%"
rd_lvl <- function(alpha) paste0(pct(alpha), "%")

## "significant at the 5% level (p = 0.0216)". A p-value below alpha / 5,
## the level of the double star, is reported at that level.
rd_sig <- function(p, alpha)
  sprintf("%s at the %s level (%s)", if (p < alpha) "significant" else "not significant",
          rd_lvl(if (p < alpha / 5) alpha / 5 else alpha), rd_p(p, alpha))

## "the effect of Nitrogen depends on the level of Variety"; with three or
## more factors, "the interaction of A and B differs between the levels of C"
rd_inter <- function(v) {
  if (length(v) == 2) sprintf("the effect of %s depends on the level of %s", v[1], v[2])
  else sprintf("the interaction of %s differs between the levels of %s", join_and(v[-length(v)]), v[length(v)])
}

## the environment of a pooled analysis
rd_env <- function(res) {
  g <- Filter(function(e) isTRUE(e$is_gxe), res$effects)
  if (length(g)) g[[1]]$env else NA_character_
}

## an effect's means as the tables of means show them: back-transformed when
## the response was transformed
rd_shown <- function(e, tr = "none") {
  m <- e$means
  if (!identical(tr, "none") && "Mean_bt" %in% names(m)) m$Mean_bt else m$Mean
}

## "V2 (48.78) and V5 (48.45)"
rd_named <- function(lab, pr) join_and(head_more(sprintf("%s (%s)", lab, pr), 6))

## A reading as HTML: a heading, then one paragraph or a list of bullets.
rd_html <- function(txt, head = "What the table shows.") {
  txt <- txt[nzchar(txt)]
  if (!length(txt)) return("")
  body <- if (length(txt) == 1) esc(txt) else paste0("<ul>", paste0("<li>", esc(txt), "</li>", collapse = ""), "</ul>")
  sprintf("<div class='box'><b>%s</b> %s</div>", head, body)
}

## A plot's note: what it shows, then what it shows here
rd_note <- function(t) {
  if (is.null(t)) return("")
  sprintf("<div class='note'>%s</div>", if (nzchar(t$what))
    paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading)) else esc(t$reading))
}

## In an exact fit the error mean square is only rounding residue, so every
## F-test, C.D. and letter compares the effects with that residue; the
## readings say so instead of reading the verdicts.
RD_EXACT <- paste("The model fits every value exactly, so the error mean square is zero but for rounding: the F-tests,",
                  "C.D.s and letters compare the effects with that rounding and mean nothing. Check that the values are",
                  "the observations of each plot.")

## two figures to enough decimals that they print differently when they differ
rd_pair <- function(x, y, digits) {
  dg <- digits
  while (dg < digits + 4 && fmt(x, dg) == fmt(y, dg) && x != y) dg <- dg + 1
  c(fmt(x, dg), fmt(y, dg))
}

## ------------------------------------------------------------ ANOVA ----

## The rows of an ANOVA table, by their labels: what each row tests, in the
## terms of the design. `vars` are the factors a treatment row involves.
aov_terms <- function(res) {
  out <- list()
  add <- function(lab, kind, vars = character(0)) out[[lab]] <<- list(kind = kind, vars = vars)
  subsets <- function(f)
    unlist(lapply(seq_along(f), function(k) utils::combn(f, k, simplify = FALSE)), recursive = FALSE)
  des <- res$design
  if (des %in% c("CRD", "RCBD", "LSD", "FCRD", "FRCBD")) {
    for (s in subsets(res$facs)) add(paste(s, collapse = ":"), "treatment", s)
    if (des == "LSD") { add(res$blks[1], "row"); add(res$blks[2], "column") }
    else for (b in res$blks) add(b, "block")
  } else if (des %in% c("SPLIT", "STRIP")) {
    A <- res$facs[1]; B <- res$facs[2]
    add(A, "treatment", A); add(B, "treatment", B); add(paste(A, "x", B), "treatment", c(A, B))
  } else {
    env <- rd_env(res); trt <- setdiff(res$facs, env)
    add(paste0("Environment (", env, ")"), "env", env)
    add("Replication within environment", "rep_env")
    if (des %in% c("POOLRCBD", "POOLCRD")) {
      add(paste0("Treatment (", trt, ")"), "treatment", trt)
      add(paste0(env, " x ", trt), "gxe", trt)
    } else for (s in subsets(trt)) {
      add(paste(s, collapse = " x "), "treatment", s)
      add(paste0(env, " x ", paste(s, collapse = " x ")), "gxe", s)
    }
  }
  out
}

## The reading of one response's ANOVA table: a sentence for each row with an
## F-test, in the order of the table, saying what the test asks in the terms
## of the design; then which error each row is tested against, where the
## design has more than one; then the CV.
anova_text <- function(res, tr = "none") {
  if (exact_fit(res)) return(RD_EXACT)
  a <- res$alpha; an <- res$anova; terms <- aov_terms(res)
  out <- character(0)
  for (i in which(!is.na(an$p))) {
    src <- an$Source[i]; t <- terms[[src]]
    if (is.null(t)) next
    yes <- an$p[i] < a; v <- t$vars
    head <- sprintf("%s: %s", src, rd_sig(an$p[i], a))
    out <- c(out, switch(t$kind,
      treatment = if (length(v) == 1) {
        if (yes) sprintf("%s: there is evidence that the %s means are not all equal; the tables of means show which differ.", head, v)
        else sprintf("%s: insufficient evidence that the %s means differ.", head, v)
      } else if (yes) {
        if (length(v) == 2)
          sprintf(paste("%s: there is evidence that %s. The %s means and the %s means then each average over combinations",
                        "that behave differently, so read the table of %s x %s means and the interaction plot first."),
                  head, rd_inter(v), v[1], v[2], v[1], v[2])
        else sprintf("%s: there is evidence that %s; the detailed tables of means show the %s combinations.", head, rd_inter(v), paste(v, collapse = " x "))
      } else sprintf("%s: insufficient evidence that %s. A small interaction may have gone undetected.", head, rd_inter(v)),
      block = , row = , column = {
        what <- c(block = "blocks", row = "rows of the square", column = "columns of the square")[[t$kind]]
        if (yes) sprintf(paste("%s: there is evidence that the %s differed. The analysis takes that variation out of the error,",
                               "which makes the treatment comparisons more precise."), head, what)
        else sprintf("%s: insufficient evidence that the %s differed.", head, what)
      },
      env = if (yes) sprintf("%s: there is evidence that the environments' overall means differ.", head)
            else sprintf("%s: insufficient evidence that the environments' overall means differ.", head),
      rep_env = if (yes) sprintf("%s: there is evidence that the replications within an environment differed; the analysis takes that variation out of the error.", head)
                else sprintf("%s: insufficient evidence that the replications within an environment differed.", head),
      gxe = {
        what <- if (length(v) == 1) sprintf("the differences between the %s means change", v)
                else sprintf("the %s interaction changes", paste(v, collapse = " x "))
        if (yes) sprintf("%s: there is evidence that %s from one environment to another, so the %s means over all environments average over environments that behave differently.",
                         head, what, paste(v, collapse = " x "))
        else sprintf("%s: insufficient evidence that %s from one environment to another.", head, what)
      }))
  }
  c(out, rd_errors(res), rd_cv(res, tr))
}

## which error each row is tested against, where there is more than one
rd_errors <- function(res) {
  A <- res$facs[1]; B <- res$facs[2]
  switch(res$design,
    SPLIT = sprintf(paste("%s, the main-plot factor, is tested against Error (a), the variation between main plots; %s and the",
                          "%s x %s interaction are tested against Error (b), the variation between sub plots within a main plot."),
                    A, B, A, B),
    STRIP = sprintf("%s is tested against Error (a), %s against Error (b), and the %s x %s interaction against Error (c).",
                    A, B, A, B),
    POOLRCBD = , POOLCRD = , POOLFRCBD = , POOLFCRD = paste(
      "Each treatment row is tested against its own interaction with the environment, not against the pooled error:",
      "the environments are taken as a sample of the places or seasons the results should apply to, so a treatment",
      "difference counts only if it is large beside how much it changes from one environment to another. Each",
      "interaction with the environment is tested against the pooled error, and the environments against",
      if (res$design %in% c("POOLRCBD", "POOLFRCBD")) "the replications within environments." else "the pooled error."),
    NULL)
}

## The CV, judged by the usual guide for field experiments, on the figure as
## printed. On a transformed scale, or with a mean that is not positive, the
## guide does not apply and the text says so instead.
rd_cv <- function(res, tr = "none") {
  cv <- res$cv
  nm <- sub(" [(]%[)]$", "", names(cv))
  shown <- sprintf("%s %s%%", nm, fmt(cv, 2))
  if (!identical(tr, "none"))
    return(sprintf("The %s on the transformed scale, so the usual guide for judging a CV does not apply to %s.",
                   if (length(cv) == 1) paste(shown, "is") else paste(join_and(shown), "are"),
                   if (length(cv) == 1) "it" else "them"))
  if (!is.finite(res$grand) || res$grand <= 0)
    return("The grand mean is not positive, so the CV has no useful meaning here.")
  rank <- vapply(as.numeric(fmt(cv, 2)), function(z) if (z < 10) 1L else if (z < 20) 2L else if (z < 30) 3L else 4L, 1L)
  band <- c("low", "moderate", "high", "very high")[rank]
  guide <- paste("by the usual guide for field experiments (below 10% low, 10 to 20% moderate, 20 to 30% high,",
                 "30% and above very high), though what is usual depends on the crop and the character measured")
  out <- if (length(cv) == 1) sprintf("The CV is %s%%, %s %s.", fmt(cv, 2), band, guide)
         else sprintf("The CVs are %s, %s.", join_and(sprintf("%s (%s)", shown, band)), guide)
  w <- which(rank == max(rank))
  if (max(rank) < 3) return(out)
  paste0(out, if (length(cv) == 1) " With this much error variation only large differences between treatments can be detected"
              else sprintf(" With %s this high, only large differences can be detected in the comparisons %s",
                           join_and(nm[w]), pl(length(w), "it applies to", "they apply to")),
         if (max(rank) == 4) "; check the data for recording errors." else ".")
}

## The reading of the table of mean squares of every response: for each row,
## the responses for which it is significant.
anova_combined_text <- function(rr) {
  fits <- rr$fits; a <- rr$alpha
  if (length(fits) < 2) return(character(0))
  src <- fits[[1]]$final$anova$Source
  hd <- vapply(fits, function(f) f$header, character(1))
  out <- character(0)
  for (i in seq_along(src)) {
    p <- vapply(fits, function(f) f$final$anova$p[i], numeric(1))
    if (all(is.na(p))) next
    yes <- hd[!is.na(p) & p < a]; no <- hd[!is.na(p) & p >= a]
    out <- c(out,
      if (!length(no)) sprintf("%s is significant at the %s level for every response.", src[i], rd_lvl(a))
      else if (!length(yes)) sprintf("%s is not significant at the %s level for any response.", src[i], rd_lvl(a))
      else sprintf("%s is significant at the %s level for %s, but not for %s.", src[i], rd_lvl(a), join_and(yes), join_and(no)))
  }
  out
}

## ------------------------------------------------------------ means ----

## The means of `e` that do not differ significantly from mean i: those
## within the C.D. of it, as the letters decide (i itself included).
rd_par <- function(e, i) {
  mu <- e$means$Mean
  unname(which(abs(mu - mu[i]) <= e$cd_mat[, i]))
}

## "the mean of N120 is", "the means of V1 and V3 are", or, for more than
## three, "the other 4 Variety means are"
rd_rest <- function(lab, v = NULL) {
  if (length(lab) == 1) sprintf("the mean of %s is", lab)
  else if (length(lab) <= 3) sprintf("the means of %s are", join_and(lab))
  else sprintf("the other %d %smeans are", length(lab), if (is.null(v)) "" else paste0(v, " "))
}

## The rule for a significant difference: one C.D., or a range of them,
## printed to `digits` decimals as the table's foot prints them
rd_cd_rule <- function(e, what, digits, tr) {
  lvl <- rd_lvl(e$alpha)
  scale <- if (identical(tr, "none")) "" else "; it applies to the transformed values in parentheses, not to the back-transformed means"
  if (!is.na(e$cd))
    sprintf("Two %s must differ by more than the C.D., %s, to be declared different at the %s level%s.",
            what, err_text(e, "cd", digits, html = FALSE), lvl, scale)
  else sprintf("With unequal replication each pair of %s has its own C.D. at the %s level, from %s; the Post-hoc tab lists them pair by pair%s.",
               what, lvl, err_text(e, "cd", digits, html = FALSE), scale)
}

## The significant interactions that involve factor `v`, named by their
## factors: its means average over the levels of the others, which then
## behave differently.
rd_inter_with <- function(res, v) {
  an <- res$anova; terms <- aov_terms(res); env <- rd_env(res)
  out <- character(0)
  for (i in which(!is.na(an$p) & an$p < res$alpha)) {
    t <- terms[[an$Source[i]]]
    if (is.null(t) || !t$kind %in% c("treatment", "gxe")) next
    vars <- if (t$kind == "gxe") c(env, t$vars) else t$vars
    if (length(vars) > 1 && v %in% vars) out <- c(out, paste(vars, collapse = " x "))
  }
  out
}

## the caution under a main effect whose factor takes part in a significant
## interaction
rd_avg_note <- function(res, v) {
  w <- rd_inter_with(res, v)
  if (!length(w)) return("")
  sprintf("The %s %s significant at the %s level, so these %s means average over conditions that behave differently; the two-way table and the interaction plot show how.",
          join_and(w), pl(length(w), "interaction is", "interactions are"), rd_lvl(res$alpha), v)
}

## The reading of a one-factor table of means: the highest and the lowest
## mean as printed, and, when the F-test is significant, which means do not
## differ significantly from them, as the letters and the C.D. decide. The
## comparisons are those of the analysis, on the transformed scale when there
## is one; the means are named as the table shows them. Given the analysis
## `res`, a significant interaction involving the factor is pointed out.
means_text <- function(e, digits = 2, tr = "none", res = NULL) {
  if (!is.null(res) && exact_fit(res)) return(RD_EXACT)
  note <- if (is.null(res)) "" else rd_avg_note(res, e$vars)
  out <- means_body(e, digits, tr)
  if (nzchar(note)) paste(out, note) else out
}

## the reading itself, without the caution about interactions
means_body <- function(e, digits, tr) {
  a <- e$alpha; lvl <- rd_lvl(a); v <- e$vars
  lab <- as.character(e$means[[v]]); k <- length(lab)
  shown <- rd_shown(e, tr); pr <- fmt(shown, digits)
  top <- at_extreme(pr, max); bot <- at_extreme(pr, min)
  sig <- effect_sig(e)
  verdict <- if (is.na(e$p)) "" else if (sig) "" else
    sprintf(paste("The F-test for %s is not significant at the %s level (%s), so no C.D. is quoted and no letters are given:",
                  "there is insufficient evidence that the %s means differ."), v, lvl, rd_p(e$p, a), v)
  if (length(top) == k)
    return(paste0(sprintf("All %d %s means print as %s.", k, v, pr[1]), if (nzchar(verdict)) paste0(" ", verdict) else ""))
  ends <- if (k == 2) sprintf("%s has the higher mean (%s) and %s the lower (%s).", lab[top], pr[top], lab[bot], pr[bot])
          else sprintf("%s %s the highest mean (%s) and %s %s the lowest (%s).",
                       join_and(lab[top]), pl(length(top), "has", "share"), pr[top[1]],
                       join_and(lab[bot]), if (length(bot) == 1) "" else "share", pr[bot[1]])
  ends <- gsub("  ", " ", ends, fixed = TRUE)
  if (!sig) return(paste(ends, verdict))
  rule <- rd_cd_rule(e, paste(v, "means"), digits, tr)
  mu <- e$means$Mean
  if (k == 2) {
    d <- abs(mu[1] - mu[2]); C <- e$cd_mat[1, 2]
    f <- rd_pair(d, C, digits)
    return(paste(ends, sprintf("%s difference, %s, is %s than the C.D. (%s), so it is %s at the %s level.",
                               if (identical(tr, "none")) "The" else "On the transformed scale the",
                               f[1], if (d > C) "larger" else "not larger", f[2],
                               if (d > C) "significant" else "not significant", lvl)))
  }
  ## the means at par with each end: within the C.D. of every mean tied there
  side <- function(ends_i, word) {
    par <- setdiff(Reduce(intersect, lapply(ends_i, function(i) rd_par(e, i))), ends_i)
    rest <- setdiff(seq_len(k), c(ends_i, par))
    ref <- join_and(lab[ends_i])
    ord <- par[order(shown[par], decreasing = word == "lower")]
    if (!length(par)) sprintf("Every other %s mean is significantly %s than %s.", v, word,
                              sprintf("%s of %s", pl(length(ends_i), "that", "those"), ref))
    else if (!length(rest)) sprintf("No other %s mean differs significantly from %s.", v, ref)
    else sprintf("%s %s not differ significantly from %s; %s significantly %s.",
                 rd_named(lab[ord], pr[ord]), pl(length(par), "does", "do"), ref,
                 rd_rest(lab[rest[order(shown[rest], decreasing = word == "lower")]], v), word)
  }
  paste(ends, rule, side(top, "lower"), side(bot, "higher"))
}

## The reading of a two-way table of means of `e` (`f1` in the rows, `f2` in
## the columns): the interaction's verdict, then, when it is significant, how
## the pattern changes from row to row, judged on the means as printed, and
## which C.D. compares two cells. `cd_digits` are the decimals of the C.D.
## in the table's foot.
twoway_text <- function(res, e, f1, f2, digits = 2, tr = "none", cd_digits = digits) {
  if (is.null(e) || is.na(e$p)) return("")
  if (exact_fit(res)) return(RD_EXACT)
  a <- res$alpha; lvl <- rd_lvl(a)
  if (!effect_sig(e))
    return(sprintf(paste("The %s x %s interaction is not significant at the %s level (%s): there is insufficient evidence that the",
                         "differences between the %s means change with %s, so the marginal means (the last row and column) can",
                         "be read on their own, each with its own factor's F-test and C.D. A small interaction may have gone undetected."),
                   f1, f2, lvl, rd_p(e$p, a), f2, f1))
  m <- e$means; shown <- rd_shown(e, tr); pr <- fmt(shown, digits)
  l1 <- levels(res$data[[f1]]); l1 <- l1[l1 %in% as.character(m[[f1]])]
  rows <- lapply(l1, function(x) which(as.character(m[[f1]]) == x))
  best <- vapply(rows, function(i) join_and(as.character(m[[f2]][i][at_extreme(pr[i], max)])), "")
  out <- sprintf("The %s x %s interaction is significant at the %s level (%s): there is evidence that the differences between the %s means are not the same at every level of %s.",
                 f1, f2, rd_lvl(if (e$p < a / 5) a / 5 else a), rd_p(e$p, a), f2, f1)
  n2 <- length(unique(as.character(m[[f2]])))
  if (length(unique(best)) == 1) {
    ## the same level is highest everywhere: say whether the order of the
    ## others holds too, and with two levels how far apart they are. Each
    ## row's order runs highest first, with ties as printed marked "=" and
    ## tied levels in the table's order, so equal orders compare equal.
    ord <- vapply(rows, function(i) {
      o <- i[order(-as.numeric(pr[i]))]
      paste0(as.character(m[[f2]][o]), c(ifelse(diff(as.numeric(pr[o])) == 0, "=", ">"), ""), collapse = "")
    }, "")
    same <- length(unique(ord)) == 1
    out <- c(out, if (n2 == 2) {
      gap <- vapply(rows, function(i) max(as.numeric(pr[i])) - min(as.numeric(pr[i])), numeric(1))
      paste0(sprintf("%s has the higher mean at every level of %s, so the interaction lies in the size of the difference", best[1], f1),
             if (!identical(tr, "none")) "."
             else if (length(l1) == 2) sprintf(": %s at %s and %s at %s.", fmt(gap[1], digits), l1[1], fmt(gap[2], digits), l1[2])
             else sprintf(", which ranges from %s at %s to %s at %s.", fmt(min(gap), digits), l1[which.min(gap)],
                          fmt(max(gap), digits), l1[which.max(gap)]))
    } else if (same) sprintf("The %s means keep the same order at every level of %s, with %s highest, so the interaction lies in the size of the differences.",
                             f2, f1, best[1])
    else sprintf("%s has the highest mean at every level of %s, but the order of the other %s means changes.", best[1], f1, f2))
  } else {
    grp <- split(l1, factor(best, levels = unique(best)))
    out <- c(out, sprintf("The %s with the highest mean changes with %s: %s.", f2, f1,
                          join_and(sprintf("%s at %s", names(grp), vapply(grp, join_and, "")))))
  }
  other <- setdiff(e$vars, e$slice)
  out <- c(out, if (is.null(e$slice)) rd_cd_rule(e, sprintf("%s x %s means", f1, f2), cd_digits, tr)
    else sprintf("Two %s means at the same level of %s must differ by more than %s to be declared different at the %s level; the other comparisons have their own C.D., given with the table.",
                 other, e$slice, err_text(e, "cd", cd_digits, html = FALSE), lvl))
  paste(out, collapse = " ")
}

## The reading under a table of the detailed means, which prints one-factor
## and many-factor tables to three decimals, two-way tables to `digits`, and
## every C.D. to three. An environment x treatment table carries its own note.
detail_text <- function(res, e, digits = 2, tr = "none") {
  if (isTRUE(e$is_gxe)) return("")
  if (exact_fit(res)) return(RD_EXACT)
  if (length(e$vars) == 1) means_text(e, 3, tr, res)
  else if (length(e$vars) == 2) twoway_text(res, e, e$vars[1], e$vars[2], digits, tr, cd_digits = 3)
  else multi_text(e, 3, tr)
}

## The reading of a table of means of three or more factors: the verdict,
## and the highest and lowest combination as printed.
multi_text <- function(e, digits = 2, tr = "none") {
  if (is.na(e$p)) return("")
  a <- e$alpha; v <- e$vars
  shown <- rd_shown(e, tr); pr <- fmt(shown, digits)
  cell <- apply(e$means[v], 1, function(z) paste(trimws(z), collapse = " x "))
  top <- at_extreme(pr, max); bot <- at_extreme(pr, min)
  paste(sprintf("The %s interaction is %s: %s.", paste(v, collapse = " x "), rd_sig(e$p, a),
                paste(if (effect_sig(e)) "there is evidence that" else "there is insufficient evidence that", rd_inter(v))),
        sprintf("The highest mean is that of %s (%s) and the lowest that of %s (%s).", join_and(cell[top]), pr[top[1]],
                join_and(cell[bot]), pr[bot[1]]))
}

## ---------------------------------------------------------- post-hoc ----

## What each procedure does, in plain words. The orderings hold for these
## implementations with equal replication: the LSD declares every pair the
## multiple-range tests declare, and they every pair Tukey's test declares.
PH_WHAT <- c(
  "LSD (Fisher's protected)" = paste(
    "The least significant difference compares each pair of means on its own, once the F-test is significant. It is the",
    "least cautious of these tests: among many pairs, some may be declared different by chance."),
  "LSD (Bonferroni-adjusted)" = paste(
    "Each pair is tested at the significance level divided by the number of pairs, so the chance of declaring any",
    "difference that is not real stays within the chosen level. It is cautious, and more so the more means there are."),
  "Tukey HSD" = paste(
    "Tukey's test judges every pair against the range expected across the whole set of means, so the chance of declaring",
    "any difference that is not real stays within the chosen level. It is more cautious than the LSD."),
  "Duncan's DMRT" = paste(
    "Duncan's test widens the critical range as more means lie between the two compared, but by less than Tukey's test",
    "does; it usually declares more pairs different than Tukey's test and fewer than the LSD."),
  "Student-Newman-Keuls" = paste(
    "The Student-Newman-Keuls test widens the critical range with the number of means spanned, using Tukey's range for",
    "each span; it is usually more cautious than Duncan's test and less cautious than Tukey's."),
  "Scheffe" = paste(
    "Scheffe's test allows for every possible comparison among the means, not only pairs, so for pairs it is usually",
    "the most cautious of these tests."),
  "Dunnett" = paste(
    "Dunnett's test compares each treatment with the control and with nothing else. Because it makes only those",
    "comparisons, it can keep the chance of declaring any difference from the control that is not real within the",
    "chosen level while staying more sensitive than a test of every pair. The one-sided forms ask only whether",
    "treatments are higher, or only whether they are lower, than the control."))

## The reading of Dunnett's test: in each family, which treatments are
## significantly higher or lower than the control, and which are not, in
## the terms of the question asked (two-sided or one side)
dunnett_groups_text <- function(x, e, tr = "none") {
  lvl <- rd_lvl(x$alpha); alt <- x$dunnett$alternative
  one <- function(fm, where = "") {
    pr <- fmt(fm$mu, 3); oth <- fm$others; ctrl <- fm$lab[fm$ctrl]
    up <- oth[fm$sig & fm$d > 0]; down <- oth[fm$sig & fm$d < 0]; ns <- oth[!fm$sig]
    named <- function(ix) rd_named(fm$lab[ix], pr[ix])
    lead <- sprintf("%sompared with the control, %s (%s), by Dunnett's test at the %s level", if (nzchar(where)) paste0(where, ", c") else "C",
                    ctrl, pr[fm$ctrl], lvl)
    parts <- switch(alt,
      two.sided = c(if (length(up)) sprintf("%s %s significantly higher", named(up), pl(length(up), "is", "are")),
                    if (length(down)) sprintf("%s %s significantly lower", named(down), pl(length(down), "is", "are")),
                    if (length(ns)) sprintf("%s %s not differ significantly from it", named(ns), pl(length(ns), "does", "do"))),
      greater = c(if (length(up)) sprintf("%s %s significantly higher", named(up), pl(length(up), "is", "are")),
                  if (length(ns)) sprintf("for %s there is insufficient evidence that %s higher", named(ns), pl(length(ns), "it is", "they are"))),
      less = c(if (length(down)) sprintf("%s %s significantly lower", named(down), pl(length(down), "is", "are")),
               if (length(ns)) sprintf("for %s there is insufficient evidence that %s lower", named(ns), pl(length(ns), "it is", "they are"))))
    paste0(lead, ": ", join_and(parts), ".")
  }
  fams <- x$families
  out <- if (length(fams) == 1 && is.null(fams[[1]]$within)) one(fams[[1]])
         else paste(vapply(fams, function(fm) one(fm, sprintf("At %s", fm$within)), ""), collapse = " ")
  if (identical(tr, "none")) out else paste(out, sprintf("The means here are on the transformed scale (%s).", TRANS[[tr]]$lab))
}

## The reading of Dunnett's comparisons: how many are significant, the one
## critical value they share, and, two-sided, the count Fisher's protected
## LSD gives for the same comparisons
dunnett_pairs_text <- function(x, lsd = NULL) {
  lvl <- rd_lvl(x$alpha); p <- x$pairs; m <- nrow(p); k <- sum(p$Significant == "Yes")
  cv <- x$dunnett$crit
  count <- if (m == 1) sprintf("The comparison with the control is %s", if (k) "significant" else "not significant")
           else if (k == 0) sprintf("None of the %d comparisons with the control is significant", m)
           else if (k == m) sprintf("All %d comparisons with the control are significant", m)
           else sprintf("%d of the %d comparisons with the control %s significant", k, m, pl(k, "is", "are"))
  out <- c(sprintf("%s by this test at the %s level.", count, lvl),
           if (length(unique(signif(cv, 10))) == 1)
             sprintf("Every comparison is judged against the same critical value, %s%s.", fmt(cv[1]),
                     if (diff(range(p$`Critical difference`)) > 1e-9 * max(p$`Critical difference`))
                       ", times its own standard error of a difference, so the critical differences differ" else "")
           else sprintf("The critical value differs between the levels compared within (%s to %s).", fmt(min(cv)), fmt(max(cv))))
  if (!is.null(lsd) && x$dunnett$alternative == "two.sided") {
    kl <- sum(vapply(seq_along(x$families), function(f) {
      fm <- x$families[[f]]; sum(lsd$families[[f]]$sig[fm$ctrl, fm$others])
    }, 0))
    out <- c(out, sprintf("Fisher's protected LSD, which the letters in the tables of means use, finds %s of these comparisons significant.",
                          if (kl == k) "the same number" else as.character(kl)))
  }
  paste(out, collapse = " ")
}

## The reading of the post-hoc groups: in each family of means, the highest
## and lowest mean, and which means this test finds do not differ from them.
## The means are those of the analysis, on the transformed scale when there
## is one.
posthoc_groups_text <- function(x, e, tr = "none", exact = FALSE) {
  if (exact) return(RD_EXACT)
  lvl <- rd_lvl(x$alpha)
  if (!is.null(x$dunnett) && isTRUE(x$f_sig)) return(dunnett_groups_text(x, e, tr))
  scale <- if (identical(tr, "none")) "" else
    sprintf(" The means here are on the transformed scale (%s).", TRANS[[tr]]$lab)
  if (!isTRUE(x$f_sig))
    return(paste0(sprintf("Because the F-test for %s is not significant at the %s level, this test declares %s: there is insufficient evidence that the means differ.",
                          e$label, lvl, if (is.null(x$dunnett)) "no pair different and gives no letters" else "no treatment different from the control"), scale))
  ## the means are compared as the groups table prints them (three decimals)
  one <- function(fm, where = "") {
    pr <- fmt(fm$mu, 3); k <- length(pr)
    top <- at_extreme(pr, max); bot <- at_extreme(pr, min)
    if (length(top) == k) return(sprintf("All the means%s print as %s.", where, pr[1]))
    if (k == 2)
      return(sprintf("%s has the higher mean%s (%s); by this test the other mean is %s.", fm$lab[top], where, pr[top],
                     if (fm$sig[1, 2]) "significantly lower" else "not significantly different from it"))
    end <- function(ix, word, high) {
      par <- setdiff(Reduce(intersect, lapply(ix, function(i) which(!fm$sig[i, ]))), ix)
      rest <- setdiff(seq_len(k), c(ix, par))
      ref <- if (length(ix) == 1) "it" else "them"
      lead <- sprintf("%s %s the %s mean%s (%s)", join_and(fm$lab[ix]), pl(length(ix), "has", "share"), high, where, pr[ix[1]])
      if (!length(par)) sprintf("%s; by this test every other mean%s is significantly %s.", lead, where, word)
      else if (!length(rest)) sprintf("%s; by this test no other mean%s differs significantly from %s.", lead, where, ref)
      else {
        o <- par[order(fm$mu[par], decreasing = word == "lower")]
        sprintf("%s; by this test %s %s not differ significantly from %s, and %s significantly %s.", lead,
                rd_named(fm$lab[o], pr[o]), pl(length(par), "does", "do"), ref,
                rd_rest(fm$lab[rest[order(fm$mu[rest], decreasing = word == "lower")]]), word)
      }
    }
    paste(end(top, "lower", "highest"), end(bot, "higher", "lowest"))
  }
  ## means compared within each level of a factor: the highest at each level
  ## only, since every level read in full would run to a page
  top_only <- function(fm) {
    pr <- fmt(fm$mu, 3); k <- length(pr); top <- at_extreme(pr, max)
    if (length(top) == k) return(sprintf("At %s all the means print as %s.", fm$within, pr[1]))
    par <- setdiff(Reduce(intersect, lapply(top, function(i) which(!fm$sig[i, ]))), top)
    sprintf("At %s, %s %s the highest mean (%s), and %s.", fm$within, join_and(fm$lab[top]), pl(length(top), "has", "share"), pr[top[1]],
            if (!length(par)) "every other mean there is significantly lower"
            else if (length(par) == k - length(top)) "no other mean there differs significantly from it"
            else sprintf("%s %s not differ significantly from it", rd_named(fm$lab[par[order(-fm$mu[par])]], pr[par[order(-fm$mu[par])]]),
                         pl(length(par), "does", "do")))
  }
  fams <- x$families
  out <- if (length(fams) == 1 && is.null(fams[[1]]$within)) one(fams[[1]])
         else paste(c(sprintf("By this test, comparing %s means within each level of %s:",
                              paste(setdiff(e$vars, e$slice), collapse = " x "), e$slice),
                      vapply(fams, top_only, "")), collapse = " ")
  paste0(out, scale)
}

## The reading of the pairwise table: how many pairs this test declares, the
## pairs the step-down rule holds back, and how the count compares with
## Fisher's protected LSD, which the letters in the tables of means use.
## `lsd` is that procedure's result for the same effect.
posthoc_pairs_text <- function(x, e, method, lsd = NULL, exact = FALSE) {
  if (exact) return(RD_EXACT)
  lvl <- rd_lvl(x$alpha)
  if (!isTRUE(x$f_sig))
    return(sprintf("The F-test is not significant at the %s level, so no %s declared different.", lvl,
                   if (is.null(x$dunnett)) "pair is" else "treatment is"))
  if (!is.null(x$dunnett)) return(dunnett_pairs_text(x, lsd))
  p <- x$pairs; m <- nrow(p)
  yes <- p$Significant == "Yes"; held <- grepl("inside a non-significant range", p$Significant, fixed = TRUE)
  k <- sum(yes)
  count <- if (m == 1) sprintf("the one pair %s", if (k) "differs significantly" else "does not differ significantly")
           else if (k == m) sprintf("all %d pairs differ significantly", m)
           else if (k == 0) sprintf("none of the %d pairs differs significantly", m)
           else sprintf("%d of the %d pairs %s significantly", k, m, pl(k, "differs", "differ"))
  out <- sprintf("%s%s by this test at the %s level.",
                 if (is.null(e$slice)) "" else sprintf("Comparing %s means within each level of %s, ",
                                                        paste(setdiff(e$vars, e$slice), collapse = " x "), e$slice),
                 if (is.null(e$slice)) cap1(count) else count, lvl)
  if (any(held))
    out <- c(out, sprintf(paste("%d more %s a difference larger than %s critical range but %s not declared different, because",
                                "%s inside a wider range of means that is not significant (the step-down rule of multiple-range tests)."),
                          sum(held), pl(sum(held), "pair has", "pairs have"), pl(sum(held), "its", "their"),
                          pl(sum(held), "is", "are"), pl(sum(held), "it lies", "they lie")))
  if (any(yes) && any(!yes)) {
    i <- which(yes)[which.min(p$Difference[yes])]; j <- which(!yes)[which.max(p$Difference[!yes])]
    f <- rd_pair(p$Difference[i], p$Difference[j], 3)
    out <- c(out, sprintf("The smallest difference declared is %s (%s); the largest not declared is %s (%s).",
                          f[1], p$Comparison[i], f[2], p$Comparison[j]))
  }
  if (!is.null(lsd) && method != "LSD (Fisher's protected)") {
    kl <- sum(lsd$pairs$Significant == "Yes")
    out <- c(out, sprintf("Fisher's protected LSD, which the letters in the tables of means use, declares %s.",
                          if (kl == sum(yes)) "the same number" else sprintf("%d %s", kl, pl(kl, "pair", "pairs"))))
  }
  paste(out, collapse = " ")
}

## ------------------------------------------------------- assumptions ----

## the test the equal-variance verdict comes from, as the readings name it
rd_hov_name <- function(asm) {
  if (identical(asm$hov_test, "Bartlett")) "Bartlett's test"
  else if (identical(asm$hov_test, "Levene") && isTRUE(asm$levene$dropped > 0)) "Levene's test, on the cells with three or more values,"
  else "Levene's test"
}

## Whether a standardised residual (residual / SD of the residuals) can lie
## beyond 3 at all: |e_i| / s cannot exceed sqrt((1 - h_ii)(n - 1)), which in
## a balanced layout with nine or fewer error degrees of freedom is below 3
## for every plot.
rd_can_flag <- function(res) {
  h <- stats::hatvalues(res$lm); n <- length(h)
  max(sqrt(pmax(1 - h, 0) * (n - 1))) > 3
}

## the rows of the analysed data whose standardised residual lies beyond 3,
## by their row numbers in the data table
rd_far_rows <- function(res) as.integer(row_ids(res$data)[reg_far(res$resid / stats::sd(res$resid))])

## The reading of the table of assumption checks: each verdict in words, and
## what they mean for the F-tests and C.D.s.
assum_text <- function(res, asm, tr = "none") {
  lvl <- rd_lvl(asm$alpha); a <- asm$alpha
  if (isTRUE(asm$exact))
    return(paste("The model fits every value exactly, so every residual is zero: there is no error variation to check,",
                 "and the checks of normality and equal variances do not apply."))
  out <- if (identical(tr, "none")) character(0)
         else sprintf("These checks are of the analysis of the transformed values (%s).", TRANS[[tr]]$lab)
  norm <- if (is.na(asm$p_norm)) sprintf("Normality could not be tested: %s.", asm$norm_why %||% "too few residuals")
    else if (asm$p_norm < a) sprintf("The Shapiro-Wilk test finds that the residuals depart from a normal distribution, significant at the %s level (%s); the Q-Q plot below shows where.", lvl, rd_p(asm$p_norm, asm$alpha))
    else sprintf("The Shapiro-Wilk test finds insufficient evidence at the %s level that the residuals depart from a normal distribution (%s).", lvl, rd_p(asm$p_norm, asm$alpha))
  hov <- if (is.na(asm$p_hov)) sprintf("Equal variances could not be tested: %s.", asm$hov_why %||% "too few values")
    else if (asm$p_hov < a) sprintf("%s finds that the variances differ between the treatments, significant at the %s level (%s).", rd_hov_name(asm), lvl, rd_p(asm$p_hov, asm$alpha))
    else sprintf("%s finds insufficient evidence at the %s level that the variances differ between the treatments (%s).", rd_hov_name(asm), lvl, rd_p(asm$p_hov, asm$alpha))
  far <- rd_far_rows(res)
  outl <- if (length(far)) sprintf("%s %s a standardised residual beyond -3 or 3; check %s for a recording error.",
                                   cap1(rows_text(far)), pl(length(far), "has", "have"), pl(length(far), "it", "them"))
    else if (!rd_can_flag(res)) sprintf(paste("With %s error %s of freedom no standardised residual can go beyond -3 or 3, so that check",
                                              "cannot pick out a stray value here."), df_text(res$dfe), pl(res$dfe, "degree", "degrees"))
    else "No standardised residual lies beyond -3 or 3."
  bad <- (!is.na(asm$p_norm) && asm$p_norm < a) || (!is.na(asm$p_hov) && asm$p_hov < a)
  untested <- is.na(asm$p_norm) || is.na(asm$p_hov)
  verdict <- if (bad) paste("The F-tests and C.D.s assume residuals that are normal with equal variances, so with this",
                            "departure they are less trustworthy; the suggestion below says what may help.")
    else if (untested) "A check that could not be run has not been passed; the suggestion below takes this into account."
    else paste("Neither check finds a departure, so they give no reason to doubt the F-tests and C.D.s, though a small",
               "departure may have gone undetected.")
  paste(c(out, norm, hov, outl, verdict), collapse = " ")
}

## "What it shows" and the reading of the Box-Cox profile, judged on the
## figures as the plot's title prints them (two decimals).
boxcox_text <- function(asm, tr = "none") {
  bc <- asm$bc
  what <- paste("The curve shows, for each power lambda, how well the analysis fits after the response is raised to that power",
                "(the higher, the better). The dashed line marks the best power and the dotted lines its confidence interval.",
                "Lambda = 1 leaves the values as they are, 0.5 is the square root, 0 the logarithm and -1 the reciprocal.")
  if (isTRUE(asm$exact) || is.null(bc)) return(NULL)
  lvl <- rd_lvl(asm$alpha); f <- function(z) sprintf("%.2f", z)
  lo <- as.numeric(f(bc$ci[1])); hi <- as.numeric(f(bc$ci[2])); best <- f(bc$lambda)
  ## the plot runs from lambda = -2 to 2; a peak or an interval that reaches
  ## either end may continue beyond it
  at <- function(z) c(if (z <= min(bc$x) + 1e-9) "lower", if (z >= max(bc$x) - 1e-9) "upper")
  edge <- c(at(lo)[at(lo) == "lower"], at(hi)[at(hi) == "upper"])
  inside <- function(z) lo <= z + 1e-9 && hi >= z - 1e-9
  std <- c("the square root (0.5)" = 0.5, "the logarithm (0)" = 0, "the reciprocal (-1)" = -1)
  ins <- names(std)[vapply(std, inside, logical(1))]
  out <- c(
    if (!identical(tr, "none")) sprintf("These are powers of the analysed values, already transformed (%s): lambda = 1 means no further transformation.", TRANS[[tr]]$lab),
    if (length(at(bc$lambda))) sprintf("The curve is highest at the %s end of the range drawn, lambda = %s, so the best power may lie beyond it.", at(bc$lambda), best)
    else sprintf("The best power is lambda = %s.", best),
    sprintf("The %s%% confidence interval runs from %s to %s%s.", pct(bc$level), f(bc$ci[1]), f(bc$ci[2]),
            if (length(edge)) sprintf("; it reaches the %s %s of the range drawn, so it may extend further",
                                      join_and(edge), pl(length(edge), "end", "ends")) else ""),
    if (inside(1)) sprintf("The interval includes lambda = 1, so there is insufficient evidence at the %s level that a power transformation fits better than the values as they are.", lvl)
    else sprintf("The interval excludes lambda = 1: at the %s level the values as they are fit significantly worse than the best power.", lvl),
    if (length(ins)) sprintf("Of the usual transformations, %s %s inside the interval.", join_and(ins), pl(length(ins), "lies", "lie"))
    else "None of the usual transformations (square root, logarithm, reciprocal) lies inside the interval.")
  list(what = what, reading = paste(out, collapse = " "))
}

## "What it shows" and the reading of the mean-variance plot: the slope as
## the title prints it, and whether it differs significantly from 0.
meanvar_text <- function(res, asm) {
  what <- paste("Each point is one cell of the design (a treatment, or a combination of the factors): the logarithm of its mean",
                "across, the logarithm of its variance up. The slope b of the line (Taylor's power law) says how the variance",
                "changes with the mean: about 0, not at all; about 1, in proportion to the mean, as with counts, where the square",
                "root helps; about 2, as the square of the mean, where the logarithm helps.")
  if (isTRUE(asm$exact) || nrow(asm$mv) < 3) return(NULL)
  lvl <- rd_lvl(asm$alpha); b <- as.numeric(sprintf("%.2f", asm$slope))
  cells <- table(hov_cells(res))
  left <- length(cells) - nrow(asm$mv)
  how <- if (b < 0) "falls as the mean rises"
         else if (b < 0.5) "rises with the mean, though more slowly than in proportion to it"
         else if (b < 1.5) "rises roughly in proportion to the mean"
         else if (b < 2.5) "rises roughly as the square of the mean"
         else "rises faster than the square of the mean"
  out <- c(
    if (left > 0) sprintf("%d %s not drawn, because %s mean is not positive or %s no variance (a single value, or values all equal).",
                          left, pl(left, "cell is", "cells are"), pl(left, "its", "their"), pl(left, "it has", "they have")),
    if (is.na(asm$slope_p)) sprintf("The slope is b = %.2f; with only three points it cannot be tested.", asm$slope)
    else if (asm$slope_p < asm$alpha) sprintf("The slope, b = %.2f, differs significantly from 0 at the %s level (%s): the variance %s.", asm$slope, lvl, rd_p(asm$slope_p, asm$alpha), how)
    else sprintf("The slope, b = %.2f, does not differ significantly from 0 at the %s level (%s): insufficient evidence that the variance changes with the mean.", asm$slope, lvl, rd_p(asm$slope_p, asm$alpha)),
    if (min(cells) <= 3) sprintf("Each variance rests on only %s values, so the points scatter widely and the slope is uncertain.",
                                 if (min(cells) == max(cells)) min(cells) else sprintf("%d to %d", min(cells), max(cells))))
  list(what = what, reading = paste(out, collapse = " "))
}

## the residuals' Q-Q reading, shared by the Q-Q plot and the histogram: the
## shape the plot shows, or why it cannot show one
rd_qq_pattern <- function(res) {
  r <- res$resid; s <- r / stats::sd(r)
  if (isTRUE(res$dfe < 2) || length(unique(signif(abs(r), 8))) == 1) "one"
  else if (length(s) < 5) NA
  else if (diff(stats::quantile(s, c(0.25, 0.75), names = FALSE)) == 0) "heaped"
  else if (qq_coarse(r)) "coarse" else qq_pattern(s)
}

## "What it shows" and the reading of the plot of residuals against fitted
## values: the test of equal variances, and the plots beyond -3 or 3.
resid_text <- function(res, asm) {
  if (isTRUE(asm$exact)) return(NULL)
  lvl <- rd_lvl(asm$alpha)
  what <- paste("Each point is one plot: the value the analysis fits for it across, and its residual (observed minus fitted)",
                "divided by the standard deviation of the residuals up. If the analysis suits the data, the points form an even",
                "band about the dashed line at 0; a funnel, wider towards one side, means the spread changes with the mean.",
                "The dotted lines mark -3 and 3.")
  far <- rd_far_rows(res)
  hov <- if (is.na(asm$p_hov)) sprintf("Equal variances could not be tested: %s.", asm$hov_why %||% "too few values")
    else if (asm$p_hov < asm$alpha) sprintf("%s finds that the variances differ between the treatments, significant at the %s level (%s): look for the band to widen towards one side.",
                                           rd_hov_name(asm), lvl, rd_p(asm$p_hov, asm$alpha))
    else sprintf("%s finds insufficient evidence at the %s level that the variances differ between the treatments (%s).", rd_hov_name(asm), lvl, rd_p(asm$p_hov, asm$alpha))
  out <- c(hov,
    if (length(far)) sprintf("%s %s beyond -3 or 3, which normal residuals seldom are; check %s for a recording error.",
                             cap1(rows_text(far)), pl(length(far), "lies", "lie"), pl(length(far), "it", "them"))
    else if (!rd_can_flag(res)) sprintf(paste("With %s error %s of freedom no point can lie beyond -3 or 3, so the dotted lines cannot pick",
                                              "out a stray value; look instead for one point well apart from the rest."), df_text(res$dfe), pl(res$dfe, "degree", "degrees"))
    else "No point lies beyond -3 or 3.")
  list(what = what, reading = paste(out, collapse = " "))
}

## "What it shows" and the reading of the normal Q-Q plot of the residuals:
## how its ends bend, against what normal residuals of the same number show
## by chance, and the Shapiro-Wilk test, with the reason when the two differ.
qq_aov_text <- function(res, asm) {
  if (isTRUE(asm$exact)) return(NULL)
  what <- paste("Each point is a residual, placed against where it would fall if the residuals followed a normal distribution.",
                "Points close to the straight line mean the residuals look normal, as the F-tests assume.")
  lvl <- rd_lvl(asm$alpha)
  pattern <- rd_qq_pattern(res)
  s <- res$resid / stats::sd(res$resid)
  read <- !is.na(pattern) && !pattern %in% c("heaped", "coarse", "one")
  plot_read <- if (identical(pattern, "one"))
      paste(if (isTRUE(res$dfe < 2)) "With one error degree of freedom the residuals take the same pattern whatever the data,"
            else "Every residual has the same size,", "so the plot cannot show the shape of their distribution.")
    else if (is.na(pattern)) "There are too few points to judge."
    else if (pattern == "heaped") "At least half the residuals are the same value, so many points sit in one flat run."
    else if (pattern == "coarse") paste("The residuals sit on a coarse grid, as happens when the response is recorded in large steps,",
                                        "so the points form flat runs and the ends of this plot cannot be read reliably.")
    else gsub("values", "residuals", qq_reading(s), fixed = TRUE)
  far <- reg_far(s)
  if (read && length(far)) {
    up <- s[far] > 0
    at_end <- if (all(up)) "far above the line at the top end" else if (!any(up)) "far below the line at the bottom end"
              else "far from the line at the two ends"
    lead <- sprintf("%s %s %s.", cap1(rows_text(row_ids(res$data)[far])), pl(length(far), "sits", "sit"), at_end)
    plot_read <- if (identical(pattern, "ok ok"))
      paste(lead, sprintf("Apart from %s, %s", pl(length(far), "it", "them"), sub("^Neither", "neither", plot_read)))
    else paste(lead, plot_read)
  }
  ## with one error degree of freedom the plot's reading has already said why
  test <- if (identical(pattern, "one") && isTRUE(res$dfe < 2)) NULL
    else if (is.na(asm$p_norm)) sprintf("Normality could not be tested: %s.", asm$norm_why %||% "too few residuals")
    else if (asm$p_norm < asm$alpha) sprintf("The Shapiro-Wilk test finds a significant departure from normal at the %s level (%s).", lvl, rd_p(asm$p_norm, asm$alpha))
    else sprintf("The Shapiro-Wilk test finds insufficient evidence at the %s level that the residuals are not normal (%s).", lvl, rd_p(asm$p_norm, asm$alpha))
  calm <- identical(pattern, "ok ok")
  clash <- !is.na(asm$p_norm) && read && ((asm$p_norm < asm$alpha && calm) || (asm$p_norm >= asm$alpha && !calm))
  out <- c(plot_read, test,
    if (clash) paste("The two need not agree: the test weighs every point, while the reading of the plot looks at how far the",
                     "outer tenth of the points at each end bends on average."),
    if (read && length(s) < 20) sprintf("With only %d residuals, a Q-Q plot can show only a large departure from normal.", length(s)))
  list(what = what, reading = paste(out, collapse = " "))
}

## "What it shows" and the reading of the histogram of the residuals: their
## skewness, read with Bulmer's bands, and the rule for more than one hump.
hist_aov_text <- function(res, asm) {
  if (isTRUE(asm$exact)) return(NULL)
  what <- "The bars count the residuals in each range of values. Normal residuals form a single hump, roughly symmetric about 0."
  s <- desc_one(res$resid); sh <- shape_of(s); g1 <- dfmt(s$Skewness, 2)
  ## how far the bars reach either side of 0 (the mean of the residuals), to
  ## three significant figures, so the direction stated is the one drawn
  f <- function(z) format(signif(z, 3), trim = TRUE, scientific = FALSE)
  up <- as.numeric(f(s$Max)); down <- -as.numeric(f(s$Min))
  reach <- if (up > down) "up" else if (down > up) "down" else "even"
  side <- if (sh$kind == "right") "up" else "down"
  out <- c(
    switch(sh$kind,
      right = , left = paste0(sprintf("The skewness of the residuals is %s, %s skew", g1, paste(sh$strength, sh$kind)),
                              if (reach == side) sprintf(": the bars reach further %s 0 (to %s) than %s it (to %s).",
                                                         if (side == "up") "above" else "below", f(if (side == "up") s$Max else s$Min),
                                                         if (side == "up") "below" else "above", f(if (side == "up") s$Min else s$Max))
                              else if (reach == "even")
                                sprintf(", though the lowest and the highest residual (%s and %s) lie equally far from 0, so the skew does not come from a long tail of %s residuals.",
                                        f(s$Min), f(s$Max), if (side == "up") "high" else "low")
                              else sprintf(", though the %s residual (%s) lies further from 0 than the %s (%s), so the skew does not come from a long tail of %s residuals.",
                                           if (side == "up") "lowest" else "highest", f(if (side == "up") s$Min else s$Max),
                                           if (side == "up") "highest" else "lowest", f(if (side == "up") s$Max else s$Min),
                                           if (side == "up") "high" else "low"),
                              if (!sh$clear) sprintf(" With %d residuals, a skewness this size can also arise by chance.", s$N) else ""),
      symmetric = sprintf("The skewness of the residuals is %s, close to zero, so by this measure they spread roughly evenly about 0.", g1),
      constant = "Every residual is the same.",
      "There are too few residuals to judge the shape."),
    "If the bars form two or more separate humps, some plots behave differently from the rest; look for a recording error or a treatment that differs in spread.")
  list(what = what, reading = paste(out, collapse = " "))
}

## "What it shows" and the reading of the scale-location plot: the Taylor
## slope, which asks the same question of the treatment means.
scale_text <- function(res, asm) {
  if (isTRUE(asm$exact)) return(NULL)
  what <- paste("Each point is one plot: the value the analysis fits for it across, and the square root of the size of its",
                "standardised residual up, so large residuals of either sign sit high. The line follows their average height. If",
                "the spread is the same at every fitted value, the line runs roughly level; a line that climbs means larger",
                "residuals where the fitted values are larger.")
  lvl <- rd_lvl(asm$alpha)
  out <- if (is.na(asm$slope)) "The mean-variance slope could not be estimated, so the line is the only guide here."
    else if (is.na(asm$slope_p)) sprintf("The mean-variance slope (b = %.2f) rests on three treatments and cannot be tested, so the line is the main guide here.", asm$slope)
    else if (asm$slope_p < asm$alpha) sprintf("The mean-variance slope, b = %.2f, differs significantly from 0 at the %s level (%s): the variance %s with the mean, so look for the line to %s.",
                                              asm$slope, lvl, rd_p(asm$slope_p, asm$alpha), if (asm$slope > 0) "rises" else "falls",
                                              if (asm$slope > 0) "climb" else "fall")
    else sprintf("The mean-variance slope, b = %.2f, gives insufficient evidence at the %s level that the variance changes with the mean (%s).", asm$slope, lvl, rd_p(asm$slope_p, asm$alpha))
  list(what = what, reading = out)
}

## ------------------------------------------------------------ plots ----

## "What it shows" and the reading of the main plot on the Plots tab, worked
## out from the means the plot draws (on the scale the analysis used) and the
## effect's F-test. `x_var` is the factor on the X-axis of an interaction.
main_plot_text <- function(res, effect, type = "bar", show_letters = TRUE, x_var = NULL, digits = 2) {
  e <- res$effects[[effect]]
  if (is.null(e)) return(NULL)
  a <- res$alpha; lvl <- rd_lvl(a); v <- e$vars; m <- e$means
  pr <- fmt(m$Mean, digits); sig <- effect_sig(e)
  scale <- if (is.null(res$trans) || identical(res$trans, "none")) "" else ", on the transformed scale,"
  verdict <- if (exact_fit(res)) RD_EXACT else if (is.na(e$p)) "" else if (sig) sprintf("The F-test for %s is %s.", e$label, rd_sig(e$p, a))
    else sprintf("The F-test for %s is %s: insufficient evidence that %s.", e$label, rd_sig(e$p, a),
                 if (length(v) == 1) sprintf("the %s means differ", v) else rd_inter(v))
  if (type == "box") {
    d <- res$data; resp <- res$resp
    keys <- do.call(paste, c(unname(lapply(d[v], as.character)), sep = " x "))
    med <- tapply(d[[resp]], factor(keys, levels = unique(keys[do.call(order, unname(as.list(d[v])))])), stats::median)
    pm <- fmt(med, digits)
    what <- sprintf(paste("Each box shows the observations%s of one %s as recorded: the line inside is the median, the box",
                          "holds the middle half of the values, and points more than 1.5 box lengths beyond the box are drawn on their own."),
                    scale, if (length(v) == 1) sprintf("level of %s", v) else "combination")
    top <- at_extreme(pm, max); bot <- at_extreme(pm, min)
    reading <- paste(
      if (length(top) == length(pm)) sprintf("Every median is %s.", pm[1])
      else sprintf("The median is highest for %s (%s) and lowest for %s (%s).", join_and(names(med)[top]), pm[top[1]],
                   join_and(names(med)[bot]), pm[bot[1]]),
      verdict,
      "Single observations vary more than means do, so boxes can overlap even where the means differ significantly; the F-test and the letters compare the means.")
    note <- if (length(v) == 1 && !exact_fit(res)) rd_avg_note(res, v) else ""
    return(list(what = what, reading = if (nzchar(note)) paste(reading, note) else reading))
  }
  if (length(v) == 1) {
    lab <- as.character(m[[v]])
    what <- paste0(sprintf("Each %s is the mean of one level of %s%s and the error bars reach one standard error either side of it.",
                           if (type == "bar") "bar" else "point", v, if (nzchar(scale)) scale else ","),
                   if (show_letters && sig) sprintf(" %s sharing a letter do not differ significantly at the %s level.",
                                                    if (type == "bar") "Bars" else "Points", lvl) else "")
    top <- at_extreme(pr, max); bot <- at_extreme(pr, min)
    reading <- paste(
      if (length(top) == length(pr)) sprintf("Every mean is %s.", pr[1])
      else sprintf("The highest mean is that of %s (%s) and the lowest that of %s (%s).", join_and(lab[top]), pr[top[1]],
                   join_and(lab[bot]), pr[bot[1]]),
      verdict,
      "Error bars that do not overlap are not enough to show that two means differ significantly; the letters or the C.D. decide.")
    note <- if (exact_fit(res)) "" else rd_avg_note(res, v)
    return(list(what = what, reading = if (nzchar(note)) paste(reading, note) else reading))
  }
  f1 <- if (length(x_var) == 1L && isTRUE(x_var %in% v)) x_var else default_x(e, res$data)
  rest <- setdiff(v, f1); f2 <- rest[1]
  panel <- if (length(rest) > 1) do.call(paste, c(lapply(m[rest[-1]], as.character), sep = " x ")) else rep("", nrow(m))
  ## a split, strip or pooled interaction's bars are the SE(m) for means at
  ## the same level of its slicing factor, as the plot's caption says
  bars <- if (is.null(e$slice)) "the error bars reach one standard error either side"
          else sprintf("the error bars reach one SE(m) either side, the standard error for comparing means at the same level of %s", e$slice)
  what <- switch(type,
    bar = sprintf("Each bar is the mean%s of one combination of %s (along the X-axis) and %s (the colours); %s.", scale, f1, f2, bars),
    line = sprintf("Each line joins the means%s of one level of %s across the levels of %s; %s.", scale, f2, f1, bars),
    heat = sprintf("Each tile is the mean%s of one combination of %s (across) and %s (up); the darker the tile, the higher the mean, and the figure in it is the mean to one decimal.", scale, f1, f2),
    "")
  if (length(rest) > 1) what <- paste(what, sprintf("Each panel is one level of %s.", join_and(rest[-1])))
  if (type == "heat") {
    hp <- fmt(m$Mean, 1)
    cell <- apply(m[v], 1, function(z) paste(trimws(z), collapse = " x "))
    top <- at_extreme(hp, max); bot <- at_extreme(hp, min)
    reading <- paste(sprintf("The darkest tile is %s (%s) and the palest %s (%s).", join_and(cell[top]), hp[top[1]],
                             join_and(cell[bot]), hp[bot[1]]), verdict)
    return(list(what = what, reading = reading))
  }
  ## does the order of the f2 levels change along the X-axis, in any panel?
  l1 <- levels(res$data[[f1]]); l1 <- l1[l1 %in% as.character(m[[f1]])]
  changes <- vapply(unique(panel), function(pn) {
    mi <- m[panel == pn, , drop = FALSE]
    ords <- vapply(l1, function(x) { z <- mi[as.character(mi[[f1]]) == x, ]; paste(as.character(z[[f2]])[order(-z$Mean)], collapse = ">") }, "")
    length(unique(ords)) > 1
  }, logical(1))
  best <- if (length(rest) == 1) vapply(l1, function(x) {
    i <- which(as.character(m[[f1]]) == x); join_and(as.character(m[[f2]][i][at_extreme(pr[i], max)]))
  }, "") else NULL
  facets <- length(rest) > 1
  where <- if (facets) sprintf(" in %d of the %d panels", sum(changes), length(changes)) else ""
  pattern <- if (!any(changes)) {
    if (type == "line") sprintf("The %s levels keep the same order at every level of %s%s, so the lines do not cross.", f2, f1,
                                if (facets) " in every panel" else "")
    else sprintf("The %s bars stand in the same order in every group%s.", f2, if (facets) " and every panel" else "")
  } else paste0(sprintf("The order of the %s levels changes along the X-axis%s%s", f2, where, if (type == "line") ", so some lines cross" else ""),
                if (is.null(best)) "."
                else if (length(unique(best)) > 1) {
                  grp <- split(l1, factor(best, levels = unique(best)))
                  sprintf(": the highest is %s.", join_and(sprintf("%s at %s", names(grp), vapply(grp, join_and, ""))))
                } else sprintf(", though %s is highest at every level of %s.", best[1], f1))
  rule <- if (type == "line") sprintf(paste("Lines that run parallel mean the difference between the %s levels is the same at every level",
                                            "of %s; the F-test for the interaction says whether the lines depart from parallel by more than chance."), f2, f1)
          else sprintf(paste("If the differences between the %s bars were the same in every group, the effect of %s would not depend on %s;",
                             "the F-test for the interaction says whether they differ by more than chance."), f2, f2, f1)
  ## what the verdict means for the pattern drawn
  then <- if (is.na(e$p) || exact_fit(res)) ""
    else if (sig && !any(changes)) "The order does not change, so the interaction lies in the size of the differences."
    else if (!sig && any(changes)) "The change in order may therefore be due to chance."
    else ""
  list(what = what, reading = paste(c(pattern, rule, verdict, then)[nzchar(c(pattern, rule, verdict, then))], collapse = " "))
}
