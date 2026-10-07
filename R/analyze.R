###############################################################################
##  MAIN ANALYSIS ENGINE
###############################################################################
#' Analyse one response from a designed experiment
#'
#' Fits the analysis of variance for a single response variable and returns the
#' means, standard errors and critical differences for every legitimate
#' comparison the design allows.
#'
#' The error term is chosen to match the design. A split plot is fitted with
#' \code{Error(rep/main)} and a strip plot with \code{Error(rep/(A+B))}, so the
#' main-plot and sub-plot comparisons are each tested against their own error;
#' the two mixed comparisons use a weighted \emph{t}. In a pooled (combined)
#' analysis over environments, each treatment effect is tested against its own
#' interaction with the environment, and each environment by treatment
#' interaction against the pooled error.
#'
#' Unequal replication is handled exactly in the designs with a single error
#' term (CRD, RCBD, Latin square and the factorials): each mean gets its own
#' standard error and each pair of means its own standard error of a difference
#' and critical difference. When blocks are incomplete or factorial cells are
#' unequal, the means are adjusted (least-squares) means and every term is
#' tested adjusted for all the others (Type III). Designs with several error
#' strata (split plot, strip plot and the pooled analyses) can only be analysed
#' exactly when they are complete; with a plot missing \code{analyze()} stops
#' and says which combinations are missing.
#'
#' @param d A data frame in long format: one row per plot, with columns for the
#'   design factors and the response. Text factor columns are put in natural
#'   order (D0, D60, D120; T1, T2, ..., T10), with a label carrying no number,
#'   such as Control, placed first; a column that is already a factor keeps its
#'   levels, so supply a factor to fix an order such as Vegetative, Flowering,
#'   Maturity.
#' @param design The design code. One of the values of \code{\link{DESIGNS}},
#'   for example \code{"RCBD"}, \code{"SPLIT"} or \code{"POOLFRCBD"}.
#' @param map A named list mapping roles to column names of \code{d}. Always
#'   needs \code{response}. Then, by design: \code{treat} (CRD, RCBD, LSD);
#'   \code{block} (RCBD, factorial RCBD); \code{row}, \code{col} (LSD);
#'   \code{factors}, a character vector of two to four column names (factorial);
#'   \code{rep}, \code{main}, \code{sub} (split and strip plots); \code{env}
#'   together with \code{treat} or \code{factors}, and \code{rep} for an RCBD
#'   base (pooled designs).
#' @param alpha The significance level. It sets the critical differences, the
#'   grouping letters and the significance of every F-test. Defaults to 0.05.
#'
#' @return A list with, among others:
#'   \describe{
#'     \item{\code{anova}}{The analysis of variance table.}
#'     \item{\code{effects}}{One entry per effect. Each holds \code{means}, the
#'       table of means with columns \code{Mean}, \code{N}, \code{SD}, \code{SE}
#'       (the standard error of that mean), the grouping letters and, when the
#'       means are adjusted, \code{Raw_mean}; \code{sed_mat} and \code{cd_mat},
#'       the standard error of the difference and the critical difference for
#'       every pair of means; \code{sem}, \code{sed} and \code{cd}, single
#'       values that are \code{NA} when replication is unequal and they differ
#'       from mean to mean (\code{equal_rep} is then \code{FALSE}, and
#'       \code{se_range}, \code{sed_range} and \code{cd_range} give the range);
#'       \code{mse} and \code{df}, the error mean square and degrees of freedom
#'       the effect is tested against; \code{p}, \code{F} and \code{alpha}. The
#'       interaction of a split plot, strip plot or pooled analysis also has
#'       \code{slice}, the factor within whose levels its means are compared:
#'       its \code{SE}, \code{sed} and \code{cd} apply to those comparisons,
#'       \code{sed_mat} and \code{cd_mat} are \code{NA} across levels of
#'       \code{slice}, and \code{extra} gives the standard errors and critical
#'       differences of the other comparisons.}
#'     \item{\code{mse}, \code{dfe}}{The error mean square and its degrees of
#'       freedom.}
#'     \item{\code{balanced}}{Whether every cell of the design holds the same
#'       number of observations.}
#'     \item{\code{cv}, \code{grand}, \code{resid}, \code{lm}}{The coefficient of
#'       variation, grand mean, residuals and the fitted linear model.}
#'   }
#'
#' @seealso \code{\link{run_all}} to analyse several responses at once, and
#'   \code{\link{build_report}} to render the result.
#'
#' @examples
#' d <- demo_data("RCBD")
#' res <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety",
#'                                block = "Block"))
#' res$anova
#' res$effects[["Variety"]]$means
#'
#' @export
analyze <- function(d, design, map, alpha = 0.05) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 0.5)
    stop("The significance level must be a single number between 0 and 0.5, such as 0.05.")
  resp <- map$response
  d[[resp]] <- suppressWarnings(as.numeric(as.character(d[[resp]])))

  facs <- switch(design,
    CRD = map$treat, RCBD = map$treat, LSD = map$treat,
    FCRD = map$factors, FRCBD = map$factors,
    SPLIT = c(map$main, map$sub), STRIP = c(map$main, map$sub),
    POOLRCBD = c(map$env, map$treat), POOLCRD = c(map$env, map$treat),
    POOLFRCBD = c(map$env, map$factors), POOLFCRD = c(map$env, map$factors))
  blks <- switch(design,
    CRD = character(0), RCBD = map$block, LSD = c(map$row, map$col),
    FCRD = character(0), FRCBD = map$block, SPLIT = map$rep, STRIP = map$rep,
    POOLRCBD = map$rep, POOLCRD = character(0),
    POOLFRCBD = map$rep, POOLFCRD = character(0))

  ## the tables of means add columns with these names, so a design factor
  ## called N (for nitrogen, say) would be overwritten by the counts
  clash <- intersect(c(facs, blks), RESERVED_COLS)
  if (length(clash))
    stop(sprintf(paste0("A column used as a design factor is called %s, a name DOEpro ",
      "uses in its tables of means. Please rename it (for example 'Nitrogen' rather ",
      "than 'N') and run the analysis again."), join_and(sprintf("'%s'", clash))),
      call. = FALSE)
  keep <- unique(c(resp, facs, blks))
  d <- d[stats::complete.cases(d[, keep, drop = FALSE]), keep, drop = FALSE]
  if (nrow(d) < 3) stop("Not enough complete rows to analyse.")
  ## levels in natural order, so tables and plots run D0, D60, D120 and
  ## T1, T2, ..., T10 instead of alphabetically
  for (v in c(facs, blks)) d[[v]] <- factor(d[[v]], levels = natural_levels(d[[v]]))

  lay <- check_layout(d, design, map, facs, blks)
  grand <- mean(d[[resp]])
  res <- list(design = design, resp = resp, data = d, facs = facs, blks = blks,
              alpha = alpha, grand = grand, balanced = lay$balanced,
              reps = lay$reps, n_range = lay$n_range)

  ## ------------------------------------------------- classic / factorial ----
  if (design %in% c("CRD", "RCBD", "LSD", "FCRD", "FRCBD")) {
    rhs <- paste(c(blks, paste(facs, collapse = "*")), collapse = " + ")
    form <- stats::as.formula(paste(resp, "~", rhs))
    fit <- stats::aov(form, data = d)
    an  <- tidy_aov(fit)
    mse <- an$MS[an$Source == "Residuals"]
    dfe <- an$Df[an$Source == "Residuals"]
    if (!length(dfe) || dfe < 1) stop("Zero error degrees of freedom - you need replication.")
    lmfit <- stats::lm(form, data = d)
    if (!lay$balanced) an <- type3_anova(an, form, d, c(blks, facs))

    effs <- list()
    for (k in seq_along(facs)) {
      for (v in utils::combn(facs, k, simplify = FALSE)) {
        lab <- paste(v, collapse = ":")
        row <- an[an$Source == lab, ]
        effs[[lab]] <- new_effect(d, resp, v, mse, dfe,
                                  if (nrow(row)) row$p[1] else NA,
                                  if (nrow(row)) row$F[1] else NA, alpha,
                                  label = paste(v, collapse = " x "),
                                  fit = if (lay$balanced) NULL else lmfit)
      }
    }
    res$anova <- add_total(an, d[[resp]])
    ## with one treatment factor and no blocks there is nothing to adjust for:
    ## the means are the plain averages and the sums of squares still add up
    res$adjusted_ss <- !lay$balanced && length(c(blks, facs)) > 1
    res$mse <- mse; res$dfe <- dfe
    res$cv <- c("CV (%)" = 100 * sqrt(mse) / grand)
    res$effects <- effs
    res$lm <- lmfit
  }

  ## ----------------------------------------------------------- split plot ---
  if (design == "SPLIT") {
    A <- map$main; B <- map$sub; R <- map$rep
    a <- nlevels(d[[A]]); b <- nlevels(d[[B]]); r <- nlevels(d[[R]])
    f <- stats::as.formula(sprintf("%s ~ %s*%s + Error(%s/%s)", resp, A, B, R, A))
    fit <- stats::aov(f, data = d)
    tab <- tidy_aovlist(fit)
    st  <- unique(tab$Stratum)
    s_a <- paste0("Error: ", R, ":", A)
    s_w <- st[grepl("Within", st)][1]
    if (!s_a %in% st) stop("Could not identify the main-plot error stratum.")
    Ea <- strat_res(tab, s_a); Eb <- strat_res(tab, s_w)
    if (is.na(Ea$ms) || is.na(Eb$ms) || Ea$df < 1 || Eb$df < 1)
      stop("Split-plot needs at least 2 replications and 2 levels of each factor.")

    get <- function(stratum, src) {
      x <- tab[tab$Stratum == stratum & tab$Source == src, ]
      if (!nrow(x)) return(data.frame(Df = NA, SS = NA, MS = NA, F = NA, p = NA))
      x[, c("Df", "SS", "MS", "F", "p")]
    }
    rep_row <- strat_res(tab, st[1])
    rA <- get(s_a, A); rB <- get(s_w, B); rAB <- get(s_w, paste0(A, ":", B))
    an <- rbind(
      data.frame(Source = "Replication", Df = rep_row$df, SS = rep_row$ss,
                 MS = rep_row$ms, F = NA, p = NA),
      data.frame(Source = A, rA),
      data.frame(Source = "Error (a)", Df = Ea$df, SS = Ea$ss, MS = Ea$ms, F = NA, p = NA),
      data.frame(Source = B, rB),
      data.frame(Source = paste(A, "x", B), rAB),
      data.frame(Source = "Error (b)", Df = Eb$df, SS = Eb$ss, MS = Eb$ms, F = NA, p = NA))
    names(an) <- c("Source", "Df", "SS", "MS", "F", "p")
    an <- add_total(an, d[[resp]])

    ta <- stats::qt(1 - alpha / 2, Ea$df); tb <- stats::qt(1 - alpha / 2, Eb$df)

    eA <- new_effect(d, resp, A, Ea$ms, Ea$df, rA$p, rA$F, alpha, label = A)
    eB <- new_effect(d, resp, B, Eb$ms, Eb$df, rB$p, rB$F, alpha, label = B)

    ## interaction: two different comparisons
    m <- eff_means(d, resp, c(A, B))
    sed_b <- sqrt(2 * Eb$ms / r)                                   # B within same A
    sed_a <- sqrt(2 * ((b - 1) * Eb$ms + Ea$ms) / (r * b))         # A at same B
    tw  <- t_weighted((b - 1) * Eb$ms, tb, Ea$ms, ta)
    cd_b <- tb * sed_b; cd_a <- tw * sed_a
    m$Letter_within_MP <- ave_letters(m, A, m$Mean, cd_b)
    m$Letter_within_SP <- ave_letters(m, B, m$Mean, cd_a)
    cdl <- cd_name(alpha)
    eAB <- slice_effect(m, c(A, B), slice = A, label = paste(A, "x", B),
      se = sqrt(Eb$ms / r), sed = sed_b, tcrit = tb, mse = Eb$ms, df = Eb$df,
      alpha = alpha, p = rAB$p, Fv = rAB$F,
      extra = stats::setNames(list(sed_b, cd_b, sed_a, cd_a), c(
        "SE(d): two sub-plot means at the same main plot",
        paste0(cdl, ": two sub-plot means at the same main plot"),
        "SE(d): two main-plot means at the same sub plot",
        paste0(cdl, ": two main-plot means at the same sub plot"))),
      extra_p = c(NA, rAB$p, NA, rAB$p),
      notes = paste0("Letters 'within MP' compare sub-plot means inside one main plot (",
        cdl, " = ", fmt(cd_b), "). Letters 'within SP' compare main-plot means at one ",
        "sub-plot level (", cdl, " = ", fmt(cd_a), ", weighted t = ", fmt(tw, 2), ")."))

    res$anova <- an
    res$mse <- Eb$ms; res$dfe <- Eb$df
    res$cv <- c("CV(a) (%)" = 100 * sqrt(Ea$ms) / grand,
                "CV(b) (%)" = 100 * sqrt(Eb$ms) / grand)
    res$effects <- stats::setNames(list(eA, eB, eAB), c(A, B, paste0(A, ":", B)))
    res$lm <- stats::lm(stats::as.formula(
      sprintf("%s ~ %s + %s*%s + %s:%s", resp, R, A, B, R, A)), data = d)
    res$errors <- list(`Error (a)` = Ea, `Error (b)` = Eb)
  }

  ## ----------------------------------------------------------- strip plot ---
  if (design == "STRIP") {
    A <- map$main; B <- map$sub; R <- map$rep
    a <- nlevels(d[[A]]); b <- nlevels(d[[B]]); r <- nlevels(d[[R]])
    f <- stats::as.formula(sprintf("%s ~ %s*%s + Error(%s/(%s+%s))", resp, A, B, R, A, B))
    fit <- stats::aov(f, data = d)
    tab <- tidy_aovlist(fit)
    st  <- unique(tab$Stratum)
    s_a <- paste0("Error: ", R, ":", A)
    s_b <- paste0("Error: ", R, ":", B)
    s_w <- st[grepl("Within", st)][1]
    if (!all(c(s_a, s_b) %in% st)) stop("Could not identify the strip-plot error strata.")
    Ea <- strat_res(tab, s_a); Eb <- strat_res(tab, s_b); Ec <- strat_res(tab, s_w)
    if (any(is.na(c(Ea$ms, Eb$ms, Ec$ms))))
      stop("Strip plot needs >= 2 replications and >= 2 levels of each factor.")

    get <- function(stratum, src) {
      x <- tab[tab$Stratum == stratum & tab$Source == src, ]
      if (!nrow(x)) return(data.frame(Df = NA, SS = NA, MS = NA, F = NA, p = NA))
      x[, c("Df", "SS", "MS", "F", "p")]
    }
    rep_row <- strat_res(tab, st[1])
    rA <- get(s_a, A); rB <- get(s_b, B); rAB <- get(s_w, paste0(A, ":", B))
    an <- rbind(
      data.frame(Source = "Replication", Df = rep_row$df, SS = rep_row$ss,
                 MS = rep_row$ms, F = NA, p = NA),
      data.frame(Source = A, rA),
      data.frame(Source = "Error (a)", Df = Ea$df, SS = Ea$ss, MS = Ea$ms, F = NA, p = NA),
      data.frame(Source = B, rB),
      data.frame(Source = "Error (b)", Df = Eb$df, SS = Eb$ss, MS = Eb$ms, F = NA, p = NA),
      data.frame(Source = paste(A, "x", B), rAB),
      data.frame(Source = "Error (c)", Df = Ec$df, SS = Ec$ss, MS = Ec$ms, F = NA, p = NA))
    names(an) <- c("Source", "Df", "SS", "MS", "F", "p")
    an <- add_total(an, d[[resp]])

    ta <- stats::qt(1 - alpha / 2, Ea$df); tb <- stats::qt(1 - alpha / 2, Eb$df)
    tc <- stats::qt(1 - alpha / 2, Ec$df)

    eA <- new_effect(d, resp, A, Ea$ms, Ea$df, rA$p, rA$F, alpha, label = A)
    eB <- new_effect(d, resp, B, Eb$ms, Eb$df, rB$p, rB$F, alpha, label = B)

    m <- eff_means(d, resp, c(A, B))
    sed_a_at_b <- sqrt(2 * ((b - 1) * Ec$ms + Ea$ms) / (r * b))
    sed_b_at_a <- sqrt(2 * ((a - 1) * Ec$ms + Eb$ms) / (r * a))
    tw_a <- t_weighted((b - 1) * Ec$ms, tc, Ea$ms, ta)
    tw_b <- t_weighted((a - 1) * Ec$ms, tc, Eb$ms, tb)
    cd_a_at_b <- tw_a * sed_a_at_b; cd_b_at_a <- tw_b * sed_b_at_a
    m$Letter_within_A <- ave_letters(m, A, m$Mean, cd_b_at_a)
    m$Letter_within_B <- ave_letters(m, B, m$Mean, cd_a_at_b)
    ## B means at one level of A: their SE(d) mixes Error (b) and Error (c), so
    ## tests that need degrees of freedom use Satterthwaite's approximation
    w <- c((a - 1) * Ec$ms, Eb$ms); wdf <- c(Ec$df, Eb$df)
    df_s <- sum(w)^2 / sum(w^2 / wdf)
    cdl <- cd_name(alpha)
    eAB <- slice_effect(m, c(A, B), slice = A, label = paste(A, "x", B),
      se = sed_b_at_a / sqrt(2), sed = sed_b_at_a, tcrit = tw_b,
      mse = sum(w) / a, df = df_s, alpha = alpha, p = rAB$p, Fv = rAB$F,
      extra = stats::setNames(list(sed_a_at_b, cd_a_at_b, sed_b_at_a, cd_b_at_a), c(
        sprintf("SE(d): two %s means at the same level of %s", A, B),
        sprintf("%s: two %s means at the same level of %s", cdl, A, B),
        sprintf("SE(d): two %s means at the same level of %s", B, A),
        sprintf("%s: two %s means at the same level of %s", cdl, B, A))),
      extra_p = c(NA, rAB$p, NA, rAB$p),
      notes = paste0("The strip-plot interaction uses a weighted t. Letters 'within ", A,
        "' compare ", B, " means at a fixed ", A, "; letters 'within ", B, "' compare ",
        A, " means at a fixed ", B, "."),
      t_mix = list(w = w, df = wdf),
      error_desc = sprintf(paste0("Comparisons of %s means at the same level of %s ",
        "combine Error (b) and Error (c). The critical difference uses the weighted t ",
        "of Gomez &amp; Gomez; tests that need degrees of freedom use Satterthwaite's ",
        "approximation (%s df)."), B, A, fmt(df_s, 1)))

    res$anova <- an
    res$mse <- Ec$ms; res$dfe <- Ec$df
    res$cv <- c("CV(a) (%)" = 100 * sqrt(Ea$ms) / grand,
                "CV(b) (%)" = 100 * sqrt(Eb$ms) / grand,
                "CV(c) (%)" = 100 * sqrt(Ec$ms) / grand)
    res$effects <- stats::setNames(list(eA, eB, eAB), c(A, B, paste0(A, ":", B)))
    res$lm <- stats::lm(stats::as.formula(
      sprintf("%s ~ %s + %s*%s + %s:%s + %s:%s", resp, R, A, B, R, A, R, B)), data = d)
    res$errors <- list(`Error (a)` = Ea, `Error (b)` = Eb, `Error (c)` = Ec)
  }

  ## ----------------------------------------- pooled / combined over envs ----
  if (design %in% c("POOLRCBD", "POOLCRD")) {
    E <- map$env; Tf <- map$treat; R <- if (design == "POOLRCBD") map$rep else NULL
    e <- nlevels(d[[E]]); t <- nlevels(d[[Tf]])
    if (e < 2) stop("Pooled analysis needs at least two environments.")
    if (t < 2) stop("Pooled analysis needs at least two treatments.")
    r <- lay$reps                             # replications in every environment

    ## homogeneity of error variances across environments (Bartlett) -----------
    per_env <- lapply(levels(d[[E]]), function(lv) {
      di <- d[d[[E]] == lv, , drop = FALSE]
      form <- if (design == "POOLRCBD") sprintf("%s ~ %s + %s", resp, R, Tf)
              else sprintf("%s ~ %s", resp, Tf)
      a <- tryCatch(tidy_aov(stats::aov(stats::as.formula(form), data = droplevels(di))),
                    error = function(err) NULL)
      if (is.null(a)) return(c(df = NA, ss = NA))
      c(df = a$Df[a$Source == "Residuals"], ss = a$SS[a$Source == "Residuals"])
    })
    edf <- vapply(per_env, `[`, numeric(1), "df")
    ess <- vapply(per_env, `[`, numeric(1), "ss")
    hom <- bartlett_ms(edf, ess / edf)

    ## combined ANOVA via nested Error strata ---------------------------------
    f <- if (design == "POOLRCBD")
      stats::as.formula(sprintf("%s ~ %s*%s + Error(%s/%s)", resp, E, Tf, E, R))
    else stats::as.formula(sprintf("%s ~ %s*%s + Error(%s)", resp, E, Tf, E))
    fit <- stats::aov(f, data = d)
    tab <- tidy_aovlist(fit)
    st  <- unique(tab$Stratum)
    s_top <- paste0("Error: ", E)
    s_w   <- st[grepl("Within", st)][1]

    get <- function(stratum, src) {
      x <- tab[tab$Stratum == stratum & tab$Source == src, ]
      if (!nrow(x)) return(data.frame(Df = NA, SS = NA, MS = NA, F = NA, p = NA))
      x[, c("Df", "SS", "MS", "F", "p")]
    }
    Erow <- get(s_top, E)                     # Environment (e-1)
    Trow <- get(s_w, Tf)                      # Treatment (t-1)
    ETrow <- get(s_w, paste0(E, ":", Tf))     # E x T
    Eerr <- strat_res(tab, s_w)               # pooled error

    if (design == "POOLRCBD") {
      s_re <- paste0("Error: ", E, ":", R)
      RE <- strat_res(tab, s_re)              # replications within environment
      err_for_env <- RE                       # E tested against R(E)
    } else {
      RE <- NULL
      err_for_env <- Eerr                     # CRD: E tested against pooled error
    }

    ## recompute the F-tests with the CORRECT error terms ---------------------
    fp <- function(ms, dfn, msE, dfd) {
      if (any(is.na(c(ms, msE))) || msE <= 0) return(c(F = NA, p = NA))
      Fv <- ms / msE
      c(F = Fv, p = stats::pf(Fv, dfn, dfd, lower.tail = FALSE))
    }
    Fe  <- fp(Erow$MS,  Erow$Df,  err_for_env$ms, err_for_env$df)
    Ft  <- fp(Trow$MS,  Trow$Df,  ETrow$MS,       ETrow$Df)
    Fet <- fp(ETrow$MS, ETrow$Df, Eerr$ms,        Eerr$df)
    Fre <- if (!is.null(RE)) fp(RE$ms, RE$df, Eerr$ms, Eerr$df) else c(F = NA, p = NA)

    ## ANOVA table ------------------------------------------------------------
    rows <- list(data.frame(Source = paste0("Environment (", E, ")"),
                            Df = Erow$Df, SS = Erow$SS, MS = Erow$MS,
                            F = Fe["F"], p = Fe["p"]))
    if (!is.null(RE))
      rows <- c(rows, list(data.frame(Source = "Replication within environment",
                            Df = RE$df, SS = RE$ss, MS = RE$ms, F = Fre["F"], p = Fre["p"])))
    rows <- c(rows,
      list(data.frame(Source = paste0("Treatment (", Tf, ")"),
                      Df = Trow$Df, SS = Trow$SS, MS = Trow$MS, F = Ft["F"], p = Ft["p"]),
           data.frame(Source = paste0(E, " x ", Tf),
                      Df = ETrow$Df, SS = ETrow$SS, MS = ETrow$MS, F = Fet["F"], p = Fet["p"]),
           data.frame(Source = "Pooled error", Df = Eerr$df, SS = Eerr$ss,
                      MS = Eerr$ms, F = NA, p = NA)))
    an <- do.call(rbind, lapply(rows, function(z) { names(z) <- c("Source","Df","SS","MS","F","p"); z }))
    an <- add_total(an, d[[resp]])
    rownames(an) <- NULL

    ## effects with the correct error term for each comparison ----------------
    eEnv <- new_effect(d, resp, E, err_for_env$ms, err_for_env$df, unname(Fe["p"]),
                       unname(Fe["F"]), alpha, label = paste0("Environment (", E, ")"))
    eT   <- new_effect(d, resp, Tf, ETrow$MS, ETrow$Df, unname(Ft["p"]), unname(Ft["F"]),
                       alpha, label = paste0("Treatment (", Tf, ") - over environments"))

    ## E x T cell means: compare treatments within an environment (pooled error)
    m <- eff_means(d, resp, c(E, Tf))
    t_we   <- stats::qt(1 - alpha / 2, Eerr$df)
    sed_we <- sqrt(2 * Eerr$ms / r)                    # two treatments, same environment
    sed_to <- sqrt(2 * ETrow$MS / (e * r))             # two treatment means over environments
    cd_we  <- t_we * sed_we
    m$Letter_within_env <- ave_letters(m, E, m$Mean, cd_we)
    cdl <- cd_name(alpha)
    eET <- slice_effect(m, c(E, Tf), slice = E, label = paste0(E, " x ", Tf),
      se = sqrt(Eerr$ms / r), sed = sed_we, tcrit = t_we, mse = Eerr$ms, df = Eerr$df,
      alpha = alpha, p = unname(Fet["p"]), Fv = unname(Fet["F"]),
      extra = stats::setNames(list(sed_we, cd_we, sed_to,
                                   stats::qt(1 - alpha / 2, ETrow$Df) * sed_to), c(
        "SE(d): two treatments in the same environment",
        paste0(cdl, ": two treatments in the same environment"),
        "SE(d): two treatment means over all environments",
        paste0(cdl, ": two treatment means over all environments"))),
      extra_p = c(NA, unname(Fet["p"]), NA, unname(Ft["p"])),
      notes = gxe_note(Fet["p"], alpha, E, Tf, cd_we, cdl),
      env = E, is_gxe = TRUE)

    an[["F"]] <- as.numeric(an[["F"]]); an[["p"]] <- as.numeric(an[["p"]])
    res$anova <- an
    res$mse <- Eerr$ms; res$dfe <- Eerr$df
    res$cv  <- c("CV (%)" = 100 * sqrt(Eerr$ms) / grand)
    res$effects <- stats::setNames(list(eEnv, eT, eET), c(E, Tf, paste0(E, ":", Tf)))
    lm_rhs <- if (design == "POOLRCBD") sprintf("%s + %s:%s + %s*%s", E, E, R, E, Tf)
              else sprintf("%s*%s", E, Tf)
    res$lm <- stats::lm(stats::as.formula(paste(resp, "~", lm_rhs)), data = d)
    res$errors <- list(`Pooled error` = Eerr,
                       `Rep within env` = if (!is.null(RE)) RE else NULL)
    res$homogeneity <- hom
    res$pooled <- TRUE
  }

  ## ------------------------------ factorial pooled / combined over envs ------
  if (design %in% c("POOLFRCBD", "POOLFCRD")) {
    E <- map$env; fs <- map$factors; R <- if (design == "POOLFRCBD") map$rep else NULL
    e <- nlevels(d[[E]]); k <- length(fs)
    if (e < 2) stop("Pooled analysis needs at least two environments.")
    if (k < 2) stop("Factorial pooled analysis needs at least two treatment factors.")
    r <- lay$reps                             # replications in every environment

    ## homogeneity of the per-environment error variances (Bartlett) ----------
    per_env <- lapply(levels(d[[E]]), function(lv) {
      di <- droplevels(d[d[[E]] == lv, , drop = FALSE])
      rhs <- paste(fs, collapse = "*")
      form <- if (design == "POOLFRCBD") sprintf("%s ~ %s + %s", resp, R, rhs)
              else sprintf("%s ~ %s", resp, rhs)
      a <- tryCatch(tidy_aov(stats::aov(stats::as.formula(form), data = di)),
                    error = function(err) NULL)
      if (is.null(a)) return(c(df = NA, ss = NA))
      c(df = a$Df[a$Source == "Residuals"], ss = a$SS[a$Source == "Residuals"])
    })
    edf <- vapply(per_env, `[`, numeric(1), "df")
    ess <- vapply(per_env, `[`, numeric(1), "ss")
    hom <- bartlett_ms(edf, ess / edf)

    ## combined ANOVA: E crossed with the full treatment factorial -------------
    trt_rhs <- paste(fs, collapse = "*")
    f <- if (design == "POOLFRCBD")
      stats::as.formula(sprintf("%s ~ %s*(%s) + Error(%s/%s)", resp, E, trt_rhs, E, R))
    else stats::as.formula(sprintf("%s ~ %s*(%s) + Error(%s)", resp, E, trt_rhs, E))
    fit <- stats::aov(f, data = d)
    tab <- tidy_aovlist(fit)
    st  <- unique(tab$Stratum)
    s_w   <- st[grepl("Within", st)][1]
    s_top <- paste0("Error: ", E)

    ## match a source row by the *set* of its factor components ----------------
    find_src <- function(comps) {
      rows <- tab[tab$Stratum == s_w & tab$Source != "Residuals", , drop = FALSE]
      cs <- sort(comps)
      for (i in seq_len(nrow(rows)))
        if (identical(sort(strsplit(rows$Source[i], ":")[[1]]), cs)) return(rows[i, ])
      NULL
    }
    err <- strat_res(tab, s_w)                       # pooled error

    Erow <- tab[tab$Stratum == s_top & tab$Source == E, , drop = FALSE]
    if (design == "POOLFRCBD") {
      RE <- strat_res(tab, paste0("Error: ", E, ":", R)); err_env <- RE
    } else { RE <- NULL; err_env <- err }

    fp <- function(ms, dfn, msE, dfd) {
      if (any(is.na(c(ms, msE))) || msE <= 0) return(c(F = NA, p = NA))
      Fv <- ms / msE; c(F = Fv, p = stats::pf(Fv, dfn, dfd, lower.tail = FALSE))
    }
    Fe  <- fp(Erow$MS[1], Erow$Df[1], err_env$ms, err_env$df)
    Fre <- if (!is.null(RE)) fp(RE$ms, RE$df, err$ms, err$df) else c(F = NA, p = NA)

    ## every treatment effect (main effects + all interactions) ---------------
    subsets <- unlist(lapply(1:k, function(mm) utils::combn(fs, mm, simplify = FALSE)),
                      recursive = FALSE)
    trt_block <- list(); ext_block <- list(); effects <- list()
    for (S in subsets) {
      Trow  <- find_src(S)
      ETrow <- find_src(c(E, S))
      if (is.null(Trow) || is.null(ETrow)) next
      Ft  <- fp(Trow$MS,  Trow$Df,  ETrow$MS, ETrow$Df)     # treatment vs E x treatment
      Fet <- fp(ETrow$MS, ETrow$Df, err$ms,   err$df)       # E x treatment vs pooled error
      lbl <- paste(S, collapse = " x ")
      trt_block[[length(trt_block) + 1]] <- data.frame(
        Source = lbl, Df = Trow$Df, SS = Trow$SS, MS = Trow$MS, F = Ft["F"], p = Ft["p"])
      ext_block[[length(ext_block) + 1]] <- data.frame(
        Source = paste0(E, " x ", lbl), Df = ETrow$Df, SS = ETrow$SS, MS = ETrow$MS,
        F = Fet["F"], p = Fet["p"])
      effects[[paste(S, collapse = ":")]] <- new_effect(
        d, resp, S, ETrow$MS, ETrow$Df, unname(Ft["p"]), unname(Ft["F"]), alpha,
        label = lbl)
    }

    ## assemble the ANOVA table -----------------------------------------------
    anrows <- list(data.frame(Source = paste0("Environment (", E, ")"),
                              Df = Erow$Df[1], SS = Erow$SS[1], MS = Erow$MS[1],
                              F = Fe["F"], p = Fe["p"]))
    if (!is.null(RE))
      anrows <- c(anrows, list(data.frame(Source = "Replication within environment",
                              Df = RE$df, SS = RE$ss, MS = RE$ms, F = Fre["F"], p = Fre["p"])))
    anrows <- c(anrows, trt_block, ext_block,
                list(data.frame(Source = "Pooled error", Df = err$df, SS = err$ss,
                                MS = err$ms, F = NA, p = NA)))
    an <- do.call(rbind, lapply(anrows, function(z) {
      names(z) <- c("Source","Df","SS","MS","F","p"); z }))
    an <- add_total(an, d[[resp]])
    an[["F"]] <- as.numeric(an[["F"]]); an[["p"]] <- as.numeric(an[["p"]])
    rownames(an) <- NULL

    ## Environment effect, and the E x (full treatment) stability table --------
    eEnv <- new_effect(d, resp, E, err_env$ms, err_env$df, unname(Fe["p"]),
                       unname(Fe["F"]), alpha, label = paste0("Environment (", E, ")"))
    ETfull <- find_src(c(E, fs))
    Ffull <- if (!is.null(ETfull)) fp(ETfull$MS, ETfull$Df, err$ms, err$df) else c(F = NA, p = NA)
    m <- eff_means(d, resp, c(E, fs))
    t_we   <- stats::qt(1 - alpha / 2, err$df)
    sed_we <- sqrt(2 * err$ms / r)
    cd_we  <- t_we * sed_we
    m$Letter_within_env <- ave_letters(m, E, m$Mean, cd_we)
    eETfull <- slice_effect(m, c(E, fs), slice = E,
      label = paste0(E, " x ", paste(fs, collapse = " x ")),
      se = sqrt(err$ms / r), sed = sed_we, tcrit = t_we, mse = err$ms, df = err$df,
      alpha = alpha, p = unname(Ffull["p"]), Fv = unname(Ffull["F"]), extra = NULL,
      notes = gxe_note(Ffull["p"], alpha, E, "treatment", cd_we, cd_name(alpha), factorial = TRUE),
      env = E, is_gxe = TRUE)

    res$effects <- c(effects, stats::setNames(list(eEnv), E),
                     stats::setNames(list(eETfull), paste0(E, ":", paste(fs, collapse = ":"))))
    res$anova <- an
    res$mse <- err$ms; res$dfe <- err$df
    res$cv  <- c("CV (%)" = 100 * sqrt(err$ms) / grand)
    res$facs <- fs                                   # means section loops treatment factors
    lm_rhs <- if (design == "POOLFRCBD")
      sprintf("%s + %s:%s + %s*(%s)", E, E, R, E, trt_rhs) else sprintf("%s*(%s)", E, trt_rhs)
    res$lm <- stats::lm(stats::as.formula(paste(resp, "~", lm_rhs)), data = d)
    res$errors <- list(`Pooled error` = err,
                       `Rep within env` = if (!is.null(RE)) RE else NULL)
    res$homogeneity <- hom
    res$pooled <- TRUE
  }
  res$resid  <- stats::residuals(res$lm)
  res$fitted <- stats::fitted(res$lm)
  res$X      <- stats::model.matrix(res$lm)   # for the Box-Cox profile
  res
}

## "C.D. (5%)", at whatever level was chosen
cd_name <- function(alpha) sprintf("C.D. (%s%%)", pct(alpha))

## The note under a pooled analysis's environment x treatment table. A
## non-significant interaction is reported as insufficient evidence, not as
## proof that the treatments behave alike everywhere. In a factorial this is
## the interaction with the full treatment combination only; the interactions
## of the environment with each factor are separate rows of the ANOVA.
gxe_note <- function(p, alpha, E, what, cd, cdl, factorial = FALSE) {
  if (!is.na(p) && p < alpha)
    paste0("The ", E, " x ", what, " interaction is significant at the ", pct(alpha),
      "% level: ", what, " performance is shown separately for each environment below. ",
      "Letters compare means within one environment only (pooled error, ", cdl, " = ",
      fmt(cd), ").")
  else
    paste0("The ", E, " x ", what, " interaction is <b>not significant</b> at the ",
      pct(alpha), "% level (NS). There is insufficient evidence that the differences ",
      "between ", if (factorial) "treatment combinations" else "treatments",
      " change from one environment to another, so the pooled tables above summarise ",
      "them and the individual environment cells are not compared.",
      if (factorial) paste0(" The interactions of ", E, " with each single factor are ",
                            "tested separately in the ANOVA table.") else "")
}

## columns the tables of means add, which a design factor must not be called
RESERVED_COLS <- c("Mean", "N", "SD", "SE", "Raw_mean", "Mean_bt", ".se",
                   "Letter", "Letter_within_MP", "Letter_within_SP",
                   "Letter_within_A", "Letter_within_B", "Letter_within_env")

## Is the layout one DOEpro can analyse exactly, and is it balanced? Designs
## with a single error term (CRD, RCBD, Latin square, the factorials) cope with
## unequal replication through adjusted means, as long as every treatment
## combination was observed. Designs with several error strata (split plot,
## strip plot, the pooled analyses) do not: with a plot missing the strata are
## no longer orthogonal, aov() moves treatment sums of squares into the
## replication strata where they silently vanish, and an exact analysis needs a
## mixed model fitted by REML, which base R does not provide. Those designs
## must be complete; the message says what is missing instead of printing a
## wrong ANOVA.
check_layout <- function(d, design, map, facs, blks) {
  pooled <- design %in% c("POOLRCBD", "POOLCRD", "POOLFRCBD", "POOLFCRD")
  trt <- if (pooled) setdiff(facs, map$env) else facs
  one <- c(facs, blks)[vapply(c(facs, blks), function(v) nlevels(d[[v]]) < 2, logical(1))]
  if (length(one))
    stop(sprintf(paste0("%s %s only one level in the complete rows of data, so there is ",
      "nothing to compare. Check that the right column is mapped, and that the response ",
      "is not missing for every other level."), join_and(sprintf("'%s'", one)),
      if (length(one) == 1) "has" else "have"), call. = FALSE)
  cells <- table(d[trt])
  if (length(trt) > 1 && any(cells == 0)) {
    empty <- as.data.frame(cells, stringsAsFactors = FALSE)
    empty <- empty[empty$Freq == 0, trt, drop = FALSE]
    stop(sprintf(paste0("Every combination of %s needs at least one observation, but ",
      "there are none for %s. Without them the main effects cannot be separated from ",
      "the interaction. Check the data, or leave out the factor with the gap."),
      join_and(trt), list_cells(empty)), call. = FALSE)
  }

  if (design %in% c("SPLIT", "STRIP")) {
    v <- c(map$rep, map$main, map$sub)
    tab <- as.data.frame(table(d[v]), stringsAsFactors = FALSE)
    miss <- tab[tab$Freq == 0, v, drop = FALSE]
    dup  <- tab[tab$Freq > 1, v, drop = FALSE]
    if (nrow(miss) || nrow(dup))
      stop(paste0(if (design == "SPLIT") "A split-plot" else "A strip-plot",
        " analysis needs exactly one observation for every combination of ",
        join_and(v), ". ",
        if (nrow(miss)) sprintf("Missing: %s. ", list_cells(miss)) else "",
        if (nrow(dup)) sprintf("More than one row: %s. ", list_cells(dup)) else "",
        "With a plot missing or repeated, the design's separate error terms can no ",
        "longer be estimated exactly, and DOEpro does not estimate missing plots. ",
        "Remove the incomplete replication and analyse the complete ones",
        if (nrow(dup)) ", and average repeated measurements of one plot into a single value" else "",
        "."), call. = FALSE)
    return(list(balanced = TRUE, reps = nlevels(d[[map$rep]]),
                n_range = rep(nlevels(d[[map$rep]]), 2)))
  }

  if (design %in% c("POOLRCBD", "POOLFRCBD")) {
    E <- map$env; R <- map$rep
    probs <- character(0); nrep <- integer(0)
    for (lv in levels(d[[E]])) {
      di <- d[d[[E]] == lv, , drop = FALSE]
      di[[R]] <- droplevels(di[[R]])
      tab <- as.data.frame(table(di[c(R, trt)]), stringsAsFactors = FALSE)
      miss <- tab[tab$Freq == 0, c(R, trt), drop = FALSE]
      dup  <- tab[tab$Freq > 1, c(R, trt), drop = FALSE]
      if (nrow(miss)) probs <- c(probs, sprintf("in %s, %s %s missing", lv, list_cells(miss),
                                                if (nrow(miss) == 1) "is" else "are"))
      if (nrow(dup)) probs <- c(probs, sprintf("in %s, %s %s more than one row", lv,
                                               list_cells(dup), if (nrow(dup) == 1) "has" else "have"))
      nrep[[lv]] <- nlevels(di[[R]])
    }
    if (length(unique(nrep)) > 1)
      probs <- c(probs, paste("the number of replications differs between environments (",
        paste(sprintf("%s %d", names(nrep), nrep), collapse = ", "), ")", sep = ""))
    if (length(probs))
      stop(paste0("A combined analysis over environments needs a complete RCBD in every ",
        "environment, with the same number of replications in each. Here ",
        paste(head_more(probs, 3), collapse = "; "), ". With incomplete or unequal replication the ",
        "treatments cannot be tested exactly against their interaction with the ",
        "environment, so DOEpro stops rather than print approximate results. Remove ",
        "the incomplete replication(s), or analyse each environment on its own as an ",
        "RCBD, which does handle a missing plot."), call. = FALSE)
    return(list(balanced = TRUE, reps = nrep[[1]], n_range = rep(nrep[[1]], 2)))
  }

  if (design %in% c("POOLCRD", "POOLFCRD")) {
    v <- c(map$env, trt)
    tab <- as.data.frame(table(d[v]), stringsAsFactors = FALSE)
    if (length(unique(tab$Freq)) > 1) {
      common <- as.integer(names(which.max(table(tab$Freq))))
      odd <- tab[tab$Freq != common, , drop = FALSE]
      lab <- sprintf("%s has %s", do.call(paste, c(lapply(odd[v], as.character), sep = " x ")),
                     ifelse(odd$Freq == 0, "none", as.character(odd$Freq)))
      stop(sprintf(paste0("A combined analysis over environments needs the same number of ",
        "observations in every environment x treatment cell. Most cells have %d, but %s. ",
        "With unequal numbers the treatments cannot be tested exactly against their ",
        "interaction with the environment, so DOEpro stops rather than print approximate ",
        "results. Analyse each environment on its own as a CRD, which does handle unequal ",
        "replication."), common, join_and(head_more(lab))), call. = FALSE)
    }
    return(list(balanced = TRUE, reps = tab$Freq[1], n_range = rep(tab$Freq[1], 2)))
  }

  ## single error term: balanced when every cell of the full layout, blocks
  ## included, holds the same number of plots. An RCBD whose treatments are each
  ## present three times but in different blocks is not balanced: its plain
  ## means are pulled by the block effects.
  full <- if (design == "LSD")
    list(table(d[c(map$row, map$col)]), table(d[c(map$row, trt)]), table(d[c(map$col, trt)]))
  else list(table(d[c(blks, trt)]))
  balanced <- all(vapply(full, function(x) all(x == x[1]) && x[1] > 0, logical(1)))
  if (design == "LSD") balanced <- balanced && all(full[[1]] == 1L)
  list(balanced = balanced, reps = if (balanced) as.vector(cells)[1] else NA,
       n_range = range(cells))
}

## "R2 x I2 x V3, R3 x I1 x V2 and 4 more", from rows of factor levels
list_cells <- function(cells)
  join_and(head_more(do.call(paste, c(lapply(cells, as.character), sep = " x "))))

## at most five items, then "and N more"
head_more <- function(x, k = 5)
  if (length(x) > k) c(x[seq_len(k)], sprintf("%d more", length(x) - k)) else x

## "a", "a and b", "a, b and c"
join_and <- function(x)
  if (length(x) < 2) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

## A Total row that is the total sum of squares of the data, not the sum of the
## rows above it, so that nothing an error stratum dropped can go unnoticed.
add_total <- function(an, y) {
  rbind(an, data.frame(Source = "Total", Df = length(y) - 1, SS = sum((y - mean(y))^2),
                       MS = NA, F = NA, p = NA, check.names = FALSE))
}

## With unequal replication the sequential (Type I) sums of squares depend on
## the order of the terms and test hypotheses about weighted means. Replace each
## term's row with its Type III test: the term dropped from the full model fitted
## with sum-to-zero contrasts. That tests equality of the adjusted means the
## tables report, so the F-test that gates the letters and the means it gates
## are about the same thing. The rows then no longer add up to the total.
type3_anova <- function(an, form, d, fvars) {
  ctr <- stats::setNames(rep(list("contr.sum"), length(fvars)), fvars)
  fit <- stats::lm(form, data = d, contrasts = ctr)
  tl <- attr(stats::terms(fit), "term.labels")
  dr <- stats::drop1(fit, scope = tl, test = "F")
  for (x in tl) {
    i <- an$Source == x
    if (!any(i)) next
    an$SS[i] <- dr[x, "Sum of Sq"]
    an$MS[i] <- dr[x, "Sum of Sq"] / an$Df[i]
    an$F[i]  <- dr[x, "F value"]
    an$p[i]  <- dr[x, "Pr(>F)"]
  }
  an
}

## letters computed separately inside each level of `by`
ave_letters <- function(m, by, mu, cd) {
  out <- character(nrow(m))
  for (lv in unique(m[[by]])) {
    idx <- which(m[[by]] == lv)
    out[idx] <- cld_lsd(mu[idx], cd)
  }
  out
}

## every column that may carry compact-letter groupings
LETTER_COLS <- c("Letter", "Letter_within_MP", "Letter_within_SP",
                 "Letter_within_A", "Letter_within_B", "Letter_within_env")

## Protected mean separation. Returns the effect's table of means with the
## grouping letters blanked whenever the effect's F-test is not significant at
## the chosen level. This keeps the letters in step with the "C.D. = NS" in the
## footers: letters are shown only when the F-test justifies pairwise
## comparison, so an a-e sequence can never sit beneath a non-significant F.
gate_letters <- function(e) {
  m <- e$means
  if (!effect_sig(e))
    for (col in intersect(LETTER_COLS, names(m))) m[[col]] <- rep("", nrow(m))
  m
}

## is the effect significant at the chosen level (so its letters should be shown)?
effect_sig <- function(e) !is.na(e$p) && e$p < e$alpha

## does a table of means carry any non-empty grouping letter?
has_groups <- function(m) {
  cols <- intersect(LETTER_COLS, names(m))
  length(cols) > 0 && any(nzchar(unlist(m[cols])))
}
