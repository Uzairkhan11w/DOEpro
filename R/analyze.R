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
#' the two mixed comparisons use a Satterthwaite-weighted \emph{t}. In a pooled
#' (combined) analysis over environments, each treatment effect is tested
#' against its own interaction with the environment, and each environment by
#' treatment interaction against the pooled error.
#'
#' @param d A data frame in long format: one row per plot, with columns for the
#'   design factors and the response.
#' @param design The design code. One of the values of \code{\link{DESIGNS}},
#'   for example \code{"RCBD"}, \code{"SPLIT"} or \code{"POOLFRCBD"}.
#' @param map A named list mapping roles to column names of \code{d}. Always
#'   needs \code{response}. Then, by design: \code{treat} (CRD, RCBD, LSD);
#'   \code{block} (RCBD, factorial RCBD); \code{row}, \code{col} (LSD);
#'   \code{factors}, a character vector of two to four column names (factorial);
#'   \code{rep}, \code{main}, \code{sub} (split and strip plots); \code{env}
#'   together with \code{treat} or \code{factors}, and \code{rep} for an RCBD
#'   base (pooled designs).
#' @param alpha The significance level for the critical differences and the
#'   grouping letters. Defaults to 0.05.
#'
#' @return A list with, among others: \code{anova} (the analysis of variance
#'   table), \code{effects} (one entry per effect, each holding the table of
#'   means with \code{sem}, \code{sed}, \code{cd5}, \code{cd1} and the grouping
#'   letters), \code{mse} and \code{dfe} (the error mean square and its degrees
#'   of freedom), \code{cv}, \code{grand}, \code{resid} and \code{lm}.
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

  keep <- unique(c(resp, facs, blks))
  d <- d[stats::complete.cases(d[, keep, drop = FALSE]), keep, drop = FALSE]
  if (nrow(d) < 3) stop("Not enough complete rows to analyse.")
  for (v in c(facs, blks)) d[[v]] <- factor(d[[v]])

  cells <- table(d[facs])
  balanced <- length(unique(as.vector(cells))) == 1L && all(cells > 0)
  grand <- mean(d[[resp]])
  res <- list(design = design, resp = resp, data = d, facs = facs, blks = blks,
              alpha = alpha, grand = grand, balanced = balanced,
              reps = if (balanced) as.vector(cells)[1] else NA)

  ## ------------------------------------------------- classic / factorial ----
  if (design %in% c("CRD", "RCBD", "LSD", "FCRD", "FRCBD")) {
    rhs <- paste(c(blks, paste(facs, collapse = "*")), collapse = " + ")
    fit <- stats::aov(stats::as.formula(paste(resp, "~", rhs)), data = d)
    an  <- tidy_aov(fit)
    mse <- an$MS[an$Source == "Residuals"]
    dfe <- an$Df[an$Source == "Residuals"]
    if (dfe < 1) stop("Zero error degrees of freedom - you need replication.")

    effs <- list()
    for (k in seq_along(facs)) {
      for (v in utils::combn(facs, k, simplify = FALSE)) {
        lab <- paste(v, collapse = ":")
        row <- an[an$Source == lab, ]
        e <- new_effect(d, resp, v, mse, dfe,
                        if (nrow(row)) row$p[1] else NA,
                        if (nrow(row)) row$F[1] else NA, alpha,
                        label = paste(v, collapse = " x "))
        effs[[lab]] <- e
      }
    }
    an_disp <- an
    an_disp <- rbind(an_disp, data.frame(Source = "Total", Df = sum(an$Df, na.rm = TRUE),
                     SS = sum(an$SS, na.rm = TRUE), MS = NA, F = NA, p = NA, check.names = FALSE))
    res$anova <- an_disp
    res$mse <- mse; res$dfe <- dfe
    res$cv <- c("CV (%)" = 100 * sqrt(mse) / grand)
    res$effects <- effs
    res$lm <- stats::lm(stats::as.formula(paste(resp, "~", rhs)), data = d)
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
    an <- rbind(an, data.frame(Source = "Total", Df = sum(an$Df, na.rm = TRUE),
                               SS = sum(an$SS, na.rm = TRUE), MS = NA, F = NA, p = NA))
    names(an) <- c("Source", "Df", "SS", "MS", "F", "p")

    ta <- stats::qt(0.975, Ea$df); tb <- stats::qt(0.975, Eb$df)
    ta1 <- stats::qt(0.995, Ea$df); tb1 <- stats::qt(0.995, Eb$df)

    eA <- new_effect(d, resp, A, Ea$ms, Ea$df, rA$p, rA$F, alpha, label = A)
    eB <- new_effect(d, resp, B, Eb$ms, Eb$df, rB$p, rB$F, alpha, label = B)

    ## interaction: two different comparisons
    m <- eff_means(d, resp, c(A, B))
    sed_b <- sqrt(2 * Eb$ms / r)                                   # B within same A
    sed_a <- sqrt(2 * ((b - 1) * Eb$ms + Ea$ms) / (r * b))         # A at same B
    tw  <- t_weighted((b - 1) * Eb$ms, tb, Ea$ms, ta)
    tw1 <- t_weighted((b - 1) * Eb$ms, tb1, Ea$ms, ta1)
    cd_b <- tb * sed_b; cd_b1 <- tb1 * sed_b
    cd_a <- tw * sed_a; cd_a1 <- tw1 * sed_a
    m$Letter_within_MP <- ave_letters(m, A, m$Mean, cd_b)
    m$Letter_within_SP <- ave_letters(m, B, m$Mean, cd_a)
    eAB <- list(label = paste(A, "x", B), vars = c(A, B), means = m,
                n_per_mean = r, mse = Eb$ms, df = Eb$df,
                sem = sqrt(Eb$ms / r), sed = sed_b, cd5 = cd_b, cd1 = cd_b1,
                p = rAB$p, F = rAB$F,
                extra = list(
                  `SEd: two SUB-plot means at same main plot`   = sed_b,
                  `CD 5%: two SUB-plot means at same main plot` = cd_b,
                  `SEd: two MAIN-plot means at same sub plot`   = sed_a,
                  `CD 5%: two MAIN-plot means at same sub plot` = cd_a),
                notes = paste0("Letters 'within MP' compare sub-plot means inside ",
                  "one main plot (CD = ", fmt(cd_b), "). Letters 'within SP' compare ",
                  "main-plot means at one sub-plot level (CD = ", fmt(cd_a),
                  ", Satterthwaite t = ", fmt(tw, 2), ")."))

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
    an <- rbind(an, data.frame(Source = "Total", Df = sum(an$Df, na.rm = TRUE),
                               SS = sum(an$SS, na.rm = TRUE), MS = NA, F = NA, p = NA))
    names(an) <- c("Source", "Df", "SS", "MS", "F", "p")

    ta <- stats::qt(0.975, Ea$df); tb <- stats::qt(0.975, Eb$df)
    tc <- stats::qt(0.975, Ec$df)
    ta1 <- stats::qt(.995, Ea$df); tb1 <- stats::qt(.995, Eb$df); tc1 <- stats::qt(.995, Ec$df)

    eA <- new_effect(d, resp, A, Ea$ms, Ea$df, rA$p, rA$F, alpha, label = A)
    eB <- new_effect(d, resp, B, Eb$ms, Eb$df, rB$p, rB$F, alpha, label = B)

    m <- eff_means(d, resp, c(A, B))
    sed_a_at_b <- sqrt(2 * ((b - 1) * Ec$ms + Ea$ms) / (r * b))
    sed_b_at_a <- sqrt(2 * ((a - 1) * Ec$ms + Eb$ms) / (r * a))
    tw_a  <- t_weighted((b - 1) * Ec$ms, tc, Ea$ms, ta)
    tw_b  <- t_weighted((a - 1) * Ec$ms, tc, Eb$ms, tb)
    tw_a1 <- t_weighted((b - 1) * Ec$ms, tc1, Ea$ms, ta1)
    tw_b1 <- t_weighted((a - 1) * Ec$ms, tc1, Eb$ms, tb1)
    cd_a_at_b <- tw_a * sed_a_at_b; cd_b_at_a <- tw_b * sed_b_at_a
    m$Letter_within_A <- ave_letters(m, A, m$Mean, cd_b_at_a)
    m$Letter_within_B <- ave_letters(m, B, m$Mean, cd_a_at_b)
    eAB <- list(label = paste(A, "x", B), vars = c(A, B), means = m,
                n_per_mean = r, mse = Ec$ms, df = Ec$df,
                sem = sed_b_at_a / sqrt(2), sed = sed_b_at_a,
                cd5 = cd_b_at_a, cd1 = tw_b1 * sed_b_at_a,
                p = rAB$p, F = rAB$F,
                extra = list(
                  `SEd: two A means at same level of B` = sed_a_at_b,
                  `CD 5%: two A means at same level of B` = cd_a_at_b,
                  `SEd: two B means at same level of A` = sed_b_at_a,
                  `CD 5%: two B means at same level of A` = cd_b_at_a),
                notes = paste0("Strip-plot interaction uses Satterthwaite-weighted t. ",
                  "Letters 'within ", A, "' compare ", B, " means at a fixed ", A,
                  "; letters 'within ", B, "' compare ", A, " means at a fixed ", B, "."))

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
    if (design == "POOLRCBD") {
      r <- nlevels(d[[R]])
      f <- stats::as.formula(sprintf("%s ~ %s*%s + Error(%s/%s)", resp, E, Tf, E, R))
    } else {
      r <- max(table(d[[E]], d[[Tf]]))
      f <- stats::as.formula(sprintf("%s ~ %s*%s + Error(%s)", resp, E, Tf, E))
    }
    fit <- stats::aov(f, data = d)
    tab <- tidy_aovlist(fit)
    st  <- unique(tab$Stratum)
    s_top <- st[grepl(paste0(":?", E, "$"), st) & !grepl(":", sub(paste0("Error: ", E), "", st))][1]
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
    an <- rbind(an, data.frame(Source = "Total", Df = sum(an$Df, na.rm = TRUE),
                               SS = sum(an$SS, na.rm = TRUE), MS = NA, F = NA, p = NA))
    rownames(an) <- NULL

    ## effects with the correct error term for each comparison ----------------
    eEnv <- new_effect(d, resp, E, err_for_env$ms, err_for_env$df, Fe["p"], Fe["F"],
                       alpha, label = paste0("Environment (", E, ")"))
    eT   <- new_effect(d, resp, Tf, ETrow$MS, ETrow$Df, Ft["p"], Ft["F"],
                       alpha, label = paste0("Treatment (", Tf, ") - over environments"))

    ## E x T cell means: compare treatments within an environment (pooled error)
    m <- eff_means(d, resp, c(E, Tf))
    sed_we <- sqrt(2 * Eerr$ms / r)                    # two treatments, same environment
    sed_to <- sqrt(2 * ETrow$MS / (e * r))             # two treatment means over environments
    cd_we  <- stats::qt(0.975, Eerr$df) * sed_we
    cd_we1 <- stats::qt(0.995, Eerr$df) * sed_we
    m$Letter_within_env <- ave_letters(m, E, m$Mean, cd_we)
    eET <- list(label = paste0(E, " x ", Tf), vars = c(E, Tf), means = m,
                n_per_mean = r, mse = Eerr$ms, df = Eerr$df,
                sem = sqrt(Eerr$ms / r), sed = sed_we, cd5 = cd_we, cd1 = cd_we1,
                p = Fet["p"], F = Fet["F"], env = E, is_gxe = TRUE,
                extra = list(
                  `SEd: two treatments in the same environment`     = sed_we,
                  `CD 5%: two treatments in the same environment`   = cd_we,
                  `SEd: two treatment means over all environments`  = sed_to,
                  `CD 5%: two treatment means over all environments`=
                    stats::qt(0.975, ETrow$Df) * sed_to),
                notes = if (!is.na(Fet["p"]) && Fet["p"] < 0.05)
                  paste0("The ", E, " x ", Tf, " interaction is significant: treatment ",
                    "performance is shown separately for each environment below. Letters ",
                    "compare treatment means within one environment (pooled error, CD = ",
                    fmt(cd_we), ").")
                else
                  paste0("The ", E, " x ", Tf, " interaction is <b>not significant</b> (NS): the ",
                    "treatment ranking is consistent across environments, so the pooled ",
                    "Treatment table above applies to every environment and the differences ",
                    "among individual cells are within experimental error."))

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
    if (design == "POOLFRCBD") {
      r <- nlevels(d[[R]])
      f <- stats::as.formula(sprintf("%s ~ %s*(%s) + Error(%s/%s)", resp, E, trt_rhs, E, R))
    } else {
      r <- max(table(interaction(d[fs], drop = TRUE), d[[E]]))
      f <- stats::as.formula(sprintf("%s ~ %s*(%s) + Error(%s)", resp, E, trt_rhs, E))
    }
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
        d, resp, S, ETrow$MS, ETrow$Df, Ft["p"], Ft["F"], alpha,
        label = if (length(S) == 1) lbl else paste("Interaction:", lbl))
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
    an <- rbind(an, data.frame(Source = "Total", Df = sum(an$Df, na.rm = TRUE),
                               SS = sum(an$SS, na.rm = TRUE), MS = NA, F = NA, p = NA))
    an[["F"]] <- as.numeric(an[["F"]]); an[["p"]] <- as.numeric(an[["p"]])
    rownames(an) <- NULL

    ## Environment effect, and the E x (full treatment) stability table --------
    eEnv <- new_effect(d, resp, E, err_env$ms, err_env$df, Fe["p"], Fe["F"], alpha,
                       label = paste0("Environment (", E, ")"))
    ETfull <- find_src(c(E, fs))
    m <- eff_means(d, resp, c(E, fs))
    sed_we <- sqrt(2 * err$ms / r)
    cd_we  <- stats::qt(0.975, err$df) * sed_we
    cd_we1 <- stats::qt(0.995, err$df) * sed_we
    m$Letter_within_env <- ave_letters(m, E, m$Mean, cd_we)
    eETfull <- list(label = paste0(E, " x ", paste(fs, collapse = " x ")),
                    vars = c(E, fs), means = m, n_per_mean = r,
                    mse = err$ms, df = err$df, sem = sqrt(err$ms / r),
                    sed = sed_we, cd5 = cd_we, cd1 = cd_we1, env = E, is_gxe = TRUE,
                    p = if (!is.null(ETfull)) fp(ETfull$MS, ETfull$Df, err$ms, err$df)["p"] else NA,
                    F = if (!is.null(ETfull)) ETfull$MS / err$ms else NA,
                    notes = {
                      pv <- if (!is.null(ETfull)) fp(ETfull$MS, ETfull$Df, err$ms, err$df)["p"] else NA
                      if (!is.na(pv) && pv < 0.05)
                        paste0("The ", E, " x treatment interaction is significant: the treatment ",
                          "combinations are shown separately for each environment below. Letters ",
                          "compare cells within one environment (pooled error, CD = ", fmt(cd_we), ").")
                      else
                        paste0("The ", E, " x treatment interaction is <b>not significant</b> (NS): ",
                          "the treatment effects are consistent across environments and are ",
                          "summarised in the pooled tables above. Differences among individual ",
                          "environment cells are within experimental error.")
                    })

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
## 5%. This keeps the letters in step with the "CD = NS" shown in the footers:
## letters are displayed only when the omnibus F justifies pairwise comparison,
## so an a-e sequence can never sit beneath a non-significant F.
gate_letters <- function(e) {
  m <- e$means
  if (is.na(e$p) || e$p >= 0.05)
    for (col in intersect(LETTER_COLS, names(m))) m[[col]] <- rep("", nrow(m))
  m
}

## is the effect significant at 5% (so its letters should be shown)?
effect_sig <- function(e) !is.na(e$p) && e$p < 0.05

## does a table of means carry any non-empty grouping letter?
has_groups <- function(m) {
  cols <- intersect(LETTER_COLS, names(m))
  length(cols) > 0 && any(nzchar(unlist(m[cols])))
}
