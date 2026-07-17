###############################################################################
##  POST-HOC  (agricolae)
###############################################################################
## Post-hoc multiple comparisons, computed from the effect's own error term.
## For split / strip plots e$sed is already the SEd of the comparison that the
## effect's letters refer to, so every test inherits the correct error stratum.
PH_METHODS <- c("LSD (Fisher's protected)", "LSD (Bonferroni-adjusted)",
                "Tukey HSD", "Duncan's DMRT", "Student-Newman-Keuls", "Scheffe")

posthoc <- function(res, effect, method, alpha = 0.05) {
  e  <- res$effects[[effect]]
  m  <- e$means
  mu <- m$Mean
  k  <- length(mu)
  if (k < 2) stop("This effect has fewer than two means.")
  df  <- e$df
  sed <- e$sed              # SE of a difference between two means
  sbar <- sed / sqrt(2)     # SE of a single mean, as used by the studentized range
  nC  <- k * (k - 1) / 2

  ranges <- NULL
  cd <- switch(method,
    "LSD (Fisher's protected)"  = stats::qt(1 - alpha / 2, df) * sed,
    "LSD (Bonferroni-adjusted)" = stats::qt(1 - alpha / (2 * nC), df) * sed,
    "Tukey HSD"                 = stats::qtukey(1 - alpha, k, df) * sbar,
    "Scheffe"                   = sqrt((k - 1) * stats::qf(1 - alpha, k - 1, df)) * sed,
    "Student-Newman-Keuls"      = function(p) stats::qtukey(1 - alpha, p, df) * sbar,
    "Duncan's DMRT"             = function(p) {
        ap <- 1 - (1 - alpha)^(p - 1)          # Duncan's protection level
        stats::qtukey(1 - ap, p, df) * sbar
      },
    stop("Unknown method"))

  if (is.function(cd))
    ranges <- data.frame(`Means apart (p)` = 2:k,
                         `Critical range` = vapply(2:k, cd, numeric(1)),
                         check.names = FALSE)

  lets <- cld_lsd(mu, cd)
  g <- data.frame(Treatment = apply(m[e$vars], 1, paste, collapse = " : "),
                  Mean = mu, n = m$N, Group = lets,
                  row.names = NULL, check.names = FALSE)
  g <- g[order(-g$Mean), ]

  st <- data.frame(
    Item  = c("Effect", "Method", "Error mean square", "Error df",
              "SE of a mean (SEm)", "SE of a difference (SEd)",
              "Number of means (k)", "alpha",
              if (is.function(cd)) "Critical difference" else "Critical difference (CD)"),
    Value = c(e$label, method, fmt(e$mse, 4), as.character(df),
              fmt(sbar), fmt(sed), as.character(k), fmt(alpha, 2),
              if (is.function(cd)) "varies with p - see the table of critical ranges"
              else fmt(cd)),
    check.names = FALSE)

  list(groups = g, stats = st, ranges = ranges, note = e$note)
}
