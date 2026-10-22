###############################################################################
##  PLOTS
###############################################################################
## One look for every plot in the app: white ground, light horizontal guides,
## the brand's navy for text and its blue for single series, and a palette
## that colour-blind readers can tell apart for groups. Colour is kept for
## what it means: red marks rows the checks pick out, nothing else.
SLX_BLUE <- "#3B7DD8"
SLX_NAVY <- "#14284B"
SLX_FILL <- "#BBD3F2"
SLX_RED  <- "#C0392B"
## Okabe and Ito's colour-blind-safe palette, starting with blues
SLX_PAL <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#6B6B6B", "#F0E442")

## the colours for k groups: the palette while it lasts, evenly spaced hues after
slx_pal <- function(k) if (k <= length(SLX_PAL)) SLX_PAL[seq_len(k)] else grDevices::hcl.colors(k, "Dark 3")

theme_doe <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#E8EDF5", linewidth = 0.45),
          axis.line = element_line(colour = "#9AA9C0", linewidth = 0.45),
          axis.ticks = element_line(colour = "#9AA9C0", linewidth = 0.45),
          axis.text = element_text(colour = "#3D4B63"),
          axis.title = element_text(colour = "#1C2B4A"),
          legend.position = "bottom",
          legend.title = element_text(colour = "#1C2B4A", face = "bold", size = rel(0.85)),
          legend.text = element_text(colour = "#3D4B63", size = rel(0.85)),
          plot.title = element_text(face = "bold", size = rel(1.02), colour = SLX_NAVY, margin = margin(b = 4)),
          plot.subtitle = element_text(colour = "#5B6B86", size = rel(0.85), margin = margin(b = 6)),
          plot.caption = element_text(colour = "#5B6B86", size = rel(0.68), hjust = 0, lineheight = 1.15),
          plot.title.position = "plot", plot.caption.position = "plot",
          strip.text = element_text(face = "bold", colour = SLX_NAVY),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(10, 14, 8, 10))
}

## when the response was transformed, say so on the plot
scale_note <- function(res) {
  if (is.null(res$trans) || identical(res$trans, "none")) return(NULL)
  paste("Transformed scale:", TRANS[[res$trans]]$lab)
}

## What the error bars and letters on a plot of means mean, at the chosen level.
## For a split, strip or pooled interaction both differ: the bars are the SE(m)
## for comparing means at the same level of the slicing factor, and the letters
## restart in each of its levels. `lets_on` is FALSE, TRUE, or the name of the
## procedure the letters come from.
plot_caption <- function(e, bars, lets_on) {
  sl <- e$slice
  by <- if (is.character(lets_on)) paste0(" by ", lets_on) else ""
  bars <- if (!bars) NULL
          else if (is.null(sl)) "Error bars: one standard error either side of each mean."
          else sprintf("Error bars: SE(m) for comparing means at the same level of %s (SE(d) / sqrt(2)).", sl)
  lets <- if (isFALSE(lets_on)) NULL
          else if (!effect_sig(e))
            sprintf("No letters: the F-test is not significant at the %s%% level.", pct(e$alpha))
          else if (is.null(sl))
            sprintf("Means sharing a letter are not significantly different%s at the %s%% level.", by, pct(e$alpha))
          else sprintf(paste0("Letters compare means within the same level of %s only; there, ",
                              "means sharing a letter are not significantly different%s at the %s%% level."),
                       sl, by, pct(e$alpha))
  if (is.null(c(bars, lets))) NULL else paste(c(bars, lets), collapse = " ")
}

## a caption broken into lines that fit under the plot
wrap_cap <- function(x) if (is.null(x)) NULL else paste(strwrap(x, 110), collapse = "\n")

## How strongly a factor asks to be the X-axis of an interaction plot. Only
## numbered labels qualify: their natural order is their order in time or
## size, whereas worded labels (Vegetative, Flowering) sort alphabetically and
## a line through them would run in the wrong order. Time comes first,
## recognised by the column name (Days, Week, Stage, Interval...) or by a
## common stem (D0, D60; Week1, Week2; 30 DAS). Next come labels that carry
## real quantities (N0, N60, N120; 2021, 2022; 0, 25, 50) rather than mere
## numbering (T1, T2, V1, V2, V4), which is read as codes. One unnumbered label
## such as Control or Fresh may sit among the numbered ones.
TIME_NAME <- "(^|[^a-z])(day|days|das|dat|dae|doy|time|week|weeks|month|months|year|years|stage|stages|interval|period|storage|hour|hours|date)([^a-z]|$)"
TIME_STEM <- "^(d|day|days|das|dat|dap|dae|doy|h|hr|hrs|hour|hours|wk|wks|week|weeks|month|months|yr|yrs|year|years)$"

x_score <- function(name, lv) {
  num <- "[0-9]+(?:[.][0-9]+)?"
  cnt <- lengths(regmatches(lv, gregexpr(num, lv, perl = TRUE)))
  lab <- lv[cnt == 1]
  if (length(lab) < 2 || any(cnt > 1) || sum(cnt == 0) > 1) return(0)
  stem <- unique(gsub("[^a-z]", "", tolower(gsub(num, "", lab, perl = TRUE))))
  one_stem <- length(stem) == 1
  n <- sort(as.numeric(regmatches(lab, regexpr(num, lab, perl = TRUE))))
  step <- diff(n)
  ## codes: whole numbers starting at 0 or 1 that either run 1, 2, 3 or skip
  ## unevenly (a dropped entry: V1, V2, V4); evenly spaced rates (0, 2, 4;
  ## 0, 40, 80) and other values are quantities
  counting <- anyDuplicated(n) > 0 ||
    (all(n == round(n)) && n[1] %in% c(0, 1) &&
     (all(step == 1) || length(unique(round(step, 9))) > 1))
  ## a one-letter stem (D, H) counts as time only on real quantities, so D0,
  ## D60 and H6, H12 are times while the dose codes D1, D2 and hybrids H1, H2
  ## are not
  stem_time <- one_stem && grepl(TIME_STEM, stem) && (nchar(stem) > 1 || !counting)
  is_time <- grepl(TIME_NAME, tolower(name)) || stem_time
  2 * is_time + (one_stem && !counting)
}

## The factor that goes on the X-axis by default: the best-scoring one above,
## the one with more levels on a tie, and otherwise the effect's first factor.
default_x <- function(e, d) {
  v <- e$vars
  sc <- vapply(v, function(f) x_score(f, levels(d[[f]])), numeric(1))
  if (max(sc) <= 0) return(v[1])
  cand <- v[sc == max(sc)]
  cand[which.max(vapply(cand, function(f) nlevels(d[[f]]), numeric(1)))]
}

## `x_var` names the factor for the X-axis of an interaction plot; by default
## default_x() chooses it. The other factor becomes the lines (or the bar
## colours), and any further factors become panels. `letters` holds a letter
## for each mean, in the order of the effect's table of means (from the
## comparison chosen), or is NULL for none; `method` names the procedure they
## come from, for the caption.
plot_main <- function(res, effect, type = "bar", letters = NULL, x_var = NULL, method = NULL) {
  e <- res$effects[[effect]]; d <- res$data; resp <- res$resp
  m <- e$means
  v <- e$vars
  show <- length(letters) == nrow(m) && effect_sig(e) && any(nzchar(letters))
  m$.let <- if (show) letters else rep("", nrow(m))
  m$.se <- m$SE                     # each mean's own SE: replication may differ
  lets_on <- if (!length(letters)) FALSE else if (!is.null(method)) method else TRUE

  if (length(v) == 1) {
    p <- ggplot(m, aes(x = .data[[v]], y = Mean)) +
      (if (type == "bar")
        geom_col(fill = SLX_BLUE, width = .62)
       else if (type == "box")
        geom_boxplot(data = d, aes(x = .data[[v]], y = .data[[resp]]),
                     fill = SLX_FILL, colour = SLX_NAVY, inherit.aes = FALSE, width = .6)
       else geom_point(size = 3.2, colour = SLX_BLUE)) +
      (if (type != "box") geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se),
                                        width = .15, colour = SLX_NAVY) else NULL) +
      (if (show && type != "box")
        geom_text(aes(y = Mean + .se, label = .let), vjust = -0.6, size = 4.3, colour = SLX_NAVY) else NULL) +
      scale_y_continuous(expand = expansion(mult = c(if (type == "bar" && all(m$Mean - m$.se >= 0)) 0 else 0.05, 0.12))) +
      labs(title = paste("Effect of", v, "on", resp), y = resp, x = v,
           subtitle = scale_note(res),
           caption = wrap_cap(plot_caption(e, type != "box", if (type == "box") FALSE else lets_on))) +
      theme_doe()
    return(p)
  }

  f1 <- if (length(x_var) == 1L && isTRUE(x_var %in% v)) x_var else default_x(e, d)
  rest <- setdiff(v, f1)
  f2 <- rest[1]
  pal <- slx_pal(nlevels(factor(m[[f2]])))
  base <- ggplot(m, aes(x = .data[[f1]], y = Mean, fill = .data[[f2]],
                        colour = .data[[f2]], group = .data[[f2]]))
  p <- switch(type,
    "bar" = base + geom_col(position = position_dodge(.8), width = .72, colour = NA) +
      geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se),
                    position = position_dodge(.8), width = .2, colour = "grey30") +
      (if (show) geom_text(aes(y = Mean + .se, label = .let),
        position = position_dodge(.8), vjust = -0.5, size = 3.9, colour = SLX_NAVY) else NULL) +
      scale_y_continuous(expand = expansion(mult = c(if (all(m$Mean - m$.se >= 0)) 0 else 0.05, 0.12))),
    "line" = base + geom_line(linewidth = 1) + geom_point(size = 3) +
      geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se), width = .12),
    "heat" = ggplot(m, aes(x = .data[[f1]], y = .data[[f2]], fill = Mean)) +
      geom_tile(colour = "white", linewidth = 0.8) +
      geom_text(aes(label = fmt(Mean, 1)), colour = SLX_NAVY, size = 4) +
      scale_fill_gradient(low = "#EAF1FB", high = "#3B7DD8"),
    "box" = ggplot(d, aes(x = .data[[f1]], y = .data[[resp]], fill = .data[[f2]])) +
      geom_boxplot(position = position_dodge(.8), colour = "grey25"))
  if (type != "heat") p <- p + scale_fill_manual(values = pal) + scale_colour_manual(values = pal)
  if (length(rest) > 1) p <- p + facet_wrap(stats::as.formula(
    paste("~", paste(rest[-1], collapse = "+"))))
  p <- p + labs(title = paste("Interaction:", e$label), y = resp, x = f1,
                colour = if (type %in% c("bar", "line")) f2, subtitle = scale_note(res),
                caption = wrap_cap(plot_caption(e, type %in% c("bar", "line"),
                                                if (type == "bar") lets_on else FALSE))) + theme_doe()
  if (type == "heat") p + labs(fill = paste("Mean", resp), y = f2) else p + labs(fill = f2)
}

## ------------------------------------------------------- interaction plot ----

## Can the cell means of effect `e` carry confidence intervals? Only when
## each mean's own standard error is known, as in a design with one error
## term. The SE of a split, strip or pooled interaction's cell is the SE(m)
## for comparing means within one level of its slicing factor, not the
## standard error of the cell mean on its own, and a pooled factorial's
## carries several environment interactions; intervals built from either
## would be wrong.
ci_ok <- function(e) is.null(e$slice) && is.null(e$mix) && !isTRUE(e$is_gxe)

## why an effect's cell means carry no confidence intervals
ci_why <- function(e) {
  if (!is.null(e$mix)) "in a pooled factorial each cell mean carries several environment interactions, so it has no single standard error"
  else if (!is.null(e$slice)) sprintf(paste("the standard error printed for these means is the one for comparing them within the same",
                                            "level of %s, not the standard error of each mean on its own"), e$slice)
  else ""
}

## The interaction plot: one line for each level of the second factor across
## the levels of the factor on the X-axis, with each cell mean's confidence
## interval at the analysis's level where one can be given (ci_ok()).
plot_interaction <- function(res, effect, x_var = NULL, ci = TRUE) {
  e <- res$effects[[effect]]; v <- e$vars; m <- e$means; resp <- res$resp
  if (length(v) < 2) return(NULL)
  f1 <- if (length(x_var) == 1L && isTRUE(x_var %in% v)) x_var else default_x(e, res$data)
  rest <- setdiff(v, f1); f2 <- rest[1]
  ci <- isTRUE(ci) && ci_ok(e)
  tq <- stats::qt(1 - e$alpha / 2, e$df)
  m$lo <- m$Mean - tq * m$SE; m$hi <- m$Mean + tq * m$SE
  pd <- position_dodge(width = if (ci) 0.18 else 0)
  k <- nlevels(factor(m[[f2]]))
  p <- ggplot(m, aes(x = .data[[f1]], y = Mean, colour = .data[[f2]], group = .data[[f2]], shape = .data[[f2]])) +
    (if (ci) geom_errorbar(aes(ymin = .data$lo, ymax = .data$hi), width = 0.14, linewidth = 0.6, position = pd) else NULL) +
    geom_line(linewidth = 1, position = pd) + geom_point(size = 3.1, position = pd) +
    scale_colour_manual(values = slx_pal(k)) +
    scale_shape_manual(values = rep(c(16, 17, 15, 18, 1, 2, 0, 5), length.out = k))
  if (length(rest) > 1) p <- p + facet_wrap(stats::as.formula(paste("~", paste(rest[-1], collapse = "+"))))
  cap <- if (ci) sprintf("Error bars: %s%% confidence interval of each cell mean (t on %s error d.f. times its standard error).",
                         pct(1 - e$alpha), df_text(e$df))
         else if (isTRUE(ci_ok(e))) NULL
         else sprintf("No confidence intervals: %s.", ci_why(e))
  p + labs(title = sprintf("Interaction of %s", paste(v, collapse = " and ")), x = f1, y = resp,
           colour = f2, shape = f2, subtitle = scale_note(res),
           caption = wrap_cap(cap)) +
    theme_doe()
}

## "What it shows" and the reading of the interaction plot: the pattern the
## lines draw, judged against the interaction's F-test, never on the lines alone
interaction_text <- function(res, effect, x_var = NULL, ci = TRUE, digits = 2) {
  e <- res$effects[[effect]]
  if (is.null(e) || length(e$vars) < 2) return(NULL)
  f1 <- if (length(x_var) == 1L && isTRUE(x_var %in% e$vars)) x_var else default_x(e, res$data)
  f2 <- setdiff(e$vars, f1)[1]
  ci <- isTRUE(ci) && ci_ok(e)
  scale <- if (is.null(res$trans) || identical(res$trans, "none")) "" else ", on the transformed scale,"
  what <- paste0(sprintf("Each line joins the means%s of one level of %s across the levels of %s.", scale, f2, f1),
                 if (ci) sprintf(paste(" The bars are %s%% confidence intervals for each cell mean: they show how precisely each mean",
                                       "is known, and two intervals that overlap can still belong to means that differ significantly."),
                                 pct(1 - e$alpha))
                 else if (!ci_ok(e)) sprintf(" No confidence intervals are drawn: %s.", ci_why(e)) else "",
                 if (length(e$vars) > 2) sprintf(" Each panel is one level of %s.", join_and(setdiff(e$vars, c(f1, f2)))) else "")
  base <- main_plot_text(res, effect, "line", FALSE, f1, digits)
  list(what = what, reading = base$reading)
}

## The residuals are standardised (divided by their standard deviation), the
## scale on which the assumption checks flag a value beyond -3 or 3; those
## points are labelled with their row numbers in the data table.
plot_diag <- function(res) {
  df <- data.frame(fit = res$fitted, res = res$resid,
                   std = res$resid / stats::sd(res$resid), row = row_ids(res$data))
  far <- df[reg_far(df$std), , drop = FALSE]
  p1 <- ggplot(df, aes(fit, std)) + geom_hline(yintercept = 0, lty = 2, colour = "#5B6B86") +
    geom_hline(yintercept = c(-3, 3), linetype = 3, colour = "grey45") +
    geom_point(colour = SLX_BLUE, size = 2.2, alpha = 0.85) +
    (if (nrow(far)) geom_text(data = far, aes(label = paste("row", .data$row), vjust = ifelse(.data$std > 0, -0.8, 1.8)),
                              size = 3.4, colour = SLX_RED)) +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    labs(title = "Residuals against fitted values", x = "Fitted value", y = "Standardised residual") + theme_doe()
  p2 <- ggplot(df, aes(sample = std)) + stat_qq(colour = SLX_BLUE, size = 2.2) + stat_qq_line(colour = SLX_RED) +
    labs(title = "Normal Q-Q plot of the residuals", x = "Theoretical quantile", y = "Standardised residual") + theme_doe()
  p3 <- ggplot(df, aes(res)) + geom_histogram(bins = 12, fill = SLX_FILL, colour = "white") +
    labs(title = "Histogram of the residuals", x = "Residual", y = "Count") + theme_doe()
  ## With only a few distinct fitted values (a CRD has one per treatment) a
  ## loess curve swings between them and rises where there are no points, so
  ## the trend joins the mean at each fitted value instead.
  df$sl <- sqrt(abs(df$std))
  df$at <- signif(df$fit, 10)
  trend <- if (length(unique(df$at)) <= 10) {
    m <- stats::aggregate(sl ~ at, data = df, FUN = mean)
    geom_line(data = m, aes(.data$at, .data$sl), colour = SLX_RED, linewidth = 0.9)
  } else geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                     method.args = list(degree = 1), colour = SLX_RED)
  p4 <- ggplot(df, aes(.data$fit, .data$sl)) + geom_point(colour = SLX_BLUE, size = 2.2, alpha = 0.85) + trend +
    labs(title = "Scale-location", x = "Fitted value", y = "sqrt|standardised residual|") + theme_doe()
  list(p1, p2, p3, p4)
}

plot_boxcox <- function(asm) {
  bc <- asm$bc
  if (is.null(bc)) return(NULL)
  df <- data.frame(lambda = bc$x, loglik = bc$y)
  df <- df[stats::complete.cases(df), ]
  ggplot(df, aes(lambda, loglik)) +
    geom_line(colour = SLX_BLUE, linewidth = 1) +
    geom_vline(xintercept = bc$lambda, colour = SLX_RED, lty = 2) +
    geom_vline(xintercept = bc$ci, colour = "grey55", lty = 3) +
    annotate("text", x = bc$lambda, y = min(df$loglik), vjust = -0.4, hjust = -0.1,
             label = sprintf("lambda = %.2f", bc$lambda), colour = SLX_RED, size = 3.6) +
    labs(title = sprintf("Box-Cox profile (optimal lambda = %.2f, %s%% CI %.2f to %.2f)",
                         bc$lambda, pct(bc$level), bc$ci[1], bc$ci[2]),
         x = "lambda", y = "log-likelihood") + theme_doe()
}

plot_meanvar <- function(asm) {
  if (nrow(asm$mv) < 3) return(NULL)
  ggplot(asm$mv, aes(log(m), log(v))) + geom_point(size = 2.6, colour = SLX_BLUE) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = SLX_RED) +
    labs(title = sprintf("Mean-variance relationship (Taylor slope b = %.2f)", asm$slope),
         x = "log(cell mean)", y = "log(cell variance)") + theme_doe()
}

## ------------------------------------------------------------- downloads ----

## A plot as it is saved: the same plot, with the platform named at its
## foot, under the plot's own caption
plot_brand <- function(p) {
  cap <- p$labels$caption
  p + labs(caption = paste(c(if (length(cap) && !is.na(cap) && nzchar(cap)) cap,
                             paste("Generated with", APP_BRAND)), collapse = "\n"))
}

## a file name for a saved plot: StatLabX_<what>_<response>.png
plot_file <- function(what, resp = NULL, ext = "png") {
  clean <- function(x) gsub("^-+|-+$", "", gsub("[^A-Za-z0-9]+", "-", x))
  parts <- c(APP_BRAND, clean(what), if (length(resp) && nzchar(resp)) clean(resp))
  paste0(paste(parts[nzchar(parts)], collapse = "_"), ".", ext)
}
