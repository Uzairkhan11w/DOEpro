###############################################################################
##  PLOTS
###############################################################################
theme_doe <- function() {
  theme_bw(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 13))
}

## when the response was transformed, say so on the plot
scale_note <- function(res) {
  if (is.null(res$trans) || identical(res$trans, "none")) return(NULL)
  paste("Transformed scale:", TRANS[[res$trans]]$lab)
}

plot_main <- function(res, effect, type = "bar", show_letters = TRUE) {
  e <- res$effects[[effect]]; d <- res$data; resp <- res$resp
  m <- e$means
  v <- e$vars
  m$.se <- e$sem
  lab_y <- max(m$Mean + m$.se) * 1.06

  if (length(v) == 1) {
    p <- ggplot(m, aes(x = .data[[v]], y = Mean)) +
      (if (type == "bar")
        geom_col(fill = "#3B7DD8", width = .65)
       else if (type == "box")
        geom_boxplot(data = d, aes(x = .data[[v]], y = .data[[resp]]),
                     fill = "#BBD3F2", inherit.aes = FALSE)
       else geom_point(size = 3, colour = "#3B7DD8")) +
      (if (type != "box") geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se),
                                        width = .15) else NULL) +
      (if (show_letters && type != "box")
        geom_text(aes(y = Mean + .se, label = Letter), vjust = -0.6, size = 4.5) else NULL) +
      labs(title = paste("Effect of", v, "on", resp), y = resp, x = v,
           subtitle = scale_note(res)) +
      theme_doe()
    return(p)
  }

  f1 <- v[1]; f2 <- v[2]
  lt <- if ("Letter" %in% names(m)) "Letter" else
        if ("Letter_within_MP" %in% names(m)) "Letter_within_MP"
        else if ("Letter_within_env" %in% names(m)) "Letter_within_env"
        else "Letter_within_A"
  base <- ggplot(m, aes(x = .data[[f1]], y = Mean, fill = .data[[f2]],
                        colour = .data[[f2]], group = .data[[f2]]))
  p <- switch(type,
    "bar" = base + geom_col(position = position_dodge(.8), width = .7, colour = NA) +
      geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se),
                    position = position_dodge(.8), width = .2, colour = "grey30") +
      (if (show_letters) geom_text(aes(y = Mean + .se, label = .data[[lt]]),
        position = position_dodge(.8), vjust = -0.5, size = 4, colour = "black") else NULL),
    "line" = base + geom_line(linewidth = 1) + geom_point(size = 3) +
      geom_errorbar(aes(ymin = Mean - .se, ymax = Mean + .se), width = .12),
    "heat" = ggplot(m, aes(x = .data[[f1]], y = .data[[f2]], fill = Mean)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = fmt(Mean, 1)), colour = "black", size = 4) +
      scale_fill_gradient(low = "#EAF1FB", high = "#1B4F9C"),
    "box" = ggplot(d, aes(x = .data[[f1]], y = .data[[resp]], fill = .data[[f2]])) +
      geom_boxplot(position = position_dodge(.8)))
  if (length(v) > 2) p <- p + facet_wrap(stats::as.formula(
    paste("~", paste(v[-(1:2)], collapse = "+"))))
  p <- p + labs(title = paste("Interaction:", e$label), y = resp, x = f1,
                colour = f2, subtitle = scale_note(res)) + theme_doe()
  if (type == "heat") p + labs(fill = paste("Mean", resp), y = f2) else p + labs(fill = f2)
}

plot_diag <- function(res) {
  df <- data.frame(fit = res$fitted, res = res$resid,
                   std = res$resid / stats::sd(res$resid))
  p1 <- ggplot(df, aes(fit, res)) + geom_hline(yintercept = 0, lty = 2) +
    geom_point(colour = "#3B7DD8") + labs(title = "Residuals vs fitted",
    x = "Fitted", y = "Residual") + theme_doe()
  p2 <- ggplot(df, aes(sample = std)) + stat_qq(colour = "#3B7DD8") + stat_qq_line() +
    labs(title = "Normal Q-Q plot", x = "Theoretical", y = "Standardised residual") + theme_doe()
  p3 <- ggplot(df, aes(res)) + geom_histogram(bins = 12, fill = "#BBD3F2", colour = "white") +
    labs(title = "Histogram of residuals", x = "Residual", y = "Count") + theme_doe()
  p4 <- ggplot(df, aes(fit, sqrt(abs(std)))) + geom_point(colour = "#3B7DD8") +
    geom_smooth(se = FALSE, method = "loess", formula = y ~ x, colour = "#C0392B") +
    labs(title = "Scale-location", x = "Fitted", y = "sqrt|std residual|") + theme_doe()
  list(p1, p2, p3, p4)
}

plot_boxcox <- function(asm) {
  bc <- asm$bc
  if (is.null(bc)) return(NULL)
  df <- data.frame(lambda = bc$x, loglik = bc$y)
  df <- df[stats::complete.cases(df), ]
  ggplot(df, aes(lambda, loglik)) +
    geom_line(colour = "#3B7DD8", linewidth = 1) +
    geom_vline(xintercept = bc$lambda, colour = "#C0392B", lty = 2) +
    geom_vline(xintercept = bc$ci, colour = "grey55", lty = 3) +
    annotate("text", x = bc$lambda, y = min(df$loglik), vjust = -0.4, hjust = -0.1,
             label = sprintf("lambda = %.2f", bc$lambda), colour = "#C0392B", size = 3.6) +
    labs(title = sprintf("Box-Cox profile (optimal lambda = %.2f, 95%% CI %.2f to %.2f)",
                         bc$lambda, bc$ci[1], bc$ci[2]),
         x = "lambda", y = "log-likelihood") + theme_doe()
}

plot_meanvar <- function(asm) {
  if (nrow(asm$mv) < 3) return(NULL)
  ggplot(asm$mv, aes(log(m), log(v))) + geom_point(size = 2.5, colour = "#3B7DD8") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#C0392B") +
    labs(title = sprintf("Mean-variance relationship (Taylor slope b = %.2f)", asm$slope),
         x = "log(cell mean)", y = "log(cell variance)") + theme_doe()
}
