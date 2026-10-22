###############################################################################
##  DATA OVERVIEW: the examples' roles, the screening of the responses, the
##  quick summary on the Data tab and the structure of every column
###############################################################################

## Each example's design and the role of each of its columns, so loading an
## example sets the design, the factors and the responses at once instead of
## leaving the column mapping to guess.
DEMO_MAPS <- list(
  CRD      = list(design = "CRD",  map = list(treat = "Treatment"), response = "Yield"),
  RCBD     = list(design = "RCBD", map = list(block = "Block", treat = "Variety"), response = "Yield"),
  LSD      = list(design = "LSD",  map = list(row = "Row", col = "Column", treat = "Treatment"), response = "Yield"),
  FCRD     = list(design = "FCRD", map = list(factors = c("Variety", "Nitrogen")),
                  response = c("Yield", "Plant_height")),
  FRCBD    = list(design = "FRCBD", map = list(block = "Block", factors = c("Nitrogen", "Variety")), response = "Yield"),
  SPLIT    = list(design = "SPLIT", map = list(rep = "Rep", main = "Irrigation", sub = "Variety"), response = "Yield"),
  STRIP    = list(design = "STRIP", map = list(rep = "Rep", main = "Tillage", sub = "Mulch"), response = "Yield"),
  POOLRCBD = list(design = "POOLRCBD", map = list(env = "Location", rep = "Rep", treat = "Variety"), response = "Yield"),
  POOLCRD  = list(design = "POOLCRD", map = list(env = "Season", treat = "Treatment"), response = "Yield"),
  POOLFACT = list(design = "POOLFRCBD", map = list(env = "Location", rep = "Rep", factors = c("Nitrogen", "Variety")),
                  response = "Yield"),
  POOLFACTC = list(design = "POOLFCRD", map = list(env = "Season", factors = c("Spacing", "Variety")), response = "Yield"))

## the examples, as the selector on the Data tab offers them
DEMO_CHOICES <- c("CRD" = "CRD", "RCBD" = "RCBD", "Latin square" = "LSD",
                  "Factorial CRD (two responses)" = "FCRD", "Factorial RCBD" = "FRCBD",
                  "Split plot" = "SPLIT", "Strip plot" = "STRIP",
                  "Pooled over environments (RCBD)" = "POOLRCBD",
                  "Pooled over environments (CRD)" = "POOLCRD",
                  "Pooled factorial over environments (RCBD)" = "POOLFACT",
                  "Pooled factorial over environments (CRD)" = "POOLFACTC")

DEMO_NOTE <- paste("Demonstration data: synthetic values built for illustration from chosen effects and a fixed",
                   "set of random deviates. They are not observations from a real experiment.")

## ---------------------------------------------------------- first guesses --
## Names that usually mean a column's role in a design, for a first guess at
## the mapping of data that are not one of the examples; the user can change
## every choice. Column names are compared without case or punctuation.
DESIGN_KEYS <- c("rep", "reps", "replication", "replicate", "replicationno", "repno", "r",
                 "block", "blocks", "blockno", "blk", "b", "row", "rowno", "col", "column",
                 "colno", "columnno")
ROLE_HINTS <- list(
  block = c("block", "blocks", "blockno", "blk", "b", "rep", "reps", "replication", "replicate", "r"),
  rep   = c("rep", "reps", "replication", "replicate", "replicationno", "repno", "r", "block", "blocks", "blk"),
  row   = c("row", "rows", "rowno"),
  col   = c("col", "column", "columns", "colno", "columnno"),
  env   = c("location", "locations", "loc", "site", "sites", "env", "environment", "season", "seasons",
            "year", "years", "yr"),
  treat = c("treatment", "treatments", "trt", "treat", "variety", "varieties", "genotype", "genotypes", "cultivar"))

## The first guess at a column for each role of the design: a column named
## for the role when there is one, else the next column that is not a
## measurement; the response is the last column of numbers that is a
## measurement (many different values, not named like an identifier or a
## design column). `roles` are the role names the design needs, in order.
guess_map <- function(d, roles, num = NULL) {
  cn <- names(d)
  num <- intersect(num %||% cn[vapply(d, is.numeric, logical(1))], cn)
  key <- gsub("[^a-z]", "", tolower(cn))
  named <- key %in% unlist(ROLE_HINTS, use.names = FALSE) | is_id_name(cn)
  many <- vapply(cn, function(v) {
    k <- length(unique(stats::na.omit(response_values(d[[v]]))))
    k > max(2, nrow(d) / 4)
  }, logical(1))
  meas <- cn[cn %in% num & !named & many]
  resp <- if (length(meas)) utils::tail(meas, 1) else if (length(num)) utils::tail(num, 1) else utils::tail(cn, 1)
  free <- setdiff(cn, resp)
  out <- list(resp = resp)
  for (r in roles) {
    hint <- ROLE_HINTS[[if (startsWith(r, "f") || r %in% c("main", "sub")) "treat" else r]]
    k <- key[match(free, cn)]
    pick <- if (r %in% c("block", "rep", "row", "col", "env")) free[k %in% hint][1] else NA_character_
    if (is.na(pick)) {
      cand <- setdiff(free, meas)
      pick <- if (length(cand)) cand[1] else free[1]
    }
    if (is.na(pick)) pick <- cn[1]
    out[[r]] <- pick
    free <- setdiff(free, pick)
  }
  out
}

## the roles each design needs, in the order the mapping shows them
design_roles <- function(design, nf = 2) {
  fs <- paste0("f", seq_len(nf))
  switch(design,
    CRD = "treat", RCBD = c("block", "treat"), LSD = c("row", "col", "treat"),
    FCRD = fs, FRCBD = c("block", fs),
    SPLIT = , STRIP = c("rep", "main", "sub"),
    POOLRCBD = c("env", "rep", "treat"), POOLCRD = c("env", "treat"),
    POOLFRCBD = c("env", "rep", fs), POOLFCRD = c("env", fs),
    character(0))
}

## what each role is called beside its selector
role_label <- function(role, design) {
  strip <- identical(design, "STRIP"); pooled <- startsWith(design, "POOL")
  if (grepl("^f[0-9]$", role)) return(paste(if (pooled) "Treatment factor" else "Factor", LETTERS[as.integer(substring(role, 2))]))
  switch(role,
    treat = "Treatment", block = "Block", row = "Row", col = "Column",
    rep = if (pooled) "Replication / block (within environment)" else "Replication",
    main = if (strip) "Factor A (horizontal strips)" else "Main-plot factor",
    sub = if (strip) "Factor B (vertical strips)" else "Sub-plot factor",
    env = "Environment (location / year / season)", role)
}

## a mapping of roles (resp, treat, block, f1, f2, ...) as analyze() takes it
roles_to_map <- function(vals) {
  m <- list(response = vals$resp)
  for (r in c("treat", "block", "row", "col", "rep", "main", "sub", "env"))
    if (!is.null(vals[[r]])) m[[r]] <- vals[[r]]
  fs <- unlist(vals[grep("^f[0-9]$", names(vals))])
  if (length(fs)) m$factors <- unname(fs)
  m
}

## the roles of a mapping as the selectors hold them (the inverse of roles_to_map)
map_to_roles <- function(map) {
  out <- list(resp = map$response)
  for (r in c("treat", "block", "row", "col", "rep", "main", "sub", "env"))
    if (!is.null(map[[r]])) out[[r]] <- map[[r]]
  for (i in seq_along(map$factors)) out[[paste0("f", i)]] <- map$factors[i]
  out
}

## ------------------------------------------------------------------ roles --
## What each column does in the analysis, in words, from the design and the
## column mapping. A column mapped to no role is not described as anything.
map_roles <- function(map, design = NULL) {
  if (is.null(map)) return(character(0))
  strip <- identical(design, "STRIP")
  in_env <- isTRUE(design %in% c("POOLRCBD", "POOLFRCBD"))
  lab <- c(response = "Response", treat = "Treatment factor", factors = "Treatment factor",
           block = "Block", rep = if (in_env) "Replication within environment" else "Replication",
           row = "Row (blocking)", col = "Column (blocking)", env = "Environment",
           main = if (strip) "Factor A (horizontal strips)" else "Main-plot factor",
           sub = if (strip) "Factor B (vertical strips)" else "Sub-plot factor")
  out <- character(0)
  for (k in intersect(names(lab), names(map))) {
    cols <- map[[k]]
    cols <- cols[!is.na(cols) & nzchar(cols)]
    out[cols] <- lab[[k]]
  }
  out
}

## ------------------------------------------------------------ quick summary --
## The numbers to look at before any analysis, for each response: how many
## values, how many missing, where they lie and how widely they spread. SE is
## the standard error of the mean of all the values together (SD / sqrt(n)),
## a measure of how precisely that one overall mean is known; it is not the
## SE(m) of a treatment mean, which comes from the analysis of variance. CV
## only where it means something: values that cannot be negative and a
## positive mean (describe_data() leaves it out otherwise).
quick_describe <- function(d, vars) {
  vars <- intersect(vars, names(d))
  if (!length(vars)) return(NULL)
  tab <- describe_data(d, vars)
  data.frame(Response = tab$Variable, n = tab$N, Missing = tab$Missing,
             Mean = tab$Mean, SD = tab$SD,
             SE = ifelse(tab$N >= 2, tab$SD / sqrt(tab$N), NA_real_),
             Median = tab$Median, Q1 = tab$Q1, Q3 = tab$Q3, Min = tab$Min, Max = tab$Max,
             CV = tab$CV, check.names = FALSE, stringsAsFactors = FALSE)
}

## the quick summary as a table, each response's figures to the decimals its
## values support (as the Exploration tab prints them)
quick_describe_html <- function(tab, d) {
  if (is.null(tab) || !nrow(tab)) return("")
  num <- c("Mean", "SD", "SE", "Median", "Q1", "Q3", "Min", "Max")
  body <- lapply(seq_len(nrow(tab)), function(i) {
    dg <- var_digits(response_values(d[[tab$Response[i]]]))
    c(esc(tab$Response[i]), as.character(tab$n[i]), as.character(tab$Missing[i]),
      vapply(num, function(k) dfmt(tab[[k]][i], if (k == "SE") dg + 1L else dg), ""),
      if (is.na(tab$CV[i])) "&ndash;" else fmt(tab$CV[i], 1))
  })
  head_ <- c("Response", "n", "Missing", "Mean", "SD", "SE of mean", "Median", "Q1", "Q3", "Min", "Max", "CV (%)")
  paste0(raw_table(head_, body, caption = "Quick descriptive statistics of the response variables", cls = "doe doe-quick"),
         "<div class='note'>n counts the values used and Missing the empty cells and entries that are not ",
         "plain numbers (missing values are never treated as zeros). SD is the standard deviation of the ",
         "values; SE of mean = SD / &radic;n is the precision of the overall mean of all of them, not the ",
         "standard error of a treatment mean, which the analysis of variance gives. Q1 and Q3 are the quartiles ",
         "(as Excel's QUARTILE.INC gives them). CV = 100 &times; SD / mean, shown only when no value is negative ",
         "and the mean is positive.</div>")
}

## -------------------------------------------------------- column structure --
## One row for every column of the data: the role it has in the analysis,
## how it is stored, what kind of variable it is, and how complete it is. The
## kind is not read from the storage alone: a column mapped as a treatment
## factor is categorical even when its labels are 1, 2, 3, and a column of
## whole numbers mapped to no role is reported as such rather than guessed to
## be a measurement.
data_structure <- function(d, map = NULL, design = NULL) {
  roles <- map_roles(map, design)
  n <- nrow(d)
  rows <- lapply(names(d), function(v) {
    x <- d[[v]]
    s <- trimws(as.character(x))
    miss <- is.na(x) | is.na(s) | s == "" | tolower(s) %in% MISSING_MARKS
    if (is.numeric(x)) miss <- miss | !is.finite(x)
    obs <- s[!miss]
    k <- length(unique(obs))
    storage <- if (is.factor(x)) "Factor" else if (is.integer(x)) "Integer" else if (is.numeric(x)) "Numeric"
               else if (is.logical(x)) "Logical" else if (inherits(x, "Date")) "Date"
               else if (is.character(x)) "Text" else class(x)[1]
    ent <- read_entries(x)
    nums <- ent$kind[!miss] %in% c("number", "comma", "thousands", "european")
    vals <- ent$value[!miss][nums]
    is_num <- length(obs) > 0 && mean(nums) >= 0.75
    whole <- is_num && length(vals) > 0 && all(abs(vals - round(vals)) < 1e-9)
    key <- gsub("[^a-z]", "", tolower(v))
    role <- if (v %in% names(roles)) roles[[v]]
            else if (key %in% DESIGN_KEYS) "Not used (named like a design column)"
            else if (is_id_name(v) || (k == length(obs) && length(obs) > 1 && !is_num) ||
                     (is_num && whole && k == length(obs) && length(obs) > 2 &&
                      all(diff(sort(vals)) == 1))) "Identifier (not used)"
            else "Not used"
    kind <- if (role == "Response") {
              if (!is_num) "Not numeric: cannot be analysed"
              else if (whole && all(vals >= 0)) "Integer / count" else "Continuous"
            } else if (v %in% names(roles)) "Categorical (design factor)"
            else if (startsWith(role, "Identifier")) "Identifier"
            else if (key %in% DESIGN_KEYS) "Categorical"
            else if (is_num && !whole) "Continuous"
            else if (is_num) "Integer (count or code)"
            else if (k <= max(1, length(obs) / 2)) "Categorical"
            else "Unclassified"
    bad <- if (is_num) sum(!nums) else 0L
    note <- c(if (bad) sprintf("%d %s not plain %s (see the data check)", bad,
                               pl(bad, "entry is", "entries are"), pl(bad, "number", "numbers")),
              if (role == "Response" && k <= 1 && length(obs)) "every value is the same",
              if (v %in% names(roles) && role != "Response" && k < 2) "only one level")
    data.frame(Variable = v, Role = role, Storage = storage, Type = kind, Total = n,
               Observed = length(obs), Missing = sum(miss), Missing_pct = if (n) 100 * sum(miss) / n else NA_real_,
               Distinct = k, Note = paste(note, collapse = "; "),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

data_structure_html <- function(tab) {
  if (is.null(tab) || !nrow(tab)) return("")
  body <- lapply(seq_len(nrow(tab)), function(i) c(
    esc(tab$Variable[i]), esc(tab$Role[i]), esc(tab$Storage[i]), esc(tab$Type[i]),
    as.character(tab$Total[i]), as.character(tab$Observed[i]), as.character(tab$Missing[i]),
    fmt(tab$Missing_pct[i], 1), as.character(tab$Distinct[i]), esc(tab$Note[i])))
  paste0(raw_table(c("Variable", "Assigned role", "Storage type", "Analytical type", "Total N", "Non-missing N",
                     "Missing N", "Missing (%)", "Distinct values", "Note"), body,
                   caption = "Data structure and quality: every column", cls = "doe doe-structure"),
         "<div class='note'>The role comes from the design and the columns chosen on the Data tab. The ",
         "analytical type follows the role, not the way the column is stored: a treatment coded 1, 2, 3 is ",
         "categorical. Missing counts empty cells and entries such as n/a or a dash; distinct values are ",
         "counted among the values present.</div>")
}

## -------------------------------------------------------------- screening --
## The screening of each response the analysis will use: the untransformed
## model for the chosen design, its residual checks and the adviser's
## suggestion. `fits` is a named list with, for each response, either the
## result of fit_base() or list(err = message).
screen_rows <- function(fits) {
  rows <- lapply(names(fits), function(v) {
    f <- fits[[v]]
    if (!is.null(f$err))
      return(data.frame(Response = v, Rows = NA_integer_, Normality = "", Variances = "", Slope = "",
                        BoxCox = "", CV = "", Suggestion = "", Note = f$err, ok = FALSE,
                        check.names = FALSE, stringsAsFactors = FALSE))
    a <- f$asm0; r <- f$base; al <- r$alpha
    verdict <- function(p) if (!is.finite(p)) "" else if (p < al) " &ndash; <b>departure</b>" else " &ndash; no departure shown"
    norm <- if (isTRUE(a$exact)) "not available: exact fit"
            else if (is.na(a$p_norm)) paste("not available:", esc(a$norm_why %||% "too few residuals"))
            else paste0("p = ", p_text(a$p_norm, al), verdict(a$p_norm))
    hov <- if (isTRUE(a$exact)) "not available: exact fit"
           else if (is.na(a$p_hov)) paste("not available:", esc(a$hov_why %||% "too few values"))
           else paste0(esc(a$hov_short), " p = ", p_text(a$p_hov, al), verdict(a$p_hov))
    slope <- if (is.na(a$slope)) "&ndash;"
             else paste0("b = ", fmt(a$slope, 2),
                         if (is.na(a$slope_p)) " (cannot be tested)" else paste0(" (p = ", p_text(a$slope_p, al), ")"))
    bc <- if (is.null(a$bc)) "&ndash;"
          else if (abs(a$lambda) >= 2 - 1e-9) sprintf("beyond %s", fmt(a$lambda, 2))
          else sprintf("%s (%s%% CI %s to %s)", fmt(a$lambda, 2), pct(a$bc$level), fmt(a$bc$ci[1], 2), fmt(a$bc$ci[2], 2))
    cv <- if (length(r$cv) == 1) fmt(r$cv, 2)
          else paste(sprintf("%s %s", sub(" [(]%[)]$", "", names(r$cv)), fmt(r$cv, 2)), collapse = "; ")
    sug <- paste0(TRANS[[f$sug$method]]$lab, if (isTRUE(f$sug$optional)) " (optional)" else "")
    data.frame(Response = v, Rows = nrow(r$data), Normality = norm, Variances = hov, Slope = slope,
               BoxCox = bc, CV = cv, Suggestion = sug, Note = f$sug$why, ok = TRUE,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

screening_html <- function(rows, alpha) {
  if (is.null(rows) || !nrow(rows)) return("")
  good <- rows[rows$ok, , drop = FALSE]; bad <- rows[!rows$ok, , drop = FALSE]
  tab <- if (nrow(good)) raw_table(
    c("Response", "Rows analysed", "Normality of residuals (Shapiro-Wilk)", "Equal variances",
      "Mean-variance slope", "Box-Cox &lambda;", "CV (%)", "Suggested transformation"),
    lapply(seq_len(nrow(good)), function(i) c(esc(good$Response[i]), as.character(good$Rows[i]),
      good$Normality[i], good$Variances[i], good$Slope[i], good$BoxCox[i], good$CV[i], esc(good$Suggestion[i]))),
    caption = "Quick screening of the selected responses", cls = "doe doe-screen") else ""
  why <- if (nrow(good)) paste0("<ul class='slx-why'>", paste0("<li><b>", esc(good$Response), ":</b> ",
                                                                esc(good$Note), "</li>", collapse = ""), "</ul>") else ""
  errs <- if (nrow(bad)) paste0("<div class='warn'><b>Not screened.</b><ul>", paste0("<li><b>", esc(bad$Response),
                                ":</b> ", esc(bad$Note), "</li>", collapse = ""), "</ul></div>") else ""
  paste0(tab, why, errs,
         "<div class='note'>A first look, from the model for the design chosen above and before any ",
         "transformation: the normality check uses the model's residuals, not the raw values, and the ",
         "equal-variance check compares the treatment groups (on the residuals when the design has blocks). ",
         sprintf("Each verdict is at the %s%% level. ", pct(alpha)),
         "The Assumptions tab gives the full checks with their plots, and the choice of transformation.</div>")
}
