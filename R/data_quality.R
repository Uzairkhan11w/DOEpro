###############################################################################
##  DATA QUALITY
###############################################################################
## Checks run on the data as soon as it is loaded, before any analysis. Each
## problem is described in plain words with the rows it affects and, where a
## correction is safe, what that correction will be. Nothing is changed until
## the user asks, and then only entry by entry: a value is rewritten only when
## its meaning is certain (5,6 is 5.6; "n/a" means no value). An entry whose
## meaning is not certain (12a, nil, 1,250, a word among numbers, a % sign on
## some entries only) is never changed or emptied; it is listed for the user,
## and an analysis that cannot use it says so.

## what people type for "no value", including the error values Excel pastes
## along with the data. "nil" is not here: in field records it usually means
## zero, so it is shown to the user instead.
MISSING_MARKS <- c("na", "n/a", "n.a.", "n.a", "missing", "-", "--", "---",
                   ".", "?", "*", "nd", "n.d.", "#n/a", "#value!", "#div/0!",
                   "#num!", "#ref!", "#name?", "#null!")

## Units that may follow a number in a column of measurements. Only these are
## recognised, so that a variety such as "1121 Basmati", a stage such as
## "30 DAS" or DMRT letters such as "12ab" are never taken for units.
KNOWN_UNITS <- c("kg", "g", "mg", "t", "q", "qt", "ha", "cm", "mm", "m", "km",
                 "m2", "cm2", "mm2", "ml", "l", "kg/ha", "q/ha", "t/ha", "g/plant",
                 "g/m2", "kg/m2", "kg/plot", "g/plot", "ppm", "ppb", "ds/m",
                 "mg/kg", "mg/l", "mg/g", "g/kg")

## user text going into HTML: "<LOD" must show as text, not open a tag
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

## one or many: pl(1, "row", "rows")
pl <- function(n, one, many) if (n == 1) one else many

## the row numbers shown in the data table, which are the data frame's row
## names, so that numbers in messages are ones the user can find
row_ids <- function(d) {
  id <- suppressWarnings(as.integer(rownames(d)))
  if (anyNA(id)) seq_len(nrow(d)) else id
}

## "row 3" or "rows 3, 7 and 12" (at most eight listed)
rows_text <- function(i) {
  if (!length(i)) return("")
  paste(pl(length(i), "row", "rows"), join_and(head_more(as.character(i), 8)))
}

## a few entries, quoted, saying "such as" when there are more
eg_text <- function(x) {
  u <- unique(x)
  paste0(if (length(u) > 3) "such as " else "", join_and(sprintf("'%s'", head(u, 3))))
}

## a number as it will be written back: as many digits as it needs, no more
num_text <- function(v) vapply(v, function(z) format(z, digits = 15, trim = TRUE), "")

## rows with no value in any cell: empty, or holding only entries such as "-"
## or "n/a", so that one pass of the corrections removes them
empty_rows <- function(d) {
  if (!nrow(d) || !ncol(d)) return(integer(0))
  which(Reduce(`&`, lapply(d, function(x) {
    s <- trimws(as.character(x))
    is.na(x) | s == "" | tolower(s) %in% MISSING_MARKS
  })))
}

## a label tidied of spacing only: no-break spaces made plain, runs of spaces
## made single, the ends trimmed. Nothing else in a label is touched; a dash
## or a capital letter may carry meaning.
label_tidy <- function(x) {
  s <- as.character(x)
  for (ch in ODD_SPACES) s <- gsub(ch, " ", s, fixed = TRUE)
  gsub("[[:space:]]+", " ", trimws(s))
}

## Characters that look like a minus sign or a space but are not the plain
## ones: the minus sign and en dash from Word, the no-break spaces Excel puts
## between thousands. Built here rather than written into the source, which
## must stay ASCII.
ODD_MINUS  <- intToUtf8(c(0x2212, 0x2013), multiple = TRUE)
ODD_SPACES <- intToUtf8(c(0x00A0, 0x202F, 0x2009), multiple = TRUE)

## How the number part of an entry reads (no % sign or unit attached).
read_core <- function(core) {
  n <- length(core)
  kind <- rep("text", n); value <- rep(NA_real_, n)
  num <- function(v) suppressWarnings(as.numeric(v))
  plain <- grepl("^[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)?$", core)
  ## 1,250 may be a thousands separator or a decimal comma; 0,125 cannot be
  ## thousands, so it is a decimal comma
  three <- grepl("^[-+]?[1-9][0-9]{0,2},[0-9]{3}$", core)
  comma <- grepl("^[-+]?[0-9]+,[0-9]+$", core) & !three
  thous <- grepl("^[-+]?[1-9][0-9]{0,2}(,[0-9]{3})+([.][0-9]+)?$", core) & !three
  space <- grepl("^[-+]?[1-9][0-9]{0,2}( [0-9]{3})+([.,][0-9]+)?$", core)
  euro  <- grepl("^[-+]?[1-9][0-9]{0,2}([.][0-9]{3})+,[0-9]+$", core)
  value[plain] <- num(core[plain])
  value[comma] <- num(sub(",", ".", core[comma], fixed = TRUE))
  value[thous] <- num(gsub(",", "", core[thous], fixed = TRUE))
  value[space] <- num(sub(",", ".", gsub(" ", "", core[space], fixed = TRUE), fixed = TRUE))
  value[euro]  <- num(sub(",", ".", gsub(".", "", core[euro], fixed = TRUE), fixed = TRUE))
  kind[plain] <- "number"; kind[comma] <- "comma"; kind[thous | space] <- "thousands"
  kind[euro] <- "european"; kind[three] <- "ambiguous"
  list(kind = kind, value = value)
}

## How each entry reads as a number. `kind` is one of
##   blank      empty
##   mark       a no-value marker such as n/a, - or #DIV/0!
##   number     a plain number with a decimal point (5.6, -2, 1e3)
##   comma      a decimal comma (5,6; 0,125; 1234,5)
##   ambiguous  1,250: a thousands separator or a decimal comma
##   thousands  1,250,000; 1,250.5; 1 250
##   european   1.250,5 (point between thousands, decimal comma)
##   percent    45% or 4,5 %
##   unit       a number followed by a known unit (45 cm, 2.5kg, 12 q/ha)
##   text       anything else
## `value` is the number it would be, `text` the entry tidied of odd spaces and
## minus signs, `unit` the unit text, and `core` the kind of the number part of
## an entry with a % sign or a unit.
read_entries <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  for (ch in ODD_SPACES) s <- gsub(ch, " ", s, fixed = TRUE)
  for (ch in ODD_MINUS) s <- gsub(ch, "-", s, fixed = TRUE)
  s <- trimws(s)
  n <- length(s)
  pct <- grepl("^[-+]?[0-9][0-9., ]*%$", s)
  sci <- grepl("^[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)[eE][-+]?[0-9]+$", s)
  ut  <- ifelse(!pct & !sci, sub("^[-+]?[0-9][0-9.,]* ?", "", s), "")
  ## a known unit, with a space before it or at least two letters (45 cm,
  ## 2.5kg): a single letter stuck to a number (5m, 12a) may well be a code
  unit <- !pct & !sci & grepl("^[-+]?[0-9][0-9.,]*( [A-Za-z]|[A-Za-z]{2})[A-Za-z/0-9]*$", s) &
          tolower(ut) %in% KNOWN_UNITS
  core <- s
  core[pct] <- trimws(sub("%$", "", s[pct]))
  core[unit] <- sub("^([-+]?[0-9][0-9.,]*) ?[A-Za-z].*$", "\\1", s[unit])
  r <- read_core(core)
  kind <- r$kind; value <- r$value; ckind <- r$kind
  ok <- !ckind %in% c("text", "ambiguous")
  kind[pct & ok] <- "percent"
  kind[unit & ok] <- "unit"
  kind[(pct | unit) & !ok] <- ifelse(ckind[(pct | unit) & !ok] == "ambiguous", "ambiguous", "text")
  un <- rep("", n)
  un[unit] <- ut[unit]
  un[pct] <- "%"
  kind[tolower(s) %in% MISSING_MARKS & s != ""] <- "mark"
  kind[s == ""] <- "blank"
  value[!kind %in% c("number", "comma", "thousands", "european", "percent", "unit")] <- NA_real_
  list(kind = kind, value = value, text = s, unit = un, core = ckind)
}

NUMBER_KINDS <- c("number", "comma", "ambiguous", "thousands", "european", "percent", "unit")

## A response as the analysis uses it: a column of numbers as it is; a column
## of text read entry by entry, keeping only entries that are plainly numbers.
## Anything the data check would flag (5,6; 45%; 0x10; 12a) is left out, with
## the reason recorded by excluded_rows().
response_values <- function(x) {
  if (is.numeric(x)) return(x)
  r <- read_entries(x)
  ifelse(r$kind == "number", r$value, NA_real_)
}

## Everything the check and the corrections need to know about one column.
## A column is read as numbers when at least three quarters of its filled-in,
## non-marker entries read as numbers in some notation, and as labels when
## fewer than a quarter do; in between it is a mixture the user should see.
## `new` holds each entry's corrected form, or NA where nothing is changed.
assess_column <- function(x) {
  raw <- as.character(x)
  r <- read_entries(x)
  k <- r$kind; txt <- r$text
  real <- !k %in% c("blank", "mark")
  share <- if (any(real)) mean(k[real] %in% NUMBER_KINDS) else 0
  role <- if (!any(real)) "empty" else if (share >= 0.75) "numbers" else
          if (share >= 0.25) "mixed" else "labels"
  ## which notation the column writes its decimals in; an entry like 1.250 is
  ## not evidence of decimal points, since it is the very thing in doubt
  dot3_pat <- "^[-+]?[0-9]{1,3}([.][0-9]{3})+$"
  dot_dec <- any((k == "number" & grepl(".", txt, fixed = TRUE) & !grepl(dot3_pat, txt)) |
                 (k == "thousands" & grepl("[.][0-9]+$", txt)) |
                 (k %in% c("percent", "unit") & r$core == "number" & grepl(".", txt, fixed = TRUE)))
  comma_dec <- any(k %in% c("comma", "european") |
                   (k == "thousands" & grepl(",[0-9]+$", txt) & grepl(" ", txt, fixed = TRUE)) |
                   (k %in% c("percent", "unit") & r$core == "comma"))
  sep <- any(k == "thousands" & grepl(",[0-9]{3}", txt))
  ## 1,250 is settled by the rest of the column: decimal commas and no
  ## decimal points means a decimal; decimal points or separators and no
  ## decimal commas means a thousands separator; both or neither, the user
  ## must say
  three <- if (comma_dec && !dot_dec && !sep) "comma" else
           if ((dot_dec || sep) && !comma_dec) "thousands" else NA_character_
  ## in a column written with decimal commas, 1.250 is the mirror image
  dot3_rows <- if (comma_dec && !dot_dec) which(k == "number" & grepl(dot3_pat, txt)) else integer(0)
  ## a % sign or a unit is dropped only when every number in the column
  ## carries it (and, for units, the same one): 62% beside 0.45 and 0.50, or
  ## 1.5 kg beside 1500, may be on another scale. Entries such as 1,250 kg
  ## count too.
  numlike <- k %in% NUMBER_KINDS
  has_pct <- r$unit == "%"
  has_unit <- r$unit != "" & !has_pct
  units <- unique(r$unit[has_unit & numlike])
  all_pct <- any(has_pct & numlike) && all(has_pct[numlike])
  all_unit <- length(units) == 1 && all(has_unit[numlike])
  val <- r$value
  amb <- k == "ambiguous"
  ## While a 1,250 or a 1.250 waits for the user, no other number in the
  ## column is rewritten: changing 5,6 to 5.6 would change the evidence, and a
  ## second press of the button would then settle the doubtful entry by itself.
  hold <- (any(amb) && is.na(three)) || length(dot3_rows) > 0
  if (any(amb) && !is.na(three)) {
    cr <- sub(" ?%$", "", sub(" ?[A-Za-z].*$", "", txt[amb]))
    val[amb] <- suppressWarnings(as.numeric(
      if (three == "comma") sub(",", ".", cr, fixed = TRUE) else gsub(",", "", cr, fixed = TRUE)))
  }
  sign_ok <- (!has_unit & !has_pct) | (has_unit & all_unit) | (has_pct & all_pct)
  new <- rep(NA_character_, length(k))
  if (role == "numbers" && !hold) {
    change <- (k %in% c("comma", "thousands", "european") & sign_ok) |
              (k %in% c("percent", "unit") & sign_ok) | (amb & !is.na(three) & sign_ok) |
              (k == "number" & !is.na(raw) & txt != trimws(raw))       # odd minus or spaces
    new[change] <- num_text(val[change])
  }
  list(r = r, kind = k, txt = txt, raw = raw, role = role, share = share, three = three,
       dot3_rows = dot3_rows, all_pct = all_pct, all_unit = all_unit, units = units,
       has_pct = has_pct, has_unit = has_unit, hold = hold, value = val, new = new)
}

## The checks on labels (used for columns of labels and for mixtures): extra
## spaces, which are certainly a slip and are tidied; and labels that differ
## only in capitals or punctuation, which can carry meaning (genotypes AA, Aa,
## aa; codes T1 and T-1) and are therefore shown, never merged.
label_issues <- function(v, a, keep, at, add) {
  real <- intersect(which(!a$kind %in% c("blank", "mark")), keep)
  if (!length(real)) return(invisible())
  tidy <- label_tidy(a$raw)
  sp <- real[!is.na(a$raw[real]) & a$raw[real] != tidy[real]]
  if (length(sp))
    add(v, "correct", sprintf(paste0("In '%s', %s %s extra spaces in the label (%s). ",
      "They will be removed so that %s the same label as the rest."), v, rows_text(at(sp)),
      pl(length(sp), "has", "have"), eg_text(a$raw[sp]),
      pl(length(sp), "it counts as", "they count as")), at(sp))
  if (length(real) < 2) return(invisible())
  lab <- tidy[real]
  g1 <- split(real, tolower(lab))
  g1 <- g1[vapply(g1, function(i) length(unique(tidy[i])) > 1, logical(1))]
  if (length(g1)) {
    ex <- head_more(vapply(g1, function(i) join_and(sprintf("'%s'", unique(tidy[i]))), ""), 4)
    add(v, "check", sprintf(paste0("In '%s', some labels differ only in capital letters (%s), so ",
      "the analysis treats them as different groups. If they are the same, retype them in the ",
      "table so they match exactly."), v, paste(ex, collapse = "; ")),
      at(sort(unlist(g1))), fixable = FALSE)
  }
  ## the same letters and the same numbers written with different spacing or
  ## punctuation (T1, T-1, T 1); labels whose numbers differ (1.5 kg and
  ## 15 kg, -10 and 10) are not grouped
  nums <- vapply(regmatches(lab, gregexpr("(?<![[:alnum:].])[-+]?[0-9]+([.][0-9]+)?|[0-9]+([.][0-9]+)?",
                                          lab, perl = TRUE)), paste, "", collapse = "|")
  key <- paste(gsub("[^[:alpha:]]", "", tolower(lab)), nums)
  g2 <- split(real, key)
  g2 <- g2[vapply(g2, function(i) length(unique(tolower(tidy[i]))) > 1, logical(1))]
  if (length(g2)) {
    ## one spelling for each way of writing it, ignoring capitals (those are
    ## reported above), with the rows that use exactly that spelling
    rep_rows <- lapply(g2, function(i) {
      cls <- split(i, tolower(tidy[i]))
      unlist(lapply(cls, function(j) { tb <- table(tidy[j]); j[tidy[j] == names(tb)[which.max(tb)]] }))
    })
    ex <- head_more(vapply(rep_rows, function(i) {
      u <- unique(tidy[i])
      join_and(sprintf("'%s' (%s)", u, vapply(u, function(z) rows_text(at(i[tidy[i] == z])), "")))
    }, ""), 4)
    add(v, "check", sprintf(paste0("In '%s', some labels differ only in spaces or punctuation: %s. ",
      "The analysis treats them as different groups. If they are the same, retype them in the ",
      "table so they match exactly."), v, paste(ex, collapse = "; ")),
      at(sort(unlist(rep_rows))), fixable = FALSE)
  }
}

#' Check data for common entry problems
#'
#' Looks through every column for the problems that most often spoil an
#' analysis of field data: numbers written with a decimal comma, decimal points
#' and commas mixed in one column, thousands separators, per cent signs and
#' units typed after numbers, numbers stored as text, entries that are not
#' numbers, empty cells and the markers people type for them (such as n/a or
#' Excel's #DIV/0!), labels that differ only in spacing, capitals or
#' punctuation, and empty or repeated rows. Each problem is described with the
#' rows it affects and, where a correction is safe, what the correction would
#' be. Nothing is changed; see \code{\link{fix_data}}.
#'
#' @param d A data frame, as loaded.
#'
#' @return A list with \code{issues}, a list with one element per problem
#'   (\code{column}; \code{level}: \code{"correct"} for a correction
#'   \code{\link{fix_data}} will make, \code{"check"} for something only the
#'   user can settle, \code{"note"} for information; \code{message};
#'   \code{rows}, as row numbers of the data; \code{fixable}), and
#'   \code{numeric} and \code{labels}, the columns read as numbers and as
#'   labels.
#'
#' @examples
#' d <- data.frame(Variety = c("V1", "V1", "V2", "V2"),
#'                 Yield = c("5,6", "6.1", "n/a", "12a"))
#' chk <- check_data(d)
#' vapply(chk$issues, `[[`, "", "message")
#'
#' @export
check_data <- function(d) {
  issues <- list()
  add <- function(column, level, message, rows = integer(0), fixable = level == "correct")
    issues[[length(issues) + 1L]] <<- list(column = column, level = level, message = message,
                                           rows = rows, fixable = fixable)
  ids <- row_ids(d)
  numeric_cols <- character(0); label_cols <- character(0)

  empty <- empty_rows(d)
  if (length(empty))
    add(NA, "correct", sprintf("%s %s no values and will be removed.",
      sub("^r", "R", rows_text(ids[empty])), pl(length(empty), "has", "have")), ids[empty])
  keep <- setdiff(seq_len(nrow(d)), empty)
  at <- function(i) ids[intersect(i, keep)]

  for (v in names(d)) {
    x <- d[[v]]
    if (is.numeric(x)) {
      numeric_cols <- c(numeric_cols, v)
      miss <- at(which(is.na(x)))
      if (length(miss))
        add(v, "note", sprintf("In '%s', %s %s empty, so %s left out of any analysis that uses '%s'.",
          v, rows_text(miss), pl(length(miss), "is", "are"), pl(length(miss), "it is", "they are"), v), miss)
      next
    }
    a <- assess_column(x)
    k <- a$kind; txt <- a$txt
    marks <- function() {
      w <- intersect(which(k == "mark"), keep)
      if (length(w))
        add(v, "correct", sprintf(paste0("In '%s', %s %s %s, meaning no value. %s will be emptied ",
          "and treated as missing. If a value meant zero, type 0 instead."), v, rows_text(at(w)),
          pl(length(w), "has", "have"), eg_text(txt[w]), pl(length(w), "It", "They")), at(w))
    }
    blanks <- function() {
      w <- at(which(k == "blank"))
      if (length(w))
        add(v, "note", sprintf("In '%s', %s %s empty, so %s left out of any analysis that uses '%s'.",
          v, rows_text(w), pl(length(w), "is", "are"), pl(length(w), "it is", "they are"), v), w)
    }
    if (a$role == "empty") {
      if (any(k == "mark")) { label_cols <- c(label_cols, v); marks() }
      else add(v, "note", sprintf("Column '%s' is empty.", v))
      next
    }

    if (a$role == "numbers") {
      numeric_cols <- c(numeric_cols, v)
      hp <- a$has_pct & k %in% NUMBER_KINDS; hu <- a$has_unit & k %in% NUMBER_KINDS
      ## while a doubtful entry waits for the user, the column's other number
      ## corrections wait too (see assess_column); say what is waiting
      pend <- if (a$hold) c(
        if (any(k[keep] == "comma")) "decimal commas",
        if (any(k[keep] %in% c("thousands", "european"))) "thousands separators",
        if (any(hp[keep])) "% signs", if (any(hu[keep])) "units") else character(0)
      waiting <- if (length(pend)) sprintf(" The other corrections to '%s' (%s) will be offered once this is settled.",
                                           v, join_and(pend)) else ""
      if (!a$hold) {
        w <- intersect(which(k == "comma"), keep)
        if (length(w))
          add(v, "correct", sprintf(paste0("In '%s', %s %s written with a decimal comma (%s)%s. ",
            "%s will be changed to %s, so %s becomes %s."), v, rows_text(at(w)),
            pl(length(w), "is", "are"), eg_text(txt[w]),
            if (any(k == "number" & grepl(".", txt, fixed = TRUE))) ", while other entries use a decimal point" else "",
            pl(length(w), "The comma", "The commas"), pl(length(w), "a point", "points"),
            txt[w[1]], a$new[w[1]]), at(w))
        w <- intersect(which(k == "thousands"), keep)
        if (length(w))
          add(v, "correct", sprintf(paste0("In '%s', %s %s a separator between the thousands (%s). ",
            "It will be removed, so %s becomes %s."), v, rows_text(at(w)),
            pl(length(w), "has", "have"), eg_text(txt[w]), txt[w[1]], a$new[w[1]]), at(w))
        w <- intersect(which(k == "european"), keep)
        if (length(w))
          add(v, "correct", sprintf(paste0("In '%s', %s %s written with a point between the thousands ",
            "and a decimal comma (%s). %s will be read the usual way, so %s becomes %s."),
            v, rows_text(at(w)), pl(length(w), "is", "are"), eg_text(txt[w]),
            pl(length(w), "It", "They"), txt[w[1]], a$new[w[1]]), at(w))
        w <- intersect(which(hp), keep)
        if (length(w) && a$all_pct)
          add(v, "correct", sprintf(paste0("In '%s', every value has a %% sign (%s). The signs will be ",
            "dropped, so %s becomes %s."), v, eg_text(txt[w]), txt[w[1]], a$new[w[1]]), at(w))
        w <- intersect(which(hu), keep)
        if (length(w) && a$all_unit)
          add(v, "correct", sprintf(paste0("In '%s', every value has the unit '%s' after it. It will be ",
            "dropped, so %s becomes %s; consider putting the unit in the column name."),
            v, a$units[1], txt[w[1]], a$new[w[1]]), at(w))
        w <- intersect(which(k == "number" & !is.na(a$new)), keep)
        if (length(w))
          add(v, "correct", sprintf(paste0("In '%s', %s %s a minus sign or space typed with an unusual ",
            "character (%s). %s will be written the usual way, as %s."), v, rows_text(at(w)),
            pl(length(w), "has", "have"), eg_text(a$raw[w]), pl(length(w), "It", "They"),
            join_and(head(a$new[w], 3))), at(w))
      }
      w <- intersect(which(hp), keep)
      if (length(w) && !a$all_pct)
        add(v, "check", sprintf(paste0("In '%s', %s %s a %% sign (%s) but other values do not, so they ",
          "may be on a different scale (62%% beside 0.45 could be 0.62). Make them consistent in the ",
          "table; until then %s left out of any analysis of '%s'."), v, rows_text(at(w)),
          pl(length(w), "has", "have"), eg_text(txt[w]), pl(length(w), "it is", "they are"), v),
          at(w), fixable = FALSE)
      w <- intersect(which(hu), keep)
      if (length(w) && !a$all_unit)
        add(v, "check", if (length(a$units) > 1)
          sprintf(paste0("In '%s', the values carry different units (%s). Convert them to one unit, ",
            "without the unit text, in the table; until then %s left out of any analysis of '%s'."),
            v, eg_text(txt[w]), pl(length(w), "that row is", "those rows are"), v)
          else sprintf(paste0("In '%s', %s %s the unit '%s' after the number but other values have none, ",
            "so they may be on a different scale. Make them consistent in the table; until then %s ",
            "left out of any analysis of '%s'."), v, rows_text(at(w)), pl(length(w), "has", "have"),
            a$units[1], pl(length(w), "it is", "they are"), v), at(w), fixable = FALSE)
      w <- intersect(which(k == "ambiguous"), keep)
      two <- function(z) { z <- sub(" ?%$", "", sub(" ?[A-Za-z].*$", "", z))
        c(gsub(",", "", z, fixed = TRUE), sub(",", ".", z, fixed = TRUE)) }
      w2 <- w[!is.na(a$new[w])]                 # settled, and their sign or unit can go
      if (length(w2) && !is.na(a$three))
        add(v, "correct", sprintf(paste0("In '%s', %s (%s) could mean %s or %s. %s will be read as %s, ",
          "because the rest of the column uses %s. If that is wrong, retype %s in the table."),
          v, rows_text(at(w2)), eg_text(txt[w2]), two(txt[w2[1]])[1], two(txt[w2[1]])[2],
          pl(length(w2), "It", "They"), a$new[w2[1]],
          if (a$three == "comma") "decimal commas" else "decimal points or thousands separators",
          pl(length(w2), "it", "them")), at(w2))
      else if (length(w) && is.na(a$three))
        add(v, "check", sprintf(paste0("In '%s', %s (%s) could mean %s or %s, and the rest of the ",
          "column does not show which. Retype %s as %s or as %s, whichever you meant; until then ",
          "%s left out of any analysis of '%s'.%s"), v, rows_text(at(w)), eg_text(txt[w]),
          two(txt[w[1]])[1], two(txt[w[1]])[2], pl(length(w), "it", "each"),
          two(txt[w[1]])[1], two(txt[w[1]])[2], pl(length(w), "it is", "they are"), v, waiting),
          at(w), fixable = FALSE)
      w <- intersect(a$dot3_rows, keep)
      if (length(w))
        add(v, "check", sprintf(paste0("In '%s', %s (%s) could mean %s or %s: the rest of the column ",
          "uses decimal commas, where a point usually separates thousands. Retype %s as %s or as %s, ",
          "whichever you meant.%s"), v, rows_text(at(w)), eg_text(txt[w]),
          gsub(".", "", txt[w[1]], fixed = TRUE), txt[w[1]], pl(length(w), "it", "each"),
          gsub(".", "", txt[w[1]], fixed = TRUE), txt[w[1]],
          if (any(k[keep] == "ambiguous") && is.na(a$three)) "" else waiting),
          at(w), fixable = FALSE)
      nil <-intersect(which(k == "text" & tolower(txt) %in% c("nil", "none", "zero")), keep)
      if (length(nil))
        add(v, "check", sprintf(paste0("In '%s', %s %s %s. If %s zero, type 0; if %s no value, clear ",
          "the cell. Until then %s left out of any analysis of '%s'."), v, rows_text(at(nil)),
          pl(length(nil), "has", "have"), eg_text(txt[nil]), pl(length(nil), "it means", "they mean"),
          pl(length(nil), "it means", "they mean"), pl(length(nil), "that row is", "those rows are"), v),
          at(nil), fixable = FALSE)
      w <- setdiff(intersect(which(k == "text"), keep), nil)
      if (length(w)) {
        recurs <- any(duplicated(tolower(txt[w])))
        add(v, "check", sprintf(paste0("'%s' is mostly numbers, but %s %s %s (%s).%s If '%s' holds ",
          "measurements, correct %s in the table; until then %s left out of any analysis of '%s'."),
          v, rows_text(at(w)), pl(length(w), "has", "have"),
          pl(length(w), "an entry that is not a number", "entries that are not numbers"), eg_text(txt[w]),
          if (recurs) sprintf(" That is fine if '%s' holds labels, such as treatment codes with a check.", v)
          else "", v, pl(length(w), "it", "them"), pl(length(w), "that row is", "those rows are"), v),
          at(w), fixable = FALSE)
      }
      if (!any(k[keep] %in% c("comma", "thousands", "european", "percent", "unit", "ambiguous",
                              "text", "mark")) && !length(a$dot3_rows))
        add(v, "correct", sprintf(paste0("'%s' contains only numbers but is being read as text. It ",
          "will be changed to a column of numbers."), v))
      marks(); blanks()
      next
    }

    label_cols <- c(label_cols, v)
    if (a$role == "mixed") {
      w <- intersect(which(!k %in% c(NUMBER_KINDS, "blank", "mark")), keep)
      add(v, "check", sprintf(paste0("'%s' is being read as labels, not numbers, because %d of its %d ",
        "filled-in entries %s (%s). If it should hold numbers, correct %s in the table."), v,
        length(w), sum(!k[keep] %in% c("blank", "mark")),
        pl(length(w), "is not a number", "are not numbers"), eg_text(txt[w]),
        pl(length(w), "that entry", "those entries")), at(w), fixable = FALSE)
    }
    marks(); blanks()
    label_issues(v, a, keep, at, add)
  }

  ## identical rows: often two plots that happen to have the same values, so
  ## this is information, not an error
  if (length(keep) > 1) {
    rk <- do.call(paste, c(lapply(d[keep, , drop = FALSE], as.character), sep = "|"))
    grp <- split(keep, rk)
    grp <- grp[lengths(grp) > 1]
    if (length(grp)) {
      sets <- vapply(grp, function(i) join_and(as.character(ids[i])), "")
      rest <- sets[-1]
      more <- if (!length(rest)) "" else paste0(", and so are rows ",
        paste(head(rest, 3), collapse = "; rows "),
        if (length(rest) > 3) sprintf("; and %d more groups", length(rest) - 3) else "")
      add(NA, "note", sprintf(paste0("Rows %s are identical in every column%s. If a row was entered ",
        "twice, delete the extra copy in your spreadsheet and load it again; if they are separate ",
        "plots that happen to have the same values, leave them."), sets[1], more),
        ids[sort(unlist(grp))])
    }
  }
  list(issues = issues, numeric = numeric_cols, labels = label_cols)
}

#' Apply the safe corrections found by check_data
#'
#' Makes only the corrections whose meaning is certain, entry by entry:
#' decimal commas become points, thousands separators are removed, a per cent
#' sign or a unit carried by every value in a column is dropped, entries such
#' as n/a or - are emptied, extra spaces in labels are removed, and empty rows
#' are deleted. A column becomes a column of numbers once every entry in it is
#' a number. Entries whose meaning is not certain, such as 12a, nil, or 1,250
#' where the column does not show which notation it uses, are left exactly as
#' they are, and labels that differ in capitals or punctuation are not merged.
#'
#' @param d A data frame, as loaded.
#'
#' @return A list with \code{data}, the corrected data frame (row names kept,
#'   so row numbers still match the data as loaded), and \code{log}, a
#'   character vector saying what was changed.
#'
#' @examples
#' d <- data.frame(Variety = c("V1", "V1", "V2", "V2"),
#'                 Yield = c("5,6", "6.1", "n/a", "7,2"))
#' fix_data(d)
#'
#' @export
fix_data <- function(d) {
  log <- character(0)
  ids <- row_ids(d)
  empty <- empty_rows(d)
  keep <- setdiff(seq_len(nrow(d)), empty)
  for (v in names(d)) {
    x <- d[[v]]
    if (is.numeric(x)) next
    a <- assess_column(x)
    k <- a$kind
    out <- a$raw
    say <- function(w, what) if (length(w))
      log <<- c(log, sprintf("'%s': %s (%s).", v, what, rows_text(ids[w])))
    if (a$role == "numbers") {
      ch <- intersect(which(!is.na(a$new)), keep)
      out[ch] <- a$new[ch]
      say(intersect(which(k == "comma"), ch), "decimal commas changed to points")
      say(intersect(which(k %in% c("thousands", "european")), ch), "thousands separators removed")
      say(intersect(which(k == "percent"), ch), "per cent signs dropped")
      say(intersect(which(k == "unit"), ch), sprintf("the unit '%s' dropped", a$units[1]))
      say(intersect(which(k == "ambiguous"), ch),
          if (identical(a$three, "comma")) "entries such as 1,250 read as decimals"
          else "entries such as 1,250 read as thousands")
      say(intersect(which(k == "number"), ch), "unusual minus signs or spaces tidied")
    }
    mk <- intersect(which(k == "mark"), keep)
    out[mk] <- NA
    say(mk, "entries meaning no value emptied")
    if (a$role %in% c("labels", "mixed")) {
      tidy <- label_tidy(a$raw)
      sp <- intersect(which(!is.na(out) & out != tidy & !k %in% c("blank", "mark")), keep)
      out[sp] <- tidy[sp]
      say(sp, "extra spaces removed from labels")
    }
    ## a column of numbers in which every entry is now certainly a number
    ## becomes one; the values come from the parsed entries, so nothing that
    ## reads as a number is lost in the conversion
    if (a$role == "numbers") {
      after <- read_entries(out[keep])
      if (all(after$kind %in% c("number", "blank")) && !length(a$dot3_rows)) {
        num <- rep(NA_real_, length(out)); num[keep] <- after$value
        out <- num
        log <- c(log, sprintf("'%s' is now a column of numbers.", v))
      } else if (length(intersect(which(!is.na(a$new) | k == "mark"), keep)))
        log <- c(log, sprintf(paste0("'%s' still has entries that are not certain numbers, so it ",
          "stays as text until they are corrected (see 'Needs you to decide')."), v))
    }
    if (is.numeric(out)) d[[v]] <- out
    else if (!identical(out, a$raw)) {
      d[[v]] <- if (is.factor(x)) {
        ## keep the order the factor's levels had, under their new spellings
        old <- vapply(levels(x), function(l) { i <- match(l, a$raw); if (is.na(i)) NA_character_ else out[i] }, "")
        factor(out, levels = unique(c(stats::na.omit(old), unique(stats::na.omit(out)))))
      } else out
    }
  }
  if (length(empty)) {
    log <- c(log, sprintf("%s had no values and %s removed; the other rows keep their numbers.",
      sub("^r", "R", rows_text(ids[empty])), pl(length(empty), "has been", "have been")))
    d <- d[-empty, , drop = FALSE]
    ## a level used only in a removed row would linger in a factor otherwise
    d[] <- lapply(d, function(z) if (is.factor(z)) droplevels(z) else z)
  }
  list(data = d, log = log)
}

## The data check as HTML for the Data tab: green when nothing needs the
## user, otherwise the problems grouped as what can be corrected, what needs
## the user and what is for information.
check_html <- function(chk, d) {
  is_ <- chk$issues
  lv <- vapply(is_, `[[`, character(1), "level")
  cols <- function(x) join_and(sprintf("<b>%s</b>", esc(x)))
  head_ <- paste0(sprintf("%d %s and %d %s.", nrow(d), pl(nrow(d), "row", "rows"),
                          ncol(d), pl(ncol(d), "column", "columns")),
                  if (length(chk$numeric)) paste0(" Read as numbers: ", cols(chk$numeric), ".") else "",
                  if (length(chk$labels)) paste0(" Read as labels: ", cols(chk$labels), ".") else "")
  if (!length(is_))
    return(paste0("<div class='sugbox'><b>Data check: no problems found.</b> ", head_, "</div>"))
  block <- function(level, title)
    if (any(lv == level)) paste0("<p><b>", title, "</b></p><ul>",
      paste0("<li>", esc(vapply(is_[lv == level], `[[`, character(1), "message")), "</li>",
             collapse = ""), "</ul>") else ""
  paste0("<div class='", if (any(lv %in% c("correct", "check"))) "warn" else "sugbox", "'>",
         "<b>Data check.</b> ", head_,
         block("correct", "Can be corrected for you"),
         block("check", "Needs you to decide"),
         block("note", "For information"),
         "</div>")
}

## Whether double quotes in some lines of data are real quoting or inch marks
## (12" pot). Real quoting always leaves an even number of quotes on a line;
## an inch mark leaves an odd number, and reading it as a quote would swallow
## the rest of the line and the next ones. So quotes are honoured only when
## every line has an even number of them.
quote_char <- function(lines) {
  n <- lengths(regmatches(lines, gregexpr("\"", lines, fixed = TRUE)))
  if (any(n %% 2 == 1)) "" else "\""
}

## Turn a column of text into numbers only when nothing is lost: labels such
## as 1.1 and 1.10, or 01 and 1, must not become the same number. Anything
## else stays as text for the data check to look at.
convert_lossless <- function(col) {
  if (!is.character(col)) return(col)
  conv <- utils::type.convert(col, as.is = TRUE, na.strings = c("NA", ""))
  if (!is.numeric(conv)) return(conv)
  s <- trimws(col); ok <- !is.na(col) & s != ""
  if (length(unique(s[ok])) > length(unique(conv[ok]))) col else conv
}

## Read an uploaded CSV. The separator is the one (comma, semicolon or tab)
## that splits every line into the same number of fields, at least two; a
## comma file with unquoted decimal commas, which would be cut apart wrongly,
## is refused with a reason. An inch mark (12" pot) that opens a quoted field
## and swallows lines is read again without quoting. Text that looks like
## numbers is converted only when nothing is lost, so the data check sees
## decimal commas and no-value markers as typed.
read_upload <- function(path) {
  counts <- function(s, q) tryCatch({
    n <- utils::count.fields(path, sep = s, quote = q, comment.char = "", blank.lines.skip = TRUE)
    n[!is.na(n)] }, error = function(e) integer(0))
  pick <- function(q) {
    fits <- vapply(c(",", ";", "\t"), function(s) {
      n <- counts(s, q); if (length(n) && all(n == n[1]) && n[1] > 1) n[1] else 0L }, integer(1))
    if (any(fits > 0)) c(",", ";", "\t")[which.max(fits)] else NA_character_
  }
  quote <- quote_char(readLines(path, warn = FALSE))
  sep <- pick(quote)
  if (is.na(sep)) {
    one <- vapply(c(",", ";", "\t"), function(s) { n <- counts(s, ""); length(n) > 0 && all(n == 1) }, logical(1))
    if (!all(one))
      stop(paste0("The lines of this file do not all have the same number of values. This usually ",
                  "means numbers with decimal commas in a comma-separated file. Save it from Excel as ",
                  "'CSV (semicolon delimited)', or copy the cells and use the paste box."), call. = FALSE)
    sep <- ";"
  }
  d <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
                       na.strings = c("NA", ""), strip.white = TRUE, row.names = NULL,
                       quote = quote, comment.char = "", colClasses = "character")
  d[] <- lapply(d, convert_lossless)
  names(d) <- make.names(names(d), unique = TRUE)
  d
}

## Rows left out of an analysis and why: one record for each row and column
## that had no usable value. `raw` is the data as supplied, `d` the same rows
## after the response was read as numbers and no-value markers in the factors
## were emptied, `keep` the columns the analysis needs.
excluded_rows <- function(raw, d, keep) {
  miss <- is.na(as.matrix(d[, keep, drop = FALSE]))
  drop <- which(rowSums(miss) > 0)
  if (!length(drop)) return(NULL)
  idx <- which(miss[drop, , drop = FALSE], arr.ind = TRUE)
  r <- drop[idx[, 1]]; cols <- keep[idx[, 2]]
  vals <- vapply(seq_along(r), function(j) {
    z <- raw[[cols[j]]][r[j]]; if (is.na(z)) "" else trimws(as.character(z)) }, "")
  out <- data.frame(row = row_ids(raw)[r], column = cols, kind = read_entries(vals)$kind,
                    value = vals, stringsAsFactors = FALSE, row.names = NULL)
  out[order(out$row), , drop = FALSE]
}

## "3 rows were left out of this analysis: Yield is written with a decimal
## comma in rows 1 and 4 ('6,5' and '7,2'); Yield is empty in row 9." Grouped
## by column and reason so a long list reads as a few sentences. `html`
## escapes the user's entries; `hint` adds what to do in the app.
excluded_text <- function(ex, html = FALSE, hint = FALSE) {
  if (is.null(ex) || !nrow(ex)) return("")
  n <- length(unique(ex$row))
  grp <- split(ex, paste(ex$column, ex$kind))
  grp <- grp[order(vapply(grp, function(g) min(g$row), numeric(1)))]
  parts <- vapply(grp, function(g) {
    v <- g$column[1]; rr <- rows_text(unique(g$row)); eg <- eg_text(g$value)
    switch(g$kind[1],
      blank = sprintf("%s is empty in %s", v, rr),
      mark = sprintf("%s is marked as having no value in %s (%s)", v, rr, eg),
      comma = sprintf("%s is written with a decimal comma in %s (%s)", v, rr, eg),
      ambiguous = sprintf("%s could be read two ways in %s (%s)", v, rr, eg),
      percent = sprintf("%s has a %% sign in %s (%s)", v, rr, eg),
      unit = sprintf("%s has a unit after the number in %s (%s)", v, rr, eg),
      thousands = , european = sprintf("%s has a thousands separator in %s (%s)", v, rr, eg),
      sprintf("%s is not a number in %s (%s)", v, rr, eg))
  }, "")
  out <- sprintf("%d %s left out of this analysis: %s.", n, pl(n, "row was", "rows were"),
                 paste(parts, collapse = "; "))
  ## what to do in the app, only where something can be done: decimal commas
  ## and separators are corrected by the button; entries that are not numbers,
  ## doubtful ones and partial units or % signs need the user
  if (hint) {
    fixable <- any(ex$kind %in% c("comma", "thousands", "european"))
    manual <- any(ex$kind %in% c("text", "ambiguous", "percent", "unit"))
    if (fixable)
      out <- paste(out, "To include the rows with decimal commas or thousands separators, press 'Apply the corrections' on the Data tab.")
    if (manual)
      out <- paste(out, sprintf("To include %s, correct those entries in the table on the Data tab, where the data check lists them.",
                                if (fixable) "the others" else "these rows"))
    if (fixable || manual) out <- paste(out, "Then run the analysis again.")
  }
  if (html) esc(out) else out
}
