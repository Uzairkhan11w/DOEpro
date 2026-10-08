## The data-quality stage: reading entries, checking a data frame, applying
## only the corrections whose meaning is certain, reading pasted and uploaded
## data as typed, and reporting the rows an analysis leaves out.
##
## Expectations are worked out by hand from the rules, not taken from the
## code's own output:
##   * 5,6 is a decimal comma (5.6); 1,250 could be 1250 or 1.25 and is settled
##     only by the rest of the column (decimal commas and no decimal points:
##     a decimal; decimal points or separators and no decimal commas: a
##     thousands separator; otherwise the user decides);
##   * a column is read as numbers when at least three quarters of its
##     filled-in, non-marker entries read as numbers, as a mixture between a
##     quarter and three quarters, and as labels below a quarter;
##   * a correction is made only where its meaning is certain; nothing
##     uncertain (12a, a word among numbers, an unresolved 1,250, mixed units,
##     '1.250' among decimal commas) is ever changed or emptied, and labels that
##     differ in capitals or punctuation are never merged;
##   * applying the corrections twice changes nothing more than applying them
##     once, and leaves nothing that the check still offers to correct.

## ------------------------------------------------------------- helpers ----

## the issues for one column (NA for whole-row issues) whose message matches
issues_for <- function(chk, column, pattern, fixed = FALSE) {
  Filter(function(z) {
    same_col <- if (is.na(column)) is.na(z$column) else identical(z$column, column)
    same_col && grepl(pattern, z$message, fixed = fixed)
  }, chk$issues)
}

## exactly one matching issue, returned
the_issue <- function(chk, column, pattern, fixed = FALSE) {
  hit <- issues_for(chk, column, pattern, fixed)
  expect_length(hit, 1)
  if (length(hit)) hit[[1]] else list(level = NA, rows = NA, fixable = NA, message = "")
}

issue_messages <- function(chk) vapply(chk$issues, `[[`, character(1), "message")
issue_levels   <- function(chk) vapply(chk$issues, `[[`, character(1), "level")
any_fixable    <- function(chk) any(vapply(chk$issues, function(z) isTRUE(z$fixable), logical(1)))

## characters built from code points so this file stays ASCII
U_MINUS  <- "−"   # the minus sign Word inserts
EN_DASH  <- "–"
NBSP     <- " "   # no-break space
NNBSP    <- " "   # narrow no-break space (Excel's thousands gap)
THIN     <- " "   # thin space

## ------------------------------------------------------- read_entries -----

test_that("blank cells and no-value markers are recognised, Excel errors included", {
  r <- read_entries(c("", "   ", NA, " "))
  expect_identical(r$kind, rep("blank", 4))
  expect_true(all(is.na(r$value)))

  marks <- c("na", "N/A", "n.a.", "Missing", "-", "--", ".", "?", "*", "ND", "n.d.",
             "#N/A", "#DIV/0!", "#VALUE!", "#NUM!", "#REF!", "#NAME?", "#NULL!", " - ")
  r <- read_entries(marks)
  expect_identical(r$kind, rep("mark", length(marks)))
  expect_true(all(is.na(r$value)))
  # the text is kept, trimmed, so the check can quote what was typed
  expect_identical(r$text[19], "-")
  # "nil" is not a marker: in field records it usually means zero, so the
  # user is asked rather than the value emptied
  expect_identical(read_entries("nil")$kind, "text")
  chk <- check_data(data.frame(Count = c("3", "nil", "5", "4")))
  expect_identical(the_issue(chk, "Count", "'nil'", fixed = TRUE)$level, "check")
  expect_identical(r$text[13], "#DIV/0!")
})

test_that("plain numbers with a decimal point read as numbers, scientific notation included", {
  x <- c("5.6", "-2", "+3", "1e3", "2.5E-2", "1E3", ".5", "7.", "0", " 12 ", " 5.6 ")
  r <- read_entries(x)
  expect_identical(r$kind, rep("number", length(x)))
  expect_equal(r$value, c(5.6, -2, 3, 1000, 0.025, 1000, 0.5, 7, 0, 12, 5.6))
  # 1e3 is a number, not 1 followed by the unit "e3"
  expect_identical(r$unit[4], "")
  # numbers already stored as numbers, factors and NA all go through the same way
  r <- read_entries(c(5.6, NA, 1e6))
  expect_identical(r$kind, c("number", "blank", "number"))
  expect_equal(r$value, c(5.6, NA, 1e6))
  r <- read_entries(factor(c("5,6", "7")))
  expect_identical(r$kind, c("comma", "number"))
  expect_equal(r$value, c(5.6, 7))
})

test_that("a decimal comma reads as the decimal it stands for", {
  x <- c("5,6", "12,25", "1234,5", "-0,5", "+3,75", "1,2345", " 6,1 ")
  r <- read_entries(x)
  expect_identical(r$kind, rep("comma", length(x)))
  expect_equal(r$value, c(5.6, 12.25, 1234.5, -0.5, 3.75, 1.2345, 6.1))
})

test_that("1,250 is ambiguous and is not given a value, with or without a sign or unit after it", {
  x <- c("1,250", "12,500", "-123,456", "999,999", "1,250%", "1,250 kg")
  r <- read_entries(x)
  expect_identical(r$kind, rep("ambiguous", length(x)))
  expect_true(all(is.na(r$value)))
})

test_that("thousands separators and European notation read as the whole number", {
  r <- read_entries(c("1,250,000", "1,250.5", "12,345,678.25", "-1,000,000", "1 250", "1 250 000", "1 250,5"))
  expect_identical(r$kind, rep("thousands", 7))
  expect_equal(r$value, c(1250000, 1250.5, 12345678.25, -1e6, 1250, 1250000, 1250.5))

  r <- read_entries(c("1.250,5", "12.345.678,9", "-1.000,25"))
  expect_identical(r$kind, rep("european", 3))
  expect_equal(r$value, c(1250.5, 12345678.9, -1000.25))
})

test_that("a per cent sign is read as the number before it, and the number part is described", {
  r <- read_entries(c("45%", "4,5 %", "12.5%", "45 %", "-3%"))
  expect_identical(r$kind, rep("percent", 5))
  expect_equal(r$value, c(45, 4.5, 12.5, 45, -3))
  expect_identical(r$unit, rep("%", 5))
  expect_identical(r$core, c("number", "comma", "number", "number", "number"))
})

test_that("a unit needs a space before it or at least two letters, and must be a known unit", {
  x <- c("45 cm", "2.5kg", "5,6 kg", "3 t/ha", "1.250,5 kg", "45 mm", "5 m")
  r <- read_entries(x)
  expect_identical(r$kind, rep("unit", length(x)))
  expect_equal(r$value, c(45, 2.5, 5.6, 3, 1250.5, 45, 5))
  expect_identical(r$unit, c("cm", "kg", "kg", "t/ha", "kg", "mm", "m"))
  expect_identical(r$core, c("number", "number", "comma", "number", "european", "number", "number"))
  # a code such as 12a, or 5m with one letter and no space, is not a number with a unit
  r <- read_entries(c("12a", "5m", "1e3a"))
  expect_identical(r$kind, rep("text", 3))
  expect_true(all(is.na(r$value)))
  # words after a number are not units unless they are known ones: a variety,
  # a growth stage and DMRT letters stay text
  r <- read_entries(c("1121 Basmati", "30 DAS", "12ab", "45 cm/s"))
  expect_identical(r$kind, rep("text", 4))
})

test_that("the Unicode minus and no-break spaces are read as the plain characters", {
  x <- c(paste0(U_MINUS, "3.5"), paste0(EN_DASH, "2"), paste0(U_MINUS, "0,5"),
         paste0("1", NBSP, "250"), paste0("1", NNBSP, "250,5"), paste0(NBSP, "5.6", NBSP),
         paste0("12", THIN, "345", THIN, "678"), U_MINUS)
  r <- read_entries(x)
  expect_identical(r$kind, c("number", "number", "comma", "thousands", "thousands", "number",
                             "thousands", "mark"))
  expect_equal(r$value, c(-3.5, -2, -0.5, 1250, 1250.5, 5.6, 12345678, NA))
  expect_identical(r$text, c("-3.5", "-2", "-0,5", "1 250", "1 250,5", "5.6", "12 345 678", "-"))
})

test_that("anything else is text with no value", {
  x <- c("12a", "abc", "5..6", "1.2.3", "5,6,7", "5 6", "$5", "abc%", "1,2.3,4", "V1", "<LOD", "%")
  r <- read_entries(x)
  expect_identical(r$kind, rep("text", length(x)))
  expect_true(all(is.na(r$value)))
  expect_identical(r$text, x)
})

## ------------------------------------------------------ assess_column -----

test_that("a column holds numbers when three quarters of its real entries are numbers", {
  role <- function(x) assess_column(x)$role
  expect_identical(role(c("1", "2", "3", "abc")), "numbers")                 # exactly 3 / 4
  expect_identical(role(c("1", "2", "abc")), "mixed")                        # 2 / 3
  expect_identical(role(c(as.character(1:6), "a", "b")), "numbers")          # 6 / 8
  expect_identical(role(c(as.character(1:5), "a", "b")), "mixed")            # 5 / 7
  expect_identical(role(c("1", "a", "b", "c")), "mixed")                     # exactly 1 / 4
  expect_identical(role(c("1", "a", "b", "c", "d")), "labels")               # 1 / 5
  expect_identical(role(c("V1", "V2", "V3")), "labels")
  expect_identical(role(c(5.6, 7, NA)), "numbers")
  # blanks and no-value markers are not counted either way
  expect_identical(role(c("1", "2", "3", "abc", "", NA, "n/a", "-", "#DIV/0!")), "numbers")
  expect_identical(role(c("", NA, "n/a", "-")), "empty")
  # every notation counts as a number, the ambiguous 1,250 included
  expect_identical(role(c("5,6", "1,250", "1.250,5", "45%", "1,250,000", "12a")), "numbers")
  expect_identical(role(c("45 cm", "2.5kg", "x", "y")), "mixed")
  expect_equal(assess_column(c("1", "2", "abc"))$share, 2 / 3)
})

test_that("1,250 is resolved from the notation the rest of the column uses", {
  three <- function(x) assess_column(x)$three
  # decimal commas and no decimal points: a decimal
  expect_identical(three(c("1,250", "5,6", "7")), "comma")
  expect_identical(three(c("1,250", "4,5 %", "7")), "comma")
  expect_identical(three(c("1,250", "5,6 kg", "7")), "comma")
  # decimal points or separators and no decimal commas: thousands
  expect_identical(three(c("1,250", "5.6")), "thousands")
  expect_identical(three(c("1,250", "2,500,000")), "thousands")
  # spaces between thousands are also used with decimal commas (1 250,5), so
  # they do not settle what 1,250 means
  expect_identical(three(c("1,250", "1 250 000", "12")), NA_character_)
  # nothing to go on, or both notations: the user decides
  expect_identical(three(c("1,250", "12", "300")), NA_character_)
  expect_identical(three(c("1,250", "2.5", "3,75")), NA_character_)
  expect_identical(three(c("1,250", "2,500,000", "5,6")), NA_character_)
})

test_that("1,250 in a column written in European notation is read as a decimal", {
  # 1.250,5 has a decimal comma and no decimal point, so by the rule a 1,250
  # beside it is a decimal: in that notation the thousands are marked by points
  expect_identical(assess_column(c("1,250", "1.250,5"))$three, "comma")
})

test_that("only entries whose correction is certain get a corrected form", {
  # a % sign on one value among plain numbers is not certain (62% beside 0.45
  # may be 0.62), so only the decimal comma and the separator are corrected
  a <- assess_column(c("5,6", "1,250,000", "45%", "n/a", "12a", "7", ""))
  expect_identical(a$role, "numbers")
  expect_identical(!is.na(a$new), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(as.numeric(a$new[1:2]), c(5.6, 1250000))
  # when every value has the sign it is dropped
  expect_equal(as.numeric(assess_column(c("45%", "4,5 %", "12%"))$new), c(45, 4.5, 12))
  # a resolved 1,250 is corrected; an unresolved one is not
  expect_equal(as.numeric(assess_column(c("1,250", "5,6", "7"))$new[1]), 1.25)
  expect_equal(as.numeric(assess_column(c("1,250", "2.5", "7"))$new[1]), 1250)
  expect_true(all(is.na(assess_column(c("1,250", "12", "15"))$new)))
  # one unit carried by every value can be dropped; mixed units, or a unit on
  # some values only, cannot
  expect_equal(as.numeric(assess_column(c("45 cm", "50cm", "52 cm"))$new), c(45, 50, 52))
  expect_true(all(is.na(assess_column(c("45 cm", "50cm", "52"))$new)))
  expect_true(all(is.na(assess_column(c("45 cm", "0.5 m", "52"))$new)))
  # nothing in a column of labels or a mixture is rewritten as a number
  expect_true(all(is.na(assess_column(c("V1", "V2", "5,6", "V3", "V4"))$new)))
  expect_true(all(is.na(assess_column(c("5,6", "x", "y"))$new)))
})

## --------------------------------------------------------- check_data -----

test_that("clean data raise no issues and the columns are sorted into numbers and labels", {
  chk <- check_data(demo_data("RCBD"))
  expect_length(chk$issues, 0)
  expect_identical(chk$numeric, "Yield")
  expect_identical(chk$labels, c("Block", "Variety"))
  chk <- check_data(demo_data("CRD"))
  expect_length(chk$issues, 0)
  expect_identical(chk$numeric, c("Rep", "Yield"))
  expect_identical(chk$labels, "Treatment")
})

test_that("every issue carries the fields the app relies on, and the levels mean what they say", {
  d <- data.frame(Variety = c("V1", "v1", "V-1", "", "V2", "V2", "V3", "V3", "n/a"),
                  Yield = c("5,6", "6.1", "n/a", "", "12a", "7.4", "7.5", "7.5", "1,250"),
                  Mix = c("1", "2", "a", "b", "c", "d", "e", "3", "4"),
                  stringsAsFactors = FALSE)
  chk <- check_data(d)
  expect_gt(length(chk$issues), 5)
  expect_setequal(unique(issue_levels(chk)), c("correct", "check", "note"))
  for (z in chk$issues) {
    expect_named(z, c("column", "level", "message", "rows", "fixable"))
    expect_true(z$level %in% c("correct", "check", "note"))
    expect_type(z$message, "character")
    expect_true(nzchar(z$message))
    expect_true(is.numeric(z$rows))
    expect_true(is.logical(z$fixable) && length(z$fixable) == 1 && !is.na(z$fixable))
    # a correction is always something fix_data will do; a check is never
    # fixable (only the user can settle it); a note is information
    expect_identical(z$fixable, z$level == "correct")
    # messages are plain text: HTML is added, and escaped, by check_html
    expect_no_match(z$message, "<[a-z/]", perl = TRUE)
  }
})

test_that("decimal commas are found and offered as a correction", {
  d <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c("5,6", "6,1", "7", "8,25"))
  chk <- check_data(d)
  expect_true("Yield" %in% chk$numeric)
  iss <- the_issue(chk, "Yield", "written with a decimal comma")
  expect_identical(iss$level, "correct")
  expect_true(iss$fixable)
  expect_equal(iss$rows, c(1, 2, 4))
  expect_match(iss$message, "'5,6'", fixed = TRUE)
  expect_match(iss$message, "5.6", fixed = TRUE)
  # the column does not use decimal points, so it is not said to mix them
  expect_no_match(iss$message, "decimal points", fixed = TRUE)
})

test_that("decimal points and commas in one column are reported as a mixture", {
  d <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c("5,6", "6.1", "7.2", "8,25"))
  chk <- check_data(d)
  iss <- the_issue(chk, "Yield", "written with a decimal comma")
  expect_identical(iss$level, "correct")
  expect_true(iss$fixable)
  expect_equal(iss$rows, c(1, 4))
  expect_match(iss$message, "other entries use a decimal point", fixed = TRUE)
})

test_that("thousands separators, European notation and per cent signs are offered as corrections", {
  d <- data.frame(Big = c("1,250,000", "2,500.5", "3000"),
                  Eu = c("1.250,5", "2.000,25", "3000,5"),
                  Pct = c("45%", "50 %", "12,5%"))
  chk <- check_data(d)
  expect_true(all(c("Big", "Eu", "Pct") %in% chk$numeric))
  iss <- the_issue(chk, "Big", "thousands")
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, c(1, 2))
  expect_match(iss$message, "1250000", fixed = TRUE)
  iss <- the_issue(chk, "Eu", "point between the thousands")
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, c(1, 2))
  expect_equal(the_issue(chk, "Eu", "written with a decimal comma")$rows, 3)
  iss <- the_issue(chk, "Pct", "% sign")
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, c(1, 2, 3))
  expect_match(iss$message, "becomes 45", fixed = TRUE)
  # a % sign on some values only may mean another scale: the user decides
  chk <- check_data(data.frame(P = c("0.45", "0.50", "62%", "0.38")))
  iss <- the_issue(chk, "P", "% sign")
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 3)
})

test_that("one unit in the whole column is offered for dropping; different units are for the user", {
  chk <- check_data(data.frame(H = c("45 cm", "50 cm", "52cm", "48 cm")))
  iss <- the_issue(chk, "H", "unit")
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, 1:4)
  expect_match(iss$message, "'cm'", fixed = TRUE)
  # a unit on some values only may mean another scale (1.5 kg beside 1500)
  chk <- check_data(data.frame(W = c("1500", "1200", "1.5 kg", "1350")))
  iss <- the_issue(chk, "W", "unit")
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 3)
  # units are compared exactly: Mg (megagrams) is not mg
  chk <- check_data(data.frame(Y = c("5 Mg", "300 mg", "4 Mg", "250 mg")))
  expect_identical(the_issue(chk, "Y", "different units")$level, "check")

  chk <- check_data(data.frame(H = c("45 cm", "0.5 m", "52 cm", "48")))
  iss <- the_issue(chk, "H", "different units")
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 1:3)
  expect_false(any_fixable(chk))
})

test_that("an unresolved 1,250 is left for the user and a resolved one is corrected", {
  # nothing else in the column says which notation is meant
  chk <- check_data(data.frame(Trt = c("A", "B", "C", "D"), Yield = c("1,250", "2,500", "12", "15")))
  iss <- the_issue(chk, "Yield", "'1,250'", fixed = TRUE)
  expect_identical(iss$level, "check")
  expect_false(iss$fixable)
  expect_equal(iss$rows, c(1, 2))
  expect_match(iss$message, "1250", fixed = TRUE)
  expect_match(iss$message, "1.250", fixed = TRUE)
  expect_false(any_fixable(chk))
  # the rest of the column writes decimals with commas: 1,250 is 1.25
  chk <- check_data(data.frame(Yield = c("1,250", "5,6", "7,25", "8")))
  expect_false("check" %in% issue_levels(chk))
  iss <- the_issue(chk, "Yield", "'1,250'", fixed = TRUE)
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, 1)
  expect_match(iss$message, "read as 1.25", fixed = TRUE)
  expect_equal(the_issue(chk, "Yield", "written with a decimal comma")$rows, c(2, 3))
  # the rest of the column writes decimals with points: 1,250 is 1250
  chk <- check_data(data.frame(Yield = c("1,250", "2.5", "3.75")))
  expect_false("check" %in% issue_levels(chk))
  iss <- the_issue(chk, "Yield", "'1,250'", fixed = TRUE)
  expect_identical(iss$level, "correct")
  expect_match(iss$message, "read as 1250", fixed = TRUE)
  # both notations in the column: 1,250 is for the user, and the decimal
  # comma waits too, because correcting it would change the evidence and a
  # second pass would then settle 1,250 by itself
  chk <- check_data(data.frame(Yield = c("1,250", "2.5", "3,75", "4")))
  iss <- the_issue(chk, "Yield", "'1,250'", fixed = TRUE)
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 1)
  expect_match(iss$message, "will be offered once this is settled", fixed = TRUE)
  expect_length(issues_for(chk, "Yield", "written with a decimal comma"), 0)
  expect_false(any_fixable(chk))
})

test_that("no-value markers are offered to be emptied, in numbers and in labels", {
  d <- data.frame(Trt = LETTERS[1:5], Yield = c("5.6", "n/a", "-", "#DIV/0!", "7.1"))
  chk <- check_data(d)
  iss <- the_issue(chk, "Yield", "no value")
  expect_identical(iss$level, "correct")
  expect_true(iss$fixable)
  expect_equal(iss$rows, 2:4)
  expect_match(iss$message, "'n/a'", fixed = TRUE)
  expect_match(iss$message, "#DIV/0!", fixed = TRUE)

  chk <- check_data(data.frame(V = c("V1", "-", "V2", "n/a"), Y = 1:4))
  iss <- the_issue(chk, "V", "no value")
  expect_identical(iss$level, "correct")
  expect_equal(iss$rows, c(2, 4))
})

test_that("an entry that is not a number is named and left for the user", {
  d <- data.frame(Trt = LETTERS[1:5], Yield = c("5.6", "12a", "7.1", "8.0", "6.2"))
  chk <- check_data(d)
  expect_true("Yield" %in% chk$numeric)
  iss <- the_issue(chk, "Yield", "'12a'", fixed = TRUE)
  expect_identical(iss$level, "check")
  # it is never emptied, so there is nothing to apply
  expect_false(iss$fixable)
  expect_false(any_fixable(chk))
  expect_equal(iss$rows, 2)
  expect_match(iss$message, "left out of any analysis of 'Yield'", fixed = TRUE)
  # a one-off entry is not suggested to be a label
  expect_no_match(iss$message, "labels", fixed = TRUE)

  # entries that recur may be codes, such as a check among numbered treatments
  chk <- check_data(data.frame(Code = c("1", "2", "3", "4", "5", "6", "ck", "CK")))
  expect_true("Code" %in% chk$numeric)
  iss <- the_issue(chk, "Code", "'ck'", fixed = TRUE)
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 7:8)
  expect_match(iss$message, "fine if 'Code' holds labels", fixed = TRUE)
  expect_match(iss$message, "left out of any analysis of 'Code'", fixed = TRUE)
})

test_that("the message for an entry that is not a number says so, in tidy words", {
  chk <- check_data(data.frame(Trt = LETTERS[1:5], Yield = c("5.6", "12a", "7.1", "8.0", "6.2")))
  msg <- the_issue(chk, "Yield", "'12a'", fixed = TRUE)$message
  expect_match(msg, "not a number|not numbers")
  expect_no_match(msg, "  ", fixed = TRUE)
})

test_that("a column between numbers and labels is read as labels, and the user is told", {
  chk <- check_data(data.frame(X = c("1", "2", "a", "b", "c")))
  expect_true("X" %in% chk$labels)
  expect_false("X" %in% chk$numeric)
  iss <- the_issue(chk, "X", "read as labels")
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 3:5)
  expect_match(iss$message, "'a'", fixed = TRUE)
})

test_that("blanks are noted, and numbers stored as text are offered as a correction", {
  d <- data.frame(Trt = LETTERS[1:5], Yield = c("5.6", "", "7.1", NA, "8"))
  chk <- check_data(d)
  iss <- the_issue(chk, "Yield", "empty")
  expect_identical(iss$level, "note")
  expect_false(iss$fixable)
  expect_equal(iss$rows, c(2, 4))
  iss <- the_issue(chk, "Yield", "read as text")
  expect_identical(iss$level, "correct")
  expect_true(iss$fixable)
  expect_length(iss$rows, 0)
  # a column R already reads as numbers is not "read as text"
  chk <- check_data(data.frame(Trt = LETTERS[1:3], Yield = c(5.6, NA, 7)))
  expect_length(issues_for(chk, "Yield", "read as text"), 0)
  iss <- the_issue(chk, "Yield", "empty")
  expect_identical(iss$level, "note")
  expect_false(iss$fixable)
  expect_equal(iss$rows, 2)
})

test_that("an entirely empty column is noted", {
  d <- data.frame(Trt = LETTERS[1:3], Yield = c(5, 6, 7), Extra = NA)
  chk <- check_data(d)
  iss <- the_issue(chk, "Extra", "empty")
  expect_identical(iss$level, "note")
  expect_false(iss$fixable)
})

test_that("empty rows are offered for removal and are not reported again", {
  d <- data.frame(Trt = c("A", "B", "", NA, "C"), Yield = c("5.6", "6.1", "", NA, "7.0"))
  chk <- check_data(d)
  iss <- the_issue(chk, NA, "no values")
  expect_identical(iss$level, "correct")
  expect_true(iss$fixable)
  expect_equal(iss$rows, c(3, 4))
  expect_match(iss$message, "Rows 3 and 4 have no values", fixed = TRUE)
  # no other issue is about those rows: not a blank label, not a missing value,
  # and two empty rows are not "identical rows"
  others <- Filter(function(z) !identical(z, iss), chk$issues)
  expect_false(any(vapply(others, function(z) any(z$rows %in% 3:4), logical(1))))
  expect_length(issues_for(chk, NA, "identical"), 0)
})

test_that("extra spaces in a label are offered for tidying", {
  d <- data.frame(V = c("V1", "V1 ", " V2", "V2", "V  3", "V 3"), Y = 1:6)
  chk <- check_data(d)
  iss <- the_issue(chk, "V", "extra spaces")
  expect_identical(iss$level, "correct"); expect_true(iss$fixable)
  expect_equal(iss$rows, c(2, 3, 5))
})

test_that("labels that differ only in capitals are reported, never offered for merging", {
  d <- data.frame(Variety = c("Control", "control", "Control", "CONTROL ", "T1", "T1"),
                  Yield = c(5, 6, 7, 8, 9, 10))
  chk <- check_data(d)
  iss <- the_issue(chk, "Variety", "capital")
  expect_identical(iss$level, "check")
  expect_false(iss$fixable)
  expect_equal(iss$rows, 1:4)
  expect_match(iss$message, "'Control'", fixed = TRUE)
  expect_match(iss$message, "'control'", fixed = TRUE)
  expect_match(iss$message, "'CONTROL'", fixed = TRUE)
  # the only correction offered is the trailing space in row 4
  fixes <- Filter(function(z) z$level == "correct", chk$issues)
  expect_length(fixes, 1)
  expect_equal(fixes[[1]]$rows, 4)
  # genotypes are the textbook case where capitals carry meaning
  chk <- check_data(data.frame(G = c("AA", "Aa", "aa", "AA"), Y = 1:4))
  iss <- the_issue(chk, "G", "capital")
  expect_identical(iss$level, "check"); expect_false(iss$fixable)
  expect_equal(iss$rows, 1:4)
  expect_false(any_fixable(chk))
})

test_that("labels that differ only in punctuation or spacing are for the user to settle", {
  d <- data.frame(Variety = c("V-1", "V1", "V.1", "V1", "V 2", "V2"), Yield = 1:6)
  chk <- check_data(d)
  expect_length(issues_for(chk, "Variety", "capital"), 0)
  iss <- the_issue(chk, "Variety", "punctuation")
  expect_identical(iss$level, "check")
  expect_false(iss$fixable)
  expect_equal(iss$rows, 1:6)
  for (lab in c("'V-1'", "'V.1'", "'V 2'", "'V2'")) expect_match(iss$message, lab, fixed = TRUE)
  expect_false(any_fixable(chk))
})

test_that("labels whose numbers differ are not called the same label", {
  # 1.5 kg and 15 kg differ only by a point, but they are different doses
  d <- data.frame(Dose = c("Dose 1.5 kg", "Dose 15 kg", "Dose 1.5 kg", "Dose 15 kg"), Y = 1:4)
  chk <- check_data(d)
  expect_length(issues_for(chk, "Dose", "punctuation"), 0)
  expect_length(chk$issues, 0)
})

test_that("a case difference and a punctuation difference in one label give one issue each", {
  d <- data.frame(Variety = c("V1", "v1", "V1", "V-1"), Yield = 1:4)
  chk <- check_data(d)
  iss <- the_issue(chk, "Variety", "capital")
  expect_identical(iss$level, "check")
  expect_equal(iss$rows, 1:3)
  iss <- the_issue(chk, "Variety", "punctuation")
  expect_identical(iss$level, "check")
  expect_false(iss$fixable)
  expect_equal(iss$rows, c(1, 3, 4))
})

test_that("blank labels are noted with their rows", {
  d <- data.frame(Variety = c("V1", "", "V2", NA), Yield = c(1, 2, 3, 4))
  chk <- check_data(d)
  iss <- the_issue(chk, "Variety", "empty")
  expect_identical(iss$level, "note")
  expect_false(iss$fixable)
  expect_equal(iss$rows, c(2, 4))
})

test_that("identical rows are pointed out as information, each group listed", {
  d <- data.frame(Trt = c("A", "B", "A", "C", "B", "D"), Yield = c(5, 6, 5, 7, 6, 8))
  chk <- check_data(d)
  iss <- the_issue(chk, NA, "identical")
  expect_identical(iss$level, "note")
  expect_false(iss$fixable)
  expect_equal(iss$rows, c(1, 2, 3, 5))
  expect_match(iss$message, "^Rows 1 and 3")
  expect_match(iss$message, "2 and 5", fixed = TRUE)
  # nothing to correct: separate plots can share their values
  expect_false(any_fixable(chk))
  expect_identical(fix_data(d)$data, d)
})

test_that("factor columns are checked like text columns", {
  d <- data.frame(V = factor(c("V1", "V1 ", "V2", "V2")), Y = factor(c("5,6", "7", "8,1", "9")))
  chk <- check_data(d)
  expect_true("Y" %in% chk$numeric)
  expect_true("V" %in% chk$labels)
  expect_equal(the_issue(chk, "V", "extra spaces")$rows, 2)
  expect_equal(the_issue(chk, "Y", "written with a decimal comma")$rows, c(1, 3))
})

test_that("row numbers in the messages are the data's own row numbers", {
  # after an empty row is removed the rows keep their numbers, as in the table
  d <- data.frame(Trt = c("A", "B", "C", "D", "E"), Yield = c("5.6", "6", "7", "12a", "8,5"))
  d <- d[-2, ]
  chk <- check_data(d)
  iss <- the_issue(chk, "Yield", "'12a'", fixed = TRUE)
  expect_equal(iss$rows, 4)
  expect_match(iss$message, "row 4", fixed = TRUE)
  expect_equal(the_issue(chk, "Yield", "written with a decimal comma")$rows, 5)
})

test_that("messages about one row are worded in the singular", {
  d <- data.frame(Trt = LETTERS[1:4], Yield = c("5,6", "n/a", "", "7"),
                  Mix = c("5,6", "x", "7", "y"), stringsAsFactors = FALSE)
  chk <- check_data(d)
  expect_match(the_issue(chk, "Yield", "decimal comma")$message, "row 1 is written", fixed = TRUE)
  expect_match(the_issue(chk, "Yield", "no value")$message, "row 2 has 'n/a'", fixed = TRUE)
  expect_match(the_issue(chk, "Yield", "empty")$message, "row 3 is empty, so it is left out", fixed = TRUE)
  # "1 of its 4 filled-in entries are not numbers" would be the plural for one
  mix <- check_data(data.frame(Mix = c("5,6", "x", "7")))
  expect_no_match(the_issue(mix, "Mix", "read as labels")$message, "1 of its [0-9]+ filled-in entries are")
})

## ----------------------------------------------------------- fix_data -----

## Variety: v1 with a trailing space and a capital difference, V-2 differing
## only in punctuation, a blank in the empty row 7. Yield: decimal commas
## (1, 5), points (2, 6, 8), a marker (3), text (4), and the empty row 7.
messy <- function() data.frame(
  Variety = c("V1", "v1 ", "V1", "V2", "V2", "V-2", "", "V3"),
  Yield   = c("5,6", "6.1", "n/a", "12a", "7,25", "8", "", "9.5"),
  stringsAsFactors = FALSE)

test_that("fix_data corrects entry by entry, leaves the uncertain alone and logs it all", {
  fx <- fix_data(messy())
  d <- fx$data
  # the empty row is gone; the other rows keep their numbers
  expect_identical(nrow(d), 7L)
  expect_identical(rownames(d), c("1", "2", "3", "4", "5", "6", "8"))
  # 12a cannot be read with certainty, so the column stays text with the
  # certain corrections made and 12a exactly as typed
  expect_type(d$Yield, "character")
  expect_equal(as.numeric(d$Yield[c(1, 5)]), c(5.6, 7.25))
  expect_identical(d$Yield[c(2, 4, 6, 7)], c("6.1", "12a", "8", "9.5"))
  expect_true(is.na(d$Yield[3]))
  # only the trailing space is tidied: v1 and V-2 are not merged into V1 and V2
  expect_identical(d$Variety, c("V1", "v1", "V1", "V2", "V2", "V-2", "V3"))

  log <- paste(fx$log, collapse = "\n")
  expect_match(log, "'Yield': decimal commas changed to points (rows 1 and 5)", fixed = TRUE)
  expect_match(log, "'Yield': entries meaning no value emptied (row 3)", fixed = TRUE)
  expect_match(log, "'Variety': extra spaces removed from labels (row 2)", fixed = TRUE)
  expect_match(log, "'Yield' still has entries that are not certain numbers", fixed = TRUE)
  expect_match(log, "Row 7 had no values and has been removed", fixed = TRUE)
  expect_no_match(log, "'Yield' is now a column of numbers", fixed = TRUE)
})

test_that("fix_data converts each notation to the number it stands for", {
  d <- data.frame(Big  = c("1,250,000", "2,500.5", "3000"),
                  Eu   = c("1.250,5", "2.000,25", "3000,5"),
                  Pct  = c("45%", "4,5 %", "12.5%"),
                  AmbC = c("1,250", "5,6", "7,25"),
                  AmbT = c("1,250", "2.5", "3.75"),
                  Unit = c("45 cm", "50cm", "52.5 cm"),
                  Txt  = c("5.6", "6", "7.1"),
                  stringsAsFactors = FALSE)
  fx <- fix_data(d)
  for (v in names(d)) expect_type(fx$data[[v]], "double")
  expect_equal(fx$data$Big, c(1250000, 2500.5, 3000))
  expect_equal(fx$data$Eu, c(1250.5, 2000.25, 3000.5))
  expect_equal(fx$data$Pct, c(45, 4.5, 12.5))
  expect_equal(fx$data$AmbC, c(1.25, 5.6, 7.25))
  expect_equal(fx$data$AmbT, c(1250, 2.5, 3.75))
  expect_equal(fx$data$Unit, c(45, 50, 52.5))
  expect_equal(fx$data$Txt, c(5.6, 6, 7.1))
  line <- function(v) paste(fx$log[startsWith(fx$log, sprintf("'%s'", v))], collapse = "\n")
  expect_match(line("Big"), "thousands separators removed (rows 1 and 2)", fixed = TRUE)
  expect_match(line("Eu"), "thousands separators removed (rows 1 and 2)", fixed = TRUE)
  expect_match(line("Eu"), "decimal commas changed to points (row 3)", fixed = TRUE)
  expect_match(line("Pct"), "per cent signs dropped", fixed = TRUE)
  expect_match(line("AmbC"), "read as decimals (row 1)", fixed = TRUE)
  expect_match(line("AmbT"), "read as thousands (row 1)", fixed = TRUE)
  expect_match(line("Unit"), "the unit 'cm' dropped", fixed = TRUE)
  expect_identical(line("Txt"), "'Txt' is now a column of numbers.")
  for (v in names(d)) expect_match(line(v), sprintf("'%s' is now a column of numbers", v), fixed = TRUE)
})

test_that("the log does not claim thousands separators were removed when 1,250 was a decimal comma", {
  fx <- fix_data(data.frame(AmbC = c("1,250", "5,6", "7,25")))
  expect_equal(fx$data$AmbC, c(1.25, 5.6, 7.25))
  expect_match(paste(fx$log, collapse = "\n"), "decimal commas changed to points", fixed = TRUE)
  expect_no_match(fx$log, "thousands", fixed = TRUE)
})

test_that("fix_data leaves identical rows, missing values and clean data exactly alone", {
  d <- data.frame(Trt = c("A", "B", "A", "C"), Yield = c(5, 6, 5, NA))
  fx <- fix_data(d)
  expect_identical(fx$data, d)
  expect_length(fx$log, 0)
  for (k in c("RCBD", "CRD", "SPLIT")) {
    d <- demo_data(k)
    fx <- fix_data(d)
    expect_identical(fx$data, d)
    expect_length(fx$log, 0)
  }
})

test_that("an unresolved 1,250 and a word among numbers are never changed or emptied", {
  d <- data.frame(Trt = c("A", "B", "C", "D", "E", "F"),
                  Yield = c("1,250", "2,500", "n/a", "12", "15", "x"),
                  stringsAsFactors = FALSE)
  fx <- fix_data(d)
  # only the marker is certain
  expect_identical(fx$data$Yield, c("1,250", "2,500", NA, "12", "15", "x"))
  expect_identical(fx$data$Trt, d$Trt)
  # with nothing certain, nothing is changed and nothing is logged
  d2 <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c("1,250", "2,500", "12", "15"),
                   stringsAsFactors = FALSE)
  fx2 <- fix_data(d2)
  expect_identical(fx2$data, d2)
  expect_length(fx2$log, 0)
})

test_that("nothing uncertain is ever changed or emptied", {
  d <- data.frame(
    Trt = c("T1", "T-1", "t1", "AA", "Aa", "aa", "T 1", "T1"),
    Y1  = c("5.6", "12a", "7.1", "8", "6.2", "n/a", "5,5", "9"),          # a code among numbers
    Y2  = c("1,250", "2,500", "12", "15", "16", "17", "18", "19"),         # 1,250 unresolved
    Y3  = c("45 cm", "0.5 m", "52 cm", "48", "50", "51", "49", "47"),      # mixed units
    Y4  = c("5.6", "6.1", "abc", "7", "8", "9", "10", "11"),               # a word among numbers
    stringsAsFactors = FALSE)
  fx <- fix_data(d)
  out <- fx$data
  expect_identical(out$Trt, d$Trt)
  expect_identical(out$Y1[2], "12a")
  expect_identical(out$Y2, d$Y2)
  expect_identical(out$Y3, d$Y3)
  expect_identical(out$Y4[3], "abc")
  for (v in c("Y1", "Y2", "Y3", "Y4")) expect_type(out[[v]], "character")
  # the certain corrections in the same columns are still made
  expect_identical(out$Y1[7], "5.5")
  expect_true(is.na(out$Y1[6]))
  # no entry that was neither blank nor a marker has been emptied
  for (v in names(d)) {
    k <- read_entries(d[[v]])$kind
    expect_false(any(is.na(out[[v]]) & !k %in% c("blank", "mark")), info = v)
  }
})

test_that("labels that differ in capitals or punctuation are never merged", {
  d <- data.frame(G = c("AA", "Aa", "aa", "AA"), T = c("T1", "T-1", "T 1", "T.1"), Y = 1:4,
                  stringsAsFactors = FALSE)
  fx <- fix_data(d)
  expect_identical(fx$data, d)
  expect_length(fx$log, 0)
  # and the check still shows them afterwards
  chk <- check_data(fx$data)
  expect_identical(the_issue(chk, "G", "capital")$level, "check")
  expect_identical(the_issue(chk, "T", "punctuation")$level, "check")
})

test_that("a factor column stays a factor, with tidied labels in their original order", {
  d <- data.frame(V = factor(c("Low", "High", "Mid ", "n/a", "Mid"),
                             levels = c("Low", "Mid ", "Mid", "High", "n/a")),
                  Y = factor(c("5,6", "7", "8,1", "-", "9")))
  fx <- fix_data(d)
  # row 4 holds only markers ('n/a' and '-'), so it has no values and is removed
  expect_identical(rownames(fx$data), c("1", "2", "3", "5"))
  expect_s3_class(fx$data$V, "factor")
  expect_identical(as.character(fx$data$V), c("Low", "High", "Mid", "Mid"))
  expect_identical(levels(fx$data$V), c("Low", "Mid", "High"))
  # a factor of numbers becomes numbers
  expect_type(fx$data$Y, "double")
  expect_equal(fx$data$Y, c(5.6, 7, 8.1, 9))
  # a marker in a row that has other values is emptied, and its level goes
  d <- data.frame(V = factor(c("Low", "n/a", "High")), Y = c(1, 2, 3))
  fx <- fix_data(d)
  expect_identical(as.character(fx$data$V), c("Low", NA, "High"))
  expect_false("n/a" %in% levels(fx$data$V))
  # a factor whose labels differ only in capitals is not touched
  d <- data.frame(V = factor(c("V1", "v1", "V1", "V2")), Y = 1:4)
  expect_identical(fix_data(d)$data, d)
})

test_that("row names are kept, so later messages name the rows as they were loaded", {
  d <- data.frame(Trt = c("A", "", "B", "C", "D"), Yield = c("5,6", "", "x", "7", "8"),
                  stringsAsFactors = FALSE)
  fx <- fix_data(d)
  expect_identical(rownames(fx$data), c("1", "3", "4", "5"))
  expect_match(paste(fx$log, collapse = "\n"), "Row 2 had no values", fixed = TRUE)
  expect_match(paste(fx$log, collapse = "\n"), "other rows keep their numbers", fixed = TRUE)
  chk <- check_data(fx$data)
  iss <- the_issue(chk, "Yield", "'x'", fixed = TRUE)
  expect_equal(iss$rows, 3)
})

test_that("fix_data is idempotent and leaves nothing fixable behind", {
  d <- data.frame(
    Variety = c("V1", "v1 ", "V1", "V2", "V2", "V-2", "", "V3", "V  4"),
    Yield   = c("5,6", "6.1", "n/a", "12a", "7,25", "8", "", "9.5", "7"),
    Big     = c("1,250,000", "2,500.5", "3000", "#N/A", "4,000", "5000", "", "1,250", "6"),
    Pct     = c("45%", "4,5 %", "12.5", "-", "50", "55", "", "60", "61"),
    Amb     = c("1,250", "2,500", "12", "15", "x", "n/a", "", "17", "18"),
    stringsAsFactors = FALSE)
  fx1 <- fix_data(d)
  fx2 <- fix_data(fx1$data)
  expect_identical(fx2$data, fx1$data)
  expect_length(fx2$log, 0)
  chk <- check_data(fx1$data)
  expect_false(any_fixable(chk))
  # what is left is for the user
  expect_equal(the_issue(chk, "Yield", "'12a'", fixed = TRUE)$rows, 4)
  expect_identical(the_issue(chk, "Variety", "punctuation")$level, "check")
  expect_identical(the_issue(chk, "Amb", "'1,250'", fixed = TRUE)$level, "check")
  expect_equal(fx1$data$Big, c(1250000, 2500.5, 3000, NA, 4000, 5000, 1250, 6))
  # two values carry a % sign and the rest do not: not certain, left as typed
  expect_identical(fx1$data$Pct, c("45%", "4,5 %", "12.5", NA, "50", "55", "60", "61"))
  expect_identical(fx1$data$Variety[8], "V 4")
})

test_that("random data: corrections are safe, idempotent and complete", {
  # each numeric column draws from one notation so that the column's own
  # evidence does not change when the decimal commas are corrected
  point_pool <- c("5.6", "7", "12.25", "-3", "1e3", "1,250", "1,250,000", "2,500.5", "1 250",
                  "45%", "45 cm", "50 cm", "0.5 m", "12a", "abc", "n/a", "-", "#DIV/0!", "", NA)
  comma_pool <- c("5,6", "7", "12,25", "-0,5", "1,250", "4,5 %", "45 cm", "12a", "abc",
                  "n/a", "-", "", NA)
  lab_pool <- c("V1", "V1 ", " V1", "V  1", "v1", "V-1", "V2", "AA", "Aa", "Control", "control")
  set.seed(20261007)
  for (it in 1:150) {
    n <- sample(3:9, 1)
    d <- data.frame(L = sample(lab_pool, n, TRUE), stringsAsFactors = FALSE)
    for (j in seq_len(sample(1:3, 1))) {
      pool <- if (runif(1) < 0.5) point_pool else comma_pool
      d[[paste0("Y", j)]] <- sample(c(sample(pool, sample(2:5, 1)), "5", "7", "9"), n, TRUE)
    }
    if (runif(1) < 0.3) d$L <- factor(d$L)
    f1 <- fix_data(d)
    f2 <- fix_data(f1$data)
    expect_identical(f2$data, f1$data, info = paste(capture.output(print(d)), collapse = "\n"))
    expect_false(any_fixable(check_data(f1$data)))
    keep <- rownames(f1$data)
    for (v in names(d)) {
      before <- as.character(d[keep, v]); after <- f1$data[[v]]
      a <- assess_column(before)
      # nothing is emptied that was not blank or a marker
      expect_false(any(is.na(after) & !a$kind %in% c("blank", "mark")), info = v)
      if (v == "L") {
        # labels: only spaces are tidied
        expect_identical(as.character(after), gsub("[[:space:]]+", " ", trimws(before)))
        next
      }
      # entries whose meaning is not certain are kept exactly as typed
      unc <- which(a$kind == "text" | (a$kind == "ambiguous" & is.na(a$three)) |
                   (a$has_unit & !a$all_unit) | (a$has_pct & !a$all_pct))
      if (length(unc)) {
        expect_type(after, "character")
        expect_identical(after[unc], before[unc])
      }
      # a column turned into numbers holds the values the entries stand for
      if (is.numeric(after)) expect_equal(after, a$value)
    }
  }
})

test_that("no entry is made blank without the log saying so", {
  # 0,125% is a decimal comma with a per cent sign; it must become 0.125
  d <- data.frame(P = c("0,125%", "12,5%", "3,5%", "4%"), stringsAsFactors = FALSE)
  fx <- fix_data(d)
  expect_equal(fx$data$P, c(0.125, 12.5, 3.5, 4))
})

## The cases below each guard one way the corrections could quietly change
## data whose meaning is not certain, or leave the check offering a
## correction that applying the corrections never makes.

test_that("a number typed with a Unicode minus or a no-break space is not emptied", {
  d <- data.frame(T = c("A", "B", "C"), Y = c(paste0(U_MINUS, "3.5"), "2.1", paste0("4.2", NBSP)),
                  stringsAsFactors = FALSE)
  # the check reads them as numbers, so the column is offered as numbers ...
  expect_true("Y" %in% check_data(d)$numeric)
  fx <- fix_data(d)
  # ... and the corrections must keep their values
  expect_false(anyNA(fx$data$Y))
  expect_type(fx$data$Y, "double")
  expect_equal(fx$data$Y, c(-3.5, 2.1, 4.2))
})

test_that("'1.250' in a column written with decimal commas is left for the user", {
  d <- data.frame(Y = c("5,6", "1.250", "7,2", "8"), stringsAsFactors = FALSE)
  chk <- check_data(d)
  iss <- the_issue(chk, "Y", "'1.250'", fixed = TRUE)
  expect_identical(iss$level, "check")
  expect_false(iss$fixable)
  expect_equal(iss$rows, 2)
  # the decimal commas wait until it is settled: changing them now would make
  # 1.250 look like a decimal point on the next pass
  expect_match(iss$message, "will be offered once this is settled", fixed = TRUE)
  expect_length(issues_for(chk, "Y", "written with a decimal comma"), 0)
  expect_false(any_fixable(chk))
  # applying the corrections does not make it 1.25
  fx <- fix_data(d)
  expect_type(fx$data$Y, "character")
  expect_identical(fx$data$Y[2], "1.250")
})

test_that("an unresolved 1,250 stays unresolved however often the corrections are applied", {
  # decimal commas and decimal points in one column: 1,250 is for the user.
  # Correcting the decimal comma must not turn the column into one that
  # appears to settle 1,250 as a thousands separator on the next pass.
  d <- data.frame(Y = c("1,250", "5,6", "2.5", "4"), stringsAsFactors = FALSE)
  expect_identical(the_issue(check_data(d), "Y", "'1,250'", fixed = TRUE)$level, "check")
  f1 <- fix_data(d)
  expect_identical(f1$data$Y[1], "1,250")
  f2 <- fix_data(f1$data)
  expect_identical(f2$data, f1$data)
  expect_identical(f2$data$Y[1], "1,250")
  expect_false(any_fixable(check_data(f1$data)))
})

test_that("a row holding only no-value markers is dealt with in one pass", {
  # once its markers are emptied the row is empty; one press of the button
  # must leave nothing more to correct
  d <- data.frame(Trt = c("A", "-", "B", "C"), Yield = c("5.6", "n/a", "7", "8"),
                  stringsAsFactors = FALSE)
  f1 <- fix_data(d)
  f2 <- fix_data(f1$data)
  expect_identical(f2$data, f1$data)
  expect_false(any_fixable(check_data(f1$data)))
})

test_that("units are compared across every entry that has one, 1,250 kg included", {
  d <- data.frame(Y = c("1,250 kg", "5 g", "7,2", "8,1"), stringsAsFactors = FALSE)
  chk <- check_data(d)
  # kg and g are different units: neither may be dropped
  expect_length(issues_for(chk, "Y", "unit 'g'", fixed = TRUE), 0)
  expect_length(issues_for(chk, "Y", "different units"), 1)
  fx <- fix_data(d)
  expect_identical(fx$data$Y[1:2], c("1,250 kg", "5 g"))
})

test_that("labels written with a dash character are not said to have extra spaces", {
  d <- data.frame(Season = paste0(c("2019", "2019", "2020", "2020"), EN_DASH, c("20", "20", "21", "21")),
                  Y = 1:4, stringsAsFactors = FALSE)
  chk <- check_data(d)
  expect_length(issues_for(chk, "Season", "extra spaces"), 0)
  expect_false(any_fixable(chk))
  d2 <- data.frame(T = c(paste0("T", U_MINUS, "1"), "T2", "T3"), Y = 1:3, stringsAsFactors = FALSE)
  expect_false(any_fixable(check_data(fix_data(d2)$data)))
})

test_that("a label with a no-break space is not left offered for tidying after the corrections", {
  d <- data.frame(V = c(paste0("V", NBSP, "1"), "V 1", "V2"), Y = 1:3, stringsAsFactors = FALSE)
  f1 <- fix_data(d)
  expect_false(any_fixable(check_data(f1$data)))
})

test_that("a corrected entry is written as typed, with only the notation changed", {
  # in a column that stays text the corrected entries are what the user sees:
  # 5,6 becomes 5.6, not 5.60
  d <- data.frame(Y = c("5,6", "8,25", "12a", "7", "1,250,000"), stringsAsFactors = FALSE)
  fx <- fix_data(d)
  expect_identical(fx$data$Y, c("5.6", "8.25", "12a", "7", "1250000"))
  msg <- the_issue(check_data(d), "Y", "written with a decimal comma")$message
  expect_match(msg, "5,6 becomes 5.6.", fixed = TRUE)
})

## ---------------------------------------------------------- check_html -----

test_that("the check is shown as escaped HTML, grouped by what needs doing", {
  d <- data.frame(Trt = LETTERS[1:5], Yield = c("5.6", "<LOD", "7.1", "8,2", ""),
                  stringsAsFactors = FALSE)
  html <- check_html(check_data(d), d)
  expect_match(html, "^<div class='warn'>")
  expect_match(html, "5 rows and 2 columns", fixed = TRUE)
  expect_match(html, "Read as numbers: <b>Yield</b>", fixed = TRUE)
  expect_match(html, "Read as labels: <b>Trt</b>", fixed = TRUE)
  expect_match(html, "Can be corrected for you", fixed = TRUE)
  expect_match(html, "Needs you to decide", fixed = TRUE)
  expect_match(html, "For information", fixed = TRUE)
  # the user's entry is shown as text, not read as a tag
  expect_match(html, "'&lt;LOD'", fixed = TRUE)
  expect_no_match(html, "<LOD", fixed = TRUE)

  d <- demo_data("RCBD")
  html <- check_html(check_data(d), d)
  expect_match(html, "no problems found", fixed = TRUE)
  expect_match(html, "sugbox", fixed = TRUE)

  # information only: not a warning
  d <- data.frame(Trt = LETTERS[1:3], Yield = c(5, NA, 7))
  html <- check_html(check_data(d), d)
  expect_match(html, "^<div class='sugbox'>")
  expect_match(html, "For information", fixed = TRUE)
  expect_no_match(html, "Can be corrected", fixed = TRUE)
})

## --------------------------------------------------- reading the data -----

test_that("read_pasted keeps '-', '.', '#DIV/0!' and apostrophes as typed", {
  txt <- "Trt\tYield\nA\t5.6\nB\t-\nC\t.\nD\t#DIV/0!\nE\tNA\nFarmer's\t\n"
  d <- read_pasted(txt, "tab", TRUE)
  expect_identical(names(d), c("Trt", "Yield"))
  expect_identical(nrow(d), 6L)
  expect_identical(d$Trt, c("A", "B", "C", "D", "E", "Farmer's"))
  expect_identical(d$Yield[1:4], c("5.6", "-", ".", "#DIV/0!"))
  expect_true(all(is.na(d$Yield[5:6])))
  # and the check then shows the markers instead of R hiding them
  chk <- check_data(d)
  expect_equal(the_issue(chk, "Yield", "no value")$rows, 2:4)
  expect_equal(the_issue(chk, "Yield", "empty")$rows, 5:6)

  d <- read_pasted("Trt,Yield\nA,5.6\nB,-\nC,#N/A\n", "comma", TRUE)
  expect_identical(d$Yield, c("5.6", "-", "#N/A"))
})

test_that("read_pasted does not treat # as a comment or an inch mark as a quote", {
  d <- read_pasted("Trt\tYield\n#1\t5,6\n#2\t6,1\n", "tab", TRUE)
  expect_identical(d$Trt, c("#1", "#2"))
  expect_identical(d$Yield, c("5,6", "6,1"))
  # 12" pot must not swallow the rest of the data
  d <- read_pasted("Pot\tYield\n12\" pot\t5.6\n10\" pot\t6.1\n8 cm\t7\n", "tab", TRUE)
  expect_identical(nrow(d), 3L)
  expect_identical(d$Pot, c("12\" pot", "10\" pot", "8 cm"))
  expect_equal(d$Yield, c(5.6, 6.1, 7))
  # a properly quoted label is still read without its quotes
  d <- read_pasted("Trt\tYield\n\"A B\"\t5.6\nC\t6\n", "tab", TRUE)
  expect_identical(d$Trt, c("A B", "C"))
  # semicolons, and Windows line endings
  d <- read_pasted("Trt;Yield\r\nA;5,6\r\nB;6,1\r\n", "semicolon", TRUE)
  expect_identical(d$Yield, c("5,6", "6,1"))
  expect_null(read_pasted("   ", "tab", TRUE))
})

test_that("read_upload reads a semicolon CSV with decimal commas as typed", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("Variety;Yield;Dose", "V1;5,6;10", "V2;6,1;20", "V3;-;30", "V4;;40"), f)
  d <- read_upload(f)
  expect_identical(names(d), c("Variety", "Yield", "Dose"))
  expect_identical(nrow(d), 4L)
  expect_identical(rownames(d), as.character(1:4))
  expect_identical(d$Variety, c("V1", "V2", "V3", "V4"))
  expect_identical(d$Yield, c("5,6", "6,1", "-", NA))
  expect_equal(d$Dose, c(10, 20, 30, 40))
  chk <- check_data(d)
  expect_equal(the_issue(chk, "Yield", "written with a decimal comma")$rows, 1:2)
  expect_equal(the_issue(chk, "Yield", "no value")$rows, 3)
})

test_that("read_upload reads comma and tab files, quoted decimal commas and # as typed", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("Variety,Yield", "V1,5.6", "V2,-", "V3,"), f)
  d <- read_upload(f)
  expect_identical(names(d), c("Variety", "Yield"))
  expect_identical(d$Yield, c("5.6", "-", NA))

  writeLines(c("Variety,Note,Yield", "V1,\"5,6\",5.6", "V2,#1 plot,6.1", "V3,#N/A,7"), f)
  d <- read_upload(f)
  expect_identical(d$Note, c("5,6", "#1 plot", "#N/A"))
  expect_equal(d$Yield, c(5.6, 6.1, 7))

  writeLines(c("Variety\tYield\tDose", "V1\t5,6\t10", "V2\t6,1\t20", "V3\tn/a\t30"), f)
  d <- read_upload(f)
  expect_identical(names(d), c("Variety", "Yield", "Dose"))
  expect_identical(d$Yield, c("5,6", "6,1", "n/a"))
  expect_equal(d$Dose, c(10, 20, 30))
})

test_that("read_upload refuses a comma file whose decimal commas split the values", {
  # V1,5,6 has three fields where the header has two: no separator reads it
  # correctly, so the file is refused with a reason rather than read wrongly
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("Variety,Yield", "V1,5,6", "V2,6", "V3,7,1"), f)
  expect_error(read_upload(f), "decimal commas")
})

## ------------------------------------------- rows left out of analysis ----

## 12 plots of a CRD; five of them cannot be used, for three different reasons
crd_with_gaps <- function() {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 4),
                  Yield = as.character(c(10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23)),
                  stringsAsFactors = FALSE)
  d$Yield[2] <- NA
  d$Yield[3] <- NA; d$Trt[3] <- NA
  d$Yield[6] <- "12a"
  d$Yield[10] <- "5,6"
  d$Trt[12] <- NA
  d
}

test_that("analyze records every row and column it could not use, with the entry and its kind", {
  r <- analyze(crd_with_gaps(), "CRD", list(response = "Yield", treat = "Trt"))
  ex <- r$excluded
  expect_s3_class(ex, "data.frame")
  expect_named(ex, c("row", "column", "kind", "value"))
  got <- sort(paste(ex$row, ex$column, ex$kind, ex$value, sep = "|"))
  want <- sort(c("2|Yield|blank|", "3|Yield|blank|", "3|Trt|blank|", "6|Yield|text|12a",
                 "10|Yield|comma|5,6", "12|Trt|blank|"))
  expect_identical(got, want)
  expect_identical(nrow(r$data), 7L)
  # nothing left out, nothing recorded
  r <- analyze(demo_data("CRD"), "CRD", list(response = "Yield", treat = "Treatment"))
  expect_null(r$excluded)
})

test_that("a no-value marker or a blank in a treatment or block column is missing, not a level", {
  d <- data.frame(Trt = rep(c("A", "B", "C"), each = 4),
                  Yield = c(10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23), stringsAsFactors = FALSE)
  d$Trt[c(2, 7, 11)] <- c("-", "n/a", "  ")
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Trt"))
  expect_identical(levels(r$data$Trt), c("A", "B", "C"))
  expect_identical(nrow(r$data), 9L)
  expect_equal(r$excluded$row, c(2, 7, 11))
  expect_identical(r$excluded$column, rep("Trt", 3))
  expect_identical(r$excluded$kind, c("mark", "mark", "blank"))
  expect_identical(r$excluded$value, c("-", "n/a", ""))
  expect_length(r$effects$Trt$means$Trt, 3)

  d <- demo_data("RCBD")
  d$Block <- as.character(d$Block)
  d$Block[1] <- "?"
  r <- analyze(d, "RCBD", list(response = "Yield", treat = "Variety", block = "Block"))
  expect_false("?" %in% levels(r$data$Block))
  expect_identical(nlevels(r$data$Block), 4L)
  expect_identical(r$excluded$kind, "mark")
  expect_identical(r$excluded$column, "Block")
})

test_that("the rows left out are named by the data's own row numbers", {
  d <- crd_with_gaps()[c(1, 4:12), ]           # as fix_data leaves it, rows 2 and 3 gone
  r <- analyze(d, "CRD", list(response = "Yield", treat = "Trt"))
  expect_equal(sort(unique(r$excluded$row)), c(6, 10, 12))
  expect_match(excluded_text(r$excluded), "row 6 ('12a')", fixed = TRUE)
})

test_that("the errors for too few rows say which rows were left out and why", {
  d <- data.frame(Trt = c("A", "B", "C", "A", "B"),
                  Yield = c("5", "5,6", "x", NA, "6"), stringsAsFactors = FALSE)
  err <- tryCatch(analyze(d, "CRD", list(response = "Yield", treat = "Trt")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Only 2 rows can be analysed; at least 3 are needed.", fixed = TRUE)
  expect_match(err, "3 rows were left out of this analysis", fixed = TRUE)
  expect_match(err, "decimal comma in row 2 ('5,6')", fixed = TRUE)
  expect_match(err, "not a number in row 3 ('x')", fixed = TRUE)
  expect_match(err, "empty in row 4", fixed = TRUE)

  d <- data.frame(Trt = c("A", "B", "C"), Yield = c("x", "n/a", ""), stringsAsFactors = FALSE)
  err <- tryCatch(analyze(d, "CRD", list(response = "Yield", treat = "Trt")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "No rows can be analysed.", fixed = TRUE)
  expect_match(err, "3 rows were left out of this analysis", fixed = TRUE)
})

test_that("no error degrees of freedom gives a plain message, not a crash", {
  # every treatment once: the ANOVA has no residual line
  d <- data.frame(Trt = c("A", "B", "C"), Yield = c(5, 6, 7.5))
  err <- tryCatch(analyze(d, "CRD", list(response = "Yield", treat = "Trt")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "too few rows to estimate the experimental error", fixed = TRUE)
  expect_no_match(err, "left out", fixed = TRUE)
  # and when rows were left out to get there, they are named
  d <- data.frame(Trt = c("A", "B", "C", "A", "B", "C"), Yield = c("5", "6", "7", "x", "y", "z"),
                  stringsAsFactors = FALSE)
  err <- tryCatch(analyze(d, "CRD", list(response = "Yield", treat = "Trt")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "too few rows to estimate the experimental error", fixed = TRUE)
  expect_match(err, "3 rows were left out of this analysis", fixed = TRUE)
  # tidy_aov itself copes with an ANOVA that has no F column
  fit <- stats::aov(Y ~ T, data = data.frame(Y = c(1, 2, 4), T = factor(c("a", "b", "c"))))
  an <- tidy_aov(fit)
  expect_true(all(is.na(an$F)))
  expect_true(all(is.na(an$p)))
})

test_that("a layout refused after rows were left out says which rows", {
  d <- demo_data("SPLIT")
  d$Yield <- as.character(d$Yield)
  d$Yield[5] <- "12a"
  err <- tryCatch(analyze(d, "SPLIT", list(response = "Yield", rep = "Rep", main = "Irrigation",
                                           sub = "Variety")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "split-plot", fixed = TRUE)
  expect_match(err, "1 row was left out of this analysis", fixed = TRUE)
  expect_match(err, "not a number in row 5 ('12a')", fixed = TRUE)
})

## ------------------------------------------------------- excluded_text ----

ex_df <- function(row, column, kind, value)
  data.frame(row = row, column = column, kind = kind, value = value, stringsAsFactors = FALSE)

test_that("excluded_text is empty when nothing was left out", {
  expect_identical(excluded_text(NULL), "")
  expect_identical(excluded_text(ex_df(integer(0), character(0), character(0), character(0))), "")
})

test_that("excluded_text counts rows, groups reasons by column and kind, and orders them by row", {
  one <- excluded_text(ex_df(4L, "Yield", "blank", ""))
  expect_match(one, "^1 row was left out of this analysis: ")
  expect_match(one, "Yield is empty in row 4.", fixed = TRUE)

  ex <- ex_df(c(1L, 4L, 9L, 2L), c("Yield", "Yield", "Yield", "Trt"),
              c("comma", "comma", "blank", "mark"), c("6,5", "7,1", "", "n/a"))
  txt <- excluded_text(ex)
  expect_match(txt, "^4 rows were left out of this analysis: ")
  a <- regexpr("Yield is written with a decimal comma in rows 1 and 4 ('6,5' and '7,1')", txt, fixed = TRUE)
  b <- regexpr("Trt is marked as having no value in row 2 ('n/a')", txt, fixed = TRUE)
  c <- regexpr("Yield is empty in row 9", txt, fixed = TRUE)
  expect_true(all(c(a, b, c) > 0))
  expect_true(a < b && b < c)
  expect_identical(lengths(regmatches(txt, gregexpr("; ", txt, fixed = TRUE))), 2L)

  # a row with two reasons is one row left out
  two <- excluded_text(ex_df(c(3L, 3L), c("Yield", "Trt"), c("text", "blank"), c("x", "")))
  expect_match(two, "^1 row was left out", )
  expect_match(two, "Yield is not a number in row 3 ('x')", fixed = TRUE)
  expect_match(two, "Trt is empty in row 3", fixed = TRUE)
})

test_that("excluded_text names each kind of entry in plain words", {
  ex <- ex_df(c(2L, 5L, 7L, 8L, 9L, 10L), "Y",
              c("ambiguous", "percent", "unit", "thousands", "european", "text"),
              c("1,250", "5%", "5 cm", "1,250,000", "1.250,5", "12a"))
  txt <- excluded_text(ex)
  expect_match(txt, "^6 rows were left out")
  expect_match(txt, "Y could be read two ways in row 2 ('1,250')", fixed = TRUE)
  expect_match(txt, "Y has a % sign in row 5 ('5%')", fixed = TRUE)
  expect_match(txt, "Y has a unit after the number in row 7 ('5 cm')", fixed = TRUE)
  expect_match(txt, "Y has a thousands separator in row 8 ('1,250,000')", fixed = TRUE)
  expect_match(txt, "Y has a thousands separator in row 9 ('1.250,5')", fixed = TRUE)
  expect_match(txt, "Y is not a number in row 10 ('12a')", fixed = TRUE)
})

test_that("a long list is cut short", {
  many <- excluded_text(ex_df(1:10, "Yield", "blank", ""))
  expect_match(many, "^10 rows were left out")
  expect_match(many, "rows 1, 2, 3, 4, 5, 6, 7, 8 and 2 more", fixed = TRUE)
  five <- excluded_text(ex_df(1:5, "Yield", "text", c("a", "b", "c", "d", "e")))
  expect_match(five, "(such as 'a', 'b' and 'c')", fixed = TRUE)
  expect_no_match(five, "'d'", fixed = TRUE)
})

test_that("excluded_text escapes the entries for HTML only when asked", {
  ex <- ex_df(3L, "Yield", "text", "<LOD")
  expect_match(excluded_text(ex), "('<LOD')", fixed = TRUE)
  h <- excluded_text(ex, html = TRUE)
  expect_match(h, "('&lt;LOD')", fixed = TRUE)
  expect_no_match(h, "<LOD", fixed = TRUE)
})

test_that("the hint says what to do in the app, and only when there is something to do", {
  corr <- ex_df(c(1L, 2L), "Yield", c("comma", "mark"), c("5,6", "n/a"))
  expect_no_match(excluded_text(corr), "Apply the corrections", fixed = TRUE)
  expect_match(excluded_text(corr, hint = TRUE), "Apply the corrections", fixed = TRUE)
  txt <- ex_df(1L, "Yield", "text", "12a")
  h <- excluded_text(txt, hint = TRUE)
  expect_match(h, "correct those entries in the table", fixed = TRUE)
  expect_no_match(h, "Apply the corrections", fixed = TRUE)
  blank <- ex_df(1L, "Yield", "blank", "")
  expect_identical(excluded_text(blank, hint = TRUE), excluded_text(blank))
})

test_that("the interpretation and the report mention the rows left out", {
  rr <- run_all(crd_with_gaps(), "CRD", list(treat = "Trt"), "Yield")
  f <- rr$fits$Yield
  txt <- as.character(interpret(f$final, f$asm, f$sug))
  sec1 <- regmatches(txt, regexpr("(?s)<h4>1\\..*?</p>", txt, perl = TRUE))
  expect_length(sec1, 1)
  expect_match(sec1, "(7 observations)", fixed = TRUE)
  expect_match(sec1, "5 rows were left out of this analysis", fixed = TRUE)
  expect_match(sec1, "Yield is not a number in row 6 ('12a')", fixed = TRUE)
  expect_match(sec1, "Trt is empty in rows 3 and 12", fixed = TRUE)
  expect_match(build_report(rr), "5 rows were left out", fixed = TRUE)

  # the user's entries are escaped in the HTML
  d <- crd_with_gaps(); d$Yield[1] <- "<LOD"
  rr <- run_all(d, "CRD", list(treat = "Trt"), "Yield")
  f <- rr$fits$Yield
  sec1 <- regmatches(txt <- as.character(interpret(f$final, f$asm, f$sug)),
                     regexpr("(?s)<h4>1\\..*?</p>", txt, perl = TRUE))
  expect_match(sec1, "'&lt;LOD'", fixed = TRUE)
  expect_no_match(sec1, "<LOD", fixed = TRUE)

  rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
  f <- rr$fits$Yield
  expect_no_match(as.character(interpret(f$final, f$asm, f$sug)), "left out of this analysis",
                  fixed = TRUE)
})

test_that("a transformed analysis of a text response keeps the rows left out as entered", {
  # the transformation is applied to what can be read as numbers; an entry that
  # is not a number is left out as in the untransformed analysis, not reported
  # as a value outside the transformation's range
  d <- crd_with_gaps()
  for (tr in c("sqrt", "log")) {
    rr <- run_all(d, "CRD", list(treat = "Trt"), "Yield", trans = stats::setNames(list(tr), "Yield"))
    f <- rr$fits$Yield
    expect_identical(f$trans, tr)
    expect_identical(f$final$excluded, f$base$excluded)
    expect_equal(sort(unique(f$final$excluded$row)), c(2, 3, 6, 10, 12))
    expect_identical(nrow(f$final$data), 7L)
    expect_equal(f$final$data$Yield, TRANS[[tr]]$f(c(10, 13, 14, 16, 17, 20, 22), f$lambda))
    sec1 <- as.character(interpret(f$final, f$asm, f$sug, TRANS[[tr]]$lab))
    expect_match(sec1, "Yield is not a number in row 6 ('12a')", fixed = TRUE)
  }
  # a value genuinely outside the range is still refused
  d$Yield[4] <- "-3"
  expect_error(run_all(d, "CRD", list(treat = "Trt"), "Yield", trans = list(Yield = "sqrt")),
               "not defined for 'Yield'")
})

## ---------------------------------------------------------------- app -----

## capture the app's notifications
local_notes <- function(env = parent.frame()) {
  notes <- new.env()
  notes$msg <- character(0); notes$type <- character(0)
  local_mocked_bindings(showNotification = function(ui, ..., type = "default") {
    notes$msg <- c(notes$msg, paste(as.character(ui), collapse = " "))
    notes$type <- c(notes$type, type)
    invisible()
  }, .package = "DOEpro", .env = env)
  notes
}

test_that("the app checks pasted data at once and applies the corrections on request", {
  skip_on_cran()
  notes <- local_notes()
  shiny::testServer(doepro_server, {
    # Block comes last, so the response is guessed from the check, not position
    txt <- "Trt\tYield\tBlock\nA\t5,6\tB1\nA\t6,1\tB2\nB\tn/a\tB1\nB\t7,2\tB2\nC\t8,4\tB1\nC\t9,0\tB2\n"
    session$setInputs(paste = txt, sep = "tab", header = TRUE, load = 1)
    expect_identical(rv$data$Yield, c("5,6", "6,1", "n/a", "7,2", "8,4", "9,0"))
    html <- output$dqOut$html
    expect_match(html, "Data check", fixed = TRUE)
    expect_match(html, "decimal comma", fixed = TRUE)
    expect_match(html, "'n/a'", fixed = TRUE)
    expect_match(html, 'id="dqFix"', fixed = TRUE)
    expect_no_match(html, "Corrections made", fixed = TRUE)

    # the response is guessed from the columns the check reads as numbers
    session$setInputs(design = "CRD")
    expect_match(output$mapUI$html, '<option value="Yield" selected>', fixed = TRUE)

    session$setInputs(dqFix = 1)
    expect_type(rv$data$Yield, "double")
    expect_equal(rv$data$Yield, c(5.6, 6.1, NA, 7.2, 8.4, 9.0))
    expect_identical(rv$data$Trt, c("A", "A", "B", "B", "C", "C"))
    html <- output$dqOut$html
    expect_match(html, "Corrections made", fixed = TRUE)
    expect_match(html, "decimal commas changed to points", fixed = TRUE)
    expect_match(html, "is now a column of numbers", fixed = TRUE)
    # nothing is left to correct, so the button goes
    expect_no_match(html, 'id="dqFix"', fixed = TRUE)
    expect_true("Corrections applied." %in% notes$msg)

    # a new load clears the log of the previous corrections
    session$setInputs(demo = "CRD", loaddemo = 1)
    html <- output$dqOut$html
    expect_no_match(html, "Corrections made", fixed = TRUE)
    expect_match(html, "no problems found", fixed = TRUE)
    expect_no_match(html, 'id="dqFix"', fixed = TRUE)
  })
})

test_that("the app offers the button only when something can be corrected", {
  skip_on_cran()
  notes <- local_notes()
  shiny::testServer(doepro_server, {
    d0 <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c("5.6", "12a", "7", "8"),
                     stringsAsFactors = FALSE)
    rv$data <- d0
    session$flushReact()
    html <- output$dqOut$html
    expect_match(html, "Needs you to decide", fixed = TRUE)
    expect_match(html, "'12a'", fixed = TRUE)
    expect_no_match(html, 'id="dqFix"', fixed = TRUE)
    # pressed anyway, nothing changes and the user is told
    session$setInputs(dqFix = 1)
    expect_identical(rv$data, d0)
    expect_true(any(grepl("Nothing could be corrected", notes$msg, fixed = TRUE)))
    expect_identical(notes$type[length(notes$type)], "warning")
    expect_no_match(output$dqOut$html, "Corrections made", fixed = TRUE)
  })
})

test_that("the app reads an uploaded semicolon CSV and checks it", {
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("Trt;Yield", "A;5,6", "B;6,1", "C;7,0"), f)
  shiny::testServer(doepro_server, {
    session$setInputs(file = list(name = "yield.csv", datapath = f))
    expect_identical(rv$data$Yield, c("5,6", "6,1", "7,0"))
    expect_match(output$dqOut$html, "decimal comma", fixed = TRUE)
  })
})

test_that("cell edits in a column of numbers are read like the check reads them", {
  skip_on_cran()
  notes <- local_notes()
  redrawn <- list()
  local_mocked_bindings(replaceData = function(proxy, data, ...) {
    redrawn[[length(redrawn) + 1L]] <<- list(data = data, args = list(...))
    invisible(proxy)
  }, .package = "DT")
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = c("A", "B", "C"), Yield = c(1.5, 2, 3), stringsAsFactors = FALSE)
    # DT reports rows from 1 and columns from 0, and column 0 is the row numbers
    session$setInputs(tbl_cell_edit = list(row = 2, col = 2, value = "5,6"))
    expect_type(rv$data$Yield, "double")
    expect_equal(rv$data$Yield, c(1.5, 5.6, 3))

    # not a number: refused, the row named, and the table redrawn as it was
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "abc"))
    expect_equal(rv$data$Yield, c(1.5, 5.6, 3))
    last <- notes$msg[length(notes$msg)]
    expect_match(last, "'abc' is not a number", fixed = TRUE)
    expect_match(last, "row 1 of 'Yield'", fixed = TRUE)
    expect_identical(notes$type[length(notes$type)], "warning")
    expect_length(redrawn, 1)
    expect_equal(redrawn[[1]]$data$Yield, c(1.5, 5.6, 3))
    expect_true(isTRUE(redrawn[[1]]$args$rownames))

    # 1,250 could be either; a marker should be typed as an empty cell; a unit
    # is not a number of this column
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "1,250"))
    expect_match(notes$msg[length(notes$msg)], "could mean 1250 or 1.250", fixed = TRUE)
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "n/a"))
    expect_match(notes$msg[length(notes$msg)], "clear the cell", fixed = TRUE)
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "12 cm"))
    expect_equal(rv$data$Yield, c(1.5, 5.6, 3))
    expect_length(redrawn, 4)

    # certain notations are accepted
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "1,250,000"))
    expect_equal(rv$data$Yield[1], 1250000)
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = paste0(U_MINUS, "3.5")))
    expect_equal(rv$data$Yield[1], -3.5)

    # clearing a cell leaves it empty
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = ""))
    expect_true(is.na(rv$data$Yield[1]))
    expect_type(rv$data$Yield, "double")

    # the row numbers cannot be edited; a label takes the edit as typed
    session$setInputs(tbl_cell_edit = list(row = 3, col = 0, value = "99"))
    expect_identical(rv$data$Trt, c("A", "B", "C"))
    expect_equal(rv$data$Yield, c(NA, 5.6, 3))
    session$setInputs(tbl_cell_edit = list(row = 3, col = 1, value = "D"))
    expect_identical(rv$data$Trt, c("A", "B", "D"))
    session$setInputs(tbl_cell_edit = list(row = 2, col = 1, value = " "))
    expect_identical(rv$data$Trt, c("A", NA, "D"))
  })
})

test_that("an edit message names the row as numbered in the table", {
  skip_on_cran()
  notes <- local_notes()
  local_mocked_bindings(replaceData = function(proxy, data, ...) invisible(proxy), .package = "DT")
  shiny::testServer(doepro_server, {
    # row 2 was removed as empty: the second row shown is row 3
    rv$data <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c(1, 2, 3, 4))[-2, ]
    session$setInputs(tbl_cell_edit = list(row = 2, col = 2, value = "abc"))
    expect_match(notes$msg[length(notes$msg)], "row 3 of 'Yield'", fixed = TRUE)
    session$setInputs(tbl_cell_edit = list(row = 2, col = 2, value = "7,5"))
    expect_equal(rv$data$Yield, c(1, 7.5, 4))
    expect_identical(rownames(rv$data), c("1", "3", "4"))
  })
})

test_that("correcting a text entry in the table lets the column become numbers", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = c("A", "B", "C", "D"), Yield = c("5.6", "12a", "7", "8"),
                          stringsAsFactors = FALSE)
    session$setInputs(tbl_cell_edit = list(row = 2, col = 2, value = "12.5"))
    expect_identical(rv$data$Yield, c("5.6", "12.5", "7", "8"))
    html <- output$dqOut$html
    expect_no_match(html, "'12a'", fixed = TRUE)
    expect_match(html, "read as text", fixed = TRUE)
    expect_match(html, 'id="dqFix"', fixed = TRUE)
    session$setInputs(dqFix = 1)
    expect_equal(rv$data$Yield, c(5.6, 12.5, 7, 8))
  })
})

test_that("the screening offers responses the check reads as numbers", {
  skip_on_cran()
  d <- demo_data("RCBD")
  d$Yield <- as.character(d$Yield)
  d$Yield[3] <- "n/a"
  d$Height <- as.character(c(52.1, 49.8, 55.3, 50.2, 61.0, 58.7, 60.4, 57.9, 47.5, 49.9, 46.2, 48.8,
                             66.3, 69.1, 64.0, 67.2, 59.5, 62.8, 58.1, 60.6, 53.3, 55.0, 51.9, 54.4))
  d$Height[4] <- "-"
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$setInputs(design = "RCBD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", block = "Block", treat = "Variety")
    s <- scan_tab()
    expect_setequal(s$Variable, c("Yield", "Height"))
  })
})

test_that("the run summary lists the rows left out of the analysis", {
  skip_on_cran()
  # the RCBD example with two plots spoilt; enough distinct fitted values that
  # the diagnostic plots the mock session renders stay quiet
  d <- demo_data("RCBD")
  d$Yield <- as.character(d$Yield)
  d$Yield[c(5, 10)] <- c("5,6", "<LOD")
  shiny::testServer(doepro_server, {
    rv$data <- d
    session$setInputs(design = "RCBD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", block = "Block", treat = "Variety")
    session$setInputs(tr_1 = "none", run = 1)
    html <- output$runNote$html
    expect_match(html, "22 observations", fixed = TRUE)
    expect_match(html, "2 rows were left out of this analysis", fixed = TRUE)
    expect_match(html, "decimal comma in row 5 ('5,6')", fixed = TRUE)
    expect_match(html, "not a number in row 10 ('&lt;LOD')", fixed = TRUE)
    expect_no_match(html, "'<LOD'", fixed = TRUE)
    expect_match(html, "Apply the corrections", fixed = TRUE)
  })
})

test_that("an analysis refused for too few rows points to the data check", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- data.frame(Trt = c("A", "B", "C", "A", "B"), Yield = c("5", "5,6", "x", NA, "6"),
                          stringsAsFactors = FALSE)
    session$setInputs(design = "CRD", alpha = "0.05", dtype = "auto")
    session$setInputs(resp = "Yield", treat = "Trt", tr_1 = "none", run = 1)
    html <- output$runNote$html
    expect_match(html, "Only 2 rows can be analysed", fixed = TRUE)
    expect_match(html, "3 rows were left out of this analysis", fixed = TRUE)
    expect_match(html, "The data check on the Data tab lists these entries", fixed = TRUE)
  })
})
