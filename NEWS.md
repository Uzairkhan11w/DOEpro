# DOEpro (development version)

* Unequal replication: every mean now has its own standard error and every pair
  of means its own SE(d) and C.D. Blocked designs with a missing plot and
  factorials with unequal cells use adjusted (least-squares) means with
  Type III tests. Split plots, strip plots and pooled analyses with a missing or
  repeated plot are refused with a message naming it.
* The chosen significance level now drives every C.D., letter, F-test verdict,
  assumption verdict, Box-Cox interval, plot caption and line of text. The
  `cd5` and `cd1` effect fields are replaced by `cd` at that level.
* Grouping letters use Piepho's (2004) insert-and-absorb algorithm; SNK and
  Duncan enforce the step-down rule; Tukey becomes Tukey-Kramer with unequal
  replication; post-hoc output includes a table of every pairwise comparison.
* Factor levels follow their natural order everywhere: D0, D60, D120, D180 and
  T1, T2, ..., T10 rather than alphabetically. Interaction plots put a time
  factor (or, failing that, a quantitative one such as N0, N60, N120) on the
  X-axis by default, and the Plots tab lets you choose the X-axis factor.
* A data check runs as soon as data are loaded. It finds decimal commas
  (5,6), decimal points and commas mixed in one column, thousands separators,
  per cent signs and units typed after numbers, numbers stored as text, entries
  that are not numbers, no-value markers (including Excel's #DIV/0! and #N/A),
  empty cells, labels that differ only in spacing, capitals or punctuation, and
  empty or repeated rows. It says what is wrong and which rows are affected.
  Corrections are applied only when asked, and only where the meaning is
  certain; anything uncertain (12a, nil, 1,250, a % sign on some values only,
  AA beside aa) is listed for the user and never changed, emptied or merged.
  New exported functions `check_data()` and `fix_data()` do the same from R.
  The data table now shows row numbers, which the messages use.
* Rows left out of an analysis are listed with the reason, in the app and in
  the report, instead of disappearing silently. Pasted "-" and "." are no
  longer turned into missing values without notice, "#" is no longer read as
  the start of a comment, and semicolon-separated CSV files are recognised.
* A new Explore menu holds descriptive statistics. For each numeric column:
  N, missing, mean, SD, minimum, Q1, median, Q3 (as Excel's QUARTILE.INC
  gives them), maximum, CV and skewness (as Excel's SKEW gives it),
  optionally for each level of a grouping column. A histogram, density
  curve, box plot and normal Q-Q plot go with them, each followed by a
  plain-language reading worked out from the numbers that plot shows.
  Skewness is read with Bulmer's bands, and the direction is stated only in
  figures the table shows. Unusual values are those beyond the box-plot
  fences, judged within each group when the table is grouped. The Q-Q
  reading compares the bend at each end of the plot with what normal
  samples of the same size show. Values recorded in steps get histogram
  bars on their grid. New exported function `describe_data()`.
* The Assumptions tab now shows all four residual plots. Before, only the
  scale-location plot appeared. With few distinct fitted values (a CRD), its
  trend line joins the mean at each fitted value instead of a loess curve
  that rose where there were no points.
* Numbers written back by the data-check corrections are never in scientific
  notation (400000, not 4e+05).

# DOEpro 2.0.1

Changes made in response to the CRAN review of version 2.0.0.

* Software names are quoted and the methods are referenced with their DOIs in
  the package description.
* `\value` is documented for the exported `DESIGNS` and `TRANS` objects.
* `demo_data()` no longer sets a random seed. The example datasets are now built
  from a stored vector of deviates, so they are identical on every machine and
  the user's random number stream is left untouched.
* The plain-text report saves and restores the user's graphical parameters with
  an immediate `on.exit()`.

# DOEpro 2.0.0

First public release.

* Analyses eleven designs: completely randomised, randomised complete block,
  Latin square, factorial CRD and RCBD (two to four factors), split-plot,
  strip-plot, and pooled (combined) analysis over environments for both a single
  treatment factor and factorial treatments.
* Chooses the correct error term for every comparison. A split plot reports its
  four distinct standard errors of a difference, the two mixed comparisons using
  a Satterthwaite-weighted *t*.
* Pooled analyses test each treatment effect against its own interaction with
  the environment, check the homogeneity of the error variances across
  environments (Bartlett), and break a significant environment by treatment
  interaction down environment by environment.
* Analyses several response variables at once, each with its own transformation.
* Screens the data against the assumptions and advises on a variance-stabilising
  transformation; nine transformations are available, with means reported back on
  the original scale.
* Grouping letters are shown only where the F-test is significant.
* Six post-hoc procedures: Fisher's protected LSD, Bonferroni, Tukey HSD,
  Duncan, Student-Newman-Keuls and Scheffe.
* Produces a downloadable HTML report carrying the DOEpro citation.
* Runs in the browser at <https://doepro.pages.dev> with no installation.
