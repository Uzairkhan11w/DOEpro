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
* Explore > Correlation: Pearson's r, Spearman's rho or Kendall's tau-b for
  every pair of the chosen numeric columns, each with N (the rows that have
  both values), the p-value, a significance mark at the chosen level, a
  confidence interval for r, and a one-line reading. A matrix, a heat map, a
  scatter plot of any pair, and a reading of the whole table go with them.
  Spearman's and Kendall's p-values are exact whenever the orderings of the
  values can be counted (eight rows, and many more when values are tied, as
  in sparse counts), estimated from random orderings when there are ties and
  too many to count (Spearman's at any size, Kendall's up to 20 rows, with a
  closer count near a significance threshold), and marked where they are a
  large-sample approximation. Strength uses Cohen's 0.1, 0.3 and 0.5 for r,
  carried to rho and tau through their relation to r for normal data. A
  result short of significance is reported as insufficient evidence of an
  association, and the reading warns about chance findings among many pairs
  and about plot-level correlations mixing treatment effects. New exported
  function `correlate_data()`.
* Explore > Regression: simple and multiple linear regression of one numeric
  column on one or more others, using the rows that have every value. The
  fitted equation; each coefficient with its standard error, t, p-value,
  significance mark and confidence interval at the chosen level, printed to
  the precision its standard error supports (and, with several predictors,
  its variance inflation factor); N, R-squared, adjusted R-squared, MSE,
  RMSE, MAE and the F test of the model; and the regression's analysis of
  variance. The main plot shows the rows, the fitted line, its confidence
  band, the equation and R-squared (with several predictors, the line for
  one predictor with the others at their means, and each row adjusted to
  them). Residuals against fitted values, a normal Q-Q plot and observed
  against predicted check the fit, with Shapiro-Wilk, Breusch-Pagan
  (Koenker) and a test for a curve; with up to 200 rows the first two take
  their p-values from normal data simulated on the same predictor values,
  since with few rows the usual ones can be far off for residuals. Every
  table and plot has a reading; a slope
  short of significance is reported as insufficient evidence, and the
  reading says when the intercept is an extrapolation and that the equation
  holds only over the observed range. A predictor fitted with its powers
  (Dose with Dose2, or Dose2 and Dose3) is drawn and read as one curve, and
  a response surface (N, P, N2, P2 and NP) as one surface; a quadratic is
  read with the dose at which it turns. New exported function
  `regress_data()`.
* Explore > Principal components: principal component analysis of the
  chosen numeric columns, on their correlations (each standardised) or their
  covariances. The eigenvalues with the share of the variation each
  component carries; the loadings (each variable's correlation with each
  component) and communalities; the scores; and the scree plot, score plot
  (coloured by a grouping column if chosen), loading plot and biplot. The
  number of components worth keeping comes from Horn's parallel analysis,
  with Kaiser's rule beside it. Each component is read from its strong
  loadings as printed, and the readings say only which variables rise and
  fall together, never what causes it. New exported function `pca_data()`.
* Explore > Cluster analysis: hierarchical clustering of the rows, or of the
  means of each level of a grouping column (varieties by their trait means),
  on the chosen numeric columns, standardised by default, with Euclidean or
  Manhattan distances and Ward's, complete, average or single linkage. The
  number of clusters is suggested by the average silhouette width (and can
  be chosen), read with Kaufman and Rousseeuw's bands; the cophenetic
  correlation shows how faithfully the tree keeps the distances. Tables of
  the clusters, their members, silhouettes and means; the dendrogram
  coloured at the cut, the clusters on the first two principal components,
  and the silhouette width by number of clusters. Each cluster is read from
  its means as printed, and the reading says that clustering is no test.
  New exported function `cluster_data()`.
* Explore > Factor analysis: maximum-likelihood factor analysis
  (`factanal()`) of the chosen numeric columns, with the Kaiser-Meyer-Olkin
  measure (overall and for each variable, in Kaiser's words) and Bartlett's
  test of sphericity; the number of factors from parallel analysis (the
  same as on the principal components tab) or chosen; varimax, promax or no
  rotation; the loadings with communalities and uniquenesses, the variance
  each factor carries, the factor correlations after promax, the test that
  the number of factors is enough, and the factor scores. A scree plot, a
  heat map of the loadings and a loading plot, each with a reading. Each
  factor is read from its loadings of 0.40 or more as printed; cross-loading
  variables, low communalities and Heywood cases are named, and the reading
  leaves naming the factors to the reader. New exported function
  `fa_data()`.
* Every table and plot of the analysis of variance now has a reading after
  it. The ANOVA table: each row's verdict in the terms of the design (blocks,
  rows and columns, an interaction as one factor's effect depending on
  another), which error each row is tested against in split plots, strip
  plots and pooled analyses, and the CV against the usual guide. The tables
  of means: the highest and lowest means and the means that do not differ
  significantly from them, exactly as the letters and C.D.s decide; how a
  significant interaction changes the pattern from row to row; a caution on
  main-effect means when an interaction involving the factor is significant.
  The post-hoc tab: what the chosen test does, what it finds, how many pairs
  it declares, those the step-down rule holds back, and the count Fisher's
  protected LSD gives. The assumption checks, the Box-Cox profile, the
  mean-variance plot and the four residual plots, and the main plot on the
  Plots tab, including whether interaction lines cross. A result short of
  significance is insufficient evidence of a difference, at the level chosen;
  an exact fit is said to make the verdicts meaningless rather than read as
  findings.
* The residual plot is now of standardised residuals, with lines at -3 and 3
  and the points beyond them labelled with their row numbers.
* Possible outliers are named by their row numbers in the data. Before, they
  were their positions among the analysed rows, which point at the wrong row
  once any row has been left out.
* With one error degree of freedom the residuals take the same pattern
  whatever the data (in a 2 x 2 RCBD, Shapiro-Wilk gave p = 0.024 every
  time), so normality is reported as not testable instead of as a
  significant departure.
* The Assumptions tab now shows all four residual plots. Before, only the
  scale-location plot appeared. With few distinct fitted values (a CRD), its
  trend line joins the mean at each fitted value instead of a loess curve
  that rose where there were no points.
* Numbers written back by the data-check corrections are never in scientific
  notation (400000, not 4e+05).
* Levene's test no longer reports unequal variances whenever each cell has two
  values (a CRD with two replications, an RCBD with two blocks). Before, F
  came out near 1e28 and the false verdict drove the suggested
  transformation. With two values both lie equally far from their median, so
  such cells are left out of Levene's test. When any cell has fewer than
  three values, the verdict on equal variances uses Bartlett's test, which
  covers every cell, and says so. When neither test can be run, the
  mean-variance slope decides the suggested transformation if it is itself
  significant, and the advice no longer says that no transformation is
  needed.
* Levene's test can now find unequal variances with three values per cell.
  In a cell with an odd number of values the median is one of them, so one
  deviation is always zero; with three values per cell that capped F at 4,
  and among four or fewer treatments the test could never reject. When every
  cell has an odd number of values that zero is now left out (Hines and
  O'Hara Hines, 2000), and the table says so.
* An assumption test that could not be computed is reported as "not
  available" with the reason, never as a significant departure. When the
  model fits every value exactly, the app says so and does not run
  Shapiro-Wilk, Levene, Bartlett or Box-Cox on rounding residue or draw it
  in the residual plots.

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
