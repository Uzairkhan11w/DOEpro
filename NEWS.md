# DOEpro (development version)

* The workflow follows the work: Data (with the design and the columns), then
  Exploration & Diagnostics, Assumptions & Diagnostics, ANOVA, Means &
  Comparisons, Plots, and Interpretation & Report. The design, the response
  variable(s), the role of every column and the significance level are set on
  the Data tab, and every result follows each choice at once: there is no
  Run button, and no table, letter or reading is ever left from an earlier
  choice (the significance level included). The column mapping is drawn even
  while the Data tab is hidden, so nothing waits for a tab to be visited.
* The Data tab shows, as soon as data are loaded: a quick summary of each
  chosen response (n, missing, mean, SD, the SE of the mean, median,
  quartiles, range and CV where it means something), and the automatic
  screening of the chosen responses only, from the model for the design
  (residual normality, equal variances, the mean-variance slope, Box-Cox and
  the suggested transformation). Previously every numeric column was
  screened, and only after the Design tab had been opened. New function
  `quick_describe()` (internal) and the screening's `screen_rows()`.
* A new example, a factorial CRD (3 varieties x 3 nitrogen levels, four
  replications) with two responses, yield and plant height; loading any
  example now sets its design, its columns' roles and its responses at once.
  The examples are described as demonstration data built for illustration.
* Exploration & Diagnostics has three sections: exploratory visualisations,
  a new table of the data's structure and quality (every column's role,
  storage, analytical type, totals, missing values and distinct values; the
  type follows the role, so a treatment coded 1, 2, 3 is categorical), and
  the other exploratory analyses.
* Assumptions & Diagnostics comes before the ANOVA. Each response has a
  nature (continuous, count, percentage) and a scale chosen there; the values
  as entered are analysed until a transformation is chosen (it was applied
  automatically before). A summary sets each check beside its result, its
  meaning and the action it calls for, and separates what can be tested
  from what the design must ensure (independence). New checks: Tukey's
  one-degree-of-freedom test of additivity for designs with blocks, and
  Cook's distance with a plot. Counts and percentages are no longer treated
  as automatically normal, and the text says a transformation does not
  guarantee the assumptions.
* The ANOVA tab gives each response its own "Analysis of Variance (...)"
  heading and table, with the significance column headed by the level. The
  combined table of mean squares and the guides under the tables are gone
  (the guides are on the Data tab). A significant interaction is pointed out
  above its reading, with the way to read it.
* Means and Post-hoc are one tab, Means & Comparisons. By default the
  tables show the means alone. The comparison chosen decides what is added:
  the critical difference (C.D., Fisher's protected LSD) at the foot, or
  the superscript letters of Tukey's HSD, Duncan's, SNK, Scheffe's or the
  Bonferroni-adjusted LSD, or Dunnett's comparisons with a control, never a
  C.D. and letters together. The C.D. is labelled with the level, "C.D.
  (5%)". An interaction can be compared within each level of one of its
  factors (simple effects). Readings, the CSV, the plots and the report
  follow the comparison chosen. New exported function `comparison()`;
  `build_report()` takes `comparison` (its `letters_on` is no longer used).
* Every graph has a download menu in its corner: PNG at 300 dpi, or SVG,
  named StatLabX_<graph>_<response>, with "Generated with StatLabX" at its
  foot. A new Interaction plots section draws each cell mean's confidence
  interval where one can be given (designs with one error term) and says
  why not elsewhere; its reading follows the interaction's F-test, not the
  lines alone. One plot style throughout, with a colour-blind-safe palette.
* Every graph in the app is drawn in the browser as a crisp vector chart in
  the platform's own style, in place of R's images: bars with a soft
  gradient and rounded tops, rounded heat-map tiles whose figures turn white
  on dark tiles, the grouping letters as pills, legends as pills whose keys
  show the marks they stand for, soft confidence bands, true minus signs,
  labels kept inside the panel and off each other, a tooltip on every mark,
  and a short drawing-in animation (none when the system asks for reduced
  motion). The charts redraw to fit any screen, phones included. R still
  decides what is drawn: the browser draws the bars, intervals, points,
  lines, labels and axes that ggplot2 computes (`chart_spec()`, internal),
  so every reading beside a graph stays true, and the plot functions still
  return ggplot objects for use in R. The downloads are made in the browser
  from the chart as shown (PNG at 300 dpi, or SVG), so they no longer wait
  for R to draw the graph again. The heat maps of correlations and of
  factor loadings share one red-to-blue scale through clear tints, so a
  middling value no longer reads as grey.
* The report prints: "Print / Save as PDF" opens the browser's print dialog
  without saving a file first, on A4 with tables kept whole and wide tables
  on landscape pages where the browser allows; "Download HTML" stays; a PDF
  download is offered only where a renderer is installed. The report carries
  the StatLabX credit, the version and the date.
* Dr. Pradeep Mishra (Assistant Professor, JNKVV, Madhya Pradesh) is listed
  last among the developers, wherever they are shown, and, like Dr. Imran
  Khan, is not among the citation's authors.
* StatLabX has a website as its front page (doepro.pages.dev): plain HTML,
  CSS and JavaScript that appears at once, while R starts in the background.
  It shows the worked example's real results, what the app covers, the
  people behind it and how to cite it, with a live view of R loading; "Start
  analysing" opens the app (as soon as R is ready), "See a worked example"
  loads, maps and runs the RCBD example, and the app's About tab and logo
  bring the website back. Facts on the page come from the package at build
  time.
* The app's tabs follow the order of the work: About, Data, Explore, Design
  and ANOVA, Means, Assumptions, Post-hoc, Plots, Interpretation and report.
  Explore now comes straight after the data, ahead of the analysis of
  variance.
* The new StatLabX logo is used throughout: the website, the app's navigation
  bar, the report and the browser tab.
* Dr. Imran Khan is listed as Chief Scientist.
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
* The Help tab is gone; its guidance now sits, folded until opened, in the
  tab where it is needed: how to start on the Data tab, the designs and their
  error terms (and the pooled analysis, when a pooled design is chosen) on the
  Design tab, how SE(m), SE(d) and the C.D. are worked out on the Means tab,
  choosing a transformation on the Assumptions tab, and which post-hoc test
  to choose, now including Dunnett's, on the Post-hoc tab.
* A new look for the app, branded StatLabX. It runs on Bootstrap 5 through
  bslib's precompiled theme, so the browser compiles nothing (a theme with
  changed Sass variables took 5.3 s to compile natively, and would have been
  compiled in the browser on every visit); the styling is plain CSS. A
  floating pill-shaped navigation bar carries the logo and the name, the
  side panels, tables, plots and readings sit on white cards, numbers line up
  in tables, and red marks significance only (NS is grey). The About page
  opens with a headline, a short summary and the app's facts as chips. The
  version shown is now the package's, 2.0.1.
* Dunnett's test joins the post-hoc procedures: each treatment against a
  control chosen in the post-hoc panel from the levels of the effect (a level
  with no number, such as Control, is offered first), two-sided or one-sided
  (higher, or lower, than the control). The critical value is the quantile of
  the comparisons' joint multivariate t distribution, computed exactly with
  equal or unequal replication and with one missing plot: it reproduces
  Dunnett's (1955) tables, and Student's t for a single comparison. With two
  or more missing plots in a blocked design the correlations are given the
  nearest exact form, and the output says it is an approximation. Within a
  split, strip or pooled interaction the treatments are compared with the
  control within each level of the slicing factor. As for every other test,
  nothing is declared under a non-significant F-test.
* The browser version opens with a StatLabX loading screen instead of an
  unexplained spinner. It says what is happening (R loading, R starting, the
  packages loading and installing, the app starting), and its bar counts the
  real bytes of R and its packages as they arrive, against the true total for
  the build (about 59 MB); nothing is estimated and no percentage is printed.
  It also says that R runs in the browser, so data never leave the computer,
  and that only the first visit downloads R.
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
* In designs with blocks (RCBD, Latin square, split and strip plots, pooled
  analyses) Levene's and Bartlett's tests now compare each plot's residual,
  recentred on its treatment's mean, rather than the raw values. The raw
  values still carry the block effects, which the error variance does not:
  with block differences larger than the error the tests almost never found
  anything (0.1-1.5% at 5% when variances were equal, and 3% for a treatment
  three times as variable in a simulated 5 x 5 RCBD). On the residuals they
  hold 2-5% and find that treatment 18-33% of the time. The table says which
  values were used. The Taylor slope still uses the values themselves, since
  residuals flatten it (about 0.3 where it should be 2).
* Pooled factorial analyses test equal variances within each environment.
  The cells used to be the treatment combinations pooled across
  environments, so a treatment-by-environment pattern in the means was
  reported as unequal variances and drove the suggested transformation.
* The exact-fit check judges the residuals against the spread of the values,
  with a floor for rounding, rather than against their size: a response near
  2e9 that varies by 1 was called an exact fit and its checks were skipped.
* A p-value below a threshold no longer prints at it. The ANOVA tables, the
  readings, the interpretation, the assumption table and the captions of the
  tables of means printed p = 0.049973 as "0.05" beside a star meaning
  p < 0.05; it now prints as 0.04997, as the correlation and regression
  tables already did. A p-value exactly at the level is not significant
  everywhere, as the stars have it.
* An accuracy audit of the whole package (October 2026) found and fixed the
  following.
  - **Pooled factorial interactions.** With the environment random, two
    cell means of an interaction carry the environment's interaction with
    each factor in which they differ. Their SE(d) used MS(E x A x B) alone,
    so the C.D. was too small whenever the environment interacted with a
    single factor: in simulation, two treatments with no real difference
    were declared different 42% of the time at 5%. Each kind of comparison
    now has its own SE(d) from the expected mean squares (for two factors,
    the strip-plot formula with the environment as the replication; checked
    against the spread of the per-environment differences) and a C.D. with
    Gomez and Gomez's weighted t; post-hoc tests use Satterthwaite's
    degrees of freedom pair by pair, and Dunnett's test, whose exact
    critical value does not apply there, is refused for these means.
  - The note under a pooled factorial's environment table no longer says the
    treatment differences do not change between environments when an
    environment-by-factor interaction is significant; it names it.
  - A combined analysis with one observation per environment and treatment
    is refused with a plain message instead of R's internal error.
  - A missing entry in a design column no longer reorders its levels:
    numeric codes keep numeric order and are written as typed (100000, not
    1e+05), and a factor keeps the level order it was given, so Dunnett's
    default control no longer moves.
  - Adjusted means are kept for responses of any size; values near 1e-8 (on
    the reciprocal scale, say) had fallen back to the plain averages.
  - Inf, NaN and hexadecimal entries (0x10) are no longer read as numbers:
    the data check lists them and the analysis leaves those rows out with
    the reason, instead of analysing 16 or stopping with R's error.
  - Pasted text whose lines have more values than the header (decimal
    commas in comma-separated text) is refused, as an uploaded file is,
    instead of shifting every column one place.
  - A % sign typed into one cell of a number column is refused, as the data
    check treats it, instead of storing 62 for 62% beside 0.45.
  - Sowing dates (25 Oct, 10 Nov), numeric dates (15/06/2023, day first)
    and fractions (1/4 RDF) are ordered as people mean them; doses such as
    120/60/40 keep their order.
  - The table of means for several responses keeps every level any response
    has; the summary card and run note give each response's own count.
  - Each factor's table in a split or strip plot gives the C.V. of its own
    error (CV(a) under the main-plot factor), not the last one.
  - A split- or strip-plot interaction that is not significant no longer
    quotes C.D.s for letters that are not shown.
  - Means that print alike but differ among themselves (small values at two
    decimals) are no longer called significantly different from a mean they
    share a letter with, in the means and post-hoc readings; an SE or C.D.
    never prints as 0.00, and the report uses the decimals chosen on screen.
  - The transformation adviser no longer orders a transformation from a
    mean-variance slope or Box-Cox interval that is not significant (one
    stray value made it choose 1/y and turn a significant result
    non-significant); it points to the flagged row instead. It suggests
    only transformations defined for the data, calls a 0-1 response a
    proportion only when its name says so, and Box-Cox is refused when
    there is no optimum (zeros, or a profile still rising at -2 or 2)
    rather than applying lambda = 1.
  - The Interpretation tab and report judge the CV as the readings do (not
    on a transformed scale or for a negative mean), name the best treatment
    by its back-transformed mean (1/y reverses the order) and every level
    tied there, claim no significance for an exact fit, read a three-way
    interaction as what its test asks, and say the recommendation is
    judged on the values as entered. Post-hoc output on the 1/y scale says
    the order is reversed.
  - The assumption table's mean-variance verdict follows the slope's own
    test; the combined ANOVA table is split by response when their d.f.
    differ (a missing plot in one trait).
  - Promax factor scores are now regression scores of correlated factors
    (multiplied by the factor correlations); they had standard deviations
    above 1 and errors five times too large.

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
