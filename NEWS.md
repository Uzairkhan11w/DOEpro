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
