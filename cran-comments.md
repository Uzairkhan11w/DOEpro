## Resubmission

This is a resubmission of DOEpro, now version 2.0.1. Thank you for the review.
Each point raised has been addressed below.

**1. Software names in single quotes in the title and description.**

Done. The description now writes the package name as `'shiny'`. The title
contains no software names.

**2. References describing the methods.**

Added to the Description field, in the requested form:

* Gomez and Gomez (1984, ISBN:9780471870920), the source of the procedures the
  package implements and of the worked examples it is validated against;
* Yates and Cochran (1938) <doi:10.1017/S0021859600050978>, for the combined
  analysis over environments;
* Box and Cox (1964) <doi:10.1111/j.2517-6161.1964.tb00553.x>, for the profile
  likelihood used to choose a transformation;
* Satterthwaite (1946) <doi:10.2307/3002019>, for the approximation used in the
  two mixed comparisons of a split plot.

**3. Missing `\value` in `DESIGNS.Rd` and `TRANS.Rd`.**

Done. Both now document what is returned and what it means: `DESIGNS` returns a
named character vector whose elements are the design codes accepted by
`analyze()` and `run_all()`, and `TRANS` returns a named list whose elements each
hold a label, the transformation, and its inverse.

**4. Writing to the user's home filespace.**

We have checked every function that writes. None writes by default: the only
function that opens a file, `pdf_plain()`, takes the destination as a required
argument with no default, and the report writer uses `tempfile()`. The downloads
offered by the application are handled by 'shiny', which supplies its own
temporary path. No example, test or vignette writes outside `tempdir()`.

**5. Changing the user's options, `par` or working directory.**

Fixed in `R/run_all.R`. The graphical parameters are now saved and restored with
an immediate `on.exit()`:

```r
grDevices::pdf(file, width = 8.27, height = 11.69)
oldpar <- graphics::par(no.readonly = TRUE)
on.exit({
  graphics::par(oldpar)
  grDevices::dev.off()
}, add = TRUE)
graphics::par(mar = c(2, 1, 1, 1), family = "mono")
```

The package does not change `options()` and never calls `setwd()`.

**6. Setting a seed within a function.**

Fixed in `R/constants.R`. `demo_data()` previously called `set.seed(42)` before
generating its example datasets. It no longer sets a seed, and no longer draws
random numbers at all: the datasets are built from a stored vector of
standardised deviates, recycled as needed. The user's random number stream is
therefore untouched, and the examples are identical on every machine, which also
makes the package's tests fully deterministic.

## Test environments

* local Windows 11, R 4.6.0
* win-builder, R-devel
* GitHub Actions: Windows Server 2022 (release), macOS (release),
  Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes.

On the previous submission, win-builder reported a possibly invalid URL for
<https://doepro.pages.dev>, with an SSL connection error. The address is live and
answers normally over HTTPS/2 from every client we have tried, including R's own
checker (`curlGetHeaders()` returns `HTTP/2 200`). We believe the check machine's
connection to the host was reset in passing. We are happy to remove the URL from
DESCRIPTION and keep only the GitHub address if you would prefer.

## Downstream dependencies

There are none: this is a new package.
