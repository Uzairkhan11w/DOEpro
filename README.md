# DOEpro

[![R-CMD-check](https://github.com/Uzairkhan11w/DOEpro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Uzairkhan11w/DOEpro/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21399570.svg)](https://doi.org/10.5281/zenodo.21399570)
[![Licence: GPL-3](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Launch app](https://img.shields.io/badge/launch-doepro.pages.dev-brightgreen.svg)](https://doepro.pages.dev)

**Try it now — no installation:** <https://doepro.pages.dev>

A free, open, single-file R Shiny application for the analysis of designed agricultural
experiments. It brings the standard analyses used in field and horticultural research —
ANOVA for the common designs, mean comparisons, data transformations and clear reporting —
together in one accessible interface, and serves as a free, self-contained option for the
kind of analysis researchers carry out in tools such as OPSTAT.

Paste your data straight out of Excel, choose the design, press **Run analysis**. You get
the ANOVA, publication-format tables of means with SE(m)±, SE(d), C.D. (P≤0.05) and C.V.
at the foot, automatic advice on data transformation, post-hoc comparisons, plots, a
plain-English interpretation, and a report you can download as HTML or PDF.

**Developed by**

1. **Dr. Immad A. Shah** — Scientist (Statistics), Division of Agricultural Statistics, SKUAST-Kashmir. [ORCID 0000-0003-2761-5112](https://orcid.org/0000-0003-2761-5112) · immad11w@skuastkashmir.ac.in
2. **Mr. Uzair Javid Khan** *(maintainer)* — UG Research Student, Statistics, AAAMDC Bemina, Cluster University Srinagar. [ORCID 0009-0001-6368-0977](https://orcid.org/0009-0001-6368-0977) · uzairkhan11w@gmail.com
3. **Dr. M. Iqbal Jeelani** — Scientist (Statistics), Division of Agricultural Statistics, SKUAST-Kashmir. [ORCID 0000-0002-2974-2871](https://orcid.org/0000-0002-2974-2871)

Suggestions and feedback are welcome — write to the maintainer (uzairkhan11w@gmail.com) or open an issue in the repository.

---

## Installation

```r
install.packages(c("shiny", "DT", "ggplot2"))   # that is the whole dependency list
install.packages("pagedown")                    # optional: server-side PDF export
```

## Running

```r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and press **Run App**.

---

## What is new in v2.0

- **Pooled (combined) analysis over environments** — when the same experiment runs across several locations, years or seasons, DOEpro runs the combined ANOVA (environment, treatment, and their interaction), tests homogeneity of error variances across environments (Bartlett), tests each treatment against the treatment × environment interaction, and reports treatment means over environments alongside within-environment comparisons. **Factorial treatments (2, 3 or 4 factors) over environments** are also supported: every main effect and interaction is tested against its own interaction with the environment, and each is reported as a table of means pooled over environments.
- **Several response variables at once.** Select as many response columns as you like; each
  is analysed with the same design and they appear side by side in one table of means, the
  way a results table is actually published.
- **Automatic screening.** The moment your data are loaded, every numeric column that is
  not a factor or a block is screened — Shapiro-Wilk, Levene, Taylor's power-law slope,
  Box-Cox λ, C.V. — and the app names the transformation each variable wants, with reasons.
  One button applies all of the advice.
- **Tables of means in the standard agronomy layout** (see below), now presented with a summary card, clearly labelled sections (main effects, two-way tables, detailed tables) and a notation key.
- **PDF report**, with the package credit in the bottom-right corner of every page.
- **The Box-Cox profile is fixed** and now plots correctly.
- **No more `MASS`, `car` or `agricolae`.** Levene's test, the Box-Cox profile likelihood
  and all six post-hoc procedures are implemented directly. The app depends only on
  `shiny`, `DT` and `ggplot2` — which is what makes free WebAssembly hosting possible
  (see `DEPLOYMENT.md`).

---

## Layout of the tables of means

**One factor, several parameters** — one row per treatment, one column per measured
character, each cell `mean ± SE` with the grouping letter as a superscript:

| TREATMENT | Yield | Plant height | Fruits per plant |
|---|---|---|---|
| T1 | 42.30 ± 1.26 ᶜ | 78.4 ± 2.1 ᵇ | 31.2 ± 1.4 ᵇ |
| T2 | 47.10 ± 1.26 ᵃ | 84.9 ± 2.1 ᵃ | 38.7 ± 1.4 ᵃ |
| **SE(m) ±** | 1.26 | 2.1 | 1.4 |
| **SE(d) ±** | 1.78 | 2.97 | 1.98 |
| **C.D. (P≤0.05) Treatment** | 3.81 | 6.34 | NS |
| **C.V. (%)** | 7.60 | 5.31 | 9.02 |

**Two factors** — the familiar grid with marginal means, and the three critical differences
spelled out underneath:

|  Factor 1 \ Factor 2 | I1 | I2 | I3 | Mean |
|---|---|---|---|---|
| T1 | 30.70 ᵈ | 36.12 ᶜ | 41.06 ᵇ | 35.96 |
| T2 | 36.56 ᶜ | 38.92 ᵇᶜ | 46.62 ᵃ | 40.70 |
| **Mean** | 33.63 | 37.52 | 43.84 | 38.33 |

> SE(m) ± 0.86 / 0.70 / 1.22 SE(d) ± 1.22 / 0.99 / 1.72
> **C.D. (P≤0.05)** Factor 1: **2.71**  Factor 2: **2.21**  Factor 1 × Factor 2: **NS**
> C.V. (%) 5.49

C.D. is printed only when the F-test for that source is significant; otherwise the cell
reads `NS`. When a response has been transformed, each cell shows the **back-transformed
mean with the transformed value in parentheses**, and SE, C.D. and C.V. refer to the
transformed scale — because that is the scale on which the tests were done.

---

## Designs supported

| Design | Columns needed | Error term(s) |
|---|---|---|
| Completely randomised (CRD) | treatment, response(s) | single pooled error |
| Randomised complete block (RCBD) | block, treatment, response(s) | single pooled error |
| Latin square (LSD) | row, column, treatment, response(s) | single pooled error |
| Factorial CRD (2–4 factors) | factors A–D, response(s) | single pooled error |
| Factorial RCBD (2–4 factors) | block, factors A–D, response(s) | single pooled error |
| Split plot | replication, main plot, sub plot, response(s) | Error(a), Error(b) |
| Strip plot | replication, factor A, factor B, response(s) | Error(a), Error(b), Error(c) |
| Pooled analysis over environments (RCBD base) | environment, replication, treatment, response(s) | R(env), pooled error |
| Pooled analysis over environments (CRD base) | environment, treatment, response(s) | pooled error |
| Pooled factorial over environments (RCBD base, 2-4 factors) | environment, replication, factors A-D, response(s) | each effect vs its environment interaction; pooled error |
| Pooled factorial over environments (CRD base, 2-4 factors) | environment, factors A-D, response(s) | each effect vs its environment interaction; pooled error |

Data go in **long format**: one row per plot, one column per variable.

A split plot needs **four** different SE(d), and the app prints all four:

- two main-plot means: √(2·Ea/rb)
- two sub-plot means: √(2·Eb/ra)
- two sub-plot means within the same main plot: √(2·Eb/r)
- two main-plot means at the same sub-plot level: √(2[(b−1)Eb + Ea]/rb), with a
  Satterthwaite-weighted *t*

A strip plot needs three error strata and the analogous mixed comparisons. The cell-means
table therefore carries **two** letter columns — one for each legitimate comparison.

---

## Transformation adviser

| Signature | Suggestion |
|---|---|
| proportion in [0, 1], non-integer | arcsine √p |
| integer counts, Taylor slope *b* ≈ 1 | √y, or √(y + 0.5) with zeros |
| named as a percentage and bounded 0–100 | arcsine √(y/100) |
| *b* ≈ 2 | log y, or log(y + 1) with zeros |
| *b* > 2.5 | 1/y |
| otherwise | Box-Cox λ |

A 0–100 range on its own is **not** treated as evidence of percentage data — most yields
and plant heights live there too. The adviser uses the column name and the mean–variance
signature, and you can override it with the *Nature of the response* selector.

When the diagnostics are satisfied but the data are plainly counts or percentages, the app
still names the conventional transformation and marks it **optional**, leaving the default
at *None*. It tells you what convention expects; it does not transform behind your back.

---

## Post-hoc tests

Fisher's protected LSD, Bonferroni-adjusted LSD, Tukey's HSD, Duncan's DMRT,
Student–Newman–Keuls, and Scheffé. Each is computed from the error mean square and degrees
of freedom of the effect you select, so in a split or strip plot they automatically use the
right error stratum. Duncan and SNK report the full table of critical ranges *R*ₚ.

---

## Report

**Download report (HTML)** always works. **Download report (PDF)** appears when the machine
has a rendering engine — `pagedown` (needs Chrome or Chromium), `weasyprint`, or
`wkhtmltopdf`. On a hosted server without one, download the HTML and print to PDF from the
browser; the page footer carries the credit line either way.

Every page of the PDF carries, in the bottom-right corner:

> DOEpro · Shah, Khan & Jeelani · SKUAST-Kashmir — page *n*

---

## What was verified

Executed under R 4.3.3 on generated data for all seven designs, with two response variables
each. Checked by hand or against a reference implementation:

- Degrees of freedom and sums of squares for every stratum of the split and strip plots.
- All four split-plot SE(d) and both strip-plot mixed comparisons, including the
  Satterthwaite-weighted *t*.
- The Box-Cox profile against the textbook log-likelihood with the explicit Jacobian term
  `−n/2·log(RSS/n) + (λ−1)·Σ log y` — identical to the last decimal on six test cases.
- Levene's test against its definition (one-way ANOVA on absolute deviations from cell
  medians).
- Post-hoc identities: Duncan's *R*₂ = SNK's *R*₂ = Fisher's LSD, SNK's *R*ₖ = Tukey's HSD,
  Duncan ≤ SNK for every *p*, and Scheffé ≥ Tukey ≥ LSD. These hold to `qtukey`'s own
  numerical precision (≈2 × 10⁻⁸).
- Unbalanced data, missing values, four-factor factorials, and all nine transformations
  round-tripping exactly through their inverses.
- The `save_pdf` fallback chain, exercised against a stub renderer.

Not executed in the build environment: the Shiny UI layer, the `ggplot2` plots, and a real
PDF render. Those are desk-checked. If something misbehaves on first run, that is where to
look.

---

## Caveats

- Fisher's LSD is valid only after a significant F-test; the app warns you when the F-test
  for the selected effect is not significant.
- When an interaction is significant, interpret the cell means, not the main-effect means.
- Grouping letters assume equal replication. With unbalanced data the app warns you and the
  letters become approximate.
- Blocks and replications are treated as fixed effects.

## How to cite

If DOEpro contributes to work you publish, please cite it:

> Shah, I. A., Khan, U. J. and Jeelani, M. I. (2026). *DOEpro: analysis of designed
> agricultural experiments*. Version 2.0.0. Zenodo. doi:10.5281/zenodo.21399570

```bibtex
@software{doepro2026,
  author  = {Shah, Immad A. and Khan, Uzair Javid and Jeelani, M. Iqbal},
  title   = {DOEpro: Analysis of Designed Agricultural Experiments},
  year    = {2026},
  version = {2.0.0},
  doi     = {10.5281/zenodo.21399570},
  url     = {https://github.com/Uzairkhan11w/DOEpro}
}
```

The DOI above is the *concept* DOI: it always resolves to the most recent release.
GitHub's **Cite this repository** button reads `CITATION.cff` and produces the same
reference.

## Licence

GPL-3. See `CITATION.cff` and `DEPLOYMENT.md` for citation, DOI and publishing.
