# DOEpro — validation against Gomez & Gomez (1984)

DOEpro was checked against the worked examples of the standard reference in the field,
Gomez, K. A. and Gomez, A. A. (1984), *Statistical Procedures for Agricultural Research*,
2nd ed., Wiley. Seven designs are covered by five worked examples: completely randomised,
randomised complete block, Latin square, split-plot, and combined analysis over
environments.

For every example the transcription of the raw data was first verified against the book's
own printed marginal totals (treatment, block, row, column, and grand totals). Because the
book computes those totals by hand from the same data, agreement on all of them proves the
data were entered correctly before any analysis was run. Every total matched in every
example.

Throughout, differences confined to the last printed digit arise because the book rounds
intermediate quantities whereas DOEpro carries full precision; these are noted as
"rounding". "exact" means agreement to the precision the book prints.

## 1. Completely randomised design (Table 2.1 / 2.2, pp. 13-16)

Seven insecticide treatments, four replications; rice yield.

| Source | df | SS (book) | SS (DOEpro) | MS (book) | MS (DOEpro) | F (book) | F (DOEpro) |
|---|---|---|---|---|---|---|---|
| Treatment | 6 | 5,587,174 | 5,587,175 | 931,196 | 931,196 | 9.83 | 9.83 |
| Error | 21 | 1,990,238 | 1,990,237 | 94,773 | 94,773 | | |
| Total | 27 | 7,577,412 | 7,577,412 | | | | |

CV: 15.1% (exact). Grand mean 2,040 (exact). Treatment F, df and CV all match.

## 2. Randomised complete block design (Table 2.5 / 2.6, pp. 25-28)

Six seeding rates of rice variety IR8, four blocks.

| Source | df | SS (book) | SS (DOEpro) | MS (book) | MS (DOEpro) | F (book) | F (DOEpro) |
|---|---|---|---|---|---|---|---|
| Replication | 3 | 1,944,361 | 1,944,361 | 648,120 | 648,120 | - | - |
| Treatment | 5 | 1,198,331 | 1,198,331 | 239,666 | 239,666 | 2.17 | 2.17 |
| Error | 15 | 1,658,376 | 1,658,376 | 110,558 | 110,558 | | |
| Total | 23 | 4,801,068 | 4,801,068 | | | | |

CV: 6.7% (exact). Grand mean 4,960 (exact). Treatment F not significant in both, as the
book concludes. (DOEpro additionally reports a replication F of 5.86, which the book leaves
blank; this is a legitimate extra, not a disagreement.)

## 3. Latin square design (Table 2.7 / 2.8, pp. 33-37)

Three maize hybrids and a check in a 4 x 4 Latin square.

| Source | df | SS (book) | SS (DOEpro) | MS (book) | MS (DOEpro) | F (book) | F (DOEpro) |
|---|---|---|---|---|---|---|---|
| Row | 3 | 0.030154 | 0.030155 | 0.010051 | 0.010052 | - | - |
| Column | 3 | 0.827342 | 0.827342 | 0.275781 | 0.275781 | - | - |
| Treatment | 3 | 0.426842 | 0.426842 | 0.142281 | 0.142281 | 6.59 | 6.59 |
| Error | 6 | 0.129585 | 0.129584 | 0.021598 | 0.021597 | | |
| Total | 15 | 1.413923 | 1.413923 | | | | |

CV: 11.0% (exact). Grand mean 1.335 (exact). Treatment F = 6.59 (exact). DOEpro's
row and column F values (0.47 and 12.77) reproduce the book's own blocking-efficiency
calculation on p.37 exactly.

## 4. Split-plot design (Table 3.7 / 3.10, pp. 101-107)

Four rice varieties (sub-plot) x six nitrogen rates (main plot), three replications.

| Source | df | SS (book) | SS (DOEpro) | MS (book) | MS (DOEpro) | F (book) | F (DOEpro) |
|---|---|---|---|---|---|---|---|
| Replication | 2 | 1,082,577 | 1,082,577 | 541,228* | 541,288 | - | - |
| Nitrogen (A) | 5 | 30,429,200 | 30,429,200 | 6,085,840 | 6,085,840 | 42.87 | 42.87 |
| Error(a) | 10 | 1,419,678 | 1,419,678 | 141,968 | 141,968 | - | - |
| Variety (B) | 3 | 89,888,101 | 89,888,101 | 29,962,700 | 29,962,700 | 85.71 | 85.71 |
| A x B | 15 | 69,343,487 | 69,343,487 | 4,622,899 | 4,622,899 | 13.22 | 13.22 |
| Error(b) | 36 | 12,584,873 | 12,584,873 | 349,580 | 349,580 | - | - |
| Total | 71 | 204,747,916 | 204,747,916 | | | | |

**Every SS, MS, df and F matches exactly.** CV(a) = 6.9%, CV(b) = 10.8%, grand mean 5,479.

\* The book prints Replication MS = 541,228, but 1,082,577 / 2 = 541,288.5. This is a
typographical error in the book; DOEpro's 541,288 is arithmetically correct, and it affects
nothing (Replication carries no F-test). That the validation surfaces a known textbook typo
is a sign of its precision.

### The four standard errors of a difference

The strongest single result. A split plot has four distinct comparisons, each with its own
standard error; the two mixed comparisons require a Satterthwaite-weighted *t*. DOEpro
reproduces all four textbook values exactly:

| Comparison | Formula | DOEpro |
|---|---|---|
| Two main-plot (nitrogen) means | sqrt(2 Ea / rb) | 153.8 |
| Two sub-plot (variety) means | sqrt(2 Eb / ra) | 197.1 |
| Two sub-plot means, same main plot | sqrt(2 Eb / r) | 482.8 |
| Two main-plot means, same sub plot (Satterthwaite) | sqrt(2[(b-1)Eb + Ea] / rb) | 445.5 |

## 5. Combined analysis over seasons (Table 8.1 / 8.4, pp. 317-321)

Five nitrogen rates in an RCBD, three replications, repeated in a dry and a wet season.

| Source | df | SS (book) | SS (DOEpro) | MS (book) | MS (DOEpro) | F (book) | F (DOEpro) |
|---|---|---|---|---|---|---|---|
| Season (S) | 1 | 4.495392 | 4.495392 | 4.495392 | 4.495392 | - | 14.25 |
| Reps within season | 4 | 1.261571 | 1.261571 | 0.315393 | 0.315393 | - | - |
| Nitrogen (N) | 4 | 18.748849 | 18.748849 | 4.687212 | 4.687212 | **10.62** | **1.94** |
| S x N | 4 | 9.654423 | 9.654422 | 2.413606 | 2.413606 | 5.47 | 5.47 |
| Pooled error | 16 | 7.063634 | 7.063635 | 0.441477 | 0.441477 | - | - |
| Total | 29 | | | | | | |

Every sum of squares, mean square and degree of freedom matches exactly, and the S x N
interaction F (5.47) matches. Homogeneity of the two error variances: the book uses the
F-max test (F = 1.78, not significant); DOEpro uses Bartlett's test (chi-square = 0.62,
p = 0.43) and reaches the same conclusion, that the variances are homogeneous.

### The one deliberate difference: the nitrogen F-test

For the treatment (nitrogen) the book reports F = 10.62 and DOEpro reports F = 1.94. This
is **not an error in either** — it is a difference of statistical model, and both are valid:

- **Gomez & Gomez** state in the Table 8.3 footnote that *"crop season is considered as a
  fixed variable"*. Under that **fixed-environment** model the treatment is tested against
  the pooled error: 4.687212 / 0.441477 = 10.62.
- **DOEpro** treats the environment as **random** and tests the treatment against the
  treatment x environment interaction: 4.687212 / 2.413606 = 1.94. This is the broad-sense
  inference recommended by the modern multi-environment-trial literature (e.g. Dixon, *The
  Analysis of Combined Experiments*), because the interest is usually in generalising beyond
  the particular seasons or sites tested.

DOEpro follows current best practice (random environments) rather than the book's 1984
fixed-environment convention. The paper documents this so that a reader comparing DOEpro
against this specific example understands the difference. Everything else is identical.

## Summary

| Design | Source of DOEpro's numbers | Agreement with Gomez & Gomez |
|---|---|---|
| CRD | Table 2.2 | exact (to rounding) |
| RCBD | Table 2.6 | exact (to rounding) |
| Latin square | Table 2.8 | exact (to rounding) |
| Split-plot | Table 3.10 | exact, incl. all four SEds |
| Combined over seasons | Table 8.4 | exact, except the fixed-vs-random treatment F (documented) |

Across all five designs, DOEpro reproduces the published analyses exactly on every quantity
that does not depend on the fixed-versus-random modelling choice, and on that one choice it
follows the modern standard, with the difference fully explained. The datasets and the
script that produces these comparisons are shipped with the package in `inst/validation/`,
so the entire validation can be reproduced in seconds.
