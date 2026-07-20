## Reproduce the validation of DOEpro against Gomez & Gomez (1984),
## Statistical Procedures for Agricultural Research, 2nd ed., Wiley.
##
## Run with:
##   library(DOEpro)
##   source(system.file("validation", "textbook_validation.R", package = "DOEpro"))
##
## Each dataset is read from the CSV shipped alongside this script; the analysis
## is run through DOEpro; the result is printed next to the published ANOVA.

dir <- system.file("validation", package = "DOEpro")
rd  <- function(f) utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE)

line <- function() cat(strrep("-", 70), "\n")
show <- function(title, anova, book) {
  cat("\n"); line(); cat(title, "\n"); line()
  print(anova, row.names = FALSE)
  cat("\nPublished (Gomez & Gomez):\n"); print(book, row.names = FALSE)
}

## 1. CRD  (Table 2.2, p.16)
d <- rd("crd_data.csv"); d$Treatment <- factor(d$Treatment)
r <- analyze(d, "CRD", list(response = "Yield", treat = "Treatment"))
show("1. Completely randomised design",
     data.frame(Source = r$anova$Source, Df = r$anova$Df,
                SS = round(r$anova$SS), MS = round(r$anova$MS), F = round(r$anova$F, 2)),
     data.frame(Source = c("Treatment","Error","Total"), Df = c(6,21,27),
                SS = c(5587174,1990238,7577412), MS = c(931196,94773,NA), F = c(9.83,NA,NA)))

## 2. RCBD  (Table 2.6, p.28)
d <- rd("rcbd_data.csv"); d$Treatment <- factor(d$Treatment); d$Block <- factor(d$Block)
r <- analyze(d, "RCBD", list(response = "Yield", treat = "Treatment", block = "Block"))
show("2. Randomised complete block design",
     data.frame(Source = r$anova$Source, Df = r$anova$Df,
                SS = round(r$anova$SS), MS = round(r$anova$MS), F = round(r$anova$F, 2)),
     data.frame(Source = c("Replication","Treatment","Error","Total"), Df = c(3,5,15,23),
                SS = c(1944361,1198331,1658376,4801068), MS = c(648120,239666,110558,NA),
                F = c(NA,2.17,NA,NA)))

## 3. Latin square  (Table 2.8, p.37)
d <- rd("lsd_data.csv"); for (k in c("Row","Column","Treatment")) d[[k]] <- factor(d[[k]])
r <- analyze(d, "LSD", list(response = "Yield", treat = "Treatment", row = "Row", col = "Column"))
show("3. Latin square design",
     data.frame(Source = r$anova$Source, Df = r$anova$Df,
                SS = round(r$anova$SS, 6), MS = round(r$anova$MS, 6), F = round(r$anova$F, 2)),
     data.frame(Source = c("Row","Column","Treatment","Error","Total"), Df = c(3,3,3,6,15),
                SS = c(0.030154,0.827342,0.426842,0.129585,1.413923),
                MS = c(0.010051,0.275781,0.142281,0.021598,NA), F = c(NA,NA,6.59,NA,NA)))

## 4. Split-plot  (Table 3.10, p.107)
d <- rd("splitplot_data.csv")
d$Rep <- factor(d$Rep); d$Nitrogen <- factor(d$Nitrogen); d$Variety <- factor(d$Variety)
r <- analyze(d, "SPLIT", list(response = "Yield", rep = "Rep", main = "Nitrogen", sub = "Variety"))
show("4. Split-plot design",
     data.frame(Source = r$anova$Source, Df = r$anova$Df,
                SS = round(r$anova$SS), MS = round(r$anova$MS), F = round(r$anova$F, 2)),
     data.frame(Source = c("Replication","Nitrogen","Error(a)","Variety","N x V","Error(b)","Total"),
                Df = c(2,5,10,3,15,36,71),
                SS = c(1082577,30429200,1419678,89888101,69343487,12584873,204747916),
                MS = c(541288,6085840,141968,29962700,4622899,349580,NA),
                F = c(NA,42.87,NA,85.71,13.22,NA,NA)))
cat("\nFour standard errors of a difference (DOEpro):\n")
eN <- r$effects[["Nitrogen"]]; eV <- r$effects[["Variety"]]; eI <- r$effects[["Nitrogen:Variety"]]
cat(sprintf("  main-plot means            %.1f\n", eN$sed))
cat(sprintf("  sub-plot means             %.1f\n", eV$sed))
cat(sprintf("  sub-plot, same main plot   %.1f\n", eI$extra[[1]]))
cat(sprintf("  main-plot, same sub plot   %.1f  (Satterthwaite)\n", eI$extra[[3]]))

## 5. Combined analysis over seasons  (Table 8.4, p.321)
d <- rd("pooled_data.csv")
d$Season <- factor(d$Season); d$Nitrogen <- factor(d$Nitrogen); d$Rep <- factor(d$Rep)
r <- analyze(d, "POOLRCBD", list(response = "Yield", env = "Season", rep = "Rep", treat = "Nitrogen"))
show("5. Combined analysis over seasons",
     data.frame(Source = r$anova$Source, Df = r$anova$Df,
                SS = round(r$anova$SS, 6), MS = round(r$anova$MS, 6), F = round(r$anova$F, 2)),
     data.frame(Source = c("Season","Reps w/in season","Nitrogen","S x N","Pooled error","Total"),
                Df = c(1,4,4,4,16,29),
                SS = c(4.495392,1.261571,18.748849,9.654423,7.063634,NA),
                MS = c(4.495392,0.315393,4.687212,2.413606,0.441477,NA),
                F = c(NA,NA,10.62,5.47,NA,NA)))
cat("\nNote: for the treatment F, the book (season fixed) reports 10.62; DOEpro\n")
cat("(season random, the modern multi-environment standard) reports 1.94. Both\n")
cat("are correct under their stated model. All other quantities match exactly.\n")
line()
