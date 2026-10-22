#' DOEpro: Analysis of Designed Agricultural Experiments
#'
#' DOEpro analyses the designs used in field and horticultural research:
#' completely randomised, randomised complete block, Latin square, factorial
#' (CRD or RCBD, two to four factors), split-plot, strip-plot, and pooled
#' (combined) analysis over environments, including factorial treatments.
#'
#' For each design it fits the analysis of variance with the correct error term
#' for every comparison, screens the data against the assumptions of the
#' analysis, advises on a variance-stabilising transformation when one is
#' needed, and reports means with their standard errors, critical differences
#' and grouping letters. Several response variables may be analysed at once.
#'
#' The quickest way in is the interactive application:
#'
#' \preformatted{run_DOEpro()}
#'
#' To work directly with the functions, see \code{\link{analyze}} for a single
#' response, \code{\link{run_all}} for several responses at once, and
#' \code{\link{build_report}} to render the result as an HTML report.
#'
#' @keywords internal
#' @importFrom stats aov as.formula lm residuals fitted model.matrix setNames
#' @importFrom stats pf pt qt ptukey qtukey qf pchisq median sd var na.omit
#' @importFrom stats shapiro.test complete.cases terms coef df.residual anova
#' @importFrom stats aggregate ave qqnorm quantile bartlett.test approx
#' @importFrom utils combn head tail write.csv read.csv capture.output modifyList
#' @importFrom graphics par plot.new text mtext
#' @importFrom grDevices png dev.off
#' @importFrom rlang .data
#' @importFrom DT DTOutput renderDT datatable formatRound coerceValue
#' @import shiny
#' @import ggplot2
"_PACKAGE"

## Columns referred to inside ggplot2's aes(), which the code inspector cannot
## see because aes() evaluates them inside the data frame rather than here.
utils::globalVariables(c("Mean", ".se", ".let", "Letter", "fit", "res", "std",
                         "lambda", "loglik", "m", "v"))

## ------------------------------------------------------------------ credits --
APP_NAME    <- "DOEpro"
APP_VERSION <- "2.0.1"
AUTHORS <- list(
  list(name = "Dr. Immad A. Shah", role = "Scientist (Statistics)",
       aff  = "Division of Agricultural Statistics, SKUAST-Kashmir",
       email = "immad11w@skuastkashmir.ac.in",
       orcid = "0000-0003-2761-5112"),
  list(name = "Mr. Uzair Javid Khan", role = "UG Research Student, Statistics",
       aff  = "AAAMDC Bemina, Cluster University Srinagar",
       email = "uzairkhan11w@gmail.com",
       orcid = "0009-0001-6368-0977", maintainer = TRUE),
  list(name = "Dr. M. Iqbal Jeelani", role = "Scientist (Statistics)",
       aff  = "Division of Agricultural Statistics, SKUAST-Kashmir",
       email = NA_character_,
       orcid = "0000-0002-2974-2871"),
  ## the last two are listed with the developers, but not among the
  ## citation's authors
  list(name = "Dr. Imran Khan", role = "Chief Scientist",
       aff  = "Division of Agricultural Statistics, SKUAST-Kashmir",
       email = "imrankhan@skuastkashmir.ac.in",
       orcid = NA_character_),
  ## no email, ORCID or department was given for him, so none is shown
  list(name = "Dr. Pradeep Mishra", role = "Assistant Professor",
       aff  = "JNKVV, Madhya Pradesh",
       email = NA_character_,
       orcid = NA_character_))
APP_DOI     <- "10.5281/zenodo.21399570"   # concept DOI: always the latest release
APP_URL     <- "https://doepro.pages.dev"
## the platform's name, which every output carries; the R package keeps its
## CRAN name
APP_BRAND    <- "StatLabX"
CREDIT_SHORT <- "StatLabX \u00b7 DOEpro \u00b7 Shah, Khan & Jeelani"
CREDIT_LONG  <- paste0(
  "Generated with ", APP_BRAND, " (", APP_NAME, " v", APP_VERSION, ")",
  " \u2014 Shah, I. A., Khan, U. J. & Jeelani, M. I.")
## the citation's authors as running text
CITE_AUTHORS <- "Shah, I. A., Khan, U. J. and Jeelani, M. I."

## The StatLabX wordmark (480 px wide, 256 colours), embedded as a base64 data
## URI so the app and the report stay fully self-contained (no external image
## file to ship or serve). The website uses larger copies from site/assets.
LOGO_URI <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAeAAAACJCAMAAADDhhuBAAADAFBMVEUArOkAouQAKqoAMl4AMpgAH2YAVGgAau8AJGEAGjEA8tMAK5YAW6UAPMMAYOMAnN8AYakAwe8AjqgAe70AOTkAQn0AQX0AT+YA/wAAl+AAER8AcbEAHm0AO8AAT+EAOcIAEB8APcgAAAAAEioAADUAHEsAKWsADiQAOIsAIlYAEy0AFS8AAFQADCgAJVAAFS4A/v4AFTEADSQAAP8AAH4AVq4ARJIADCUAM3cAEisASKgADSQARJcAd9YAV84AVaoAFS8Afv4AacoADCUAVrMAO40ADCcAf38AR64AZ9AAESgAduwANG4AVVUAeekAVasACRsARo4AOKoAR48AGEkAOXcAddYAhesAKmkAHUUAZdEAJU0AO3kAGUwAZ+wARc0ATKYAFBQAZ+gAJk0AhvIASuoAOHIAVfQAY7QACh0AJ20AZrUALYkAO8kAGkUARokAWa0ApfAARYkAAKoAW80AdsoANosAOooApvMAhtEAlu4AxvcAhNgA1fkASJIAOHkAtvMApuwAedUAZLoAOYwAd9MAOI4AS8kACx4AmPEAd8QAdPAAN4kARYoAU5QACx0ApugASK0AJS8AVrAACx0Ads8Ahu4ANngACR0AKHQAhtgAVpYAbLUAeM8ApPEAVf8ACx0AGUYAVJoANXgAeOoAZs8Aac4AGk0A1/gAbMwAVqoAV8cAOrQAleIAmfQAh9AApOcA5e4AdbcAWK0ASqwAa80AGUsAh+wAi9YAl9IAarkAtvcAKm0AKmcAZfgAIlAAtvAASqwAZrQALYsAWMwA5fsAxvYAabQAhc8AtPQAOKgAN6oAOakAG28AV+QAlOgAltcAPY4AhugAWtEAicoAqfYALGwAWtEAeOkAKHAASHEAxfMAYb0AUpsAttoARHoAY5UARXcAldwAQX4AqqoAZ8MAqNMAKo4AVssARqwAK4UAcboAaugASrEAJVAAm+EAOqsAW+kAkdgAUskARHsAzMwAIU4ASdMAitoASM4ASsoA/6oARM0AwvYADlEAjOQdjtKsAAABAHRSTlOhJCIdmVYOJORnBs1vrHFRrm8RLwR7yNkB4dLNlcuTbpghAPcH8/Hv6/DWagMv1xEELtYBAqntbfGP0q/Tk9AFUgKWjs7VTwLmsa6tygOT5y2u8jAStKyN0tjMVwgsy/CyDbMQbvhP7pFxKerz+VWLk29wA7LTL7JP0I0ziysUbE2QbMxOL4/S13HqynJRKbOwDw1wTkutLYwQrBUPFzED62maknEQMY0QTk4yDaxV5coRESgmbK9RLAp1D0pr6zEwkUsuUCVKM2tmr3VUEskqSxQzkg8NsxRQiwoSqPQIKhNM0bYD4wptalNT8JJrj9TUu+XgiQVzqUhQDAOHmSLDmq9wrwAAGu5JREFUeNrtnQdc28jyx7n+3r1e/70tNsYNVzDGgA0GG0wJYEogkJBAclwuIaSR3kjv/dLbpV0uuZLrud577+1//b0rr/deZleSLcmyJdkiAaPfJ7ENtoWkr2Z2dnZ2lYFUpbUy1FOgAlalAlZ1ofW8VwWc1pp8sQo4jeVAlz2vAk5jOdFll6iA01r/ORkoq4DTV6MnI5MKOH2b4DNTvkQzVcDp2wT3Z29A7SrgdJUXPddwqdoGp6/a0aXjCo6rUXT6tsHWznEVt6OgCjg9FXTcNm71uC+QVwWcnjqDPv9N8eo/HPc6VcDp55xLwDEfmPXCr14oPg3htLfEoQJOD1mDJUEK5u9W/nQC1quPUT7aOdPkdDpUwGmh94+vfOtYLRFQnvXqY6e3qhacFq75zZU7T90x0NLSUlvbjbW2u6W2asILxeMqfvnMqs++XNG/af/+oAp4+Mp74MCJlSvvugvQ3jQXdFN3bS3wXb36mVXPfvFo/+I9i/eXqBY87OR0mkymmd4aZzSUOnfvggULFs29qxt89Kufn35TjaKHp0wa3mCR1xsMlpSQ3IbV+ji24GNXWLHzbm/3svirgIdT4NzVOHnXfffdtyIcJvFyCdVRAqyLlgLgF5EjppekAh7a0mio5+c7eo+cvLO1VavPAmVnZ/+yc/tpPExIgUcXgYs+9mkMXRXwcGBsdvlDhkwtSK/VZtGAGyrGFb96giZsRfd2d7e8Qlm0CnhYtLikwQ3afG5DJpGWEjHgrCkYcEFxcfEpZKVtuPtY7QHmBxXwsFCdy0/DZRHW6xnADRUAuJKG6kBv1dbeitThwmFgupRbjlhuAsDjiovHDiA62nprbClyqICHA12T2RjSUVDtm/2uQMBsa2zsajSZO46cbI246GwKcBFFtQSdGPtNdEgFPKSDKfxgCRDT1dlDbqPNElMm2X7ZV+ggKwKYjrKOj92pAh7qtmt2hbDdGtwB27ZoP4mhjHNYCHVkTZlCGTABfIzuC6OLKncKjvergIcKXZvLDo7Z4zJrmMyV0Ee70NtTiP0CYOA7Nto1Kl0pWLGjAh4SAroYrq2RHWkJqd35GYmwgO9vAHDtXxBTGA2dYqsKeEjKAnQNbpsl2hLH10y0gcJbsRr4rr+LMeAg+vy4GkUPSboBjwH8stSPO71zCF7Mt2197bsOK2PBX2xVAQ+9xjfg8nhclmjqSkwzocC9guZbPKHq7kiz60C3/xmpiY6hJcgx+wONcr4RRN4Cmm5xW2XVADv5vG+fChgNmZlE2DW7/AFL4oiKL4cXPUzjBb6VRVeyR4+sSAU8dNQRcAXMSP4MpO0VxQzeCc0XxYWqAr6gauzo7TXPlP+9meixcYQu4K2sal4pODqoAr7geHt7O7pQUtMXTjN4q5qrqnZK46sCPq/qmtzR0ZjcVw+hEwCXGC+oeUA4b6UCvqB4Ox6c3E4HWfLb3xOVgHdCZTNYb0vVgNWBVMBDrNPbdUlX0l8uwXwrK5urgG9ty7UDP0Yq4KEmTSNKge/OymbwzYC3Gez32L2SAmgRwBqVicwCSCLmpamGKBhECiyxgk41Y9PFKqqtAvu1KrZWpYVIQ16MH49f45f4CPAr/Bv8avx4fEHAowY/qpeGogoi685mjLaoCD9WDbwvg29cwBab0e+xr7HrDHYsXabBQL2y63R2WmvWMK8Mhuhv7Yd3jzzCdbuvdhFdDYIn45Hf379kyZIVGx958XdJBVVs93zujuZaCnBRUUvRgFUO3ziAzTE1X9Kl81hGnpVZPDrm8Nllcnp9dd/2l2VkI4Xs97UBYrlFBHHzTiSLryDgut2ZKchjQhd8IoBGc96diCsGsA4ET/o32lOIZxzo7iKYI7qwiNZOJLmDFBewS8fdUXmyd6GRKV/cU9LakSxhsNW7Fy5c2NLC8F2JgqkuCG4JZaYk8wW0X5fb7/fv3g0PLv8OpbdeozG1Q2vqtcYrrLokJ9450Y5ByJEU31tf6u6OWG9RqXy+sYA3p8bXdyG7V5ksN+lXeuN+HEtS2i38iY64tqFdkgxhK3p36doo38o7bpWYf04AuMuTGl/XBe2JDipg1pkJxbFhizsu4aNWuXDAUzw0dy3gLWX4vp8EXz7g/02Nrzvay7KZ6+rqzPBgrlMoUIWtwfbw/7q6bYKxP3tX/Eq7Eg8VOMUHjLtDP9fGI9x0L6qRx/d331qL297SUkK49JTM8FkQcCA1voZINi6gi+rflek2sTd52HKBACeyYKIxcW24aaUMwhAqvzt9PoZbSgEufRI5HCnfdUVjVyrAMrKxmxSKUnWRyF6HRAEbLwzgeIS1mdpq6V1YCKUeivItBff8GvKi1G+r44pJWhBR+atEItksCLAEt2RX5gQbWT03g3A6iRsNXBDAGvSgoJfGUwPzTyFJUbCjBJ2dO39hhG9p6am/JNP8xgCOMWBjnSaSRNdEXrCE2L9ku00/O/OhzNQs9jVjsIgCDgxikOVGieahTG6NRzirxypO2PEpQgfXEro9PfixEtxzUr2sGMABXnLDlkKfQnnARtFsypAADITb79RmCs7u1WbligbTUMp+/VzaegngyntuTdY98wH7efabgpMbBMBuVnbNLtisB4YEYCD8zs/1gnz12qxvP5R47AEa6YsgeO55r/S93J6enr6eykesKKjQre14/XSLMmdDmR6pBl8zusTN+nkDHEIidc/WJaMF+JIVNgoPJgi1YEWk9/+4sLm0p7QvF6svt++25HpHQoAt3BEkeyqAQ0oD5l4znoT5fhIemgcRsASftLFaG8uXzM8vfAnFc9NA8mBLc+V7ubk03tzt55KOrgQsmBdBp3KK7Mpnt8QAc1tpXd35d9EathGH5+hj+FILbOQ9JdwQW4PQ+rY0l+ZG+PY9glAQDRbgzM0aWQuHRGMdSDbZWX1Wv5mjOmaKs41OTBHZiOCZ/rGOe3nB7+2sENBjYz5IfRVny5IDrMH7YGP2gNkc3i94Hs/msDn690PU3+UelJd3TsJN+ijcKF+YvZ1X+q4AYTDf66cvbK7MjWj7fuR0IAUB80cIPT6bmJseL8DYjhtLFmDWa6pvTW10m0GXYEgS9613B5g/vwNvhLeVSOIQPx8ezwOcKfHSNMb581RvzOC/uk4AMG9fqP0whIxmJ3seQnhOljYWcD4Qrqg8iBxW/sCv9WBLUel7Ub43oFTdMw/wNoPAcZLEYPwMBwytePwBW+yYTiJ41FWhkTLi7LaxhqgzE24T4mzW+zqN1PRJ4vIU3eHNdZKH2XT2r0a3XIMub8qP0mWaYAw4r6JsLtf54uUmpxeVssy379YUOr9xLNgjcHwSR/mNUVM3i36H+qxFwgmDfy7BFFtMFtxCdaSiX7XI713Hs2SXZMBwJgKsdvido/l6poOkpy2YIfwtVu8HrNn6EGQmo3hzH1HEfHmAXZnJy2B0SAVM93Fs0iq8dH5eFksaYLtFcudLXLuFL39h+bexRpeWMIRZgCnCo2bdzuQvwHytS1vY5tvzvRQ7R8KAzSmNNHjMAvmkODCkAyYJFwmAa6A9ZcOyf19aG+yWdJVhG/ZILWLyaFgFzUuqsxi8FN8I4YrOcMRBL1pbVBnl23eDVRHvHDvY4E6J8Jo6aUOOhu+L1DDxT69N3M7sfBszJNFhTyAbJ8gSI3w5y01Pqs7S08piBITzCgsKOk8EiaGexeY7C8h2duL/EDyn3DmKN1yoS4kw5RbFAdPDf9JP2CAClmiXUCoqHTC7n1yDdjGEs9iEywvrc0tPnbOWIOuCtbW0e8aAFQqeB2fAn+Ssdhzmd4yi3QryTCf6AofXrMGl80xBfVT80E6n8fMDaB3vNcl8sHvKUgFvPgxrGFE7gcv4DZGh0Zisj4xiJp2PTbi9KR/TZZlwfnV5eWffHSXYEVuXdtey3PPXFQqe45Xs+FKyYV2A6hpb/DqOXVPzW6hEgo3pW2voCTBE46PPeBpMnZGzH1fDBzlDXT4EQZWFTKFBFpySwNs0sdNnIYnHT83MwfNwyCycRmboU2OxsXtdh3ezk3M4z0IOhlQlgXxGridgX15AeFV2FsdPV5fXP/yz0ziKOrtgOuOe8f++J5X0zoJVlZyEkWyFnLGNeXKDSTYdz+VxgnPBgUxNMoBRwiKhqAvyIEPiVKV5jS7OUAeckxnZWcSGiZeuxnwf24+DK8BbS7e+nYB4+2sKpK7EymZNrjUpILbJGy00aeKIGziHqGSWCGBLsoDj7oOHbZP2RIA1vDwR56AdQbSxPNL+jq6e0/kzfK/f65dOr6WsFwDX1w+G+QrPbNAE8FLjuiTLZjX8cGdzcjtm44VQbMDCJVmc0TA3UnA6CgHMuu7dok6Hl0gzof7yCN767Sew9b7SUlXJxM6dnYX1EFwpbr5xJ59Zt5l3GI1uEYUMAgGvnOFgiy3gujoyI49MzoOJeUYjvAi4WHFOiBf/xQGcZFGlxeZj7wK8xj+6Ajt8bskWHHPUZv4KDOEmwrd8zsOn74VfrCydBXTLygoKCgoL6+vrle0bKTXD3xKwC2ep7BIAa3Z4pE5h5APOFC+qdEsEXOcycFLvdFQe47/sGpE2mJtv0+2ILfNoysf2+8U5+OnA57PKCFyMFzRt0yDhTQyYnqIXmaondMoM/PMQA9govHGfjBLdED9bKQxYJ9uCbZL7PhzAIfFWxRVb5tG+pLx6w+0Y7/bcgoIoXeKdUYqziAdpjY6YZD0NOFMMsNefKWvCBK+ocrwoYJfUNlZirOFxiLvoOl0ivwXt66R+eD7+XCcmS9HFjzNwSvqDSYPSAqe+CAsvf21wxgD2CXzLEcpMCbBVdFeklEUb5WTa2V4pjgWzrzBXnDUqUbgeQy0HUYCn4Yz0n+8fnXX/4HjpjBSX1bTFtpa8cGeHSNFlEoBD4jV3EgC7xAcK5QG2JQYMa4mi21dlZ2O2NN3CejLgMKlp9OisrKPXK5qjVMhF+4SysGYRCzZnygfsVxqwTdYueCRE0b6EgKHoZfGqcowXGy9pgnNvwK4ofHQ08IUsSNO5VAqgBwkwbwDqCDmvYoDlzlE1SgDMC8PEi7pDiQw25pduCYBdrHU6zLF4vRtovOUY7uriWTe8Bm/sXwHmS2cxC++WNwNRPmCnhDsURwU3BunI1EaLQ1u1dIopkHh+BMd2DAFW7Vykjs5s9vBoeUSzGPIAW9hIQwGcVbbRMtMZZrNHHmBP3KJyiJ9eX1Genc3ghTWfZ91xAN64oh/MNyeSpi7ciYLWoWTBD8L9TmHeHIxn52BpDTWxgM0Jg5tQvKmHbp6/FS+LdssAjNsWnWhX3c2OikXbYFu8PXSC3ayYg/FmZ2PjLZ41YcLA3fid2ze8wZgvIZyfd4/SoRZ3PNgmUR22jo4HHxzzc60eX305jPRHsCsy8VvDRCGWTYrvD/AArxHvsXnELNjPuQY1Yl6cA1gn9Oc5mVLWQTvglIQBbzaFtwzwVrVgvFaCF59AuuIDTDg/r8eqbKiVgeJObRCRno02J2f06Ca9vpFqbjiA7c4ErkwXd72jUFzAOvILU+I2WGTqiknSToTYxAy87BxnWIK/eE3EQzvAp4VX4XtZZedR1lvVsvbglThZOaOJwRstq83Pz/v2u4qGWhkyy1eiFWQcutXVTU3V+t/T9dHcKNndmKCt8kmZyGDjV8cJzLow8QJ6XUD6RImA+EVm5Obs3LzpjRajQSi5g/G+TOHNhsAZWt4J105fAK3sy/0/mzM6h4U3Ajg/7z8uUpJwhsz6s1jbrcZwmyBfrs/ZI5T2xxWXELBQQ/tdXRbuudXBvdywYJjf0tiIL3z8ZOEahDkmZabz+2xmi9mMt9nIrKbJ71z5bI2mRrI5MqbfiF8xZQLcSD5ga6Q+QH24i/xrfN4QFzBs3RiVO8QNwu3joz0jCJ2x9eZh5wy+eRnGW+LYc7QcWy+3Lp4GnLf8JeS0Kg3YJC2vg0Pl1hye7dbXX9fX05Ov3xWJwkMCBfRR78ZLc+jI0kSRNYpoZfIs2CZWLaQR8kAGQel2x6axyIKbvG9kJgCcSAEG7xkKL9RQAt7iqvlLF1npetgP38bRlTDg/7/mKeXqsjIYvjYpzhl2iU03BxvvJ/XX9fR03/TDrPuiE1gCCeu2AhIyRzFlBA6PKGDJdZpxdyIzwfpfBnlrScG5aF9SnU/jBe8MznkRNUOUqOtkFgcwNfEBSmoB8KifLPyxUm6aseAuQ6ZBtOnl2C7lnMF6e+4ZuOuhT/InsSYoNRoSVRxuS6JOxCSCj3SLQjLKHmUuOCMDsJEUYqGtgJdUQANfuJfGtcuegGpnBi/4upqTfMD6COBr1jV/F72uJOCuO7U8txghS+i26nN4InEVxjvw1B/vfbGQwzdR0bNbfL5InEIgtyhgjUGyBcvdCZ/kEMVIhlm3TqLwUt55LOA9y52sAK3ZSa2QCefl5Y265pp1s7+njA1nMAFWq0GAcCsIYPIs9yvVTGB1Xd89T920wPpkYf2HvEmkRmVMjRI1GaQrJAYY2QzSk9smjzzA0hoAgw/B3rbD6BA9CQnwtlGRsyMmcDoZ0wZHAP9k3bydAt9IDjC+mpmoIhpfgNm20oD5jpkOmwHvwEs3/fHFR+o3opgkqjFhAyXr5DJr6iRaJ5XOXJkNkpPb8pZdtUm7Kkn3aQ/gzSfOluD9x7JFJHIWOPu/j51dir82ahQAvvnmp5TIeRALtmlzMFNuqEnBbRV0zYB32nXX9UHs3HPP1x8Je4UGq20hXbwWCq5mYxKAwfMbxBZB9RoNUgHL2gkArAmJTsDrgK3uejuHmn8EfMvaZkNohfFahdMtR+ippZG5aUwjDID/7dcLFSCcQZKocdfu0uoFlF9NBLVi1339httuRSjOGEid0LrxTJLYYrTLdNFYl5OKT0FkTBPR6PMbpAHGAxQeyYA16BL2xcM/ZQZ3L74n0uS3CSVivqPaZt/4NI6cS+LXavSyzjEb8DXrbv71L26Z/uOUCWPAPuNXv2rsNfaOAfVCv72XaMyYI2PYIm+PuWzMJEoffbTrg31eZuG9eGq0+Xx0NgDqJX3wAyv5bNnhIxWU+B4HLpaMRuo7Pix47XOyF4tor4tukxFrs+QjXTb+J7iyRT9rid2akCyRw6HVSz3Ru0ndrc5x3xsYLrHeURjv9McT44U96NVypqYRwNhHE8C3ZNyt5CIsySj528YktwiXhHUvTabB3glhtd/3Rj4RwfujeVuesJKid5Ed6OBMLmWujmvWrbv541tu2XLjRZGecwqANSY5qqlhbgkkoUzMaYom5PGf0cRMKhC4xQL+K4j+axqTRgAhZwpC7GZjPsL9vEZkcgOKbpjZDnvX2MLvtXeZSGSVz1hvxah183C3V8rgboQwDzCY8LyPb9mSseyl1OaCq3c+S1Ul2GFMOhqhC52c2fOWxY+sBAi3wiKHWWzAFOF5mPDflj2VUq2WCjglOfCp3z9jDjVglEfw3rzlO6TptUpudTpao7P/qeTXKCA8e/a8j3+xJeNvWxYeSCHnoQJOxXgBr/W2hwsxWIou9s1LBZMaCQk/36rnAoYtlS0HwvNu3LIlYwvcDNqrAj7vwgxfP5VbUZFHVFEBRpfxncexbw7KjRwtd2pZC3iQLnRZ2fI2TPhGaIebn0Rehwr4vON98Vhl8agKIFxBXOq8DGy80n0zOzG9505tdJUlbMIwfly2HIx4794b5y9b1rYdOZwq4POIF+xp0dqqscW/GYXhkl7R0wvIaGCSluY4qY0CBsKFhdiGl7c1N+/du3f+/La+JIeIVcCy2VL90rNL5wPfYgK3bfZe0itKxnijU5dO4uFC2kdjwtiGy5bDXaGbsRW3PXw8qYZYBSyTLv0Eiy9UTSgGtf0I0s2Uaw6mOA3oiDbSCOMaLoowWDEgbt47v61zK2pXAQ9ql4j4yLNW66Innp4PfMe2tbWt/8eypY+TNxUY2sOEGb7ZZPZSGRGFeHlBGJY4VQEPkul6iX+8/qGlTz89fX5LbRXwXQ8VsEsfT9E1c1IevdooYNIO4zlMQPi9SlBzWcMKUkSvAla8v0vgXnnwprXzr21pqQJNmAB0YRhfObpM2pKsKM0yYtpRY09dWZy9Csk1YhWwmGP2HsJPr79VW7UeVEWrhRRp4FyHknOJNHgyEMuIywtZjrqsrLisYtVimaGWCjiRgoeIvbxyrGr9A6CxRFVV3XMXWellgBWWBjW2fiViw+U04IKCAgZydnm/asHKyOklcF+74pu/uuqBqyiNHbv+2vlzr7eyAmqFZYKURxZeFy+LrXwmti4oaMjeoAJO3S/PpDKDp//0h3+5auJEGu8D0O5StlsyOHTpG03fyS7E46KeMgUov+1UAacEt52cwDf/9NfV4yZSItZb231wkBwzf452R8ff/94BmhzVrv5HH3101+TJ/bv6w5NNKuDkLZfAvfjiZ78xderEiRG+LLrW4XVIKuCI5bRT4emZi599pmHquHHwjxjwVWMnHLuLzDJwlpwnuE4RqYDlnlAT5ZXR1hXPPvPLhgagOxUDnjjxhV/99K1XzirfH1It+PzFypRTRnse/fLSr02Z0tDQMJUI7Hf1fz92BeWXvdZhe4AjGLCzpp2KVqx7vvzs0tEELsN33B/++tib/0rgHiqxDuejHJGArUFvDWW3jpdXbIDFq3DngwY8dep/feN//nScvOs9VOIY7seaMdKstoaZhfFOGNjOiSZ+sxsavlb+zLPP/V8JZbiHgmlxxCMFsCNYU8N0H899uGnFtDnlVBUkTXfK1y797NEzkXbZkTYHnu6AHU4gG2R4tS8OT5rRVFhIlcnRcMvnrHru4jORvpLTkVYnID0BOxyOILe/uC88acm0afURtoQvoF3R30+PzpjaZzrS8FRkpBdWoMqhpGkPhycB2vp6WME3mwZbWF49Z9qMjWEv0xyDS3agNFVaWvBW777Fmz76aMYPfvtbIPvDajzqhleJhInN0zZsDIdf3hPtBdc405ZtmgHev3jxvnB/eOPGjTNmTAPV13/SBHcYw09Hj/52xsYV4X0ffjfyaZPXG0xvssMXsNWLB/OcZxZ7QfsW37ZpUzgcxlhnUGSn4acfLFkyA09j3vXBB5e/w+kneWtGBtnhC/jTNy/e6t26deu+24AtwCWAQf3wf9Ou/sVeMGXvp9zv4DmvQedIAjsCukkQcZlGKtZ0A+xgBEG0wzGyiaq56JGlfwKSj6R8leUWQgAAAABJRU5ErkJggg=="

## ----------------------------------------------------------------- helpers --
#' The designs DOEpro supports
#'
#' A named character vector: the names are the labels shown in the application
#' and the values are the design codes passed to \code{\link{analyze}}.
#'
#' @format A named character vector of length 11.
#'
#' @return A named character vector of length 11. Each element is the design
#'   code passed to the \code{design} argument of \code{\link{analyze}} or
#'   \code{\link{run_all}}; the corresponding name is the label shown in the
#'   application.
#'
#' @examples
#' DESIGNS
#' DESIGNS[["Split Plot Design"]]
#'
#' @export
DESIGNS <- c(
  "Completely Randomised Design (CRD)"            = "CRD",
  "Randomised Complete Block Design (RCBD)"       = "RCBD",
  "Latin Square Design (LSD)"                     = "LSD",
  "Factorial CRD (2-4 factors)"                   = "FCRD",
  "Factorial RCBD (2-4 factors)"                  = "FRCBD",
  "Split Plot Design"                             = "SPLIT",
  "Strip Plot (Split Block) Design"               = "STRIP",
  "Pooled analysis over environments - RCBD base" = "POOLRCBD",
  "Pooled analysis over environments - CRD base"  = "POOLCRD",
  "Pooled factorial over environments - RCBD base (2-4 factors)" = "POOLFRCBD",
  "Pooled factorial over environments - CRD base (2-4 factors)"  = "POOLFCRD"
)

nz <- function(x) !is.null(x) && length(x) > 0 && all(nzchar(as.character(x)))

fmt <- function(x, d = 3) {
  ifelse(is.na(x), "-", formatC(as.numeric(x), digits = d, format = "f"))
}

## An effect's SE(m), SE(d) or C.D. for display (`what` is "sem", "sed" or "cd"):
## the single value when one applies to every mean and pair, otherwise its
## range, so a figure that fits only some of the means is never printed as if
## it fitted them all.
err_text <- function(e, what, digits = 2, html = TRUE) {
  v <- e[[what]]
  ## a small-valued response at two decimals would print SE(d) 0.00 and
  ## C.D. 0.00; decimals are added until the figure shows
  if (length(v) == 1L && !is.na(v)) {
    dg <- digits
    while (dg < digits + 4 && v != 0 && as.numeric(fmt(v, dg)) == 0) dg <- dg + 1
    return(fmt(v, dg))
  }
  rg <- e[[c(sem = "se_range", sed = "sed_range", cd = "cd_range")[[what]]]]
  if (length(rg) != 2L || anyNA(rg)) return("-")
  ## a range whose ends round to the same figure reads as nonsense, so show
  ## enough decimals to tell them apart
  dg <- digits
  while (dg < digits + 3 && (fmt(rg[1], dg) == fmt(rg[2], dg) || as.numeric(fmt(rg[1], dg)) == 0)) dg <- dg + 1
  paste0(fmt(rg[1], dg), if (html) "&ndash;" else " to ", fmt(rg[2], dg))
}

## a p-value for reading: never in scientific notation, never "= <0.0001".
## Given alpha, a p below one of the thresholds the marks use (alpha,
## alpha / 5) gains figures until it no longer prints at or above it:
## 0.049973 prints as 0.04997, not 0.05, beside its star.
p_text <- function(p, alpha = NULL) {
  s <- ifelse(is.na(p), "-", ifelse(p < 1e-4, "< 0.0001",
              trimws(formatC(signif(p, 3), format = "fg", digits = 3))))
  for (cut in alpha / c(5, 1)) {
    for (i in which(!is.na(p) & p >= 1e-4 & p < cut)) {
      d <- 3
      while (as.numeric(s[i]) >= cut && d < 12) {
        d <- d + 1
        s[i] <- trimws(formatC(signif(p[i], d), format = "fg", digits = d))
      }
    }
  }
  s
}
p_show <- function(p, alpha) p_text(p, alpha)
pval <- function(p, alpha = NULL) gsub("<", "&lt;", p_text(p, alpha), fixed = TRUE)

## "p = 0.0373" or "p &lt; 0.0001", for running text
p_eq <- function(p, alpha = NULL) ifelse(!is.na(p) & p < 1e-4, "p &lt; 0.0001", paste("p =", p_text(p, alpha)))

## Significance marks at the level the user chose: one star at alpha, two at
## alpha / 5. At alpha = 0.05 that is the familiar 5% and 1% pair; at any other
## level the marks follow the choice instead of staying at 5% and 1%.
star <- function(p, alpha) {
  ifelse(is.na(p), "", ifelse(p < alpha / 5, "**", ifelse(p < alpha, "*", "NS")))
}

## a probability as it is written in labels: 0.05, 0.01, 0.002
p_lab <- function(a) format(a, digits = 6, drop0trailing = TRUE, trim = TRUE, scientific = FALSE)

## a probability as a percentage, for "the 5% level"
pct <- function(a) format(100 * a, digits = 6, drop0trailing = TRUE, trim = TRUE, scientific = FALSE)

## the key printed under an ANOVA table, so the stars mean what they say
star_key <- function(alpha)
  sprintf("** significant at p &lt; %s; * significant at p &lt; %s; NS not significant at the %s%% level.",
          p_lab(alpha / 5), p_lab(alpha), pct(alpha))

## Letters from critical differences. `cd` is one number that applies to every
## pair, or a square matrix holding one critical difference per pair (unequal
## replication). Any missing value means no letters can be given.
cld_lsd <- function(mu, cd) {
  n <- length(mu)
  if (n < 2) return(rep("a", n))
  if (!length(cd) || anyNA(cd) || any(!is.finite(cd))) return(rep("", n))
  crit <- if (is.matrix(cd)) cd else matrix(cd, n, n)
  sig <- abs(outer(mu, mu, "-")) > crit
  diag(sig) <- FALSE
  off <- crit[row(crit) != col(crit)]
  if (diff(range(off)) <= 1e-12 * max(abs(off))) cld_sweep(mu, sig) else cld_from_sig(mu, sig)
}

## Letters when one critical difference applies to every pair: a single sweep
## down the sorted means. Exact in that case and instant for hundreds of means,
## where the general algorithm below is slow; both give identical letters.
cld_sweep <- function(mu, sig) {
  n <- length(mu)
  ord <- order(mu, decreasing = TRUE)
  groups <- list(); done <- integer(0)
  for (i in ord) {
    joined <- FALSE
    for (k in seq_along(groups)) {
      if (all(!sig[i, groups[[k]]])) {
        groups[[k]] <- c(groups[[k]], i); joined <- TRUE
      }
    }
    if (!joined) groups[[length(groups) + 1L]] <- c(done[!sig[i, done]], i)
    done <- c(done, i)
  }
  keep <- rep(TRUE, length(groups))
  for (i in seq_along(groups)) for (j in seq_along(groups)) {
    if (i != j && keep[i] && keep[j] && all(groups[[i]] %in% groups[[j]]) &&
        length(groups[[i]]) < length(groups[[j]])) keep[i] <- FALSE
  }
  groups <- groups[keep]
  groups <- groups[!duplicated(vapply(groups, function(g)
    paste(sort(g), collapse = "-"), character(1)))]
  lets <- letter_seq(length(groups))
  out <- character(n)
  for (k in seq_along(groups)) out[groups[[k]]] <- paste0(out[groups[[k]]], lets[k])
  out
}

## Compact letter display from a symmetric logical matrix `sig` (TRUE where two
## means differ significantly), by the insert and absorb steps of Piepho's
## (2004, Journal of Computational and Graphical Statistics 13, 456-466)
## algorithm. Two means share a letter exactly when they do not differ
## significantly, whatever produced `sig`. The sweep above only guarantees that
## when one critical difference applies to every pair, which unequal
## replication and the step-down range tests both break.
cld_from_sig <- function(mu, sig) {
  n <- length(mu)
  if (n < 2) return(rep("a", n))
  G <- matrix(TRUE, n, 1L)                      # one column per letter
  pairs <- which(upper.tri(sig) & sig, arr.ind = TRUE)
  for (r in seq_len(nrow(pairs))) {
    i <- pairs[r, 1]; j <- pairs[r, 2]
    both <- which(G[i, ] & G[j, ])
    if (!length(both)) next
    ## insert: split every letter holding both members of a significant pair
    ## into one copy without i and one without j
    add <- G[, both, drop = FALSE]
    add[j, ] <- FALSE
    G[i, both] <- FALSE
    G <- absorb_letters(cbind(G, add))
  }
  ## letter "a" goes to the group holding the highest mean, and so on down
  rk <- integer(n); rk[order(mu, decreasing = TRUE)] <- seq_len(n)
  key <- apply(G, 2, function(g) paste(sprintf("%06d", sort(rk[g])), collapse = "-"))
  G <- G[, order(key), drop = FALSE]
  lets <- letter_seq(ncol(G))
  vapply(seq_len(n), function(i) paste(lets[G[i, ]], collapse = ""), character(1))
}

## absorb: drop any letter whose members all carry another letter too, and any
## repeat of an identical letter. sub[a, b] is TRUE when letter a lies inside
## letter b; done as one matrix product because this runs after every insert.
absorb_letters <- function(G) {
  if (ncol(G) < 2L) return(G)
  sub <- crossprod(G, !G) == 0
  diag(sub) <- FALSE
  same <- sub & t(sub)
  drop <- rowSums(sub & !t(sub)) > 0 | rowSums(same & row(same) > col(same)) > 0
  G[, !drop, drop = FALSE]
}

## a, b, ..., z, then aa, bb, ..., then aaa, ...
letter_seq <- function(k)
  unlist(lapply(seq_len(ceiling(k / 26)), function(r) strrep(letters, r)))[seq_len(k)]

## read data pasted straight out of Excel
read_pasted <- function(txt, sep = "\t", header = TRUE) {
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  if (!nzchar(trimws(txt))) return(NULL)
  s <- switch(sep, "tab" = "\t", "comma" = ",", "semicolon" = ";",
              "space" = "", sep)
  ## only empty cells and NA count as missing here: markers such as "-" or
  ## "n/a" are left as typed so the data check can show them and say what it
  ## will do with them, rather than R changing them silently
  ## Everything is read as text first and converted only where no information
  ## is lost (convert_lossless), so codes such as 1.1 and 1.10 stay distinct.
  rd <- function(q) utils::read.table(text = txt, header = header, sep = s,
                         stringsAsFactors = FALSE, check.names = FALSE,
                         na.strings = c("NA", ""), fill = TRUE, colClasses = "character",
                         strip.white = TRUE, quote = q, comment.char = "")
  ## quotes are honoured only when they are real quoting, not inch marks
  ## (12" pot), which would swallow lines (see quote_char)
  q <- quote_char(strsplit(txt, "\n", fixed = TRUE)[[1]])
  ## A line with more values than the header (or than most lines) would push
  ## its values one column to the right: comma-separated text with decimal
  ## commas (5,6) splits each such number in two. Such text is refused, as an
  ## uploaded file is; a line with fewer values is only short of its last cells.
  n <- tryCatch(utils::count.fields(textConnection(txt), sep = s, quote = q, comment.char = "",
                                    blank.lines.skip = TRUE), error = function(e) integer(0))
  n <- n[!is.na(n)]
  body <- if (header) n[-1] else n
  base <- if (header && length(n)) n[1] else as.integer(names(which.max(table(body))))
  rownamed <- header && length(body) > 0 && all(body == base + 1)
  if (length(body) && !rownamed && any(body > base))
    stop(paste0("Some lines have more values than ", if (header) "the header has names" else "most lines",
      ". This usually means numbers written with decimal commas in comma-separated text, which splits ",
      "each of them in two. Choose 'Semicolon' or 'Tab' as the column separator, or copy the cells ",
      "straight from Excel, which separates them with tabs."), call. = FALSE)
  d <- rd(q)
  ## a header one name short on every line (row names, as R writes them) makes
  ## read.table use the first column as row names; keep it as a column, even
  ## when it holds 1, 2, 3, so row numbers in messages stay the visible ones
  if (rownamed) {
    d <- cbind(Label = rownames(d), d, stringsAsFactors = FALSE)
    rownames(d) <- NULL
  }
  d[] <- lapply(d, convert_lossless)
  names(d) <- make.names(names(d), unique = TRUE)
  d
}

## data.frame -> plain HTML table (used by the integrated-means view & report)
df_html <- function(df, caption = NULL, foot = NULL, digits = 3) {
  num <- vapply(df, is.numeric, logical(1))
  int <- names(df) %in% c("n", "N", "Df") | vapply(df, is.integer, logical(1))
  df[num & !int] <- lapply(df[num & !int], function(z) fmt(z, digits))
  df[num & int]  <- lapply(df[num & int],
    function(z) ifelse(is.na(z), "-", as.character(as.integer(round(z)))))
  df[] <- lapply(df, function(z) gsub("<", "&lt;", as.character(z), fixed = TRUE))
  hdr <- paste0("<th>", names(df), "</th>", collapse = "")
  body <- paste0(apply(df, 1, function(r)
    paste0("<tr><td>", paste(r, collapse = "</td><td>"), "</td></tr>")),
    collapse = "")
  paste0(
    "<table class='doe'>",
    if (!is.null(caption)) paste0("<caption>", caption, "</caption>") else "",
    "<thead><tr>", hdr, "</tr></thead><tbody>", body, "</tbody>",
    if (!is.null(foot)) paste0("<tfoot>", foot, "</tfoot>") else "",
    "</table>")
}

anova_display <- function(an, alpha) {
  data.frame(Source = an$Source, Df = an$Df,
             `Sum of squares` = an$SS, `Mean square` = an$MS,
             `F value` = an$F,
             `p value` = p_text(an$p, alpha),
             Signif = star(an$p, alpha), check.names = FALSE)
}

## ------------------------------------------------------------- demo datasets
## Deterministic stand-in for stats::demo_noise(), used only to build the example
## datasets returned by demo_data(). A stored vector of standardised deviates is
## recycled instead of drawing random numbers, so the examples are identical on
## every machine and the user's random number stream is left untouched. The
## vector has prime length, so recycling cannot line up with the block, plot or
## treatment structure of any of the example designs.
demo_noise <- function(n, mean = 0, sd = 1) {
  z <- c(
    1.148, 0.655, 0.101, 0.001, 1.317, 1.446, 0.718,
    0.083, -0.971, -0.872, -1.400, 0.655, 1.004, 0.486,
    -2.939, 0.063, -0.688, 0.641, -1.337, -1.727, 1.300,
    0.508, 1.206, 0.001, 0.586, -1.277, 0.092, 0.863,
    -1.769, 0.100, -1.178, -0.107, -0.280, 1.361, -0.274,
    0.579, 0.529, -0.265, -0.343, 0.616, -0.633)
  mean + sd * rep_len(z, n)
}

#' Example datasets for each design
#'
#' Generates a small, balanced example dataset for a design, so you can try
#' DOEpro without your own data. These are the datasets behind the
#' \strong{Load example} button in the application.
#'
#' @param which The example to build: \code{"CRD"}, \code{"RCBD"}, \code{"LSD"},
#'   \code{"FCRD"} (a factorial in a CRD, with two responses), \code{"FRCBD"}
#'   (factorial in blocks), \code{"SPLIT"}, \code{"STRIP"},
#'   \code{"POOLRCBD"} or \code{"POOLCRD"} (a single treatment factor over
#'   environments), \code{"POOLFACT"} or \code{"POOLFACTC"} (factorial
#'   treatments over environments, RCBD and CRD base respectively).
#'
#' @return A data frame in long format, ready to pass to \code{\link{analyze}}.
#'   The values are synthetic: built from fixed effects and a stored set of
#'   deviates for demonstration, not observed in any experiment.
#'
#' @examples
#' head(demo_data("RCBD"))
#' str(demo_data("SPLIT"))
#'
#' @export
demo_data <- function(which) {
  switch(which,
    "CRD" = {
      d <- expand.grid(Rep = 1:4, Treatment = paste0("T", 1:5))
      d$Yield <- round(demo_noise(nrow(d), rep(c(28, 31, 35, 30, 40), each = 4), 2.2), 2)
      d[, c("Treatment", "Rep", "Yield")]
    },
    "RCBD" = {
      d <- expand.grid(Block = paste0("B", 1:4), Variety = paste0("V", 1:6))
      d$Yield <- round(demo_noise(nrow(d), rep(c(42, 47, 39, 55, 50, 44), each = 4) +
                               rep(c(-1.5, 0, 1, 0.5), 6), 2.0), 2)
      d[, c("Block", "Variety", "Yield")]
    },
    "LSD" = {
      d <- expand.grid(Row = paste0("R", 1:4), Column = paste0("C", 1:4))
      d$Treatment <- c("A","B","C","D","B","C","D","A",
                       "C","D","A","B","D","A","B","C")
      d$Yield <- round(demo_noise(16, c(20, 24, 27, 22)[
        match(d$Treatment, c("A","B","C","D"))], 1.4), 2)
      d[, c("Row", "Column", "Treatment", "Yield")]
    },
    "FCRD" = {                                   # 3 x 3 factorial in pots, completely randomised
      d <- expand.grid(Rep = 1:4,
                       Variety  = c("V1", "V2", "V3"),
                       Nitrogen = c("N0", "N40", "N80"))
      vi <- as.integer(d$Variety); ni <- as.integer(d$Nitrogen)
      ## yield (g per pot): both factors matter, and V3 gains most from N80
      mu_y <- 18 + c(0, 3, 5)[vi] + c(0, 4, 6)[ni] +
              ifelse(vi == 3 & ni == 3, 3, 0) + ifelse(vi == 1 & ni == 3, -1.5, 0)
      ## plant height (cm): nitrogen raises it in every variety alike
      mu_h <- 62 + c(0, 5, -3)[vi] + c(0, 6, 10)[ni]
      d$Yield <- round(demo_noise(nrow(d), mu_y, 1.2), 2)
      d$Plant_height <- round(mu_h + demo_noise(nrow(d) + 13, 0, 2.4)[-(1:13)], 1)
      d[, c("Variety", "Nitrogen", "Rep", "Yield", "Plant_height")]
    },
    "FRCBD" = {
      d <- expand.grid(Block = paste0("B", 1:3),
                       Nitrogen = c("N0", "N60", "N120"),
                       Variety  = c("V1", "V2"))
      mu <- 30 + c(0, 6, 9)[as.integer(d$Nitrogen)] +
            c(0, 4)[as.integer(d$Variety)] +
            ifelse(d$Variety == "V2" & d$Nitrogen == "N120", 5, 0)
      d$Yield <- round(demo_noise(nrow(d), mu, 1.8), 2)
      d[, c("Block", "Nitrogen", "Variety", "Yield")]
    },
    "SPLIT" = {
      d <- expand.grid(Rep = paste0("R", 1:4),
                       Irrigation = c("I1", "I2", "I3"),
                       Variety    = c("V1", "V2", "V3", "V4"))
      mu <- 40 + c(0, 5, 8)[as.integer(d$Irrigation)] +
            c(0, 3, 6, 2)[as.integer(d$Variety)] +
            ifelse(d$Irrigation == "I3" & d$Variety == "V3", 4, 0)
      wp <- demo_noise(12, 0, 2.0)[as.integer(interaction(d$Rep, d$Irrigation))]
      d$Yield <- round(mu + wp + demo_noise(nrow(d), 0, 1.3), 2)
      d[, c("Rep", "Irrigation", "Variety", "Yield")]
    },
    "STRIP" = {
      d <- expand.grid(Rep = paste0("R", 1:4),
                       Tillage = c("Conventional", "Minimum", "Zero"),
                       Mulch   = c("M0", "M1", "M2", "M3"))
      mu <- 25 + c(0, 2, -3)[as.integer(d$Tillage)] +
            c(0, 3, 5, 6)[as.integer(d$Mulch)] +
            ifelse(d$Tillage == "Zero" & d$Mulch %in% c("M2", "M3"), 2.5, 0)
      ea <- demo_noise(12, 0, 1.0)[as.integer(interaction(d$Rep, d$Tillage))]
      eb <- demo_noise(16, 0, 0.8)[as.integer(interaction(d$Rep, d$Mulch))]
      d$Yield <- round(mu + ea + eb + demo_noise(nrow(d), 0, 0.9), 2)
      d[, c("Rep", "Tillage", "Mulch", "Yield")]
    },
    "POOLRCBD" = {
      d <- expand.grid(Rep = paste0("R", 1:3),
                       Variety = c("V1", "V2", "V3", "V4"),
                       Location = c("Shalimar", "Wadura", "Zampathan"))
      loc  <- c(0, 4, -3)[as.integer(d$Location)]        # site effect
      var  <- c(0, 3, 6, 4)[as.integer(d$Variety)]       # variety effect
      vxl  <- ifelse(d$Location == "Zampathan" & d$Variety %in% c("V3", "V4"), 3.5, 0) +
              ifelse(d$Location == "Wadura" & d$Variety == "V2", 2.0, 0)  # V x L
      repe <- demo_noise(9, 0, 1.1)[as.integer(interaction(d$Rep, d$Location))]
      d$Yield <- round(28 + loc + var + vxl + repe + demo_noise(nrow(d), 0, 1.0), 2)
      d[, c("Location", "Rep", "Variety", "Yield")]
    },
    "POOLCRD" = {
      d <- expand.grid(Rep = 1:4,
                       Treatment = c("T1", "T2", "T3", "T4"),
                       Season = c("Kharif", "Rabi", "Zaid"))
      sea <- c(0, 5, -4)[as.integer(d$Season)]
      trt <- c(0, 4, 7, 5)[as.integer(d$Treatment)]
      txs <- ifelse(d$Season == "Zaid" & d$Treatment %in% c("T3", "T4"), 3.0, 0)
      d$Yield <- round(22 + sea + trt + txs + demo_noise(nrow(d), 0, 1.3), 2)
      d[, c("Season", "Treatment", "Yield")]
    },
    "POOLFACT" = {                                   # 2-factor factorial over locations (RCBD)
      d <- expand.grid(Rep = paste0("R", 1:3),
                       Nitrogen = c("N0", "N1"),
                       Variety  = c("V1", "V2", "V3"),
                       Location = c("Shalimar", "Wadura", "Zampathan"))
      loc <- c(0, 4, -3)[as.integer(d$Location)]
      nit <- c(0, 5)[as.integer(d$Nitrogen)]
      var <- c(0, 3, 6)[as.integer(d$Variety)]
      nxv <- ifelse(d$Nitrogen == "N1" & d$Variety == "V3", 2.5, 0)           # N x V
      nxl <- ifelse(d$Nitrogen == "N1" & d$Location == "Zampathan", -2.0, 0)  # N x Location
      rep_e <- demo_noise(9, 0, 1.0)[as.integer(interaction(d$Rep, d$Location))]
      d$Yield <- round(26 + loc + nit + var + nxv + nxl + rep_e +
                       demo_noise(nrow(d), 0, 1.0), 2)
      d[, c("Location", "Rep", "Nitrogen", "Variety", "Yield")]
    },
    "POOLFACTC" = {                                  # 2-factor factorial over seasons (CRD)
      d <- expand.grid(Rep = 1:4,
                       Spacing = c("S1", "S2"),
                       Variety = c("V1", "V2", "V3"),
                       Season  = c("Kharif", "Rabi", "Zaid"))
      sea <- c(0, 5, -4)[as.integer(d$Season)]
      sp  <- c(0, 3)[as.integer(d$Spacing)]
      var <- c(0, 4, 7)[as.integer(d$Variety)]
      sxv <- ifelse(d$Spacing == "S2" & d$Variety == "V3", 3.0, 0)
      d$Yield <- round(24 + sea + sp + var + sxv + demo_noise(nrow(d), 0, 1.3), 2)
      d[, c("Season", "Spacing", "Variety", "Yield")]
    }
  )
}

## ------------------------------------------------------------ ANOVA helpers
tidy_aov <- function(fit) {
  a <- summary(fit)[[1]]
  ## with no error degrees of freedom summary() has no F column; say so later
  ## in words rather than fail here
  data.frame(Source = trimws(rownames(a)), Df = a[["Df"]], SS = a[["Sum Sq"]],
             MS = a[["Mean Sq"]],
             F = if (is.null(a[["F value"]])) NA_real_ else a[["F value"]],
             p = if (is.null(a[["Pr(>F)"]])) NA_real_ else a[["Pr(>F)"]],
             check.names = FALSE)
}

tidy_aovlist <- function(fit) {
  s <- summary(fit)
  out <- lapply(names(s), function(nm) {
    a <- s[[nm]][[1]]
    data.frame(Stratum = nm, Source = trimws(rownames(a)), Df = a[["Df"]],
               SS = a[["Sum Sq"]], MS = a[["Mean Sq"]],
               F = if (is.null(a[["F value"]])) NA_real_ else a[["F value"]],
               p = if (is.null(a[["Pr(>F)"]])) NA_real_ else a[["Pr(>F)"]],
               check.names = FALSE)
  })
  do.call(rbind, out)
}

## residual mean square + df of a given stratum
strat_res <- function(tab, stratum) {
  r <- tab[tab$Stratum == stratum & tab$Source == "Residuals", ]
  list(ms = r$MS[1], df = r$Df[1], ss = r$SS[1])
}

t_weighted <- function(w1, t1, w2, t2) (w1 * t1 + w2 * t2) / (w1 + w2)

## Bartlett's test of homogeneity of k error variances, each an s^2 with df
## degrees of freedom (as they arise from k separate error mean squares).
## Used in combined analysis to decide whether the errors may be pooled.
bartlett_ms <- function(df, s2) {
  ok <- is.finite(df) & is.finite(s2) & df > 0 & s2 > 0
  df <- df[ok]; s2 <- s2[ok]; k <- length(s2)
  if (k < 2) return(NULL)
  N <- sum(df); sp2 <- sum(df * s2) / N
  num <- N * log(sp2) - sum(df * log(s2))
  C <- 1 + (sum(1 / df) - 1 / N) / (3 * (k - 1))
  chisq <- num / C
  list(chisq = chisq, df = k - 1, p = stats::pchisq(chisq, k - 1, lower.tail = FALSE),
       pooled_var = sp2, k = k)
}

## Factor levels in natural order: numbers inside labels compare as numbers, so
## D0, D60, D120, D180 and T1, T2, ..., T10 come out in the order people mean
## rather than the alphabetical D0, D120, D180, D60. A column that is already a
## factor keeps the order it was given; a numeric column sorts numerically.
natural_levels <- function(x) {
  if (is.factor(x)) return(levels(droplevels(x)))
  if (is.numeric(x)) return(levels(factor(x)))
  u <- unique(as.character(x[!is.na(x)]))
  if (!length(u)) return(u)
  ## dates in time order: sowing dates read by their day number alone ran
  ## 10 Nov, 25 Nov, 25 Oct
  dk <- date_key(u)
  if (!is.null(dk)) return(u[order(dk, u, method = "radix")])
  ## a thousands separator is part of the number (1,000 ppm); a decimal comma
  ## (5,6) is left for the data checks to flag
  s <- gsub("(?<=[0-9]),(?=[0-9]{3}([^0-9]|$))", "", u, perl = TRUE)
  ## a simple fraction (1/4 RDF) is one number; a dose such as 120/60/40 or
  ## 120/60 is not a fraction and keeps its numbers
  fr <- gregexpr("(?<![0-9./])[0-9]+/[0-9]+(?![0-9]|/[0-9])", s, perl = TRUE)
  regmatches(s, fr) <- lapply(regmatches(s, fr), function(h) vapply(h, function(z) {
    ab <- as.numeric(strsplit(z, "/", fixed = TRUE)[[1]])
    if (ab[1] < ab[2] && ab[2] <= 16) format(ab[1] / ab[2], digits = 10) else z
  }, ""))
  ## a sign belongs to the number only when it stands apart from the letters
  ## and digits before it: "-20 C" is negative, "SR-1" and "0-15 cm" are not
  num <- "(?:(?<![[:alnum:].])[-+])?[0-9]+(?:[.][0-9]+)?"
  m <- gregexpr(num, s, perl = TRUE)
  hits <- regmatches(s, m)
  ## each number is replaced by its zero-padded rank among all the numbers, so
  ## numbers of any size or sign compare correctly as text
  vals <- sort(unique(as.numeric(unlist(hits))))
  w <- nchar(length(vals))
  regmatches(s, m) <- lapply(hits, function(h)
    formatC(match(as.numeric(h), vals), width = w, flag = "0"))
  ## a label without a number beside numbered ones is usually the control or
  ## the starting point (Control, Fresh, Local check), so it comes first
  plain <- lengths(hits) == 0
  first <- if (sum(!plain) >= 2 && any(plain)) ifelse(plain, "0", "1") else ""
  u[order(paste0(first, tolower(s)), u, method = "radix")]
}

## A sort key for labels that are all dates, or NULL. Day and month name
## (25 Oct, Oct 25, 25-October-2023), with or without a year: without one, a
## season that runs over the new year (Oct, Nov, Jan) starts after the
## longest gap between the months used. Numeric dates (15/06/2023) are read
## day first, as they are written in India, unless a middle part above 12
## shows the month comes first.
date_key <- function(u) {
  lu <- tolower(u)
  if (all(grepl("^[0-9]{1,2}[/.-][0-9]{1,2}[/.-][0-9]{2,4}$", lu))) {
    p <- do.call(rbind, lapply(strsplit(lu, "[/.-]"), as.numeric))
    yr <- ifelse(p[, 3] < 100, 2000 + p[, 3], p[, 3])
    dm <- if (any(p[, 2] > 12) && all(p[, 1] <= 12)) p[, 2:1, drop = FALSE] else p[, 1:2, drop = FALSE]
    if (any(dm[, 2] > 12) || any(dm[, 1] > 31)) return(NULL)
    return(sprintf("%04d%02d%02d", yr, dm[, 2], dm[, 1]))
  }
  mon <- c(tolower(month.abb), "sept")
  w <- regmatches(lu, regexpr("(?<![[:alpha:]])[[:alpha:]]{3,9}(?![[:alpha:]])", lu, perl = TRUE))
  if (length(w) != length(u)) return(NULL)
  full <- tolower(month.name)
  m <- ifelse(w %in% full, match(w, full), match(w, mon))
  m[!is.na(m) & m == 13] <- 9
  if (anyNA(m)) return(NULL)
  ## nothing but the month word, the day, a year and separators
  rest <- gsub("[[:alpha:]]+", "", lu)
  if (any(grepl("[^0-9 ,./-]", rest))) return(NULL)
  nums <- regmatches(lu, gregexpr("[0-9]+", lu))
  yr <- vapply(nums, function(z) { y <- as.numeric(z[nchar(z) == 4]); if (length(y) == 1) y else NA_real_ }, 0)
  dy <- vapply(nums, function(z) { d <- as.numeric(z[nchar(z) <= 2]); if (length(d) == 1 && d >= 1 && d <= 31) d else NA_real_ }, 0)
  if (anyNA(dy) || length(unique(is.na(yr))) > 1) return(NULL)
  if (all(is.na(yr))) {
    used <- sort(unique(m))
    gaps <- c(diff(used), used[1] + 12 - used[length(used)])
    start <- used[(which.max(gaps) %% length(used)) + 1]
    m <- (m - start) %% 12
    yr <- 0
  }
  sprintf("%04d%02d%02d", yr, m, dy)
}

## cell means for a set of factors
eff_means <- function(d, resp, vars) {
  agg <- stats::aggregate(d[[resp]], by = d[vars], FUN = mean)
  names(agg)[ncol(agg)] <- "Mean"
  agg$N <- stats::aggregate(d[[resp]], by = d[vars], FUN = length)$x
  agg$SD <- stats::aggregate(d[[resp]], by = d[vars], FUN = stats::sd)$x
  agg[do.call(order, agg[vars]), , drop = FALSE]
}

## one label per row of a table of means, naming its level(s), as shown to the
## user and used for the dimnames of an effect's SE(d) and C.D. matrices
level_key <- function(df, vars)
  do.call(paste, c(lapply(df[vars], as.character), sep = " : "))

## the same, joined by a character that cannot appear in data, for matching
## cells internally: two cells whose level names contain " : " could otherwise
## share a label and be merged
cell_key <- function(df, vars)
  do.call(paste, c(lapply(df[vars], as.character), sep = "\u001f"))

## Least-squares (adjusted) means of `vars` from a fitted linear model, with
## their covariance matrix, in the order of `keys`. Each mean averages the
## model's predictions over every level of the other factors with equal weight,
## so a treatment that lost a plot is not pulled up or down by the block that
## plot was in, and a marginal mean is not weighted by unequal cell sizes. With
## complete, balanced data these are the plain averages with variance MSE / n.
## Base R only: the model matrix of a reference grid, coef() and vcov().
ls_means <- function(fit, vars, keys) {
  xl <- fit$xlevels
  grid <- expand.grid(xl, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (v in names(xl)) grid[[v]] <- factor(grid[[v]], levels = xl[[v]])
  X <- stats::model.matrix(stats::delete.response(stats::terms(fit)), grid,
                           contrasts.arg = fit$contrasts)
  g <- cell_key(grid, vars)
  L <- rowsum(X, g, reorder = FALSE) / as.vector(table(g)[unique(g)])
  L <- L[match(keys, rownames(L)), , drop = FALSE]
  ## a mean is estimable only if its row of L lies in the row space of the
  ## data's model matrix; otherwise the data cannot separate it from the blocks
  Xd <- stats::model.matrix(fit)
  off <- qr.resid(qr(t(Xd)), t(L))
  if (any(abs(off) > 1e-8 * max(1, abs(L))))
    stop("Some means cannot be estimated: the treatments that are present never ",
         "appear together in a block, so their differences cannot be separated ",
         "from the block differences.", call. = FALSE)
  b <- stats::coef(fit); ok <- !is.na(b)
  Lk <- L[, ok, drop = FALSE]
  V <- Lk %*% stats::vcov(fit)[ok, ok, drop = FALSE] %*% t(Lk)
  list(est = drop(Lk %*% b[ok]), V = V)
}

## One effect's table of means, with the standard errors and critical
## differences for every comparison. Every standard error comes from V, the
## covariance matrix of the means: SE(m)_i = sqrt(V_ii) and
## SE(d)_ij = sqrt(V_ii + V_jj - 2 V_ij). Without a fitted model the means are
## plain averages with V = diag(MSE / n_i), so with unequal replication each
## mean and each pair still gets its own value. Given `fit`, the means are
## least-squares means and V comes from the model, which is what blocked
## designs with a missing plot and factorials with unequal cells need.
new_effect <- function(d, resp, vars, mse, df, p, Fv, alpha, label = NULL, fit = NULL) {
  m <- eff_means(d, resp, vars)
  key <- level_key(m, vars)
  notes <- character(0)
  if (is.null(fit)) {
    V <- diag(mse / m$N, nrow(m))
  } else {
    ls <- ls_means(fit, vars, cell_key(m, vars))
    V <- ls$V
    ## judged on the means' own scale: values near 1e-8 (counts on the
    ## reciprocal scale) lost their adjustment against a floor of 1
    if (any(abs(ls$est - m$Mean) > 1e-8 * max(abs(c(m$Mean, ls$est)), 1e-300))) {
      m$Raw_mean <- m$Mean
      m$Mean <- unname(ls$est)
      notes <- paste0("The data are unbalanced, so these are adjusted (least-squares) ",
        "means: each is estimated as if every level had appeared in every block and ",
        "alongside every level of the other factors. The plain average of the data ",
        "is shown as the unadjusted mean.")
    }
  }
  dv <- diag(V)
  S <- sqrt(pmax(outer(dv, dv, "+") - 2 * V, 0))
  diag(S) <- 0
  dimnames(S) <- list(key, key)
  tq <- stats::qt(1 - alpha / 2, df)
  C <- tq * S
  m$SE <- sqrt(dv)
  m$Letter <- cld_lsd(m$Mean, C)
  e <- list(label = label %||% paste(vars, collapse = " x "), vars = vars,
            means = m, mse = mse, df = df, alpha = alpha, tcrit = tq,
            p = p, F = Fv, notes = notes)
  set_errors(e, m$SE, S, C)
}

## Attach the standard errors to an effect: the per-mean SE (already a column
## of the means), the SE(d) and C.D. matrices, and single values sem, sed and cd
## only when one number genuinely applies to every mean and every pair. When it
## does not they are NA, the range is kept for display, and equal_rep is FALSE,
## so no table can print one SE that fits some means and not others.
set_errors <- function(e, se, S, C) {
  off <- row(S) != col(S)
  sd_ <- S[off]; sd_ <- sd_[!is.na(sd_)]
  cd_ <- C[off]; cd_ <- cd_[!is.na(cd_)]
  same <- function(x) length(x) > 0 && diff(range(x)) <= 1e-9 * max(abs(x))
  one <- function(x) if (same(x)) x[1] else NA_real_
  n <- e$means$N
  e$sem <- one(se); e$sed <- one(sd_); e$cd <- one(cd_)
  e$se_range <- range(se)
  e$sed_range <- if (length(sd_)) range(sd_) else c(NA_real_, NA_real_)
  e$cd_range <- if (length(cd_)) range(cd_) else c(NA_real_, NA_real_)
  e$equal_rep <- same(se) && same(sd_)
  e$n_per_mean <- if (length(unique(n)) == 1L) n[1] else NA_real_
  e$sed_mat <- S; e$cd_mat <- C
  e
}

## An interaction effect from a design with several error strata (split plot,
## strip plot, pooled analysis). Only comparisons inside one level of `slice`
## share a single standard error, so the SE(d) and C.D. matrices are filled for
## those pairs and left NA across slices; letters and post-hoc tests then work
## one slice at a time. These designs are analysed only when complete, so every
## within-slice comparison has the same SE(d). The SE column here is the
## conventional SE(m) for those comparisons, SE(d) / sqrt(2), not the standard
## error of a cell mean on its own, which would also carry the main-plot (or
## environment) variation; plots and tables say so. `extra` holds the SE(d) and
## C.D. of the other comparisons, and `extra_p` the p-value that gates each
## C.D. (NA for an SE(d), which is always shown).
slice_effect <- function(m, vars, slice, label, se, sed, tcrit, mse, df, alpha,
                         p, Fv, extra, notes, ...) {
  key <- level_key(m, vars)
  k <- nrow(m)
  inside <- outer(as.character(m[[slice]]), as.character(m[[slice]]), "==")
  S <- matrix(NA_real_, k, k, dimnames = list(key, key))
  S[inside] <- sed
  diag(S) <- 0
  m$SE <- rep(se, k)
  e <- c(list(label = label, vars = vars, means = m, mse = mse, df = df,
              alpha = alpha, tcrit = tcrit, p = p, F = Fv, slice = slice,
              extra = extra, notes = notes), list(...))
  set_errors(e, m$SE, S, tcrit * S)
}

## The variance of a difference between two treatment means of a pooled
## factorial, with the environment random. A mean of the factors Sp (averaged
## over the others) carries the environment's interaction with every set of
## factors that shares one with Sp, and two means that differ in the factors D
## keep, in their difference, those that share one with D. Solving the
## expected mean squares (E x T random, unrestricted model) gives
## Var = 2 / (e r) * sum over V of w_V * MS(E x V), over the sets V within Sp
## that share a factor with D, with
##   w_V = prod_{f not in V} 1/n_f * prod_{f in V, not D} (1 - 1/n_f)
##         * [prod_{f in V and D} (1 - 1/n_f) - prod_{f in V and D} (-1/n_f)],
## which is never negative, so Gomez and Gomez's weighted t applies. With two
## factors this is their strip-plot formula with the environment as the
## replication: two A means at the same B have (b - 1) MS(E x A x B) +
## MS(E x A) over e r b. A main effect keeps MS(E x A) alone. `n` holds the
## number of levels of every treatment factor, named.
pool_weights <- function(n, Sp, D) {
  Vs <- unlist(lapply(seq_along(Sp), function(k) utils::combn(Sp, k, simplify = FALSE)),
               recursive = FALSE)
  w <- vapply(Vs, function(V) {
    vd <- intersect(V, D)
    if (!length(vd)) return(0)
    prod(1 / n[setdiff(names(n), V)]) * prod(1 - 1 / n[setdiff(V, D)]) *
      (prod(1 - 1 / n[vd]) - prod(-1 / n[vd]))
  }, numeric(1))
  names(w) <- vapply(Vs, paste, "", collapse = ":")
  w[w > 1e-12]
}

## An interaction of a pooled factorial: every pair of means has the SE(d) of
## the factors in which the two differ, and a C.D. from the weighted t of the
## mean squares in it. `ms` and `dfs` are the mean squares and df of E x V,
## named by the factors of V joined by ":". Each kind of comparison is listed
## in `extra`; `mix` keeps each one's mean squares for the post-hoc tests,
## which use Satterthwaite's degrees of freedom where they need them.
pool_effect <- function(d, resp, vars, n, ms, dfs, ne, r, alpha, p, Fv, label, mse, df) {
  m <- eff_means(d, resp, vars)
  key <- level_key(m, vars); k <- nrow(m)
  lv <- as.matrix(m[vars])
  D_of <- function(i, j) vars[lv[i, ] != lv[j, ]]
  types <- list(); tkey <- character(0)
  type <- matrix(0L, k, k)
  for (i in seq_len(k)) for (j in seq_len(k)) if (i != j) {
    D <- D_of(i, j); kk <- paste(D, collapse = ":")
    if (!kk %in% tkey) { tkey <- c(tkey, kk); types[[kk]] <- D }
    type[i, j] <- match(kk, tkey)
  }
  ## fewer factors differing first, then in the order of the factors
  o <- order(lengths(types), vapply(types, function(D) paste(match(D, vars) + 10, collapse = ""), ""))
  types <- types[o]; type[type > 0] <- match(type[type > 0], o)
  comp <- lapply(types, function(D) {
    w <- pool_weights(n, vars, D)
    list(w = w * ms[names(w)], df = dfs[names(w)])
  })
  sed <- vapply(comp, function(z) sqrt(2 * sum(z$w) / (ne * r)), numeric(1))
  tq  <- vapply(comp, function(z) sum(z$w * stats::qt(1 - alpha / 2, z$df)) / sum(z$w), numeric(1))
  S <- matrix(0, k, k, dimnames = list(key, key)); Tm <- S
  S[type > 0] <- sed[type[type > 0]]; Tm[type > 0] <- tq[type[type > 0]]
  C <- Tm * S
  ## the standard error of a mean as the tables show it: SE(d) / sqrt(2) for
  ## two means that differ in every factor
  u <- pool_weights(n, vars, vars)
  m$SE <- rep(sqrt(sum(u * ms[names(u)]) / (ne * r)), k)
  m$Letter <- cld_lsd(m$Mean, C)
  what <- vapply(types, function(D) {
    same <- setdiff(vars, D)
    if (length(same)) sprintf("two %s means at the same %s", paste(D, collapse = " x "),
                              paste(same, collapse = " x "))
    else if (length(D) == 2) sprintf("two means that differ in both %s", join_and(D))
    else sprintf("two means that differ in every factor (%s)", join_and(D))
  }, "")
  cdl <- cd_name(alpha)
  extra <- as.list(c(rbind(sed, tq * sed)))
  names(extra) <- c(rbind(paste0("SE(d): ", what), paste0(cdl, ": ", what)))
  sig <- !is.na(p) && p < alpha
  dft <- vapply(comp, function(z) sum(z$w)^2 / sum(z$w^2 / z$df), numeric(1))
  e <- list(label = label, vars = vars, means = m, mse = mse, df = df, alpha = alpha,
            tcrit = NA_real_, p = p, F = Fv, extra = extra,
            extra_p = rep(c(NA, p), length(types)),
            notes = if (sig) paste0("The environment is random, so the difference between two of ",
              "these means carries the environment's interaction with each factor in which they ",
              "differ: each kind of comparison has its own SE(d) and C.D., listed below, the ",
              "C.D. with Gomez and Gomez's weighted t. The letters use the C.D. of each pair.") else "",
            error_desc = sprintf(paste0("The environment is random, so the SE(d) of each pair ",
              "combines the mean squares of the environment's interactions with the factors in ",
              "which the two means differ. The LSD and Bonferroni tests use Gomez and Gomez's ",
              "weighted t; the tests that need degrees of freedom use Satterthwaite's ",
              "approximation for each pair (%s df)."),
              if (diff(range(dft)) < 0.05) fmt(dft[1], 1) else paste(fmt(min(dft), 1), "to", fmt(max(dft), 1))),
            mix = list(type = type, comp = comp))
  set_errors(e, m$SE, S, C)
}

## An effect's further comparisons (e$extra) as label and display text, each
## C.D. replaced by "NS" when the F-test it belongs to is not significant.
extra_text <- function(e, digits = 3) {
  if (is.null(e$extra)) return(NULL)
  v <- unlist(e$extra)
  p <- if (is.null(e$extra_p)) rep(NA_real_, length(v)) else e$extra_p
  txt <- ifelse(!is.na(p) & !(p < e$alpha), "NS", fmt(v, digits))
  stats::setNames(txt, names(e$extra))
}

## Two-sided critical t for an effect at level alpha: plain t on the effect's
## error df, or, for a comparison whose standard error mixes two error terms,
## the weighted t of Gomez & Gomez (1984) stored in t_mix.
t_crit <- function(e, alpha) {
  if (is.null(e$t_mix)) return(stats::qt(1 - alpha / 2, e$df))
  w <- e$t_mix$w
  sum(w * stats::qt(1 - alpha / 2, e$t_mix$df)) / sum(w)
}
## For an effect whose pairs mix several error mean squares, each pair its
## own way (a pooled factorial's interactions, see pool_effect()): each pair's
## weighted t at two-sided level a, and its Satterthwaite degrees of freedom,
## as matrices over the effect's means.
mix_t <- function(e, a) {
  tt <- vapply(e$mix$comp, function(z) sum(z$w * stats::qt(1 - a / 2, z$df)) / sum(z$w), numeric(1))
  matrix(tt[pmax(e$mix$type, 1L)], nrow(e$mix$type))
}
mix_df <- function(e) {
  dd <- vapply(e$mix$comp, function(z) sum(z$w)^2 / sum(z$w^2 / z$df), numeric(1))
  matrix(dd[pmax(e$mix$type, 1L)], nrow(e$mix$type))
}
## the error degrees of freedom as a table states them
df_label <- function(e, ix = seq_len(nrow(e$means))) {
  if (is.null(e$mix)) return(df_text(e$df))
  r <- range(mix_df(e)[ix, ix][row(diag(length(ix))) != col(diag(length(ix)))])
  if (diff(r) < 0.05) paste(fmt(r[1], 1), "(Satterthwaite)")
  else sprintf("%s to %s, by pair (Satterthwaite)", fmt(r[1], 1), fmt(r[2], 1))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
