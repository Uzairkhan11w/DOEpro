## Helpers for the tests that drive the app through shiny::testServer().
##
## The selectors of the column mapping, of each response's transformation and
## of its nature carry the number of the data set loaded (`rv$ver`) in their
## ids, so a choice made for one data set is never read for the next. Inside
## testServer(doepro_server, ...):
##   set_map(session, rv$ver, resp = "Yield", block = "Block", treat = "Variety")
##   set_trans(session, rv$ver, rv$data, "Yield", "log")

## the column mapping, as the selectors on the Data tab report it
set_map <- function(session, ver, ...) {
  v <- list(...)
  do.call(session$setInputs, stats::setNames(v, paste0("map_", names(v), "_", ver)))
}

## the scale response `v` is analysed on, as the Assumptions tab sets it
set_trans <- function(session, ver, d, v, tr) {
  do.call(session$setInputs, stats::setNames(list(tr), paste0("tr_", ver, "_", match(v, names(d)))))
}

## the declared nature of response `v`
set_nature <- function(session, ver, d, v, nature) {
  do.call(session$setInputs, stats::setNames(list(nature), paste0("nat_", ver, "_", match(v, names(d)))))
}
