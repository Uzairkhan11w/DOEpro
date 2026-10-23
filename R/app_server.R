###############################################################################
##  SERVER
###############################################################################

## Are we running inside the browser (webR / WebAssembly)? There is no Chrome or
## external process there, so server-side PDF rendering cannot work.
is_wasm <- function() {
  grepl("emscripten|wasm", tolower(R.version$os)) ||
  grepl("wasm", tolower(R.version$arch)) ||
  "webr" %in% loadedNamespaces()
}

## Which server-side PDF engine is genuinely usable, if any. pagedown counts only
## when a Chrome/Chromium binary can actually be found; otherwise fall back to
## weasyprint or wkhtmltopdf. In the browser build, none apply.
pdf_engine <- function() {
  if (is_wasm()) return(NA_character_)
  chrome_ok <- requireNamespace("pagedown", quietly = TRUE) &&
    !is.null(tryCatch(pagedown::find_chrome(), error = function(e) NULL))
  if (isTRUE(chrome_ok)) return("pagedown")
  for (b in c("weasyprint", "wkhtmltopdf")) if (nzchar(Sys.which(b))) return(b)
  NA_character_
}

## build a CSV as a single string (so it can be handed to the browser downloader)
csv_string <- function(df) {
  paste(utils::capture.output(utils::write.csv(df, row.names = FALSE)),
        collapse = "\n")
}

#' The DOEpro server logic
#'
#' The Shiny server function. Called by \code{\link{run_DOEpro}}; you should not
#' normally need to call it yourself.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Invisibly \code{NULL}; called for its side effects.
#' @keywords internal
doepro_server <- function(input, output, session) {

  ## `ver` counts the data sets loaded: the selectors of the column mapping,
  ## the transformations and the natures of the responses carry it in their
  ## ids, so a choice made for one data set is never read for the next.
  ## `trans` and `nature` hold each response's choices; `keep` the mapping
  ## last chosen for this data set, which a change of design starts from.
  rv <- reactiveValues(data = NULL, dq_log = NULL, ver = 0L, source = NULL, demo = NULL,
                       design_hint = NULL, nfac_hint = NULL, trans = list(), nature = list(),
                       keep = list())

  save_browser <- function(filename, content, mime)
    session$sendCustomMessage("doepro_save",
      list(filename = filename, content = content, mime = mime))

  set_data <- function(d, source, demo = NULL) {
    rv$data <- d; rv$dq_log <- NULL; rv$ver <- isolate(rv$ver) + 1L
    rv$source <- source; rv$demo <- demo
    rv$trans <- list(); rv$nature <- list(); rv$keep <- list()
  }

  ## ------------------------------------------------------------ data input --
  observeEvent(input$load, {
    txt <- input$paste
    if (!nz(txt)) { showNotification("Nothing pasted.", type = "warning"); return() }
    d <- tryCatch(read_pasted(txt, input$sep, input$header), error = function(e) NULL)
    if (is.null(d) || !ncol(d))
      showNotification("Could not read the pasted text - check the separator.", type = "error")
    else { set_data(d, "paste"); showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  observeEvent(input$file, {
    d <- tryCatch(read_upload(input$file$datapath), error = function(e) conditionMessage(e))
    if (is.character(d)) showNotification(paste("Could not read that file.", d), type = "error",
                                          duration = NULL)
    else { set_data(d, "file"); showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  ## An example comes with its design and the role of each column, so the
  ## mapping, the screening and every analysis follow at once.
  observeEvent(input$loaddemo, {
    key <- input$demo
    dm <- DEMO_MAPS[[key]]
    set_data(demo_data(key), "demo", key)
    rv$design_hint <- dm$design
    nf <- length(dm$map$factors)
    if (nf >= 2) { rv$nfac_hint <- nf; updateSliderInput(session, "nfac", value = nf) }
    updateSelectInput(session, "design", selected = dm$design)
    showNotification(sprintf("Loaded the %s example.", names(DEMO_CHOICES)[match(key, DEMO_CHOICES)]), type = "message")
  })

  ## The design and number of factors an example sets are used at once, before
  ## the selectors on the page report them back; any change of the selector,
  ## the user's or the example's, ends that.
  observeEvent(input$design, rv$design_hint <- NULL, priority = 100, ignoreInit = TRUE)
  observeEvent(input$nfac, rv$nfac_hint <- NULL, priority = 100, ignoreInit = TRUE)
  design_now <- reactive(rv$design_hint %||% input$design %||% "RCBD")
  nfac_now <- reactive(rv$nfac_hint %||% input$nfac %||% 2)

  ## Row numbers are shown, because every message from the data check and the
  ## analysis names rows by them; they cannot be edited.
  output$tbl <- renderDT(rv$data, rownames = TRUE,
                         editable = list(target = "cell", disable = list(columns = 0)),
                         options = list(pageLength = 8, scrollX = TRUE))
  tbl_proxy <- DT::dataTableProxy("tbl")

  ## An edit to a column of numbers is read the way the data check reads
  ## entries, so 5,6 becomes 5.6; anything whose meaning is not certain is
  ## refused with a message, and the table shows the old value again, instead
  ## of it quietly becoming a missing value. Clearing a cell leaves it empty.
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    d <- rv$data
    j <- info$col                        # column 0 holds the row numbers
    if (j < 1 || j > ncol(d)) return()
    val <- info$value
    v <- names(d)[j]; rn <- rownames(d)[info$row]
    if (!nzchar(trimws(val))) {
      d[[j]][info$row] <- NA
    } else if (is.numeric(d[[j]])) {
      r <- read_entries(val)
      ## a % sign typed in one cell of a number column is as uncertain as the
      ## data check finds it: 62% beside 0.45 could mean 0.62
      if (r$kind %in% c("text", "ambiguous", "unit", "percent") || (r$kind == "mark")) {
        msg <- if (r$kind == "percent")
          sprintf(paste0("'%s' carries a %% sign, which the other values of '%s' do not, so row %s was not ",
                         "changed. Type the value in the column's own units (0.62 rather than 62%% if the ",
                         "column holds proportions)."), val, v, rn)
        else if (r$kind == "ambiguous")
          sprintf("'%s' could mean %s or %s, so row %s of '%s' was not changed. Type %s or %s, whichever you meant.",
                  val, gsub(",", "", val, fixed = TRUE), sub(",", ".", val, fixed = TRUE), rn, v,
                  gsub(",", "", val, fixed = TRUE), sub(",", ".", val, fixed = TRUE))
        else if (r$kind == "mark")
          sprintf("To leave row %s of '%s' empty, clear the cell. It was not changed.", rn, v)
        else sprintf(paste0("'%s' is not a number, so row %s of '%s' was not changed. Type a number ",
                            "such as 5.6, or clear the cell to leave it empty."), val, rn, v)
        showNotification(msg, type = "warning")
        DT::replaceData(tbl_proxy, d, resetPaging = FALSE, rownames = TRUE)
        return()
      }
      d[[j]][info$row] <- r$value
    } else {
      ## a label column is edited as text: a new label in a factor column
      ## would otherwise become a missing value without a word
      if (is.factor(d[[j]]) || is.logical(d[[j]])) d[[j]] <- as.character(d[[j]])
      d[[j]][info$row] <- val
    }
    rv$data <- d
  })

  output$dataNote <- renderUI({
    if (is.null(rv$data)) return(div(class = "box",
      "Paste your data, upload a CSV, or load one of the examples. ",
      "Data must be in long format: one row per plot, one column per variable. ",
      "You may analyse several response variables at once."))
    if (identical(rv$source, "demo"))
      return(div(class = "slx-demo", tags$b("Demonstration data. "),
                 sprintf("The %s example: %s", names(DEMO_CHOICES)[match(rv$demo, DEMO_CHOICES)],
                         sub("^Demonstration data: ", "", DEMO_NOTE))))
    NULL
  })

  ## ------------------------------------------------------------ data check --
  ## Every load is checked at once. Nothing is changed until the user presses
  ## the button, and the log says exactly what was changed.
  chk <- reactive({ d <- rv$data; req(d); check_data(d) })

  output$dqOut <- renderUI({
    d <- rv$data; req(d)
    ck <- chk()
    fixable <- any(vapply(ck$issues, function(z) isTRUE(z$fixable), logical(1)))
    tagList(
      if (length(rv$dq_log)) div(class = "sugbox", HTML(paste0(
        "<b>Corrections made.</b><ul>", paste0("<li>", rv$dq_log, "</li>", collapse = ""), "</ul>"))),
      HTML(check_html(ck, d)),
      if (fixable) actionButton("dqFix", "Apply the corrections", class = "btn-primary btn-sm") else NULL)
  })

  observeEvent(input$dqFix, {
    fx <- fix_data(rv$data)
    changed <- !identical(fx$data, rv$data)
    rv$data <- fx$data
    rv$dq_log <- fx$log
    showNotification(if (changed) "Corrections applied." else
                     "Nothing could be corrected yet. See 'Needs you to decide' above.",
                     type = if (changed) "message" else "warning")
  })

  ## ------------------------------------------------------- column mapping ---
  ## the id of a role's selector for the data set now loaded
  map_id <- function(role) paste0("map_", role, "_", rv$ver)

  ## The selectors start from the example's own roles, then from the choices
  ## already made for these data (a change of design keeps what still
  ## applies), then from a guess. They are drawn even while the Data tab is
  ## hidden, so the analysis never waits for the tab to be visited.
  output$mapUI <- renderUI({
    d <- rv$data; req(d)
    des <- design_now(); nf <- nfac_now()
    roles <- design_roles(des, nf)
    cn <- names(d)
    num <- intersect(cn, chk()$numeric)
    keep <- isolate(rv$keep)
    demo <- isolate(rv$demo)
    dm <- if (!is.null(demo) && identical(DEMO_MAPS[[demo]]$design, des)) map_to_roles(DEMO_MAPS[[demo]]$map) else list()
    guess <- guess_map(d, roles, num)
    pick <- function(r) {
      for (src in list(keep, dm)) {
        x <- src[[r]]
        if (length(x) && all(x %in% cn)) return(x)
      }
      guess[[r]]
    }
    resp <- if (!is.null(demo) && identical(DEMO_MAPS[[demo]]$design, des) && !length(keep$resp))
      DEMO_MAPS[[demo]]$response else pick("resp")
    tagList(
      selectizeInput(map_id("resp"), "Response variable(s)", cn, selected = intersect(resp, cn),
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = "choose one or more columns of numbers")),
      div(class = "slx-map-grid",
          lapply(roles, function(r) selectInput(map_id(r), role_label(r, des), cn, selected = pick(r)))))
  })
  outputOptions(output, "mapUI", suspendWhenHidden = FALSE)

  ## the roles as the selectors hold them now, and what stops them from
  ## making an analysis: NULL `map` until every selector is there
  map_state <- reactive({
    d <- rv$data; req(d)
    des <- design_now(); nf <- nfac_now()
    roles <- design_roles(des, nf)
    resp <- input[[map_id("resp")]]
    vals <- stats::setNames(lapply(roles, function(r) input[[map_id(r)]]), roles)
    if (any(vapply(vals, is.null, logical(1))))
      return(list(map = NULL, partial = c(list(resp = resp), Filter(Negate(is.null), vals)), problem = NULL))
    vals <- c(list(resp = resp), vals)
    cn <- names(d)
    problem <- if (!length(resp)) "Choose at least one response variable."
      else if (!all(unlist(vals) %in% cn)) "Some of the chosen columns are not in these data; choose again."
      else {
        used <- unlist(vals[roles])
        twice <- unique(used[duplicated(used)])
        both <- intersect(resp, used)
        notnum <- setdiff(resp, chk()$numeric)
        if (length(twice)) sprintf("%s %s chosen for two roles; each role needs its own column.",
                                   join_and(sprintf("'%s'", twice)), pl(length(twice), "is", "are"))
        else if (length(both)) sprintf("%s %s chosen both as a response and as a design column.",
                                       join_and(sprintf("'%s'", both)), pl(length(both), "is", "are"))
        else if (length(notnum)) sprintf(paste("%s %s not a column of numbers, so it cannot be analysed as a response;",
                                               "choose a column of measurements."), join_and(sprintf("'%s'", notnum)),
                                         pl(length(notnum), "is", "are"))
        else NULL
      }
    list(map = if (is.null(problem)) roles_to_map(vals) else NULL, partial = vals, problem = problem)
  })

  ## the last workable mapping, which a change of design starts from
  observe({
    st <- map_state()
    if (!is.null(st$map)) rv$keep <- modifyList(isolate(rv$keep), map_to_roles(st$map))
  })

  ## the roles chosen so far, complete or not, as analyze() names them
  map_partial <- reactive({
    st <- tryCatch(map_state(), error = function(e) NULL)
    if (is.null(st)) return(list())
    p <- st$partial
    roles_to_map(p[!vapply(p, is.null, logical(1))])
  })

  output$mapNote <- renderUI({
    st <- map_state()
    if (!is.null(st$problem)) div(class = "warn", st$problem) else NULL
  })

  ## Everything an analysis is made from, once the mapping is complete
  setup <- reactive({
    st <- map_state()
    req(!is.null(st$map))
    m <- st$map
    list(d = rv$data, design = design_now(), map = m[setdiff(names(m), "response")], responses = m$response,
         alpha = as.numeric(input$alpha %||% "0.05"), ver = rv$ver)
  })

  ## Each response's analysis as entered, its checks and the transformation
  ## advice: the screening on the Data tab and the start of every analysis.
  ## A response that cannot be analysed carries its reason, and the others
  ## go ahead.
  base_fits <- reactive({
    s <- setup()
    nat <- rv$nature
    out <- lapply(s$responses, function(v)
      tryCatch(fit_base(s$d, s$design, s$map, v, s$alpha, nat[[v]] %||% "auto"),
               error = function(e) list(resp = v, err = conditionMessage(e))))
    stats::setNames(out, s$responses)
  })

  ## The analysis of every response on the scale chosen for it. It follows
  ## every choice at once, the significance level included, so no table,
  ## letter or reading is ever left from an earlier choice.
  res <- reactive({
    s <- setup(); bf <- base_fits(); tr <- rv$trans
    fits <- list(); errs <- list()
    for (v in names(bf)) {
      b <- bf[[v]]
      if (!is.null(b$err)) { errs[[v]] <- b$err; next }
      t <- tr[[v]] %||% "none"
      if (!t %in% trans_ok(b)) t <- "none"
      f <- tryCatch(fit_final(b, s$d, s$design, s$map, t), error = function(e) e)
      if (inherits(f, "error")) errs[[v]] <- conditionMessage(f) else fits[[v]] <- f
    }
    rr_of(s$design, s$alpha, s$map, s$responses, fits, errs)
  })

  ok <- reactive({
    r <- res()
    validate(need(length(r$fits) > 0, paste(c("No response could be analysed.",
      sprintf("%s: %s", names(r$errors), unlist(r$errors))), collapse = " ")))
    r
  })

  ## a response's fit chosen on a tab, or the first while the choice catches up
  fit_of <- function(r, v) r$fits[[if (isTRUE(v %in% names(r$fits))) v else names(r$fits)[1]]]

  ## ------------------------------------------- quick look and screening --
  output$quickOut <- renderUI({
    d <- rv$data
    if (is.null(d)) return(div(class = "note", "Load data first."))
    st <- tryCatch(map_state(), error = function(e) NULL)
    resp <- intersect(st$partial$resp %||% character(0), chk()$numeric)
    if (!length(resp)) return(div(class = "note", "Choose the response variable(s) above to see their summary."))
    HTML(paste0("<div class='slx-scroll'>", quick_describe_html(quick_describe(d, resp), d), "</div>"))
  })

  output$screenOut <- renderUI({
    d <- rv$data
    if (is.null(d)) return(div(class = "note", "Load data first."))
    st <- map_state()
    if (!is.null(st$problem)) return(div(class = "warn", st$problem))
    if (is.null(st$map)) return(div(class = "note", "Choose the design's columns above; the screening follows at once."))
    bf <- base_fits()
    HTML(paste0("<div class='slx-scroll'>", screening_html(screen_rows(bf), setup()$alpha), "</div>"))
  })

  observeEvent(input$dl_quick, {
    d <- rv$data; req(d)
    resp <- intersect(map_partial()$response %||% character(0), chk()$numeric); req(length(resp) > 0)
    save_browser(paste0("StatLabX_quick-summary_", Sys.Date(), ".csv"), csv_string(quick_describe(d, resp)), "text/csv")
  })

  output$structOut <- renderUI({
    d <- rv$data
    if (is.null(d)) return(div(class = "box", "Load data on the Data tab first."))
    HTML(paste0("<div class='slx-card'><div class='slx-card-h'>Data structure and quality</div>",
                "<p class='slx-lead'>Every column of the data: the role it plays in the analysis, how it is stored, ",
                "what kind of variable it is and how complete it is. Entries the data check queries are listed on the Data tab.</p>",
                "<div class='slx-scroll'>", data_structure_html(data_structure(d, map_partial(), design_now())), "</div></div>"))
  })

  ## --------------------------------------------------- analysis context --
  ## the chips at the top of each analysis tab: what is analysed, and how
  ctx_ui <- function() renderUI({
    s <- tryCatch(setup(), error = function(e) NULL)
    if (is.null(s)) return(div(class = "warn",
      "Load data and choose the design and the variables on the Data tab; the results appear here at once."))
    tr <- rv$trans
    resp <- vapply(s$responses, function(v) {
      t <- tr[[v]] %||% "none"
      if (identical(t, "none")) v else sprintf("%s on %s", v, TRANS[[t]]$lab)
    }, character(1))
    m <- input$phMethod %||% "none"
    div(class = "slx-ctx",
        span("Design: ", tags$b(names(DESIGNS)[match(s$design, DESIGNS)])),
        span(if (length(resp) > 1) "Responses: " else "Response: ", tags$b(paste(resp, collapse = ", "))),
        span("Significance level: ", tags$b(paste0(pct(s$alpha), "%"))),
        span("Comparison: ", tags$b(if (identical(m, "none")) "means only" else CMP_NAME[[m]])),
        if (identical(rv$source, "demo")) span(tags$b("Demonstration data")))
  })
  output$ctxAssum <- ctx_ui(); output$ctxAnova <- ctx_ui(); output$ctxMeans <- ctx_ui()
  output$ctxPlots <- ctx_ui(); output$ctxReport <- ctx_ui()

  ## ------------------------------------------------------------ assumptions --
  ## Each response's nature and the scale it is analysed on. The selectors
  ## start at the values as entered (no transformation): a transformation is
  ## the user's decision, made with the suggestion in view.
  output$transUI <- renderUI({
    s <- setup(); bf <- base_fits()
    head <- tags$tr(tags$th("Response"), tags$th("Nature of the response"), tags$th("Analysed on the scale"))
    rows <- lapply(s$responses, function(v) {
      b <- bf[[v]]; k <- match(v, names(s$d))
      if (!is.null(b$err)) return(tags$tr(tags$td(tags$b(v)), tags$td(colspan = 2, div(class = "err", b$err))))
      allowed <- trans_ok(b)
      cur <- isolate(rv$trans[[v]]) %||% "none"
      sg <- b$sug
      tags$tr(
        tags$td(tags$b(v)),
        tags$td(selectInput(paste0("nat_", s$ver, "_", k), NULL, stats::setNames(names(NATURE), NATURE),
                            selected = isolate(rv$nature[[v]]) %||% "auto", width = "100%")),
        tags$td(selectInput(paste0("tr_", s$ver, "_", k), NULL, trans_choices()[match(allowed, names(TRANS))],
                            selected = if (cur %in% allowed) cur else "none", width = "100%"),
                div(class = "note", if (identical(sg$method, "none")) "Suggested: no transformation."
                    else sprintf("Suggested: %s%s.", TRANS[[sg$method]]$lab, if (isTRUE(sg$optional)) " (optional)" else ""))))
    })
    tags$table(class = "slx-settings", tags$thead(head), tags$tbody(rows))
  })
  outputOptions(output, "transUI", suspendWhenHidden = FALSE)

  ## the choices on the page, kept with each response
  observe({
    s <- tryCatch(setup(), error = function(e) NULL)
    req(s)
    for (v in s$responses) {
      k <- match(v, names(s$d))
      tv <- input[[paste0("tr_", s$ver, "_", k)]]
      nv <- input[[paste0("nat_", s$ver, "_", k)]]
      if (!is.null(tv) && !identical(tv, isolate(rv$trans[[v]]) %||% "none")) rv$trans[[v]] <- tv
      if (!is.null(nv) && !identical(nv, isolate(rv$nature[[v]]) %||% "auto")) rv$nature[[v]] <- nv
    }
  })

  observeEvent(input$applysug, {
    s <- tryCatch(setup(), error = function(e) NULL); req(s)
    bf <- base_fits()
    for (v in s$responses) {
      b <- bf[[v]]
      if (!is.null(b$err)) next
      m <- if (b$sug$method %in% trans_ok(b)) b$sug$method else "none"
      rv$trans[[v]] <- m
      updateSelectInput(session, paste0("tr_", s$ver, "_", match(v, names(s$d))), selected = m)
    }
  })

  output$aRespUI <- renderUI({
    r <- ok(); keep <- isolate(input$aResp)
    selectInput("aResp", "Show the checks for", names(r$fits),
                selected = if (isTRUE(keep %in% names(r$fits))) keep else names(r$fits)[1])
  })
  aFit <- reactive(fit_of(ok(), input$aResp))

  output$asmSummary <- renderUI({
    f <- aFit()
    HTML(paste0("<div class='slx-scroll'>",
                assum_summary_html(assum_summary(f, rv$nature[[f$resp]] %||% "auto"),
                                   if (identical(f$trans, "none")) f$resp else sprintf("%s, analysed on %s", f$resp, TRANS[[f$trans]]$lab)),
                "</div><div class='note'>Tested: a check made from the data, at the ", pct(f$final$alpha),
                "% level. Visual check: judged from the plots below. Design requirement: cannot be tested from the data; ",
                "it rests on how the experiment was laid out and run.</div>"))
  })
  output$assumtxt <- renderUI(HTML(assum_table_html(aFit()$asm)))
  output$asmText <- renderUI({ f <- aFit(); HTML(rd_html(assum_text(f$final, f$asm, f$trans), "What the checks show.")) })
  ## each plot's note, empty when the plot cannot be drawn (it says why)
  output$bcText <- renderUI({ f <- aFit(); HTML(rd_note(boxcox_text(f$asm, f$trans))) })
  output$mvText <- renderUI({ f <- aFit(); HTML(rd_note(meanvar_text(f$final, f$asm))) })
  output$diagFitText   <- renderUI({ f <- aFit(); HTML(rd_note(resid_text(f$final, f$asm))) })
  output$diagQQText    <- renderUI({ f <- aFit(); HTML(rd_note(qq_aov_text(f$final, f$asm))) })
  output$diagHistText  <- renderUI({ f <- aFit(); HTML(rd_note(hist_aov_text(f$final, f$asm))) })
  output$diagScaleText <- renderUI({ f <- aFit(); HTML(rd_note(scale_text(f$final, f$asm))) })
  output$diagCookText  <- renderUI({ f <- aFit(); HTML(rd_note(cooks_text(f$final))) })

  output$sugbox <- renderUI({
    f <- aFit()
    div(class = if (identical(f$sug$method, "none")) "sugbox" else "warn",
        HTML(sprintf("<b>Suggested transformation%s: %s.</b> %s%s",
          if (identical(f$trans, "none")) "" else ", judged on the values as entered",
          TRANS[[f$sug$method]]$lab, esc(f$sug$why),
          if (identical(f$trans, "none")) {
            if (identical(f$sug$method, "none")) ""
            else " <br>It is not applied: choose it above, for this response, to analyse on that scale. A transformation does not guarantee the assumptions; check the plots again after applying it."
          } else sprintf("<br><b>Applied:</b> %s; the checks on this tab are on that scale.", TRANS[[f$trans]]$lab))))
  })

  ## ---------------------------------------------------------- the graphs --
  ## Every graph is a ggplot made here, sent to the browser as a chart
  ## description (chart_spec()) and drawn there; its download menu saves it
  ## in the browser as it is shown. `what` and `resp` name the saved file.
  plots <- character(0)
  plot_out <- function(id, fun, what, resp = NULL) {
    plots <<- c(plots, id)
    output[[id]] <- render_chart(fun, what, resp)
  }

  plot_out("bcPlot", function() {
    a <- aFit()$asm
    validate(need(!isTRUE(a$exact), "The model fits every value exactly, so there is no Box-Cox profile to draw."))
    p <- plot_boxcox(a)
    validate(need(!is.null(p), "The Box-Cox profile needs a strictly positive response."))
    p }, "box-cox-profile", function() aFit()$resp)
  plot_out("mvPlot", function() {
    a <- aFit()$asm
    validate(need(!isTRUE(a$exact), "The model fits every value exactly, so there is no error variance to relate to the mean."))
    p <- plot_meanvar(a)
    validate(need(!is.null(p), "Too few cells with a positive mean and variance to estimate the mean-variance slope."))
    p }, "mean-variance", function() aFit()$resp)
  ## plot_diag() returns four plots; one output would show only one of them,
  ## so each gets its own output. An exact fit has
  ## only rounding residue for residuals, which is not drawn as if it were data.
  diag_plots <- reactive({
    f <- aFit()
    validate(need(!isTRUE(f$asm$exact), "The model fits every value exactly, so there are no residuals to plot."))
    plot_diag(f$final)
  })
  plot_out("diagFit",   function() diag_plots()[[1]], "residuals-vs-fitted", function() aFit()$resp)
  plot_out("diagQQ",    function() diag_plots()[[2]], "residual-qq-plot", function() aFit()$resp)
  plot_out("diagHist",  function() diag_plots()[[3]], "residual-histogram", function() aFit()$resp)
  plot_out("diagScale", function() diag_plots()[[4]], "scale-location", function() aFit()$resp)
  plot_out("diagCook",  function() {
    f <- aFit()
    validate(need(!isTRUE(f$asm$exact), "The model fits every value exactly, so no plot can change the fit."))
    p <- plot_cooks(f$final)
    validate(need(!is.null(p), "Cook's distance is not defined for any plot here."))
    p }, "cooks-distance", function() aFit()$resp)

  ## ------------------------------------------------------------------ ANOVA --
  output$runNote <- renderUI({
    r <- res()
    errs <- if (length(r$errors)) div(class = "err", HTML(paste0(
      "<b>Not analysed.</b><ul>", paste0("<li><b>", esc(names(r$errors)), ":</b> ", esc(unlist(r$errors)), "</li>", collapse = ""),
      "</ul>", if (any(grepl("left out of this analysis", unlist(r$errors), fixed = TRUE)))
        "<p>The data check on the Data tab lists the entries left out and corrects those it safely can.</p>" else "")))
    if (!length(r$fits)) return(errs)
    ## each response's own count and CV: a response with rows left out is
    ## analysed on fewer observations than the others
    nobs <- vapply(r$fits, function(f) nrow(f$final$data), 0L)
    unb <- Filter(function(f) !f$final$balanced, r$fits)
    sug <- Filter(function(f) identical(f$trans, "none") && !identical(f$sug$method, "none") && !isTRUE(f$sug$optional), r$fits)
    tagList(
      errs,
      div(class = "box", HTML(sprintf("<b>%s</b> &nbsp;|&nbsp; %d response variable(s) &nbsp;|&nbsp; %s",
        names(DESIGNS)[match(r$design, DESIGNS)], length(r$fits),
        if (length(unique(nobs)) == 1) sprintf("%d observations", nobs[1])
        else paste(sprintf("%s: %d observations", vapply(r$fits, `[[`, "", "header"), nobs), collapse = ", ")))),
      if (length(sug)) div(class = "warn", HTML(sprintf(paste(
        "<b>A transformation is suggested</b> for %s but not applied: the analysis below is of the values as entered.",
        "Choose it on the Assumptions &amp; Diagnostics tab if you decide to use it."),
        join_and(sprintf("%s (%s)", vapply(sug, `[[`, "", "resp"), vapply(sug, function(f) TRANS[[f$sug$method]]$lab, "")))))),
      ## rows left out, response by response, so no plot disappears unnoticed
      {
        ex <- Filter(nzchar, vapply(r$fits, function(f) {
          t <- excluded_text(f$final$excluded, html = TRUE, hint = TRUE)
          if (nzchar(t)) sprintf("<b>%s:</b> %s", f$header, t) else ""
        }, character(1)))
        if (length(ex)) div(class = "warn", HTML(paste(ex, collapse = "<br>"))) else NULL
      },
      if (length(unb)) div(class = "warn", HTML(paste0(
        "<b>The data are unbalanced</b>",
        if (length(unb) < length(r$fits)) paste0(" for ", join_and(vapply(unb, `[[`, "", "header"))) else "", ": ",
        switch(r$design,
          CRD = "the treatments have unequal numbers of replications. ",
          FCRD = "the treatment combinations have unequal numbers of replications. ",
          LSD = "a plot is missing from the Latin square. ",
          "a plot is missing from a block, or the treatments are unequally replicated. "),
        "Each mean is given its own standard error, and each pair of means its own SE(d).",
        if (any(vapply(unb, function(f) isTRUE(f$final$adjusted_ss), logical(1))))
          " The means are adjusted (least-squares) means, and each ANOVA term is tested after allowing for every other term (Type III)."
        else ""))) else NULL,
      ## Bartlett's test of the environments' error variances, for every response
      lapply(Filter(function(f) isTRUE(f$final$pooled) && !is.null(f$final$homogeneity), r$fits), function(f) {
        h <- f$final$homogeneity
        homog <- isTRUE(h$p >= r$alpha)
        div(class = if (homog) "sugbox" else "warn", HTML(sprintf(
          "<b>Homogeneity of error variances across environments (Bartlett)%s:</b> &chi;<sup>2</sup> = %s, df = %d, %s. %s",
          if (length(r$fits) > 1) paste0(", ", f$header) else "",
          fmt(h$chisq, 3), h$df, p_eq(h$p, r$alpha),
          if (homog)
            sprintf("There is insufficient evidence at the %s%% level that the environments' error variances differ, so pooling the errors is reasonable; a small difference may have gone undetected.", pct(r$alpha))
          else
            sprintf("The error variances are <b>heterogeneous</b> at the %s%% level. The pooled F-tests should be read with caution; consider a variance-stabilising transformation (see the Assumptions tab) or analysing the environments separately.", pct(r$alpha)))))
      }))
  })

  ## the note that leads from the main effects to the cell means
  inter_alert <- function(f) {
    t <- inter_alert_text(f$final)
    if (nzchar(t)) div(class = "warn", HTML(t)) else NULL
  }

  output$anovaOut <- renderUI({
    r <- ok()
    tagList(lapply(names(r$fits), function(nm) {
      f <- r$fits[[nm]]
      div(class = "slx-card",
          h3(class = "slx-card-h", anova_heading(f$header)),
          HTML(paste0("<div class='slx-scroll'>", df_html(anova_show(f$final$anova, r$alpha)), "</div>")),
          HTML(anova_note(f$final)),
          inter_alert(f),
          HTML(rd_html(anova_text(f$final, f$trans), "What the table shows.")))
    }))
  })

  ## ---------------------------------------------------- means and comparisons --
  output$cmpRespUI <- renderUI({
    r <- ok(); ch <- names(r$fits)
    keep <- intersect(isolate(input$cmpResp), ch)
    selectizeInput("cmpResp", "Response(s)", ch, selected = if (length(keep)) keep else ch, multiple = TRUE)
  })
  ## the analysis restricted to the responses chosen on this tab
  cmp_rr <- reactive({
    r <- ok()
    v <- intersect(input$cmpResp, names(r$fits))
    if (!length(v)) v <- names(r$fits)
    r$fits <- r$fits[v]
    r
  })

  output$phEffectUI <- renderUI({
    r <- ok(); ef <- r$fits[[1]]$final$effects
    keep <- isolate(input$phEff)
    selectInput("phEff", "Effect to compare in detail",
                stats::setNames(names(ef), vapply(ef, `[[`, "", "label")),
                selected = if (isTRUE(keep %in% names(ef))) keep else names(ef)[1])
  })
  ph_effect <- reactive({
    r <- ok(); ef <- r$fits[[1]]$final$effects
    if (isTRUE(input$phEff %in% names(ef))) input$phEff else names(ef)[1]
  })

  ## an interaction of a design with one error term may be compared within
  ## the levels of one of its factors (simple effects)
  output$phByUI <- renderUI({
    r <- ok(); e <- r$fits[[1]]$final$effects[[ph_effect()]]
    if (is.null(e) || length(e$vars) < 2) return(NULL)
    if (!is.null(e$slice))
      return(div(class = "note", sprintf("These means are compared within each level of %s, the only comparisons that share one error term.", e$slice)))
    if (!is.null(e$mix))
      return(div(class = "note", "Every pair of these means is compared, each with the SE(d) of the factors in which the two differ."))
    keep <- isolate(input$phBy %||% "")
    ch <- c("All the cell means together" = "", stats::setNames(e$vars, paste("Within each level of", e$vars)))
    radioButtons("phBy", "Compare the cell means", ch, selected = if (keep %in% ch) keep else "")
  })
  ph_by <- reactive({
    r <- ok(); e <- r$fits[[1]]$final$effects[[ph_effect()]]
    b <- input$phBy %||% ""
    if (nzchar(b) && !is.null(e) && b %in% e$vars && is.null(e$slice) && is.null(e$mix)) b else NULL
  })

  ## the control is chosen from the levels the comparisons run over; a level
  ## with no number (Control, Check) comes first in their natural order
  output$phControlUI <- renderUI({
    r <- ok(); e <- r$fits[[1]]$final$effects[[ph_effect()]]
    req(e)
    lv <- dunnett_levels(effect_by(e, ph_by()))
    keep <- isolate(input$phControl %||% "")
    selectInput("phControl", "Control", lv, selected = if (keep %in% lv) keep else lv[1])
  })

  ## The comparison chosen, as every table, plot and export follows it. Just
  ## after the effect changes the control can still name a level of the
  ## previous one; the first level stands in until the selector updates.
  cmp_now <- reactive({
    m <- input$phMethod %||% "none"
    if (!m %in% CMP_CHOICES) m <- "none"
    ctrl <- NULL
    if (identical(m, "Dunnett")) {
      r <- ok(); e <- r$fits[[1]]$final$effects[[ph_effect()]]
      lv <- dunnett_levels(effect_by(e, ph_by()))
      ctrl <- if (isTRUE(input$phControl %in% lv)) input$phControl else lv[1]
    }
    comparison(m, control = ctrl, alternative = input$phAlt %||% "two.sided", effect = ph_effect())
  })

  output$meansOut <- renderUI({
    HTML(means_section_html(cmp_rr(), digits = input$digits %||% 2, cmp = cmp_now(),
                            detailed = isTRUE(input$detailed), readings = TRUE))
  })

  output$phOut <- renderUI({
    r <- cmp_rr(); cmp <- cmp_now(); eff <- ph_effect()
    e <- r$fits[[1]]$final$effects[[eff]]
    head <- sprintf("<div class='ms-h'><span class='ms-badge'>D</span>Comparisons of %s</div>", esc(e$label))
    if (identical(cmp$kind, "none"))
      return(HTML(paste0("<div class='ms-section'>", head,
        "<div class='note'>Choose how to compare the means (left): the critical difference, a procedure that ",
        "gives letters, or Dunnett's test against a control. The tables above then show its result, and every ",
        "comparison is listed here.</div></div>")))
    HTML(paste0("<div class='ms-section'>", head,
                paste(vapply(names(r$fits), function(nm) cmp_effect_html(r$fits[[nm]], eff, cmp, ph_by()), ""), collapse = ""),
                "</div>"))
  })

  ## ------------------------------------------------------------------ plots --
  output$aRespUI3 <- renderUI({
    r <- ok(); keep <- isolate(input$aResp3)
    selectInput("aResp3", "Response variable", names(r$fits),
                selected = if (isTRUE(keep %in% names(r$fits))) keep else names(r$fits)[1])
  })
  gFit <- reactive(fit_of(ok(), input$aResp3))
  output$plEffectUI <- renderUI({
    ef <- gFit()$final$effects; keep <- isolate(input$plEff)
    selectInput("plEff", "Effect", stats::setNames(names(ef), vapply(ef, `[[`, "", "label")),
                selected = if (isTRUE(keep %in% names(ef))) keep else names(ef)[1])
  })

  ## for an interaction, which factor runs along the X-axis: the app suggests
  ## the time factor (or a quantitative one) and the user can change it
  output$plXUI <- renderUI({
    f <- gFit(); req(input$plEff)
    e <- f$final$effects[[input$plEff]]
    if (is.null(e) || length(e$vars) < 2) return(NULL)
    selectInput("plX", "Factor on the X-axis", e$vars, selected = default_x(e, f$final$data))
  })

  ## When the effect changes, input$plX still holds the previous effect's
  ## choice until the new selector reports back. Remember which effect the
  ## choice was made for, and use the default until the two agree, so a stale
  ## choice never draws the wrong axis.
  plx_for <- reactiveVal(NULL)
  observeEvent(input$plX, plx_for(isolate(input$plEff)))

  ## letters on a plot only from a procedure that gives letters, the one
  ## chosen on the Means & Comparisons tab
  output$plLettersUI <- renderUI({
    cmp <- cmp_now()
    if (identical(cmp$kind, "letters"))
      checkboxInput("plLetters", sprintf("Show the letters of %s", CMP_NAME[[cmp$method]]), isolate(input$plLetters) %||% TRUE)
    else div(class = "note", paste("Letters appear here when the means are compared by a procedure that gives letters",
                                   "(Tukey's HSD, for instance) on the Means & Comparisons tab."))
  })
  plot_letters <- function(f, eff) {
    cmp <- cmp_now()
    if (!identical(cmp$kind, "letters") || !isTRUE(input$plLetters)) return(NULL)
    cmp_letters(f$final, eff, cmp)$letters
  }

  mp <- reactive({
    f <- gFit(); req(input$plEff)
    validate(need(input$plEff %in% names(f$final$effects), "Choose an effect."))
    xv <- if (identical(plx_for(), input$plEff)) input$plX else NULL
    cmp <- cmp_now()
    plot_main(f$final, input$plEff, input$plType, plot_letters(f, input$plEff), xv,
              method = if (identical(cmp$kind, "letters")) CMP_NAME[[cmp$method]] else NULL)
  })
  plot_out("mainPlot", function() mp(), function() paste0("means-", input$plType %||% "bar"), function() gFit()$resp)
  output$mainPlotText <- renderUI({
    f <- gFit(); req(input$plEff %in% names(f$final$effects))
    xv <- if (identical(plx_for(), input$plEff)) input$plX else NULL
    lets <- if (is.null(plot_letters(f, input$plEff))) FALSE else CMP_NAME[[cmp_now()$method]]
    HTML(rd_note(main_plot_text(f$final, input$plEff, input$plType, lets, xv, input$digits %||% 2)))
  })

  ## the interaction plots: any effect of two or more factors
  output$ipRespUI <- renderUI({
    r <- ok(); keep <- isolate(input$ipResp)
    selectInput("ipResp", "Response variable", names(r$fits),
                selected = if (isTRUE(keep %in% names(r$fits))) keep else names(r$fits)[1])
  })
  iFit <- reactive(fit_of(ok(), input$ipResp))
  inter_effects <- reactive({
    ef <- iFit()$final$effects
    ef[vapply(ef, function(e) length(e$vars) >= 2, logical(1))]
  })
  output$ipEffectUI <- renderUI({
    ef <- inter_effects()
    if (!length(ef)) return(div(class = "note", "This design has no interaction of two or more factors to plot."))
    keep <- isolate(input$ipEff)
    selectInput("ipEff", "Interaction", stats::setNames(names(ef), vapply(ef, `[[`, "", "label")),
                selected = if (isTRUE(keep %in% names(ef))) keep else names(ef)[1])
  })
  ip_effect <- reactive({
    ef <- inter_effects(); req(length(ef) > 0)
    if (isTRUE(input$ipEff %in% names(ef))) input$ipEff else names(ef)[1]
  })
  output$ipXUI <- renderUI({
    f <- iFit(); e <- f$final$effects[[ip_effect()]]
    selectInput("ipX", "Factor on the X-axis", e$vars, selected = default_x(e, f$final$data))
  })
  ipx_for <- reactiveVal(NULL)
  observeEvent(input$ipX, ipx_for(isolate(ip_effect())))
  ip_x <- reactive(if (identical(ipx_for(), ip_effect())) input$ipX else NULL)
  plot_out("interPlot", function() {
    f <- iFit()
    plot_interaction(f$final, ip_effect(), ip_x(), isTRUE(input$ipCI))
  }, "interaction-plot", function() iFit()$resp)
  output$interPlotText <- renderUI({
    f <- iFit()
    HTML(rd_note(interaction_text(f$final, ip_effect(), ip_x(), isTRUE(input$ipCI), input$digits %||% 2)))
  })

  ## ---------------------------------------------------------- descriptives --
  ## The variables offered are the columns the data check reads as numbers.
  ## Columns that are usually labels written as numbers (Rep, Block, Plot) and
  ## columns already mapped as design factors start unselected.
  output$descUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) > 0, "No column holds numbers."))
    used <- tryCatch({ m <- map_partial(); unlist(m[setdiff(names(m), "response")]) },
                     error = function(e) character(0))
    def <- explore_defaults(num, used)
    ## the sidebar is drawn again when the data or the design mapping change;
    ## choices the user has made that still fit the data are kept
    grp <- group_choices(d)
    keep_vars <- isolate(intersect(input$descVars, num))
    keep_grp <- isolate(input$descGroup %||% "")
    keep_plot <- isolate(input$descPlotVar %||% "")
    tagList(
      selectizeInput("descVars", "Variables to summarise", num,
                     selected = if (length(keep_vars)) keep_vars else def, multiple = TRUE),
      selectInput("descGroup", "Summarise separately for each level of (optional)",
                  c("Nothing - all rows together" = "", grp),
                  selected = if (keep_grp %in% grp) keep_grp else ""),
      selectInput("descPlotVar", "Variable to plot", num,
                  selected = if (keep_plot %in% num) keep_plot else def[1]))
  })

  ## The choices as they apply to the data now loaded: just after new data
  ## arrive, the inputs can still name columns of the previous data until the
  ## sidebar is drawn again.
  desc_vars <- reactive({
    d <- rv$data; req(d)
    v <- intersect(input$descVars, chk()$numeric); req(length(v) > 0); v
  })
  desc_group <- reactive({
    g <- input$descGroup %||% ""
    if (nzchar(g) && g %in% group_choices(rv$data)) g else ""
  })

  ## a grouping column with no labels, and the like, give a message in place
  ## of the table rather than an R error
  desc_tab <- reactive({
    d <- rv$data; v <- desc_vars(); g <- desc_group()
    tryCatch(describe_data(d, v, g), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  output$descTab <- renderUI({
    d <- rv$data; tab <- desc_tab()
    HTML(paste0(describe_html(tab, d, desc_group()),
      "<div class='note'>N counts the values used; Missing counts empty cells and entries that are not ",
      "plain numbers. Q1 and Q3 are the quartiles (calculated as Excel's QUARTILE.INC does): a quarter of the ",
      "values lie below Q1 and a quarter above Q3. CV = 100 &times; SD / mean. Skewness (as Excel's SKEW ",
      "gives it) measures how lopsided the values are: about 0 when they spread evenly either side, positive ",
      "when the high values trail further out, negative when the low values do.</div>"))
  })

  ## the reading describes the table as it is shown: one figure per variable,
  ## or, when the table is split into groups, the groups
  output$descText <- renderUI({
    d <- rv$data; v <- desc_vars(); g <- desc_group(); desc_tab()
    ids <- row_ids(d)
    txt <- vapply(v, function(z)
      if (nzchar(g)) describe_group_text(z, d[[z]], group_labels(d, g), ids, g)
      else describe_text(z, d[[z]], ids), "")
    div(class = "box", HTML(paste0("<b>What the table shows.</b><ul>",
      paste0("<li>", esc(txt), "</li>", collapse = ""), "</ul>")))
  })

  desc_v <- reactive({ d <- rv$data; req(d, input$descPlotVar %in% chk()$numeric); input$descPlotVar })
  output$descPlotTitle <- renderText(paste("Distribution of", desc_v()))
  ## a plot that cannot be drawn says why, in the words its note would use
  desc_plot <- function(p, kind) {
    validate(need(!is.null(p), plot_text(kind, rv$data, desc_v(), desc_group())$reading))
    p
  }
  desc_note <- function(kind) renderUI({
    d <- rv$data; req(d)
    t <- plot_text(kind, d, desc_v(), desc_group())
    div(class = "note", HTML(if (nzchar(t$what))
      paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading))
      else esc(t$reading)))
  })
  plot_out("descHist", function() desc_plot(plot_hist(rv$data, desc_v()), "hist"), "histogram", function() desc_v())
  plot_out("descDens", function() desc_plot(plot_density(rv$data, desc_v()), "density"), "density", function() desc_v())
  plot_out("descBox", function() desc_plot(plot_box(rv$data, desc_v(), desc_group()), "box"), "box-plot", function() desc_v())
  plot_out("descQQ", function() desc_plot(plot_qq(rv$data, desc_v()), "qq"), "qq-plot", function() desc_v())
  output$descHistText <- desc_note("hist")
  output$descDensText <- desc_note("density")
  output$descBoxText  <- desc_note("box")
  output$descQQText   <- desc_note("qq")

  observeEvent(input$dl_desc, {
    tab <- tryCatch(desc_tab(), error = function(e) NULL); req(tab)
    save_browser(paste0("StatLabX_descriptives_", Sys.Date(), ".csv"), csv_string(tab), "text/csv")
  })

  ## ------------------------------------------------------------ correlation --
  ## One default for the variables and the scatter pair, as on the
  ## descriptives tab; choices that still fit the data are kept when the
  ## sidebar is drawn again.
  ## measurements only: a column that labels rows (Rep, Plot No) or is mapped
  ## as a design factor is never chosen for the user, even when that leaves
  ## fewer than two columns to start from
  cor_default <- reactive({
    num <- chk()$numeric
    used <- tryCatch({ m <- map_partial(); unlist(m[setdiff(names(m), "response")]) },
                     error = function(e) character(0))
    def <- setdiff(num, used)
    def[!is_id_name(def)]
  })
  output$corUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 2, "Correlation needs at least two columns of numbers."))
    keep <- isolate(intersect(input$corVars, num))
    selectizeInput("corVars", "Variables to correlate", num,
                   selected = if (length(keep) >= 2) keep else cor_default(), multiple = TRUE)
  })
  output$corPairUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric; req(length(num) >= 2)
    v <- isolate(intersect(input$corVars, num)); if (length(v) < 2) v <- cor_default()
    kx <- isolate(input$corX %||% ""); ky <- isolate(input$corY %||% "")
    ## the two axes always differ, whatever survives from earlier data, and
    ## neither falls back on a column the table leaves out
    x <- if (kx %in% num) kx else if (length(v)) v[1] else ""
    y <- if (ky %in% num && ky != x) ky else setdiff(v, x)[1]
    pick <- c("Choose a column" = "", num)
    tagList(tags$hr(), tags$b("Scatter plot of one pair"),
      selectInput("corX", "Across (X axis)", pick, selected = x),
      selectInput("corY", "Up (Y axis)", pick, selected = if (is.na(y)) "" else y))
  })

  cor_vars <- reactive({
    d <- rv$data; req(d)
    validate(need(length(chk()$numeric) >= 2, "Correlation needs at least two columns of numbers."))
    v <- intersect(input$corVars, chk()$numeric)
    validate(need(length(v) >= 2, if (length(v) == 1)
      sprintf("Only '%s' is chosen; choose a second variable to correlate.", v)
      else "Choose at least two variables to correlate."))
    v
  })
  cor_method <- reactive(input$corMethod %||% "pearson")
  cor_alpha <- reactive(as.numeric(input$corAlpha %||% "0.05"))
  cor_tab <- reactive(correlate_data(rv$data, cor_vars(), cor_method(), cor_alpha()))

  output$corMatrix <- renderUI({
    tab <- cor_tab(); m <- cor_method()
    HTML(paste0(cor_matrix_html(tab, cor_vars()), "<div class='note'>", esc(cor_key(cor_alpha())),
      sprintf(" Strength of %s: %s (Cohen's 0.1, 0.3 and 0.5 for r%s).", COR_SYMBOL[[m]], cor_bands(m),
              if (m == "pearson") "" else ", carried to this coefficient through its relation to r for normal data"),
      "</div>"))
  })
  output$corText <- renderUI(div(class = "box", HTML(paste0("<b>What the table shows.</b> ", esc(cor_text(cor_tab()))))))
  output$corPairs <- renderUI(HTML(cor_pairs_html(cor_tab())))
  plot_out("corHeat", function() plot_cor_heat(cor_tab(), cor_vars()), "correlation-heat-map", NULL)
  output$corHeatText <- renderUI({
    t <- cor_heat_text(cor_tab(), length(cor_vars()))
    div(class = "note", HTML(paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading))))
  })
  cor_xy <- reactive({
    num <- chk()$numeric
    req(input$corX %in% num, input$corY %in% num)
    c(input$corX, input$corY)
  })
  plot_out("corScatter", function() {
    xy <- cor_xy()
    p <- plot_cor_scatter(rv$data, xy[1], xy[2], cor_method(), cor_alpha())
    validate(need(!is.null(p), cor_scatter_text(rv$data, xy[1], xy[2], cor_method(), cor_alpha())$reading))
    p
  }, "scatter-plot", function() paste(cor_xy(), collapse = "-vs-"))
  output$corScatterText <- renderUI({
    xy <- cor_xy()
    t <- cor_scatter_text(rv$data, xy[1], xy[2], cor_method(), cor_alpha())
    div(class = "note", HTML(if (nzchar(t$what))
      paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading)) else esc(t$reading)))
  })

  observeEvent(input$dl_cor, {
    tab <- tryCatch(cor_tab(), error = function(e) NULL); req(tab)
    out <- tab; attr(out, "method") <- NULL; attr(out, "alpha") <- NULL
    out$Method <- COR_NAMES[[cor_method()]]; out$alpha <- cor_alpha()
    save_browser(paste0("StatLabX_correlation_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## ------------------------------------------------------------- regression --
  ## The response starts as the analysed response when it is a measurement
  ## column, else the last one (data are usually laid out with the response
  ## last: Nitrogen, Rain, Yield), and the predictor as the first; a column that
  ## labels rows (Rep, Plot No) or is mapped as a design factor is never
  ## chosen for the user. Choices that still fit the data are kept when the
  ## sidebar is drawn again.
  reg_defaults <- reactive({
    num <- chk()$numeric
    m <- tryCatch(map_partial(), error = function(e) list())
    used <- unlist(m[setdiff(names(m), "response")])
    def <- setdiff(num, used); def <- def[!is_id_name(def)]
    resp <- c(intersect(m$response, def), rev(def))[1]
    list(resp = if (is.na(resp)) "" else resp, pred = utils::head(setdiff(def, resp), 1))
  })
  output$regUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 2, "Regression needs at least two columns of numbers."))
    def <- reg_defaults()
    ky <- isolate(input$regY); kx <- isolate(input$regX)
    ## with no measurement column left (only Rep, Plot and the like), nothing
    ## is chosen for the user
    y <- if (isTRUE(ky %in% num)) ky else def$resp
    x <- intersect(kx, setdiff(num, y))
    if (!length(x)) x <- setdiff(def$pred, y)
    tagList(
      selectInput("regY", "Response (to be predicted)", c("Choose a column" = "", num), selected = y),
      selectizeInput("regX", "Predictors (one for a simple regression, more for a multiple one)",
                     setdiff(num, y), selected = x, multiple = TRUE))
  })
  ## a response chosen from among the predictors leaves them
  observeEvent(input$regY, {
    num <- chk()$numeric
    keep <- intersect(input$regX, setdiff(num, input$regY))
    updateSelectizeInput(session, "regX", choices = setdiff(num, input$regY), selected = keep)
  }, ignoreInit = TRUE)
  reg_res <- reactive({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 2, "Regression needs at least two columns of numbers."))
    y <- input$regY %||% ""
    validate(need(y %in% num, "Choose a response."))
    x <- intersect(input$regX, setdiff(num, y))
    validate(need(length(x) >= 1, "Choose at least one predictor."))
    r <- tryCatch(regress_data(d, y, x, as.numeric(input$regAlpha %||% "0.05")), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r)))
    r
  })
  output$regShowUI <- renderUI({
    r <- tryCatch(reg_res(), error = function(e) NULL)
    if (is.null(r) || r$k < 2) return(NULL)
    keep <- isolate(input$regShow)
    selectInput("regShow", "Predictor on the main plot", r$predictors,
                selected = if (isTRUE(keep %in% r$predictors)) keep else r$predictors[1])
  })
  reg_note <- function(t) div(class = "note", HTML(if (nzchar(t$what))
    paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading)) else esc(t$reading)))
  output$regEquation <- renderUI(div(class = "box", HTML(paste0("<b>Fitted equation.</b> ", esc(reg_res()$equation)))))
  output$regCoef  <- renderUI(HTML(reg_coef_html(reg_res())))
  output$regModel <- renderUI(HTML(reg_model_html(reg_res())))
  output$regText  <- renderUI(div(class = "box", HTML(paste0("<b>What the model shows.</b> ", esc(reg_text(reg_res()))))))
  output$regAnova <- renderUI(HTML(reg_anova_html(reg_res())))
  output$regChecks <- renderUI(HTML(reg_checks_html(reg_res())))
  plot_out("regMain", function() plot_reg_main(reg_res(), input$regShow), "regression", function() reg_res()$response)
  output$regMainText <- renderUI(reg_note(reg_main_text(reg_res(), input$regShow)))
  plot_out("regResid", function() {
    r <- reg_res(); p <- plot_reg_resid(r)
    validate(need(!is.null(p), reg_resid_text(r)$reading))
    p
  }, "regression-residuals", function() reg_res()$response)
  output$regResidText <- renderUI(reg_note(reg_resid_text(reg_res())))
  plot_out("regQQ", function() {
    r <- reg_res(); p <- plot_reg_qq(r)
    validate(need(!is.null(p), reg_qq_text(r)$reading))
    p
  }, "regression-qq-plot", function() reg_res()$response)
  output$regQQText <- renderUI(reg_note(reg_qq_text(reg_res())))
  plot_out("regObs", function() plot_reg_obs(reg_res()), "observed-vs-predicted", function() reg_res()$response)
  output$regObsText <- renderUI(reg_note(reg_obs_text(reg_res())))

  observeEvent(input$dl_reg, {
    r <- tryCatch(reg_res(), error = function(e) NULL); req(r)
    out <- r$coefficients
    out$Response <- r$response; out$alpha <- r$alpha
    save_browser(paste0("StatLabX_regression_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## -------------------------------------------------------- principal components --
  ## The variables start as the measurement columns (as on the correlation
  ## tab); the grouping for the plots starts as the treatment, or the first
  ## design factor, when one is mapped. Choices that still fit the data are
  ## kept when the sidebar is drawn again.
  output$pcaUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 2, "Principal components need at least two columns of numbers."))
    keep <- isolate(intersect(input$pcaVars, num))
    selectizeInput("pcaVars", "Variables", num, selected = if (length(keep) >= 2) keep else cor_default(), multiple = TRUE)
  })
  output$pcaGroupUI <- renderUI({
    d <- rv$data; req(d)
    grp <- group_choices(d)
    m <- tryCatch(map_partial(), error = function(e) list())
    ## intersect() gives NULL when nothing is mapped yet, so the first match
    ## is taken with a fallback rather than indexed
    first <- c(intersect(c(m$treat, m$factors, m$main), grp), "")[1]
    keep <- isolate(input$pcaGroup %||% NA)
    selectInput("pcaGroup", "Colour the points by (optional)", c("Nothing" = "", grp),
                selected = if (isTRUE(keep %in% c("", grp))) keep else first)
  })
  pca_vars <- reactive({
    d <- rv$data; req(d)
    validate(need(length(chk()$numeric) >= 2, "Principal components need at least two columns of numbers."))
    v <- intersect(input$pcaVars, chk()$numeric)
    validate(need(length(v) >= 2, if (length(v) == 1)
      sprintf("Only '%s' is chosen; choose at least two variables.", v) else "Choose at least two variables."))
    v
  })
  pca_res <- reactive({
    r <- tryCatch(pca_data(rv$data, pca_vars(), scale = !identical(input$pcaScale, "cov")), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r)))
    r
  })
  output$pcaAxesUI <- renderUI({
    p <- tryCatch(pca_res(), error = function(e) NULL)
    if (is.null(p) || p$m < 2) return(NULL)
    ch <- stats::setNames(seq_len(p$m), paste0("PC", seq_len(p$m)))
    kx <- isolate(as.integer(input$pcaX %||% 1)); ky <- isolate(as.integer(input$pcaY %||% 2))
    ax <- pca_axes(p, kx, ky)
    tagList(selectInput("pcaX", "Plot across", ch, selected = ax[1]),
            selectInput("pcaY", "Plot up", ch, selected = ax[2]))
  })
  pca_ax <- reactive({
    p <- pca_res()
    pca_axes(p, suppressWarnings(as.integer(input$pcaX %||% 1)), suppressWarnings(as.integer(input$pcaY %||% 2)))
  })
  ## the grouping for the rows used, or none
  pca_grp <- reactive({
    p <- pca_res(); g <- input$pcaGroup %||% ""
    if (!nzchar(g) || !g %in% names(rv$data)) return(NULL)
    lab <- trimws(as.character(rv$data[[g]]))[p$pos]
    lab[is.na(lab) | lab == ""] <- "(no label)"
    factor(lab, levels = natural_levels(lab))
  })
  pca_note <- function(t) div(class = "note", HTML(if (nzchar(t$what))
    paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading)) else esc(t$reading)))
  output$pcaEigen <- renderUI(HTML(pca_eigen_html(pca_res())))
  output$pcaText <- renderUI(div(class = "box", HTML(paste0("<b>What the analysis shows.</b> ", esc(pca_text(pca_res()))))))
  output$pcaLoadings <- renderUI(HTML(pca_loadings_html(pca_res())))
  plot_out("pcaScree", function() plot_pca_scree(pca_res()), "pca-scree-plot", NULL)
  output$pcaScreeText <- renderUI(pca_note(pca_scree_text(pca_res())))
  plot_out("pcaScores", function() {
    p <- pca_res(); validate(need(p$m >= 2, "Only one component has any variance, so there is nothing to plot against it."))
    plot_pca_scores(p, pca_ax(), pca_grp())
  }, "pca-scores", NULL)
  output$pcaScoresText <- renderUI({
    p <- pca_res(); req(p$m >= 2)
    pca_note(pca_scores_text(p, pca_ax(), pca_grp(), input$pcaGroup))
  })
  plot_out("pcaLoadPlot", function() { p <- pca_res(); req(p$m >= 2); plot_pca_loadings(p, pca_ax()) }, "pca-loadings", NULL)
  output$pcaLoadText <- renderUI({ p <- pca_res(); req(p$m >= 2); pca_note(pca_loadings_text(p, pca_ax())) })
  plot_out("pcaBiplot", function() { p <- pca_res(); req(p$m >= 2); plot_pca_biplot(p, pca_ax(), pca_grp()) }, "pca-biplot", NULL)
  output$pcaBiplotText <- renderUI({ p <- pca_res(); req(p$m >= 2); pca_note(pca_biplot_text(p, pca_ax())) })
  output$pcaScoreTable <- renderUI(HTML(pca_scores_html(pca_res())))
  observeEvent(input$dl_pca_load, {
    p <- tryCatch(pca_res(), error = function(e) NULL); req(p)
    out <- data.frame(Variable = p$vars, round(p$loadings, 6), check.names = FALSE)
    names(out)[-1] <- paste0(colnames(p$loadings), "_loading")
    out <- cbind(out, stats::setNames(as.data.frame(round(p$vectors, 6)), paste0(colnames(p$vectors), "_eigenvector")))
    save_browser(paste0("StatLabX_pca_loadings_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })
  observeEvent(input$dl_pca_scores, {
    p <- tryCatch(pca_res(), error = function(e) NULL); req(p)
    out <- data.frame(Row = p$rows, round(p$scores, 6), check.names = FALSE)
    save_browser(paste0("StatLabX_pca_scores_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## ----------------------------------------------------------- cluster analysis --
  ## The variables start as the measurement columns; the items start as the
  ## means of the mapped treatment (or first design factor), else the rows.
  ## Choices that still fit the data are kept when the sidebar is drawn again.
  output$clUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 2, "Cluster analysis needs at least two columns of numbers."))
    keep <- isolate(intersect(input$clVars, num))
    selectizeInput("clVars", "Variables", num, selected = if (length(keep) >= 2) keep else cor_default(), multiple = TRUE)
  })
  output$clGroupUI <- renderUI({
    d <- rv$data; req(d)
    grp <- group_choices(d)
    m <- tryCatch(map_partial(), error = function(e) list())
    first <- c(intersect(c(m$treat, m$factors, m$main), grp), "")[1]
    keep <- isolate(input$clGroup %||% NA)
    selectInput("clGroup", "Items to cluster", c("Each row" = "", stats::setNames(grp, paste("The means of each level of", grp))),
                selected = if (isTRUE(keep %in% c("", grp))) keep else first)
  })
  cl_vars <- reactive({
    d <- rv$data; req(d)
    validate(need(length(chk()$numeric) >= 2, "Cluster analysis needs at least two columns of numbers."))
    v <- intersect(input$clVars, chk()$numeric)
    validate(need(length(v) >= 2, if (length(v) == 1)
      sprintf("Only '%s' is chosen; choose at least two variables.", v) else "Choose at least two variables."))
    v
  })
  cl_args <- reactive({
    link <- input$clLink %||% "ward"
    g <- input$clGroup %||% ""
    list(d = rv$data, vars = cl_vars(), group = if (nzchar(g) && g %in% names(rv$data)) g else NULL,
         distance = if (identical(link, "ward")) "euclidean" else input$clDist %||% "euclidean",
         linkage = link, scale = !isFALSE(input$clScale))
  })
  cl_run <- function(k) {
    r <- tryCatch(do.call(cluster_data, c(cl_args(), list(k = k))), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r)))
    r
  }
  ## the suggested number of clusters, and the result for the number chosen
  cl_auto <- reactive(cl_run(NULL))
  cl_res <- reactive({
    k <- input$clK %||% "auto"
    if (identical(k, "auto") || !k %in% as.character(2:(cl_auto()$n - 1))) cl_auto() else cl_run(as.integer(k))
  })
  output$clKUI <- renderUI({
    a <- tryCatch(cl_auto(), error = function(e) NULL)
    if (is.null(a)) return(NULL)
    ks <- as.character(2:min(a$n - 1, 20))
    ch <- c(stats::setNames("auto", sprintf("Suggested (%d)", a$suggested)), stats::setNames(ks, ks))
    keep <- isolate(input$clK %||% "auto")
    selectInput("clK", "Number of clusters", ch, selected = if (keep %in% ch) keep else "auto")
  })
  cl_note <- function(t) div(class = "note", HTML(if (nzchar(t$what))
    paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading)) else esc(t$reading)))
  output$clText <- renderUI(div(class = "box", HTML(paste0("<b>What the clustering shows.</b> ", esc(cl_text(cl_res()))))))
  output$clMembers <- renderUI(HTML(cl_members_html(cl_res())))
  output$clMeans <- renderUI(HTML(cl_means_html(cl_res())))
  plot_out("clDendro", function() plot_cl_dendro(cl_res()), "dendrogram", NULL)
  output$clDendroText <- renderUI(cl_note(cl_dendro_text(cl_res())))
  plot_out("clPcs", function() plot_cl_pcs(cl_res()), "clusters-on-components", NULL)
  output$clPcsText <- renderUI(cl_note(cl_pcs_text(cl_res())))
  plot_out("clSil", function() plot_cl_silhouette(cl_res()), "silhouette", NULL)
  output$clSilText <- renderUI(cl_note(cl_silhouette_text(cl_res())))
  output$clItems <- renderUI(HTML(cl_items_html(cl_res())))
  observeEvent(input$dl_cl, {
    cl <- tryCatch(cl_res(), error = function(e) NULL); req(cl)
    out <- data.frame(Item = cl$items, Cluster = cl$cluster, Silhouette = round(cl$silhouette, 6))
    names(out)[1] <- if (is.null(cl$group)) "Row" else cl$group
    save_browser(paste0("StatLabX_clusters_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## ------------------------------------------------------------ factor analysis --
  ## The variables start as the measurement columns; the number of factors as
  ## parallel analysis suggests. Choices that still fit the data are kept.
  output$faUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) >= 3, "Factor analysis needs at least three columns of numbers."))
    keep <- isolate(intersect(input$faVars, num))
    selectizeInput("faVars", "Variables", num, selected = if (length(keep) >= 3) keep else cor_default(), multiple = TRUE)
  })
  fa_vars <- reactive({
    d <- rv$data; req(d)
    validate(need(length(chk()$numeric) >= 3, "Factor analysis needs at least three columns of numbers."))
    v <- intersect(input$faVars, chk()$numeric)
    validate(need(length(v) >= 3, sprintf("%s; factor analysis needs at least three variables.",
      if (length(v) == 0) "No variable is chosen" else sprintf("Only %s %s chosen", join_and(sprintf("'%s'", v)), pl(length(v), "is", "are")))))
    v
  })
  fa_run <- function(k) {
    r <- tryCatch(fa_data(rv$data, fa_vars(), factors = k, rotation = input$faRot %||% "varimax",
                          alpha = as.numeric(input$faAlpha %||% "0.05")), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r)))
    r
  }
  ## the suggested number of factors, and the result for the number chosen
  fa_auto <- reactive(fa_run(NULL))
  fa_res <- reactive({
    k <- input$faK %||% "auto"
    if (identical(k, "auto") || !k %in% as.character(seq_len(fa_auto()$mmax))) fa_auto() else fa_run(as.integer(k))
  })
  output$faKUI <- renderUI({
    a <- tryCatch(fa_auto(), error = function(e) NULL)
    if (is.null(a)) return(NULL)
    ks <- as.character(seq_len(a$mmax))
    ch <- c(stats::setNames("auto", sprintf("Suggested (%d)", a$suggested)), stats::setNames(ks, ks))
    keep <- isolate(input$faK %||% "auto")
    selectInput("faK", "Number of factors", ch, selected = if (keep %in% ch) keep else "auto")
  })
  output$faAxesUI <- renderUI({
    f <- tryCatch(fa_res(), error = function(e) NULL)
    if (is.null(f) || f$factors < 3) return(NULL)
    ch <- stats::setNames(seq_len(f$factors), colnames(f$loadings))
    kx <- isolate(as.integer(input$faX %||% 1)); ky <- isolate(as.integer(input$faY %||% 2))
    tagList(selectInput("faX", "Loading plot across", ch, selected = if (isTRUE(kx %in% ch)) kx else 1),
            selectInput("faY", "Loading plot up", ch, selected = if (isTRUE(ky %in% ch)) ky else 2))
  })
  fa_ax <- reactive({
    f <- fa_res()
    a <- suppressWarnings(as.integer(input$faX %||% 1)); b <- suppressWarnings(as.integer(input$faY %||% 2))
    okk <- function(v) length(v) == 1 && !is.na(v) && v >= 1 && v <= f$factors
    a <- if (okk(a)) a else 1L
    b <- if (okk(b) && b != a) b else if (a == 1L) 2L else 1L
    c(a, b)
  })
  fa_note <- function(t) div(class = "note", HTML(paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading))))
  output$faText <- renderUI(div(class = "box", HTML(paste0("<b>What the analysis shows.</b> ", esc(fa_text(fa_res()))))))
  output$faAdequacy <- renderUI(HTML(fa_adequacy_html(fa_res())))
  output$faLoadings <- renderUI(HTML(fa_loadings_html(fa_res())))
  output$faVariance <- renderUI(HTML(fa_variance_html(fa_res())))
  output$faPhi <- renderUI(HTML(fa_phi_html(fa_res())))
  plot_out("faScree", function() plot_fa_scree(fa_res()), "factor-scree-plot", NULL)
  output$faScreeText <- renderUI(fa_note(fa_scree_text(fa_res())))
  plot_out("faHeat", function() plot_fa_heat(fa_res()), "factor-loadings-heat-map", NULL)
  output$faHeatText <- renderUI(fa_note(fa_heat_text(fa_res())))
  plot_out("faLoadPlot", function() {
    f <- fa_res(); validate(need(f$factors >= 2, "With one factor there is no second axis to plot the loadings against."))
    plot_fa_loadings(f, fa_ax())
  }, "factor-loadings", NULL)
  output$faLoadText <- renderUI({ f <- fa_res(); req(f$factors >= 2); fa_note(fa_loadings_text(f, fa_ax())) })
  output$faScores <- renderUI(HTML(fa_scores_html(fa_res())))
  observeEvent(input$dl_fa_load, {
    f <- tryCatch(fa_res(), error = function(e) NULL); req(f)
    out <- data.frame(Variable = f$vars, round(f$loadings, 6), Communality = round(f$communality, 6),
                      Uniqueness = round(f$uniqueness, 6), check.names = FALSE)
    save_browser(paste0("StatLabX_factor_loadings_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })
  observeEvent(input$dl_fa_scores, {
    f <- tryCatch(fa_res(), error = function(e) NULL); req(f)
    out <- data.frame(Row = f$rows, round(f$scores, 6), check.names = FALSE)
    save_browser(paste0("StatLabX_factor_scores_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })


  ## -------------------------------------------------------- interpretation --
  output$interpOut <- renderUI({
    r <- ok(); cmp <- cmp_now()
    HTML(paste(vapply(names(r$fits), function(nm) {
      f <- r$fits[[nm]]
      paste0("<div class='slx-card'><h3 class='slx-card-h'>", esc(f$header), "</h3>",
             as.character(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab, cmp)), "</div>")
    }, character(1)), collapse = ""))
  })

  ## A PDF is offered for download only where a renderer can make one; in the
  ## browser, and wherever none is installed, the print dialog makes the PDF.
  output$pdfBtn <- renderUI({
    eng <- pdf_engine()
    if (is.na(eng)) return(NULL)
    downloadButton("dl_pdf", sprintf("Download PDF (via %s)", eng), class = "btn-default")
  })

  ## the report prints the means to the decimals and with the comparison
  ## chosen on the Means & Comparisons tab
  report <- reactive({
    r <- ok()
    nat <- vapply(names(r$fits), function(v) rv$nature[[v]] %||% "auto", character(1))
    build_report(r, comparison = cmp_now(), detailed = isTRUE(input$detailed),
                 digits = input$digits %||% 2, nature = nat)
  })

  ## The report is handed to the browser (a JavaScript Blob), not through
  ## downloadHandler, which behaves the same on a Shiny server and in the
  ## browser build, where downloadHandler's shim is unreliable. Printing goes
  ## through a hidden frame, so no file needs saving first.
  observeEvent(input$dl_html, {
    save_browser(paste0("StatLabX_report_", Sys.Date(), ".html"), report(), "text/html")
  })
  observeEvent(input$print_report, {
    session$sendCustomMessage("doepro_print", list(html = report()))
  })

  output$dl_pdf <- downloadHandler(
    filename = function() paste0("StatLabX_report_", Sys.Date(), ".pdf"),
    content = function(f) {
      okp <- withProgress(message = "Compiling the PDF", value = 0.5, save_pdf(report(), f))
      if (!isTRUE(okp))
        stop("The PDF could not be compiled. Use Print / Save as PDF instead.")
    })

  ## -------------------------------------------------------------- downloads --
  observeEvent(input$dl_anova, {
    r <- ok()
    out <- do.call(rbind, lapply(names(r$fits), function(nm) {
      a <- r$fits[[nm]]$final$anova
      data.frame(Response = r$fits[[nm]]$header, a, Signif = star(a$p, r$alpha),
                 alpha = r$alpha, row.names = NULL, check.names = FALSE)
    }))
    save_browser(paste0("StatLabX_anova_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## One row per mean; the columns follow the comparison chosen (means_csv())
  observeEvent(input$dl_means, {
    save_browser(paste0("StatLabX_means_", Sys.Date(), ".csv"), csv_string(means_csv(cmp_rr(), cmp_now())), "text/csv")
  })

  ## the comparisons of the effect chosen, response by response, saying which
  ## procedure and level produced them
  ph_tables <- function(tab) {
    r <- cmp_rr(); cmp <- cmp_now(); eff <- ph_effect()
    if (identical(cmp$kind, "none")) {
      showNotification("Choose how to compare the means first.", type = "warning")
      return(NULL)
    }
    out <- lapply(names(r$fits), function(nm) {
      f <- r$fits[[nm]]$final
      x <- tryCatch(gate_posthoc(posthoc(f, eff, cmp$method, f$alpha, control = cmp$control,
                                         alternative = cmp$alternative, by = ph_by())), error = function(e) NULL)
      if (is.null(x)) return(NULL)
      g <- x[[tab]]
      if (identical(tab, "groups") && identical(cmp$kind, "cd")) g$Group <- NULL
      cbind(Response = r$fits[[nm]]$header, Effect = f$effects[[eff]]$label, Method = x$method, alpha = x$alpha, g)
    })
    out <- Filter(Negate(is.null), out)
    if (!length(out)) return(NULL)
    do.call(rbind, out)
  }
  observeEvent(input$dl_ph, {
    tab <- ph_tables("groups"); req(tab)
    save_browser(paste0("StatLabX_comparisons_", Sys.Date(), ".csv"), csv_string(tab), "text/csv")
  })
  observeEvent(input$dl_ph_pairs, {
    tab <- ph_tables("pairs"); req(tab)
    save_browser(paste0("StatLabX_pairwise_", Sys.Date(), ".csv"), csv_string(tab), "text/csv")
  })

  ## ------------------------------------------------------------------ about --
  output$about <- renderUI({
    author_li <- paste(vapply(AUTHORS, function(a) {
      orc <- if (!is.null(a$orcid) && !is.na(a$orcid))
        sprintf(" &nbsp;<a href='https://orcid.org/%s' target='_blank'>%s</a>", a$orcid, a$orcid) else ""
      eml <- if (!is.null(a$email) && !is.na(a$email))
        sprintf("<br><a href='mailto:%s'>%s</a>", a$email, a$email) else ""
      sprintf("<li><b>%s</b>%s<br>%s, %s%s%s</li>", a$name, if (isTRUE(a$maintainer)) " <i>(maintainer)</i>" else "",
              a$role, a$aff, orc, eml)
    }, character(1)), collapse = "")

    ## counts as words, as running text writes them
    words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
    n_des <- length(DESIGNS); n_ph <- length(PH_METHODS)
    HTML(sprintf("
<div class='slx-hero'>
  <img class='slx-hero-logo' src='%s' alt='StatLabX'>
  <span class='slx-chip'><span class='slx-dot'></span>Free and open source, with R on your own computer</span>
  <h1 class='slx-h1'>Designed for the <span class='slx-grad'>field</span></h1>
  <p class='slx-lead'>Analysis of variance for %s experimental designs, with means, critical differences and
  multiple comparisons, and regression, correlation, principal components, cluster and factor analysis.
  R runs on your own computer, so your data never leave it.</p>
  <div class='slx-chips'>
    <span class='slx-chip'>%d designs</span>
    <span class='slx-chip'>%d comparison procedures</span>
    <span class='slx-chip'>Checked against Gomez &amp; Gomez (1984)</span>
    <span class='slx-chip'>Readings in plain language</span>
    <span class='slx-chip'>Powered by the %s R package, v%s</span>
  </div>
</div>

<div class='slx-section'>
<h3>About StatLabX</h3>
<p>A free and open tool for the analysis of designed agricultural experiments. It brings the
standard analyses used in field and horticultural research together in one accessible
interface, and serves as a free, self-contained option for the kind of analysis researchers
carry out in tools such as OPSTAT. It is the browser version of the %s package for R.</p>
</div>

<div class='slx-section'>
<h3>Developed by</h3>
<ol>%s</ol>
</div>

<div class='slx-section'>
<h3>Feedback and correspondence</h3>
<p>Suggestions, bug reports and relevant correspondence are welcome. Please write to:</p>
<ul>
<li><b>Dr. Immad A. Shah</b> &mdash; <a href='mailto:immad11w@skuastkashmir.ac.in'>immad11w@skuastkashmir.ac.in</a></li>
<li><b>Mr. Uzair Javid Khan</b> <i>(maintainer)</i> &mdash; <a href='mailto:uzairkhan11w@gmail.com'>uzairkhan11w@gmail.com</a></li>
</ul>
<p>You may also open an issue in the project repository.</p>
</div>

<div class='slx-section'>
<h3>How to cite</h3>
<div class='box'>%s (%s).
<i>%s: analysis of designed agricultural experiments.</i> Version %s. Zenodo.
doi:<a href='https://doi.org/%s' target='_blank'>%s</a></div>
<p class='note'>This is the concept DOI: it always resolves to the most recent release.</p>
</div>

<div class='slx-section'>
<h3>Licence and source</h3>
<p>Released under the GPL-3 licence. Source code and issue tracker:
<a href='https://github.com/Uzairkhan11w/DOEpro' target='_blank'>github.com/Uzairkhan11w/DOEpro</a>.
Run it in your browser at <a href='%s' target='_blank'>%s</a>.
Every release is archived on Zenodo and carries a DOI.</p>
</div>

<div class='slx-section'>
<h3>Statistical methods</h3>
<p>The analysis of variance is fitted with <code>stats::aov</code>, using
<code>Error(Rep/Main)</code> for split plots and <code>Error(Rep/(A+B))</code> for strip
plots. Levene's test, the Box-Cox profile likelihood, Tukey's test of additivity and all %s
comparison procedures, Dunnett's critical values among them, are implemented directly in the package,
so it depends only on <b>shiny</b>, <b>DT</b>, <b>ggplot2</b> and <b>rlang</b> (with
<b>bslib</b>, which shiny itself uses).</p>
<p class='note'>%s</p>
</div>",
      LOGO_URI, words[n_des], n_des, n_ph, APP_NAME, APP_VERSION, APP_NAME, author_li,
      CITE_AUTHORS, format(Sys.Date(), "%Y"), APP_NAME, APP_VERSION, APP_DOI, APP_DOI,
      APP_URL, APP_URL, words[n_ph], CREDIT_LONG))
  })
}
