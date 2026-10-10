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

  rv <- reactiveValues(data = NULL)

  ## ------------------------------------------------------------ data input --
  observeEvent(input$load, {
    txt <- input$paste
    if (!nz(txt)) { showNotification("Nothing pasted.", type = "warning"); return() }
    d <- tryCatch(read_pasted(txt, input$sep, input$header), error = function(e) NULL)
    if (is.null(d) || !ncol(d))
      showNotification("Could not read the pasted text - check the separator.", type = "error")
    else { rv$data <- d; rv$dq_log <- NULL; showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  observeEvent(input$file, {
    d <- tryCatch(read_upload(input$file$datapath), error = function(e) conditionMessage(e))
    if (is.character(d)) showNotification(paste("Could not read that file.", d), type = "error",
                                          duration = NULL)
    else { rv$data <- d; rv$dq_log <- NULL; showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  observeEvent(input$loaddemo, {
    rv$data <- demo_data(input$demo); rv$dq_log <- NULL
    des <- switch(input$demo, POOLFACT = "POOLFRCBD", POOLFACTC = "POOLFCRD", input$demo)
    updateSelectInput(session, "design", selected = des)
    showNotification(paste("Loaded the", input$demo, "example."), type = "message")
  })

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
      if (r$kind %in% c("text", "ambiguous", "unit") || (r$kind == "mark")) {
        msg <- if (r$kind == "ambiguous")
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
  output$mapUI <- renderUI({
    d <- rv$data; req(d)
    cn  <- names(d)
    ## columns of numbers as the data check sees them, so a response still
    ## written with decimal commas or "-" is suggested all the same
    num <- intersect(cn, chk()$numeric)
    des <- input$design
    nf  <- input$nfac %||% 2

    guess_resp <- if (length(num)) utils::tail(num, 1) else utils::tail(cn, 1)
    oth <- setdiff(cn, guess_resp)
    p <- function(i) if (length(oth) >= i) oth[i] else cn[1]

    tagList(
      selectizeInput("resp", "Response variable(s)", cn, selected = guess_resp,
                     multiple = TRUE, options = list(placeholder = "choose one or more")),
      switch(des,
        CRD  = selectInput("treat", "Treatment", cn, selected = p(1)),
        RCBD = tagList(selectInput("block", "Block", cn, selected = p(1)),
                       selectInput("treat", "Treatment", cn, selected = p(2))),
        LSD  = tagList(selectInput("row", "Row", cn, selected = p(1)),
                       selectInput("col", "Column", cn, selected = p(2)),
                       selectInput("treat", "Treatment", cn, selected = p(3))),
        FCRD = tagList(lapply(seq_len(nf), function(i)
                 selectInput(paste0("f", i), paste("Factor", LETTERS[i]), cn, selected = p(i)))),
        FRCBD = tagList(selectInput("block", "Block", cn, selected = p(1)),
                 lapply(seq_len(nf), function(i)
                   selectInput(paste0("f", i), paste("Factor", LETTERS[i]), cn, selected = p(i + 1)))),
        SPLIT = tagList(selectInput("rep", "Replication", cn, selected = p(1)),
                        selectInput("main", "Main-plot factor", cn, selected = p(2)),
                        selectInput("sub", "Sub-plot factor", cn, selected = p(3))),
        STRIP = tagList(selectInput("rep", "Replication", cn, selected = p(1)),
                        selectInput("main", "Factor A (horizontal strips)", cn, selected = p(2)),
                        selectInput("sub", "Factor B (vertical strips)", cn, selected = p(3))),
        POOLRCBD = tagList(
          selectInput("env", "Environment (location / year / season)", cn, selected = p(1)),
          selectInput("rep", "Replication / block (within environment)", cn, selected = p(2)),
          selectInput("treat", "Treatment", cn, selected = p(3))),
        POOLCRD = tagList(
          selectInput("env", "Environment (location / year / season)", cn, selected = p(1)),
          selectInput("treat", "Treatment", cn, selected = p(2))),
        POOLFRCBD = tagList(
          selectInput("env", "Environment (location / year / season)", cn, selected = p(1)),
          selectInput("rep", "Replication / block (within environment)", cn, selected = p(2)),
          lapply(seq_len(nf), function(i)
            selectInput(paste0("f", i), paste("Treatment factor", LETTERS[i]), cn, selected = p(i + 2)))),
        POOLFCRD = tagList(
          selectInput("env", "Environment (location / year / season)", cn, selected = p(1)),
          lapply(seq_len(nf), function(i)
            selectInput(paste0("f", i), paste("Treatment factor", LETTERS[i]), cn, selected = p(i + 1))))))
  })

  mapping <- reactive({
    des <- input$design; req(input$resp)
    nf <- input$nfac %||% 2
    m <- list(response = input$resp)
    if (des %in% c("CRD", "RCBD", "LSD")) { req(input$treat); m$treat <- input$treat }
    if (des %in% c("RCBD", "FRCBD"))      { req(input$block); m$block <- input$block }
    if (des == "LSD") { req(input$row, input$col); m$row <- input$row; m$col <- input$col }
    if (des %in% c("FCRD", "FRCBD")) {
      fs <- unlist(lapply(seq_len(nf), function(i) input[[paste0("f", i)]]))
      req(length(fs) == nf); m$factors <- fs
    }
    if (des %in% c("SPLIT", "STRIP")) {
      req(input$rep, input$main, input$sub)
      m$rep <- input$rep; m$main <- input$main; m$sub <- input$sub
    }
    if (des == "POOLRCBD") {
      req(input$env, input$rep, input$treat)
      m$env <- input$env; m$rep <- input$rep; m$treat <- input$treat
    }
    if (des == "POOLCRD") {
      req(input$env, input$treat)
      m$env <- input$env; m$treat <- input$treat
    }
    if (des %in% c("POOLFRCBD", "POOLFCRD")) {
      req(input$env)
      m$env <- input$env
      if (des == "POOLFRCBD") { req(input$rep); m$rep <- input$rep }
      fs <- unlist(lapply(seq_len(nf), function(i) input[[paste0("f", i)]]))
      req(length(fs) == nf)
      m$factors <- fs
    }
    req(all(nzchar(unlist(m))))
    if (anyDuplicated(unlist(m))) return(NULL)
    m
  })

  ## ------------------------------------------- automatic screening / advice --
  scan_tab <- reactive({
    d <- rv$data; req(d)
    m <- mapping(); req(!is.null(m))
    used <- unlist(m[setdiff(names(m), "response")])
    cand <- intersect(names(d), chk()$numeric)
    cand <- cand[vapply(cand, function(v) length(unique(stats::na.omit(read_entries(d[[v]])$value))) > 2, logical(1))]
    cand <- setdiff(cand, used)
    req(length(cand) > 0)
    withProgress(message = "Screening the response variables", value = 0.5,
      auto_scan(d, input$design, m, cand, as.numeric(input$alpha), input$dtype %||% "auto"))
  })

  output$scanTab <- renderDT({
    s <- scan_tab()
    validate(need(!is.null(s), "No numeric response columns to screen."))
    datatable(s[, setdiff(names(s), "Why")], rownames = FALSE,
              options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })

  output$scanNote <- renderUI({
    s <- tryCatch(scan_tab(), error = function(e) NULL)
    if (is.null(s)) return(NULL)
    div(class = "sugbox", HTML(paste0("<b>Advice</b><ul>",
      paste(sprintf("<li><b>%s</b> &rarr; <b>%s</b>. %s</li>",
                    s$Variable, s$`Suggested transformation`, s$Why), collapse = ""),
      "</ul>")))
  })

  ## per-response suggestion, used to pre-set the transformation selectors
  sugs <- reactive({
    d <- rv$data; m <- mapping(); req(d, !is.null(m))
    stats::setNames(lapply(m$response, function(v) {
      tryCatch({
        r <- analyze(d, input$design, modifyList(m, list(response = v)),
                     as.numeric(input$alpha))
        suggest_transform(r, check_assumptions(r), input$dtype %||% "auto")
      }, error = function(e) list(method = "none", optional = FALSE, why = ""))
    }), m$response)
  })

  output$transUI <- renderUI({
    m <- mapping(); req(!is.null(m))
    sg <- tryCatch(sugs(), error = function(e) NULL)
    tagList(lapply(seq_along(m$response), function(i) {
      v <- m$response[i]
      s <- if (!is.null(sg)) sg[[v]] else list(method = "none", optional = FALSE)
      opt <- isTRUE(s$optional)
      lab <- sprintf("%s %s", v,
        if (identical(s$method, "none")) "<span class='note'>(no transformation needed)</span>"
        else sprintf("<span class='note'>(suggested: %s%s)</span>",
                     TRANS[[s$method]]$lab, if (opt) ", optional" else ""))
      selectInput(paste0("tr_", i), HTML(lab), trans_choices(),
                  selected = if (opt) "none" else s$method)
    }))
  })

  observeEvent(input$applysug, {
    m <- mapping(); req(!is.null(m)); sg <- sugs()
    for (i in seq_along(m$response))
      updateSelectInput(session, paste0("tr_", i), selected = sg[[m$response[i]]]$method)
  })

  ## --------------------------------------------------------------- analysis --
  res <- eventReactive(input$run, {
    req(input$run > 0)
    d <- rv$data; req(d)
    m <- mapping()
    if (is.null(m)) return(list(err = "The same column is mapped to two different roles."))
    tr <- stats::setNames(lapply(seq_along(m$response), function(i)
      input[[paste0("tr_", i)]] %||% "none"), m$response)
    tryCatch(
      withProgress(message = "Running the analysis", value = 0.5,
        run_all(d, input$design, m, m$response, as.numeric(input$alpha), tr,
                input$dtype %||% "auto")),
      error = function(e) list(err = conditionMessage(e)))
  })

  ok <- reactive({
    r <- res()
    validate(need(is.null(r$err), r$err))
    r
  })

  output$runNote <- renderUI({
    r <- res()
    if (!is.null(r$err)) return(div(class = "err", r$err,
      if (grepl("left out of this analysis", r$err, fixed = TRUE) &&
          grepl("decimal comma|not a number|% sign|unit after|thousands separator|read two ways", r$err))
        p("The data check on the Data tab lists these entries and corrects those it safely can; correct the others in the table, then run the analysis again.") else NULL))
    f1 <- r$fits[[1]]$final
    tagList(
      div(class = "box", HTML(sprintf(
        "<b>%s</b> &nbsp;|&nbsp; %d response variable(s) &nbsp;|&nbsp; %d observations &nbsp;|&nbsp; %s",
        names(DESIGNS)[match(r$design, DESIGNS)], length(r$fits), nrow(f1$data),
        paste(sprintf("%s = %s", names(f1$cv), fmt(f1$cv, 2)), collapse = " | ")))),
      ## rows left out, response by response, so no plot disappears unnoticed
      {
        ex <- Filter(nzchar, vapply(r$fits, function(f) {
          t <- excluded_text(f$final$excluded, html = TRUE, hint = TRUE)
          if (nzchar(t)) sprintf("<b>%s:</b> %s", f$header, t) else ""
        }, character(1)))
        if (length(ex)) div(class = "warn", HTML(paste(ex, collapse = "<br>"))) else NULL
      },
      if (!f1$balanced) div(class = "warn", HTML(paste0(
        "<b>The data are unbalanced</b>: ",
        switch(r$design,
          CRD = "the treatments have unequal numbers of replications. ",
          FCRD = "the treatment combinations have unequal numbers of replications. ",
          LSD = "a plot is missing from the Latin square. ",
          "a plot is missing from a block, or the treatments are unequally replicated. "),
        "Each mean is given its own standard error, and each pair of means its own SE(d) and C.D.",
        if (isTRUE(f1$adjusted_ss))
          " The means are adjusted (least-squares) means, and each ANOVA term is tested after allowing for every other term (Type III)."
        else ""))) else NULL,
      if (isTRUE(f1$pooled) && !is.null(f1$homogeneity)) {
        h <- f1$homogeneity
        homog <- isTRUE(h$p > r$alpha)
        div(class = if (homog) "sugbox" else "warn", HTML(sprintf(
          "<b>Homogeneity of error variances across environments (Bartlett):</b> &chi;<sup>2</sup> = %s, df = %d, %s. %s",
          fmt(h$chisq, 3), h$df, p_eq(h$p),
          if (homog)
            sprintf("There is insufficient evidence at the %s%% level that the environments' error variances differ, so pooling the errors is reasonable; a small difference may have gone undetected.", pct(r$alpha))
          else
            sprintf("The error variances are <b>heterogeneous</b> at the %s%% level. The pooled F-tests should be read with caution; consider a variance-stabilising transformation (see the Assumptions tab) or analysing the environments separately.", pct(r$alpha)))))
      } else NULL)
  })

  output$anovaOut <- renderUI({
    r <- ok()
    anv <- combined_anova_html(r)
    tagList(
      if (!is.null(anv)) HTML(paste0(anv, rd_html(anova_combined_text(r)))) else NULL,
      tags$hr(),
      HTML(paste(vapply(names(r$fits), function(nm) paste0(
        "<h4>", r$fits[[nm]]$header, "</h4>",
        df_html(anova_display(r$fits[[nm]]$final$anova, r$alpha)),
        anova_note(r$fits[[nm]]$final),
        rd_html(anova_text(r$fits[[nm]]$final, r$fits[[nm]]$trans))), character(1)), collapse = "")))
  })

  ## ------------------------------------------------------------------ means --
  output$meansOut <- renderUI({
    r <- ok()
    HTML(means_section_html(r, digits = input$digits %||% 2,
                            letters_on = isTRUE(input$letters),
                            detailed = isTRUE(input$detailed), readings = TRUE))
  })

  ## --------------------------------------------------- per-response pickers --
  output$aRespUI  <- renderUI({ r <- ok(); selectInput("aResp",  "Response variable", names(r$fits)) })
  output$aRespUI2 <- renderUI({ r <- ok(); selectInput("aResp2", "Response variable", names(r$fits)) })
  output$aRespUI3 <- renderUI({ r <- ok(); selectInput("aResp3", "Response variable", names(r$fits)) })

  aFit <- reactive({ r <- ok(); r$fits[[input$aResp  %||% names(r$fits)[1]]] })
  pFit <- reactive({ r <- ok(); r$fits[[input$aResp2 %||% names(r$fits)[1]]] })
  gFit <- reactive({ r <- ok(); r$fits[[input$aResp3 %||% names(r$fits)[1]]] })

  ## ------------------------------------------------------------ assumptions --
  output$assumtxt <- renderUI(HTML(assum_table_html(aFit()$asm)))
  output$asmText <- renderUI({ f <- aFit(); HTML(rd_html(assum_text(f$final, f$asm, f$trans), "What the checks show.")) })
  ## each plot's note, empty when the plot cannot be drawn (it says why)
  output$bcText <- renderUI({ f <- aFit(); HTML(rd_note(boxcox_text(f$asm, f$trans))) })
  output$mvText <- renderUI({ f <- aFit(); HTML(rd_note(meanvar_text(f$final, f$asm))) })
  output$diagFitText   <- renderUI({ f <- aFit(); HTML(rd_note(resid_text(f$final, f$asm))) })
  output$diagQQText    <- renderUI({ f <- aFit(); HTML(rd_note(qq_aov_text(f$final, f$asm))) })
  output$diagHistText  <- renderUI({ f <- aFit(); HTML(rd_note(hist_aov_text(f$final, f$asm))) })
  output$diagScaleText <- renderUI({ f <- aFit(); HTML(rd_note(scale_text(f$final, f$asm))) })

  output$sugbox <- renderUI({
    f <- aFit()
    div(class = if (identical(f$sug$method, "none")) "sugbox" else "warn",
        HTML(sprintf("<b>Suggested transformation: %s.</b> %s%s",
          TRANS[[f$sug$method]]$lab, f$sug$why,
          if (identical(f$trans, "none")) ""
          else sprintf("<br><b>Applied:</b> %s.", TRANS[[f$trans]]$lab))))
  })

  output$bcPlot <- renderPlot({
    a <- aFit()$asm
    validate(need(!isTRUE(a$exact), "The model fits every value exactly, so there is no Box-Cox profile to draw."))
    p <- plot_boxcox(a)
    validate(need(!is.null(p), "The Box-Cox profile needs a strictly positive response."))
    p })
  output$mvPlot <- renderPlot({
    a <- aFit()$asm
    validate(need(!isTRUE(a$exact), "The model fits every value exactly, so there is no error variance to relate to the mean."))
    p <- plot_meanvar(a)
    validate(need(!is.null(p), "Too few cells with a positive mean and variance to estimate the mean-variance slope."))
    p })
  ## plot_diag() returns four plots; a single renderPlot() would print each in
  ## turn and show only the last, so each gets its own output. An exact fit has
  ## only rounding residue for residuals, which is not drawn as if it were data.
  diag_plots <- reactive({
    f <- aFit()
    validate(need(!isTRUE(f$asm$exact), "The model fits every value exactly, so there are no residuals to plot."))
    plot_diag(f$final)
  })
  output$diagFit   <- renderPlot(diag_plots()[[1]])
  output$diagQQ    <- renderPlot(diag_plots()[[2]])
  output$diagHist  <- renderPlot(diag_plots()[[3]])
  output$diagScale <- renderPlot(diag_plots()[[4]])

  ## ---------------------------------------------------------------- posthoc --
  output$phEffectUI <- renderUI(selectInput("phEff", "Effect", names(pFit()$final$effects)))

  ## the post-hoc tests run at the level the analysis was run at, so the
  ## letters here and in the tables of means always mean the same thing; and,
  ## as everywhere, no letters or verdicts are shown under a non-significant F
  ph <- reactive({
    f <- pFit(); req(input$phEff)
    validate(need(input$phEff %in% names(f$final$effects), "Choose an effect."))
    tryCatch(gate_posthoc(posthoc(f$final, input$phEff, input$phMethod, f$final$alpha)),
             error = function(e) list(err = conditionMessage(e)))
  })

  output$phNote <- renderUI({
    x <- ph()
    if (!is.null(x$err)) return(div(class = "err", x$err))
    e <- pFit()$final$effects[[input$phEff]]
    tagList(
      div(class = "box", HTML(sprintf(
        "Comparisons use the error term of <b>%s</b>: MSE = %s on %s degrees of freedom, at the %s%% significance level.",
        e$label, fmt(e$mse, 4), df_text(e$df), pct(e$alpha)))),
      if (!is.null(x$note)) div(class = "warn", HTML(paste(x$note, collapse = "<br><br>"))) else NULL)
  })

  output$phTab <- renderDT({
    x <- ph(); validate(need(is.null(x$err), x$err))
    formatRound(datatable(x$groups, rownames = FALSE,
                          options = list(pageLength = 25, dom = "tp")),
                intersect(c("Mean", "Unadjusted mean", "SE"), names(x$groups)), 3)
  })

  output$phPairs <- renderDT({
    x <- ph(); validate(need(is.null(x$err), x$err))
    formatRound(datatable(x$pairs, rownames = FALSE,
                          options = list(pageLength = 15, dom = "tp", scrollX = TRUE)),
                c("Difference", "SEd", "Critical value", "Critical difference"), 3)
  })

  output$phStats <- renderDT({
    x <- ph(); validate(need(is.null(x$err), x$err))
    datatable(x$stats, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  ## what the chosen test does, and what it finds here
  output$phText <- renderUI({
    x <- ph(); req(is.null(x$err))
    f <- pFit(); e <- f$final$effects[[input$phEff]]
    HTML(sprintf("<div class='box'><b>What the test does.</b> %s<br><b>Here.</b> %s</div>",
                 esc(PH_WHAT[[input$phMethod]]), esc(posthoc_groups_text(x, e, f$trans, isTRUE(f$asm$exact)))))
  })
  output$phPairsText <- renderUI({
    x <- ph(); req(is.null(x$err))
    f <- pFit(); e <- f$final$effects[[input$phEff]]
    lsd <- if (isTRUE(x$f_sig) && input$phMethod != PH_METHODS[1])
      tryCatch(posthoc(f$final, input$phEff, PH_METHODS[1], f$final$alpha), error = function(err) NULL)
    HTML(rd_html(posthoc_pairs_text(x, e, input$phMethod, lsd, isTRUE(f$asm$exact))))
  })

  output$phRangesUI <- renderUI({
    x <- ph()
    if (!is.null(x$err) || is.null(x$ranges)) return(NULL)
    tagList(h4("Critical ranges"), HTML(df_html(x$ranges)),
      div(class = "note",
          "p is the number of means spanned by the comparison once the means are ranked in order."))
  })

  ## ---------------------------------------------------------- descriptives --
  ## The variables offered are the columns the data check reads as numbers.
  ## Columns that are usually labels written as numbers (Rep, Block, Plot) and
  ## columns already mapped as design factors start unselected.
  output$descUI <- renderUI({
    d <- rv$data; req(d)
    num <- chk()$numeric
    validate(need(length(num) > 0, "No column holds numbers."))
    used <- tryCatch({ m <- mapping(); unlist(m[setdiff(names(m), "response")]) },
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
  output$descHist <- renderPlot(desc_plot(plot_hist(rv$data, desc_v()), "hist"))
  output$descDens <- renderPlot(desc_plot(plot_density(rv$data, desc_v()), "density"))
  output$descBox  <- renderPlot(desc_plot(plot_box(rv$data, desc_v(), desc_group()), "box"))
  output$descQQ   <- renderPlot(desc_plot(plot_qq(rv$data, desc_v()), "qq"))
  output$descHistText <- desc_note("hist")
  output$descDensText <- desc_note("density")
  output$descBoxText  <- desc_note("box")
  output$descQQText   <- desc_note("qq")

  observeEvent(input$dl_desc, {
    tab <- tryCatch(desc_tab(), error = function(e) NULL); req(tab)
    save_browser(paste0("DOEpro_descriptives_", Sys.Date(), ".csv"), csv_string(tab), "text/csv")
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
    used <- tryCatch({ m <- mapping(); unlist(m[setdiff(names(m), "response")]) },
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
  output$corHeat <- renderPlot(plot_cor_heat(cor_tab(), cor_vars()))
  output$corHeatText <- renderUI({
    t <- cor_heat_text(cor_tab(), length(cor_vars()))
    div(class = "note", HTML(paste0("<b>What it shows.</b> ", esc(t$what), "<br><b>Here.</b> ", esc(t$reading))))
  })
  cor_xy <- reactive({
    num <- chk()$numeric
    req(input$corX %in% num, input$corY %in% num)
    c(input$corX, input$corY)
  })
  output$corScatter <- renderPlot({
    xy <- cor_xy()
    p <- plot_cor_scatter(rv$data, xy[1], xy[2], cor_method(), cor_alpha())
    validate(need(!is.null(p), cor_scatter_text(rv$data, xy[1], xy[2], cor_method(), cor_alpha())$reading))
    p
  })
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
    save_browser(paste0("DOEpro_correlation_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
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
    m <- tryCatch(mapping(), error = function(e) list())
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
  output$regMain <- renderPlot(plot_reg_main(reg_res(), input$regShow))
  output$regMainText <- renderUI(reg_note(reg_main_text(reg_res(), input$regShow)))
  output$regResid <- renderPlot({
    r <- reg_res(); p <- plot_reg_resid(r)
    validate(need(!is.null(p), reg_resid_text(r)$reading))
    p
  })
  output$regResidText <- renderUI(reg_note(reg_resid_text(reg_res())))
  output$regQQ <- renderPlot({
    r <- reg_res(); p <- plot_reg_qq(r)
    validate(need(!is.null(p), reg_qq_text(r)$reading))
    p
  })
  output$regQQText <- renderUI(reg_note(reg_qq_text(reg_res())))
  output$regObs <- renderPlot(plot_reg_obs(reg_res()))
  output$regObsText <- renderUI(reg_note(reg_obs_text(reg_res())))

  observeEvent(input$dl_reg, {
    r <- tryCatch(reg_res(), error = function(e) NULL); req(r)
    out <- r$coefficients
    out$Response <- r$response; out$alpha <- r$alpha
    save_browser(paste0("DOEpro_regression_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
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
    m <- tryCatch(mapping(), error = function(e) list())
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
  output$pcaScree <- renderPlot(plot_pca_scree(pca_res()))
  output$pcaScreeText <- renderUI(pca_note(pca_scree_text(pca_res())))
  output$pcaScores <- renderPlot({
    p <- pca_res(); validate(need(p$m >= 2, "Only one component has any variance, so there is nothing to plot against it."))
    plot_pca_scores(p, pca_ax(), pca_grp())
  })
  output$pcaScoresText <- renderUI({
    p <- pca_res(); req(p$m >= 2)
    pca_note(pca_scores_text(p, pca_ax(), pca_grp(), input$pcaGroup))
  })
  output$pcaLoadPlot <- renderPlot({ p <- pca_res(); req(p$m >= 2); plot_pca_loadings(p, pca_ax()) })
  output$pcaLoadText <- renderUI({ p <- pca_res(); req(p$m >= 2); pca_note(pca_loadings_text(p, pca_ax())) })
  output$pcaBiplot <- renderPlot({ p <- pca_res(); req(p$m >= 2); plot_pca_biplot(p, pca_ax(), pca_grp()) })
  output$pcaBiplotText <- renderUI({ p <- pca_res(); req(p$m >= 2); pca_note(pca_biplot_text(p, pca_ax())) })
  output$pcaScoreTable <- renderUI(HTML(pca_scores_html(pca_res())))
  observeEvent(input$dl_pca_load, {
    p <- tryCatch(pca_res(), error = function(e) NULL); req(p)
    out <- data.frame(Variable = p$vars, round(p$loadings, 6), check.names = FALSE)
    names(out)[-1] <- paste0(colnames(p$loadings), "_loading")
    out <- cbind(out, stats::setNames(as.data.frame(round(p$vectors, 6)), paste0(colnames(p$vectors), "_eigenvector")))
    save_browser(paste0("DOEpro_pca_loadings_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })
  observeEvent(input$dl_pca_scores, {
    p <- tryCatch(pca_res(), error = function(e) NULL); req(p)
    out <- data.frame(Row = p$rows, round(p$scores, 6), check.names = FALSE)
    save_browser(paste0("DOEpro_pca_scores_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
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
    m <- tryCatch(mapping(), error = function(e) list())
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
  output$clDendro <- renderPlot(plot_cl_dendro(cl_res()))
  output$clDendroText <- renderUI(cl_note(cl_dendro_text(cl_res())))
  output$clPcs <- renderPlot(plot_cl_pcs(cl_res()))
  output$clPcsText <- renderUI(cl_note(cl_pcs_text(cl_res())))
  output$clSil <- renderPlot(plot_cl_silhouette(cl_res()))
  output$clSilText <- renderUI(cl_note(cl_silhouette_text(cl_res())))
  output$clItems <- renderUI(HTML(cl_items_html(cl_res())))
  observeEvent(input$dl_cl, {
    cl <- tryCatch(cl_res(), error = function(e) NULL); req(cl)
    out <- data.frame(Item = cl$items, Cluster = cl$cluster, Silhouette = round(cl$silhouette, 6))
    names(out)[1] <- if (is.null(cl$group)) "Row" else cl$group
    save_browser(paste0("DOEpro_clusters_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
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
  output$faScree <- renderPlot(plot_fa_scree(fa_res()))
  output$faScreeText <- renderUI(fa_note(fa_scree_text(fa_res())))
  output$faHeat <- renderPlot(plot_fa_heat(fa_res()))
  output$faHeatText <- renderUI(fa_note(fa_heat_text(fa_res())))
  output$faLoadPlot <- renderPlot({
    f <- fa_res(); validate(need(f$factors >= 2, "With one factor there is no second axis to plot the loadings against."))
    plot_fa_loadings(f, fa_ax())
  })
  output$faLoadText <- renderUI({ f <- fa_res(); req(f$factors >= 2); fa_note(fa_loadings_text(f, fa_ax())) })
  output$faScores <- renderUI(HTML(fa_scores_html(fa_res())))
  observeEvent(input$dl_fa_load, {
    f <- tryCatch(fa_res(), error = function(e) NULL); req(f)
    out <- data.frame(Variable = f$vars, round(f$loadings, 6), Communality = round(f$communality, 6),
                      Uniqueness = round(f$uniqueness, 6), check.names = FALSE)
    save_browser(paste0("DOEpro_factor_loadings_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })
  observeEvent(input$dl_fa_scores, {
    f <- tryCatch(fa_res(), error = function(e) NULL); req(f)
    out <- data.frame(Row = f$rows, round(f$scores, 6), check.names = FALSE)
    save_browser(paste0("DOEpro_factor_scores_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## ------------------------------------------------------------------ plots --
  output$plEffectUI <- renderUI(selectInput("plEff", "Effect", names(gFit()$final$effects)))

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

  mp <- reactive({
    f <- gFit(); req(input$plEff)
    validate(need(input$plEff %in% names(f$final$effects), "Choose an effect."))
    xv <- if (identical(plx_for(), input$plEff)) input$plX else NULL
    plot_main(f$final, input$plEff, input$plType, isTRUE(input$plLetters), xv)
  })
  output$mainPlot <- renderPlot(mp())
  output$mainPlotText <- renderUI({
    f <- gFit(); req(input$plEff %in% names(f$final$effects))
    xv <- if (identical(plx_for(), input$plEff)) input$plX else NULL
    HTML(rd_note(main_plot_text(f$final, input$plEff, input$plType, isTRUE(input$plLetters), xv,
                                input$digits %||% 2)))
  })

  ## -------------------------------------------------------- interpretation --
  output$interpOut <- renderUI({
    r <- ok()
    HTML(paste(vapply(names(r$fits), function(nm) {
      f <- r$fits[[nm]]
      paste0("<h3>", f$header, "</h3>",
             as.character(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)))
    }, character(1)), collapse = "<hr>"))
  })

  output$pdfBtn <- renderUI({
    eng <- pdf_engine()
    if (!is.na(eng))
      downloadButton("dl_pdf", sprintf("Download report (PDF, via %s)", eng),
                     class = "btn-primary")
    else if (is_wasm())
      div(class = "box", HTML(paste0(
        "<b>To save the report as a PDF:</b> click <b>Download report (HTML)</b> on the left, ",
        "open the downloaded file in your browser, then print it (<b>Ctrl&nbsp;+&nbsp;P</b> ",
        "&rarr; <b>Save as PDF</b>). The page footer carries the citation line. ",
        "<span class='note'>Direct PDF export is only available in the desktop R version of DOEpro.</span>")))
    else div(class = "warn", HTML(paste0(
      "<b>PDF export is not available on this machine.</b> Install one of ",
      "<code>pagedown</code> (needs Chrome or Chromium), <code>weasyprint</code>, or ",
      "<code>wkhtmltopdf</code>. Until then, download the HTML report and print it to PDF ",
      "from your browser (Ctrl+P &rarr; Save as PDF).")))
  })

  ## -------------------------------------------------------------- downloads --
  save_browser <- function(filename, content, mime)
    session$sendCustomMessage("doepro_save",
      list(filename = filename, content = content, mime = mime))

  observeEvent(input$dl_anova, {
    r <- ok()
    out <- do.call(rbind, lapply(names(r$fits), function(nm) {
      a <- r$fits[[nm]]$final$anova
      data.frame(Response = r$fits[[nm]]$header, a, Signif = star(a$p, r$alpha),
                 alpha = r$alpha, row.names = NULL, check.names = FALSE)
    }))
    save_browser(paste0("DOEpro_anova_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## One row per mean. SEm is that mean's own standard error (for a split,
  ## strip or pooled interaction, the SE(m) for comparisons within one level of
  ## its slicing factor). SEd and CD are filled only when one value applies to
  ## every pair they describe; otherwise they are left empty and the note says
  ## where the pairwise values are, rather than writing one figure that fits
  ## only some pairs.
  observeEvent(input$dl_means, {
    r <- ok()
    out <- do.call(rbind, lapply(names(r$fits), function(nm) {
      fit <- r$fits[[nm]]
      do.call(rbind, lapply(names(fit$final$effects), function(en) {
        e <- fit$final$effects[[en]]; m <- gate_letters(e)
        g <- intersect(LETTER_COLS, names(m))
        xt <- extra_text(e)
        note <- c(
          if (is.na(e$sed)) "SEd and CD differ between pairs of means; see the pairwise comparisons on the post-hoc tab."
          else if (!isTRUE(e$equal_rep)) "SEm differs between means.",
          if (!is.null(e$slice))
            sprintf("SEd and CD apply to means at the same level of %s. %s", e$slice,
                    paste(sprintf("%s = %s", names(xt), xt), collapse = "; ")))
        data.frame(
          Response = nm, Transformation = TRANS[[fit$trans]]$lab, Effect = e$label,
          Level = apply(m[e$vars], 1, paste, collapse = " x "),
          Mean = m$Mean,
          Unadjusted_mean = if ("Raw_mean" %in% names(m)) m$Raw_mean else NA_real_,
          Back_transformed = if ("Mean_bt" %in% names(m)) m$Mean_bt else NA_real_,
          N = m$N, SD = m$SD, SEm = m$SE, SEd = e$sed,
          CD = if (effect_sig(e)) e$cd else NA_real_, alpha = e$alpha, p_value = e$p,
          Group  = if (length(g) >= 1) m[[g[1]]] else NA_character_,
          Group2 = if (length(g) >= 2) m[[g[2]]] else NA_character_,
          Note = paste(note, collapse = " "),
          row.names = NULL, check.names = FALSE)
      }))
    }))
    save_browser(paste0("DOEpro_means_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  ## the post-hoc exports say which test and level produced them
  ph_csv <- function(x, tab) cbind(Method = x$method, alpha = x$alpha, x[[tab]])

  observeEvent(input$dl_ph, {
    x <- ph(); req(is.null(x$err))
    save_browser(paste0("DOEpro_posthoc_", Sys.Date(), ".csv"), csv_string(ph_csv(x, "groups")), "text/csv")
  })

  observeEvent(input$dl_ph_pairs, {
    x <- ph(); req(is.null(x$err))
    save_browser(paste0("DOEpro_pairwise_", Sys.Date(), ".csv"), csv_string(ph_csv(x, "pairs")), "text/csv")
  })

  output$dl_plot <- downloadHandler(
    filename = function() paste0("DOEpro_plot_", Sys.Date(), ".png"),
    content = function(f) ggsave(f, mp(), width = 9, height = 6, dpi = 300))

  report <- reactive(build_report(ok(), letters_on = isTRUE(input$letters),
                                  detailed = isTRUE(input$detailed)))

  ## The report download is done in the browser (a JavaScript Blob), not through
  ## downloadHandler. This behaves identically on a real Shiny server and in the
  ## browser (shinylive/webR) build, where downloadHandler's shim is unreliable.
  observeEvent(input$dl_html, {
    session$sendCustomMessage("doepro_save", list(
      filename = paste0("DOEpro_report_", Sys.Date(), ".html"),
      content  = report(),
      mime     = "text/html"))
  })

  output$dl_pdf <- downloadHandler(
    filename = function() paste0("DOEpro_report_", Sys.Date(), ".pdf"),
    content = function(f) {
      okp <- withProgress(message = "Compiling the PDF", value = 0.5, save_pdf(report(), f))
      if (!isTRUE(okp))
        stop("The PDF could not be compiled. Download the HTML report and print it to PDF from your browser.")
    })

  ## ------------------------------------------------------------------- help --
  output$help <- renderUI(HTML(paste0("
<h3>Quick start</h3>
<ol>
<li>Put your data in <b>long format</b> - one row per plot, one column per variable - and paste it in on the Data tab.</li>
<li>Choose the design and map your columns to their roles. You may select <b>several response variables at once</b>; each is analysed separately with the same design and appears side by side in the tables of means.</li>
<li>The app screens every numeric column and says which transformation, if any, it needs. Press <i>Apply all suggested</i> to accept the advice.</li>
<li>Press <b>Run analysis</b>.</li>
</ol>

<h3>Designs and their error terms</h3>
<table class='doe'>
<tr><th>Design</th><th>Columns required</th><th>Error term(s)</th></tr>
<tr><td>CRD</td><td>treatment, response</td><td>single pooled error</td></tr>
<tr><td>RCBD</td><td>block, treatment, response</td><td>single pooled error</td></tr>
<tr><td>Latin square</td><td>row, column, treatment, response</td><td>single pooled error</td></tr>
<tr><td>Factorial CRD / RCBD</td><td>(block,) factors A-D, response</td><td>single pooled error</td></tr>
<tr><td>Split plot</td><td>replication, main plot, sub plot, response</td><td>Error(a), Error(b)</td></tr>
<tr><td>Strip plot</td><td>replication, factor A, factor B, response</td><td>Error(a), Error(b), Error(c)</td></tr>
<tr><td>Pooled over environments (RCBD)</td><td>environment, replication, treatment, response</td><td>R(env), pooled error</td></tr>
<tr><td>Pooled over environments (CRD)</td><td>environment, treatment, response</td><td>pooled error</td></tr>
<tr><td>Pooled factorial over environments (2-4 factors)</td><td>environment, (replication,) factors A-D, response</td><td>each effect vs its environment interaction; pooled error</td></tr>
</table>

<h3>Pooled (combined) analysis over environments</h3>
<p>When the <b>same experiment is repeated across several environments</b> - locations,
years, seasons or other groups - a combined analysis tests the treatments, the
environments, and the treatment &times; environment interaction together. Arrange the data
with one column identifying the environment, alongside the usual replication and treatment
columns, and stack all environments in one long table.</p>
<p>The analysis proceeds in three steps:</p>
<ol>
<li><b>Homogeneity of error variances.</b> Bartlett's test compares the error variances of
the separate environments. If the test finds no significant difference at your chosen level,
the environments may be pooled; if it does, the app warns you and a variance-stabilising
transformation (or separate analyses) should be considered. The verdict is shown above the
ANOVA table.</li>
<li><b>Combined ANOVA</b> with the correct error terms:
  <ul>
  <li><b>RCBD base:</b> Environment is tested against replications-within-environment; the
  treatment is tested against the treatment &times; environment interaction; the interaction
  is tested against the pooled error.</li>
  <li><b>CRD base:</b> Environment and the interaction are tested against the pooled error;
  the treatment is tested against the interaction.</li>
  </ul>
  Testing the treatment against the interaction (rather than the pooled error) is the
  essential feature of a combined analysis - it asks whether a treatment's advantage is
  consistent enough across environments to be declared real.</li>
<li><b>Means and critical differences.</b> The treatment table gives means <b>averaged over
all environments</b>, with the critical difference built from the interaction mean square.
The environment &times; treatment table compares treatments <b>within a single environment</b>
using the pooled error, and also reports the standard error for comparing a treatment's
overall mean across environments.</li>
</ol>
<table class='doe'>
<tr><th>Comparison</th><th>Standard error of a difference</th></tr>
<tr><td>two treatment means, over all environments</td><td>&radic;(2&middot;M<sub>TxE</sub> / re)</td></tr>
<tr><td>two environment means</td><td>&radic;(2&middot;M<sub>error</sub> / rt) &nbsp;(CRD) &nbsp;or&nbsp; &radic;(2&middot;M<sub>R(E)</sub> / rt) &nbsp;(RCBD)</td></tr>
<tr><td>two treatments in the same environment</td><td>&radic;(2&middot;M<sub>error</sub> / r)</td></tr>
</table>
<p><b>Factorial treatments over environments.</b> When the treatments themselves form a
2-, 3- or 4-factor factorial, choose one of the <i>Pooled factorial over environments</i>
designs and set the number of treatment factors. The combined analysis then partitions the
treatment variation into every main effect and interaction, and applies the same rule
throughout: <b>each treatment effect (a main effect or an interaction among the treatment
factors) is tested against its own interaction with the environment</b>, and every
environment &times; treatment interaction is tested against the pooled error. Each effect's
table of means, pooled over environments, uses the critical difference built from that
effect's environment interaction. This tells you which main effects and interactions are
stable enough across environments to be declared real.</p>

<h3>Standard errors and critical differences</h3>
<p>With <i>n</i> observations behind each mean, SE(m) = &radic;(MSE/n),
SE(d) = &radic;(2&middot;MSE/n) and C.D. = t<sub>&alpha;/2, df</sub> &times; SE(d), where
&alpha; is the significance level you chose.</p>
<p>When replication is unequal, mean <i>i</i> has SE(m) = &radic;(MSE/n<sub>i</sub>) and
two means have SE(d) = &radic;(MSE(1/n<sub>i</sub> + 1/n<sub>j</sub>)), so the C.D. depends
on which two means are compared and the tables show its range. In a blocked design with a
missing plot, or a factorial with unequal cells, the means are adjusted (least-squares)
means and their standard errors come from the fitted model. Split plots, strip plots and
pooled analyses must be complete: with a plot missing the app says what is missing rather
than give an approximate answer.</p>
<p>A split plot needs <b>four</b> different SE(d):</p>
<ul>
<li>two main-plot means: &radic;(2&middot;Ea / rb)</li>
<li>two sub-plot means: &radic;(2&middot;Eb / ra)</li>
<li>two sub-plot means within the same main plot: &radic;(2&middot;Eb / r)</li>
<li>two main-plot means at the same sub-plot level: &radic;(2[(b-1)Eb + Ea] / rb), with a weighted <i>t</i></li>
</ul>
<p>A strip plot needs three error terms and the analogous mixed comparisons. The app
prints every one of them under the relevant table of means, so you never have to work
out which C.D. belongs to which comparison.</p>

<h3>Choosing a transformation</h3>
<ul>
<li><b>Counts</b> (insects, spores, grains): &radic;y, or &radic;(y+0.5) when zeros occur. Signature: integer data whose variance rises in proportion to the mean, i.e. Taylor slope b &asymp; 1.</li>
<li><b>Percentages and proportions</b>: the angular, or arcsine square-root, transformation. A 0-100 range on its own is <i>not</i> evidence - most yields live there too - so the app also looks at the column name, and you can declare the type yourself.</li>
<li><b>Variance proportional to the square of the mean</b> (b &asymp; 2): log y, or log(y+1) when zeros occur.</li>
<li><b>Variance rising faster still</b> (b &gt; 2.5): 1/y.</li>
<li>Otherwise the Box-Cox profile likelihood chooses &lambda;.</li>
</ul>
<p>Means are always <b>back-transformed</b> for presentation. SE, C.D. and C.V. stay on the
transformed scale, because that is where the tests were performed; tables therefore show
the back-transformed mean with the transformed value in parentheses.</p>

<h3>Choosing a post-hoc test</h3>
<ul>
<li><b>Least significant difference (LSD), Fisher's protected</b>: only after a significant F-test, and best with few treatments.</li>
<li><b>Tukey's honestly significant difference (HSD)</b>: controls the error rate over all pairwise comparisons; the safe default.</li>
<li><b>Duncan's multiple range test (DMRT)</b>: less conservative, still standard in agronomy.</li>
<li><b>Student-Newman-Keuls (SNK) test</b>: sits between Duncan and Tukey.</li>
<li><b>Scheffe's test</b>: the most conservative; built for arbitrary contrasts.</li>
<li><b>Bonferroni-adjusted LSD</b>: simple and strict.</li>
</ul>
<p>All six are computed from the error mean square of whichever effect you select, so in a
split or strip plot they automatically use the right error stratum. An interaction in a split
plot, strip plot or pooled analysis is compared within one level of the main-plot (or strip)
factor, or one environment, at a time. With unequal replication each pair of means uses its
own SE(d): Tukey's HSD becomes the Tukey-Kramer procedure, and Duncan's and the
Student-Newman-Keuls tests use Kramer's adjustment. As everywhere in the app, no letters are
shown when the effect's F-test is not significant.</p>

<h3>Reporting</h3>
<p>Present the ANOVA table, then the table of means with SE(m)&plusmn;, SE(d),
the C.D. at your chosen significance level and C.V. (%) at the foot. Means followed by a
common letter are not significantly different at that level. When an interaction is
significant, interpret the cell means and the simple effects rather than the main
effects.</p>
<hr>", authors_html())))

  output$about <- renderUI({
    author_li <- paste(vapply(AUTHORS, function(a) {
      orc <- if (!is.null(a$orcid) && !is.na(a$orcid))
        sprintf(" &nbsp;<a href='https://orcid.org/%s' target='_blank'>%s</a>", a$orcid, a$orcid) else ""
      eml <- if (!is.null(a$email) && !is.na(a$email))
        sprintf("<br><a href='mailto:%s'>%s</a>", a$email, a$email) else ""
      sprintf("<li><b>%s</b><br>%s, %s%s%s</li>", a$name, a$role, a$aff, orc, eml)
    }, character(1)), collapse = "")

    HTML(sprintf("
<h2>%s <small>v%s</small></h2>
<p>A free and open tool for the analysis of designed agricultural experiments. It brings the
standard analyses used in field and horticultural research together in one accessible
interface, and serves as a free, self-contained option for the kind of analysis researchers
carry out in tools such as OPSTAT.</p>

<h3>Developed by</h3>
<ol>%s</ol>

<h3>Feedback and correspondence</h3>
<p>Suggestions, bug reports and relevant correspondence are welcome. Please write to:</p>
<ul>
<li><b>Dr. Immad A. Shah</b> &mdash; <a href='mailto:immad11w@skuastkashmir.ac.in'>immad11w@skuastkashmir.ac.in</a></li>
<li><b>Mr. Uzair Javid Khan</b> <i>(maintainer)</i> &mdash; <a href='mailto:uzairkhan11w@gmail.com'>uzairkhan11w@gmail.com</a></li>
</ul>
<p>You may also open an issue in the project repository.</p>

<h3>How to cite</h3>
<div class='box'>Shah, I. A., Khan, U. J. and Jeelani, M. I. (%s).
<i>%s: analysis of designed agricultural experiments.</i> Version %s. Zenodo.
doi:<a href='https://doi.org/%s' target='_blank'>%s</a></div>
<p class='note'>This is the concept DOI: it always resolves to the most recent release.</p>

<h3>Licence and source</h3>
<p>Released under the GPL-3 licence. Source code and issue tracker:
<a href='https://github.com/Uzairkhan11w/DOEpro' target='_blank'>github.com/Uzairkhan11w/DOEpro</a>.
Run it in your browser at <a href='%s' target='_blank'>%s</a>.
Every release is archived on Zenodo and carries a DOI.</p>

<h3>Statistical methods</h3>
<p>The analysis of variance is fitted with <code>stats::aov</code>, using
<code>Error(Rep/Main)</code> for split plots and <code>Error(Rep/(A+B))</code> for strip
plots. Levene's test, the Box-Cox profile likelihood and all six multiple-comparison
procedures are implemented directly in the app, so it depends only on <b>shiny</b>,
<b>DT</b> and <b>ggplot2</b>.</p>
<p class='note'>%s</p>",
      APP_NAME, APP_VERSION, author_li,
      format(Sys.Date(), "%Y"), APP_NAME, APP_VERSION, APP_DOI, APP_DOI,
      APP_URL, APP_URL, CREDIT_LONG))
  })
}

