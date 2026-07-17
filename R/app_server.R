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
    else { rv$data <- d; showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  observeEvent(input$file, {
    d <- tryCatch(utils::read.csv(input$file$datapath, stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(d)) showNotification("Could not read that file.", type = "error")
    else { rv$data <- d; showNotification(sprintf("Loaded %d rows.", nrow(d)), type = "message") }
  })

  observeEvent(input$loaddemo, {
    rv$data <- demo_data(input$demo)
    des <- switch(input$demo, POOLFACT = "POOLFRCBD", POOLFACTC = "POOLFCRD", input$demo)
    updateSelectInput(session, "design", selected = des)
    showNotification(paste("Loaded the", input$demo, "example."), type = "message")
  })

  output$tbl <- renderDT(rv$data, editable = TRUE, rownames = FALSE,
                         options = list(pageLength = 8, scrollX = TRUE))

  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    d <- rv$data
    j <- info$col + 1L
    d[info$row, j] <- DT::coerceValue(info$value, d[info$row, j])
    rv$data <- d
  })

  output$dataNote <- renderUI({
    if (is.null(rv$data)) return(div(class = "box",
      "Paste your data, upload a CSV, or load one of the examples. ",
      "Data must be in long format: one row per plot, one column per variable. ",
      "You may analyse several response variables at once."))
    div(class = "box", sprintf("%d rows x %d columns.", nrow(rv$data), ncol(rv$data)))
  })

  ## ------------------------------------------------------- column mapping ---
  output$mapUI <- renderUI({
    d <- rv$data; req(d)
    cn  <- names(d)
    num <- cn[vapply(d, is.numeric, logical(1))]
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
    cand <- names(d)[vapply(d, function(z)
      is.numeric(z) && length(unique(stats::na.omit(z))) > 2, logical(1))]
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
    if (!is.null(r$err)) return(div(class = "err", r$err))
    f1 <- r$fits[[1]]$final
    tagList(
      div(class = "box", HTML(sprintf(
        "<b>%s</b> &nbsp;|&nbsp; %d response variable(s) &nbsp;|&nbsp; %d observations &nbsp;|&nbsp; %s",
        names(DESIGNS)[match(r$design, DESIGNS)], length(r$fits), nrow(f1$data),
        paste(sprintf("%s = %s", names(f1$cv), fmt(f1$cv, 2)), collapse = " | ")))),
      if (!f1$balanced) div(class = "warn",
        "The data are unbalanced. The ANOVA uses sequential (Type I) sums of squares and the grouping letters are approximate.") else NULL,
      if (isTRUE(f1$pooled) && !is.null(f1$homogeneity)) {
        h <- f1$homogeneity
        homog <- isTRUE(h$p > 0.05)
        div(class = if (homog) "sugbox" else "warn", HTML(sprintf(
          "<b>Homogeneity of error variances across environments (Bartlett):</b> &chi;<sup>2</sup> = %s, df = %d, p = %s. %s",
          fmt(h$chisq, 3), h$df, pval(h$p),
          if (homog)
            "The error variances are homogeneous, so the environments may be pooled and the combined ANOVA is valid."
          else
            "The error variances are <b>heterogeneous</b>. The pooled F-tests should be read with caution; consider a variance-stabilising transformation (see the Assumptions tab) or analysing the environments separately.")))
      } else NULL)
  })

  output$anovaOut <- renderUI({
    r <- ok()
    anv <- combined_anova_html(r)
    tagList(
      if (!is.null(anv)) HTML(anv) else NULL,
      tags$hr(),
      HTML(paste(vapply(names(r$fits), function(nm) paste0(
        "<h4>", r$fits[[nm]]$header, "</h4>",
        df_html(anova_display(r$fits[[nm]]$final$anova))), character(1)), collapse = "")))
  })

  ## ------------------------------------------------------------------ means --
  output$meansOut <- renderUI({
    r <- ok()
    HTML(means_section_html(r, digits = input$digits %||% 2,
                            letters_on = isTRUE(input$letters),
                            detailed = isTRUE(input$detailed)))
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

  output$sugbox <- renderUI({
    f <- aFit()
    div(class = if (identical(f$sug$method, "none")) "sugbox" else "warn",
        HTML(sprintf("<b>Suggested transformation: %s.</b> %s%s",
          TRANS[[f$sug$method]]$lab, f$sug$why,
          if (identical(f$trans, "none")) ""
          else sprintf("<br><b>Applied:</b> %s.", TRANS[[f$trans]]$lab))))
  })

  output$bcPlot <- renderPlot({
    p <- plot_boxcox(aFit()$asm)
    validate(need(!is.null(p), "The Box-Cox profile needs a strictly positive response."))
    p })
  output$mvPlot <- renderPlot({
    p <- plot_meanvar(aFit()$asm)
    validate(need(!is.null(p), "Too few cells to estimate the mean-variance slope."))
    p })
  output$diagPlot <- renderPlot(plot_diag(aFit()$final))

  ## ---------------------------------------------------------------- posthoc --
  output$phEffectUI <- renderUI(selectInput("phEff", "Effect", names(pFit()$final$effects)))

  ph <- reactive({
    f <- pFit(); req(input$phEff)
    validate(need(input$phEff %in% names(f$final$effects), "Choose an effect."))
    tryCatch(posthoc(f$final, input$phEff, input$phMethod, as.numeric(input$alpha)),
             error = function(e) list(err = conditionMessage(e)))
  })

  output$phNote <- renderUI({
    x <- ph()
    if (!is.null(x$err)) return(div(class = "err", x$err))
    e <- pFit()$final$effects[[input$phEff]]
    tagList(
      div(class = "box", HTML(sprintf(
        "Comparisons use the error term of <b>%s</b>: MSE = %s on %d degrees of freedom.",
        e$label, fmt(e$mse, 4), e$df))),
      if (!is.null(x$note)) div(class = "warn", HTML(x$note)) else NULL,
      if (identical(input$phMethod, "LSD (Fisher's protected)") &&
          !is.na(e$p) && e$p >= 0.05)
        div(class = "warn",
            "The F-test for this effect is not significant, so Fisher's LSD is not protected here. Treat these comparisons with caution.")
      else NULL)
  })

  output$phTab <- renderDT({
    x <- ph(); validate(need(is.null(x$err), x$err))
    formatRound(datatable(x$groups, rownames = FALSE,
                          options = list(pageLength = 25, dom = "tp")), "Mean", 3)
  })

  output$phStats <- renderDT({
    x <- ph(); validate(need(is.null(x$err), x$err))
    datatable(x$stats, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$phRangesUI <- renderUI({
    x <- ph()
    if (!is.null(x$err) || is.null(x$ranges)) return(NULL)
    tagList(h4("Critical ranges"), HTML(df_html(x$ranges)),
      div(class = "note",
          "p is the number of means spanned by the comparison once the means are ranked in order."))
  })

  ## ------------------------------------------------------------------ plots --
  output$plEffectUI <- renderUI(selectInput("plEff", "Effect", names(gFit()$final$effects)))

  mp <- reactive({
    f <- gFit(); req(input$plEff)
    validate(need(input$plEff %in% names(f$final$effects), "Choose an effect."))
    plot_main(f$final, input$plEff, input$plType, isTRUE(input$plLetters))
  })
  output$mainPlot <- renderPlot(mp())

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
      data.frame(Response = r$fits[[nm]]$header, a, Signif = star(a$p),
                 row.names = NULL, check.names = FALSE)
    }))
    save_browser(paste0("DOEpro_anova_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  observeEvent(input$dl_means, {
    r <- ok()
    lt <- c("Letter", "Letter_within_MP", "Letter_within_SP",
            "Letter_within_A", "Letter_within_B", "Letter_within_env")
    out <- do.call(rbind, lapply(names(r$fits), function(nm) {
      fit <- r$fits[[nm]]
      do.call(rbind, lapply(names(fit$final$effects), function(en) {
        e <- fit$final$effects[[en]]; m <- gate_letters(e)
        g <- intersect(lt, names(m))
        data.frame(
          Response = nm, Transformation = TRANS[[fit$trans]]$lab, Effect = e$label,
          Level = apply(m[e$vars], 1, paste, collapse = " x "),
          Mean = m$Mean,
          Back_transformed = if ("Mean_bt" %in% names(m)) m$Mean_bt else NA_real_,
          N = m$N, SD = m$SD, SEm = e$sem, SEd = e$sed,
          CD5 = e$cd5, CD1 = e$cd1, p_value = e$p,
          Group  = if (length(g) >= 1) m[[g[1]]] else NA_character_,
          Group2 = if (length(g) >= 2) m[[g[2]]] else NA_character_,
          row.names = NULL, check.names = FALSE)
      }))
    }))
    save_browser(paste0("DOEpro_means_", Sys.Date(), ".csv"), csv_string(out), "text/csv")
  })

  observeEvent(input$dl_ph, {
    x <- ph(); req(is.null(x$err))
    save_browser(paste0("DOEpro_posthoc_", Sys.Date(), ".csv"), csv_string(x$groups), "text/csv")
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
the separate environments. If they are homogeneous the environments may be pooled; if not,
the app warns you and a variance-stabilising transformation (or separate analyses) should be
considered. The verdict is shown above the ANOVA table.</li>
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
SE(d) = &radic;(2&middot;MSE/n) and C.D. = t<sub>&alpha;/2, df</sub> &times; SE(d).</p>
<p>A split plot needs <b>four</b> different SE(d):</p>
<ul>
<li>two main-plot means: &radic;(2&middot;Ea / rb)</li>
<li>two sub-plot means: &radic;(2&middot;Eb / ra)</li>
<li>two sub-plot means within the same main plot: &radic;(2&middot;Eb / r)</li>
<li>two main-plot means at the same sub-plot level: &radic;(2[(b-1)Eb + Ea] / rb), with a Satterthwaite-weighted <i>t</i></li>
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
<li><b>Fisher's protected LSD</b>: only after a significant F-test, and best with few treatments.</li>
<li><b>Tukey's HSD</b>: controls the error rate over all pairwise comparisons; the safe default.</li>
<li><b>Duncan's DMRT</b>: less conservative, still standard in agronomy.</li>
<li><b>Student-Newman-Keuls</b>: sits between Duncan and Tukey.</li>
<li><b>Scheffe</b>: the most conservative; built for arbitrary contrasts.</li>
<li><b>Bonferroni</b>: simple and strict.</li>
</ul>
<p>All six are computed from the error mean square of whichever effect you select, so in a
split or strip plot they automatically use the right error stratum.</p>

<h3>Reporting</h3>
<p>Present the ANOVA table, then the table of means with SE(m)&plusmn;, SE(d),
C.D. (P&le;0.05) and C.V. (%) at the foot. Means followed by a common letter do not differ
significantly. When an interaction is significant, interpret the cell means and the simple
effects rather than the main effects.</p>
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

