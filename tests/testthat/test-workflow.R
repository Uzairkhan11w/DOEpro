## The app's workflow: the Data tab sets everything, results follow every
## choice at once, and nothing waits for a tab to be visited.

test_that("loading an example sets its design, roles and responses, and the screening follows at once", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    session$setInputs(design = "RCBD", nfac = 2, alpha = "0.05")
    session$setInputs(demo = "FCRD", loaddemo = 1)
    expect_identical(design_now(), "FCRD")
    expect_equal(nfac_now(), 2)
    # the mapping is drawn for the example's own roles and responses
    ui <- output$mapUI$html
    id <- function(r) sprintf('id="map_%s_%d"', r, rv$ver)
    for (r in c("resp", "f1", "f2")) expect_match(ui, id(r), fixed = TRUE)
    expect_match(ui, '<option value="Yield" selected>', fixed = TRUE)
    expect_match(ui, '<option value="Plant_height" selected>', fixed = TRUE)
    # the selectors report back, and the analysis needs nothing else: no
    # button, no visit to another tab
    set_map(session, rv$ver, resp = c("Yield", "Plant_height"), f1 = "Variety", f2 = "Nitrogen")
    expect_identical(names(res()$fits), c("Yield", "Plant_height"))
    expect_match(output$quickOut$html, "Plant_height", fixed = TRUE)
    s <- output$screenOut$html
    expect_match(s, ">Yield<", fixed = TRUE); expect_match(s, ">Plant_height<", fixed = TRUE)
    # labelled as demonstration data
    expect_match(output$dataNote$html, "Demonstration data", fixed = TRUE)
    expect_match(output$dataNote$html, "synthetic", fixed = TRUE)
  })
})

test_that("a new data set never reuses the choices made for the last one", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    session$setInputs(design = "CRD", alpha = "0.05", demo = "CRD", loaddemo = 1)
    set_map(session, rv$ver, resp = "Yield", treat = "Treatment")
    expect_true(length(res()$fits) == 1)
    v1 <- rv$ver
    session$setInputs(demo = "RCBD", loaddemo = 1)
    expect_identical(rv$ver, v1 + 1L)
    # the old selectors still hold values, but they are not this data set's
    st <- map_state()
    expect_null(st$map)
    expect_identical(design_now(), "RCBD")
  })
})

test_that("the significance level reaches every table, letter and reading at once", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    session$setInputs(demo = "RCBD", loaddemo = 1, design = "RCBD", alpha = "0.05")
    set_map(session, rv$ver, resp = "Yield", block = "Block", treat = "Variety")
    session$setInputs(phMethod = "LSD (Fisher's protected)", digits = 2, detailed = FALSE)
    expect_equal(res()$alpha, 0.05)
    expect_match(output$meansOut$html, "C.D. (5%)", fixed = TRUE)
    session$setInputs(alpha = "0.01")
    r <- res()
    expect_equal(r$alpha, 0.01)
    expect_true(all(vapply(r$fits$Yield$final$effects, function(e) e$alpha == 0.01, logical(1))))
    mo <- output$meansOut$html
    expect_match(mo, "C.D. (1%)", fixed = TRUE)
    expect_no_match(mo, "C.D. (5%)", fixed = TRUE)
    expect_match(output$anovaOut$html, "Significance (1%)", fixed = TRUE)
    expect_no_match(output$anovaOut$html, "5% level", fixed = TRUE)
    expect_match(output$asmSummary$html, "at the 1% level", fixed = TRUE)
    expect_match(output$screenOut$html, "at the 1% level", fixed = TRUE)
  })
})

test_that("a transformation is the user's choice, offered only where it is defined", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- demo_data("CRD"); d$Count <- c(0, 2, 1, 3, 5, 8, 6, 7, 12, 15, 11, 14, 20, 25, 22, 24, 30, 35, 28, 33)[seq_len(nrow(d))]
    rv$data <- d
    session$setInputs(design = "CRD", alpha = "0.05")
    set_map(session, rv$ver, resp = c("Yield", "Count"), treat = "Treatment")
    # nothing is transformed until the user says so
    expect_identical(unname(vapply(res()$fits, `[[`, "", "trans")), c("none", "none"))
    # log(y) is not offered for a response with zeros
    ui <- output$transUI$html
    k <- match("Count", names(d))
    sel <- regmatches(ui, regexpr(sprintf('(?s)id="tr_%d_%d".*?</select>', rv$ver, k), ui, perl = TRUE))
    expect_no_match(sel, 'value="log"', fixed = TRUE)
    expect_match(sel, 'value="sqrt0.5"', fixed = TRUE)
    set_trans(session, rv$ver, d, "Count", "sqrt0.5")
    expect_identical(res()$fits$Count$trans, "sqrt0.5")
    expect_identical(res()$fits$Yield$trans, "none")
    expect_match(output$anovaOut$html, "Analysis of Variance (Count (sqrt(y + 0.5)))", fixed = TRUE)
    # a choice that the data no longer allow falls back to the values as entered
    set_trans(session, rv$ver, d, "Count", "log")
    expect_identical(res()$fits$Count$trans, "none")
  })
})

test_that("the mapping says plainly what stops an analysis", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    rv$data <- demo_data("RCBD")
    session$setInputs(design = "RCBD", alpha = "0.05")
    set_map(session, rv$ver, resp = "Yield", block = "Block", treat = "Block")
    expect_match(output$mapNote$html, "'Block' is chosen for two roles", fixed = TRUE)
    set_map(session, rv$ver, resp = "Variety", block = "Block", treat = "Variety")
    expect_match(output$mapNote$html, "chosen both as a response and as a design column", fixed = TRUE)
    set_map(session, rv$ver, resp = "Block", block = "Block", treat = "Variety")
    expect_match(output$mapNote$html, "chosen both as a response", fixed = TRUE)
    set_map(session, rv$ver, resp = character(0), block = "Block", treat = "Variety")
    expect_match(output$mapNote$html, "Choose at least one response variable.", fixed = TRUE)
    # a text column chosen as the response
    d <- demo_data("RCBD"); d$Notes <- rep(c("good", "poor"), length.out = nrow(d))
    rv$data <- d
    set_map(session, rv$ver, resp = "Notes", block = "Block", treat = "Variety")
    expect_match(output$mapNote$html, "is not a column of numbers", fixed = TRUE)
    expect_match(output$screenOut$html, "is not a column of numbers", fixed = TRUE)
  })
})

test_that("one response that cannot be analysed does not stop the others", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    d <- demo_data("CRD"); d$Sparse <- NA_real_; d$Sparse[1:2] <- c(3, 4)
    rv$data <- d
    session$setInputs(design = "CRD", alpha = "0.05")
    set_map(session, rv$ver, resp = c("Yield", "Sparse"), treat = "Treatment")
    r <- res()
    expect_identical(names(r$fits), "Yield")
    expect_true("Sparse" %in% names(r$errors))
    expect_match(output$runNote$html, "Not analysed", fixed = TRUE)
    expect_match(output$anovaOut$html, "Analysis of Variance (Yield)", fixed = TRUE)
  })
})

test_that("graphs download, the report prints, and both say StatLabX", {
  skip_on_cran()
  shiny::testServer(doepro_server, {
    session$setInputs(demo = "FCRD", loaddemo = 1, alpha = "0.05")
    set_map(session, rv$ver, resp = "Yield", f1 = "Variety", f2 = "Nitrogen")
    session$setInputs(aResp = "Yield", ipResp = "Yield", ipEff = "Variety:Nitrogen", ipCI = TRUE,
                      aResp3 = "Yield", plEff = "Variety", plType = "bar", phMethod = "Tukey HSD", plLetters = TRUE)
    # a download request for a graph, a missing graph and an unknown id
    expect_no_error(session$setInputs(plot_dl = list(id = "interPlot", fmt = "png", w = 900, h = 560, n = 1)))
    expect_no_error(session$setInputs(plot_dl = list(id = "mainPlot", fmt = "svg", w = 900, h = 560, n = 2)))
    expect_no_error(session$setInputs(plot_dl = list(id = "nothing", fmt = "png", w = 900, h = 560, n = 3)))
    h <- report()
    expect_match(h, "Generated with StatLabX", fixed = TRUE)
    expect_match(h, "Analysis of Variance (Yield)", fixed = TRUE)
    expect_match(h, "Comparison of means: <b>Tukey's HSD</b>", fixed = TRUE)
    expect_match(h, "@media print", fixed = TRUE)
    expect_no_error(session$setInputs(print_report = 1))
    expect_no_error(session$setInputs(dl_html = 1))
    # in the browser build there is no PDF engine, so no PDF button is offered
    if (is.na(pdf_engine())) expect_null(output$pdfBtn)
  })
})
