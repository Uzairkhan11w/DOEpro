###############################################################################
##  UI
###############################################################################

APP_CSS <- "
.navbar-default{background:#1B4F9C;border-color:#173F7D}
.navbar-default .navbar-brand,.navbar-default .navbar-nav>li>a{color:#fff}
.navbar-default .navbar-nav>.active>a{background:#3B7DD8;color:#fff}
h4{color:#1B4F9C}
.box{background:#F5F8FD;border:1px solid #D5E2F3;border-radius:5px;padding:10px 14px;margin-bottom:12px}
.sugbox{background:#EAF7EE;border-left:4px solid #38A169;padding:8px 12px;margin:8px 0}
.warn{background:#FFF6E5;border-left:4px solid #E8A33D;padding:8px 12px;margin:8px 0}
.err{background:#FDECEA;border-left:4px solid #C0392B;padding:8px 12px;margin:8px 0}
.appfoot{position:fixed;right:10px;bottom:6px;font-size:10px;color:#8b98a8;
  background:rgba(255,255,255,.9);padding:2px 8px;border-radius:3px;z-index:1000;
  border:1px solid #e3e3e3}
.doe-logo-chip{position:fixed;top:6px;right:12px;z-index:1100;background:#fff;
  border-radius:8px;padding:3px 8px;box-shadow:0 1px 4px rgba(0,0,0,.25);
  display:flex;align-items:center}
.doe-logo-chip img{display:block}
@media (max-width:900px){.doe-logo-chip{display:none}}
table.doe{border-collapse:collapse;margin:8px 0 4px 0;font-size:13px}
table.doe th,table.doe td{border:1px solid #b9c6d6;padding:5px 10px;text-align:right}
table.doe th{background:#EAF1FB;text-align:center}
table.doe td:first-child,table.doe th:first-child{text-align:left}
table.doe caption{caption-side:top;text-align:left;font-weight:600;padding:6px 0;color:#1B4F9C}
table.doe tfoot td{background:#FAFCFF;font-size:12px}
td.cdrow{text-align:left !important;background:#FAFCFF;font-size:12px}
span.sig{color:#C0392B;font-weight:600}
sup{color:#1B4F9C;font-weight:600}
.note{font-size:12px;color:#555;font-style:italic;margin:2px 0 14px 0}
.authors{font-size:12px;color:#444}
"

## The choices for the transformation menus. This is a function, not a stored
## object: R sources the files of a package in alphabetical order, so TRANS (in
## assumptions.R) does not yet exist while this file is being loaded.
trans_choices <- function()
  stats::setNames(names(TRANS), vapply(TRANS, `[[`, character(1), "lab"))

#' The DOEpro user interface
#'
#' Builds the Shiny UI object. Called by \code{\link{run_DOEpro}}; you should not
#' normally need to call it yourself.
#'
#' @return A Shiny UI definition.
#' @keywords internal
doepro_ui <- function() navbarPage(
  title = paste0(APP_NAME, " v", APP_VERSION),
  id = "nav", collapsible = TRUE,
  header = tagList(tags$head(tags$style(HTML(paste0(APP_CSS, MEANS_CSS))),
                   tags$script(HTML(
                     "Shiny.addCustomMessageHandler('doepro_save', function(m){",
                     " try {",
                     "  var blob = new Blob([m.content], {type: m.mime || 'text/html'});",
                     "  var url = URL.createObjectURL(blob);",
                     "  var a = document.createElement('a');",
                     "  a.href = url; a.download = m.filename;",
                     "  document.body.appendChild(a); a.click();",
                     "  setTimeout(function(){ document.body.removeChild(a); URL.revokeObjectURL(url); }, 1500);",
                     " } catch(e){ alert('Download failed: ' + e.message); }",
                     "});"))),
                   tags$div(class = "doe-logo-chip",
                            tags$img(src = LOGO_URI, alt = "DOEpro", height = "34")),
                   tags$div(class = "appfoot", CREDIT_SHORT)),

  ## ------------------------------------------------------------------ data --
  tabPanel("1. Data",
    sidebarLayout(
      sidebarPanel(width = 4,
        h4("Paste from Excel"),
        radioButtons("sep", "Column separator", inline = TRUE,
                     c("Tab" = "\t", "Comma" = ",", "Semicolon" = ";", "Space" = " ")),
        checkboxInput("header", "First row contains column names", TRUE),
        textAreaInput("paste", NULL, rows = 10, width = "100%",
          placeholder = "Block  Variety  Yield  Incidence_percent\nB1  V1  42.3  18.5\nB1  V2  47.1  12.0  ..."),
        actionButton("load", "Load pasted data", class = "btn-primary"),
        tags$hr(),
        h4("or upload a CSV"),
        fileInput("file", NULL, accept = c(".csv", ".txt")),
        tags$hr(),
        h4("or try an example"),
        selectInput("demo", NULL,
          c("CRD", "RCBD", "LSD", "Factorial RCBD" = "FRCBD",
            "Split plot" = "SPLIT", "Strip plot" = "STRIP",
            "Pooled over environments (RCBD)" = "POOLRCBD",
            "Pooled over environments (CRD)" = "POOLCRD",
            "Pooled factorial over environments (RCBD)" = "POOLFACT",
            "Pooled factorial over environments (CRD)" = "POOLFACTC")),
        actionButton("loaddemo", "Load example")),
      mainPanel(width = 8,
        uiOutput("dataNote"),
        h4("Data (click a cell to edit)"),
        DTOutput("tbl"),
        tags$hr(),
        h4("Automatic screening of the response variables"),
        div(class = "note",
            "Every numeric column that is not used as a factor or block is screened, using the design and column mapping currently set on the next tab."),
        uiOutput("scanNote"),
        DTOutput("scanTab"))
    )),

  ## ------------------------------------------------------- design and anova --
  tabPanel("2. Design & ANOVA",
    sidebarLayout(
      sidebarPanel(width = 4,
        selectInput("design", "Experimental design", DESIGNS, selected = "RCBD"),
        conditionalPanel("input.design == 'FCRD' || input.design == 'FRCBD' || input.design == 'POOLFRCBD' || input.design == 'POOLFCRD'",
          sliderInput("nfac", "Number of treatment factors", 2, 4, 2, step = 1)),
        uiOutput("mapUI"),
        selectInput("alpha", "Significance level", c(0.05, 0.01), selected = 0.05),
        selectInput("dtype", "Nature of the responses (helps the adviser)",
          c("Detect automatically" = "auto", "Counts (insects, grains, spores)" = "count",
            "Percentage / proportion" = "percent", "Continuous measurement" = "continuous")),
        tags$hr(),
        h4("Transformation"),
        uiOutput("transUI"),
        actionButton("applysug", "Apply all suggested", class = "btn-default btn-sm"),
        tags$hr(),
        actionButton("run", "Run analysis", class = "btn-primary btn-lg"),
        tags$br(), tags$br(),
        actionButton("dl_anova", "ANOVA (CSV)", icon = icon("download"))),
      mainPanel(width = 8,
        uiOutput("runNote"),
        uiOutput("anovaOut"))
    )),

  ## ----------------------------------------------------------------- means --
  tabPanel("3. Means & C.D.",
    fluidRow(
      column(3, checkboxInput("letters", "Show grouping letters", TRUE)),
      column(3, checkboxInput("detailed", "Show detailed tables", FALSE)),
      column(3, numericInput("digits", "Decimal places", 2, 0, 5, 1)),
      column(3, actionButton("dl_means", "Means (CSV)", icon = icon("download")))),
    tags$hr(),
    uiOutput("meansOut")),

  ## ----------------------------------------------------------- assumptions --
  tabPanel("4. Assumptions",
    fluidRow(column(4, uiOutput("aRespUI"))),
    uiOutput("assumtxt"),
    uiOutput("sugbox"),
    fluidRow(column(6, plotOutput("bcPlot", height = "300px")),
             column(6, plotOutput("mvPlot", height = "300px"))),
    tags$hr(), h4("Residual diagnostics"),
    plotOutput("diagPlot", height = "620px")),

  ## --------------------------------------------------------------- posthoc --
  tabPanel("5. Post-hoc",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("aRespUI2"), uiOutput("phEffectUI"),
        selectInput("phMethod", "Test", PH_METHODS),
        actionButton("dl_ph", "Groups (CSV)", icon = icon("download"))),
      mainPanel(width = 9,
        uiOutput("phNote"),
        h4("Treatment groups"), DTOutput("phTab"),
        h4("Test parameters"), DTOutput("phStats"),
        uiOutput("phRangesUI"))
    )),

  ## ----------------------------------------------------------------- plots --
  tabPanel("6. Plots",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("aRespUI3"), uiOutput("plEffectUI"),
        radioButtons("plType", "Plot type",
          c("Bar chart" = "bar", "Interaction lines" = "line",
            "Heat map" = "heat", "Box plot" = "box")),
        checkboxInput("plLetters", "Show grouping letters", TRUE),
        downloadButton("dl_plot", "Plot (PNG)")),
      mainPanel(width = 9, plotOutput("mainPlot", height = "560px")))
    ),

  ## -------------------------------------------------------------- interpret --
  tabPanel("7. Interpretation & Report",
    fluidRow(
      column(4, actionButton("dl_html", "Download report (HTML)",
                             icon = icon("download"), class = "btn-primary")),
      column(6, uiOutput("pdfBtn"))),
    tags$hr(),
    uiOutput("interpOut")),

  ## ------------------------------------------------------------------ help --
  tabPanel("Help", htmlOutput("help")),

  ## ----------------------------------------------------------------- about --
  tabPanel("About", htmlOutput("about"))
)
