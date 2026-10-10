###############################################################################
##  UI
###############################################################################

APP_CSS <- "
/* StatLabX on Bootstrap 5. The theme is bslib's precompiled default, so the
   browser compiles no Sass; everything below is plain CSS, much of it set
   through Bootstrap's own variables. The analysis screens stay calm: one
   accent colour, white cards, lines that guide the eye. The gradient belongs
   to the brand and to the About page only. */
:root{
  --slx-ink:#1C2B4A; --slx-navy:#14284B; --slx-muted:#5B6B86; --slx-line:#E2E8F2;
  --slx-blue:#3B7DD8; --slx-blue-dark:#1B4F9C; --slx-violet:#7C4DFF; --slx-bg:#F5F7FB;
  --bs-body-font-family:system-ui,-apple-system,Segoe UI,Roboto,Helvetica Neue,Arial,sans-serif;
  --bs-body-font-size:15px; --bs-body-color:#1C2B4A; --bs-body-bg:#F5F7FB;
  --bs-primary:#3B7DD8; --bs-primary-rgb:59,125,216;
  --bs-link-color:#1B4F9C; --bs-link-color-rgb:27,79,156; --bs-link-hover-color:#3B7DD8;
  --bs-border-color:#E2E8F2; --bs-border-radius:10px;
}
body{background:var(--slx-bg);color:var(--slx-ink)}
h1,h2,h3,h4,h5{color:var(--slx-navy);font-weight:650;letter-spacing:-.005em}
h4{font-size:17px;margin-top:14px}

/* ---- navigation: a floating pill ---- */
.navbar.navbar-default{background:rgba(255,255,255,.94);border:1px solid var(--slx-line);border-radius:28px;
  margin:12px 16px 22px;padding:6px 10px 6px 14px;box-shadow:0 8px 28px rgba(20,40,75,.08);
  position:sticky;top:10px;z-index:1030;-webkit-backdrop-filter:saturate(1.4) blur(10px);backdrop-filter:saturate(1.4) blur(10px)}
.navbar.navbar-default .container-fluid{padding:0;flex-wrap:wrap}
.navbar .navbar-brand{display:flex;align-items:center;gap:10px;padding:2px 0;margin-right:16px;
  color:var(--slx-navy);text-decoration:none;white-space:nowrap}
.navbar .navbar-brand img{height:32px;width:auto}
.slx-word{font-size:20px;font-weight:800;letter-spacing:-.015em;line-height:1.05;color:var(--slx-navy)}
.slx-x{background:linear-gradient(90deg,#3B7DD8,#7C4DFF);-webkit-background-clip:text;background-clip:text;color:transparent}
.slx-sub{display:block;font-size:11px;font-weight:500;color:var(--slx-muted);letter-spacing:0}
.navbar .navbar-nav{gap:2px;flex-wrap:wrap}
.navbar .nav-link{color:var(--slx-ink) !important;border-radius:999px;padding:7px 13px !important;
  font-weight:500;font-size:14px;border:0 !important}
.navbar .nav-link:hover,.navbar .nav-link:focus{background:#EEF3FB;color:var(--slx-blue-dark) !important}
.navbar .nav-link.active,.navbar .nav-item.show>.nav-link{background:var(--slx-blue);color:#fff !important}
.navbar .dropdown-menu{border-radius:14px;border:1px solid var(--slx-line);box-shadow:0 12px 32px rgba(20,40,75,.12);padding:6px}
.navbar .dropdown-item{border-radius:10px;padding:7px 12px;font-size:14px}
.navbar .dropdown-item.active,.navbar .dropdown-item:active{background:var(--slx-blue);color:#fff}
.navbar .navbar-toggle,.navbar .navbar-toggler{border:1px solid var(--slx-line);border-radius:999px;margin-left:auto}
@media (max-width:767.98px){.navbar.navbar-default{border-radius:22px;position:static}}

/* ---- panels and content ---- */
.tab-content{padding:0 4px 48px}
.well{background:#fff;border:1px solid var(--slx-line);border-radius:16px;box-shadow:0 1px 2px rgba(20,40,75,.04);padding:18px}
.well h4:first-child{margin-top:0}
.control-label{font-weight:600;font-size:13.5px;color:var(--slx-navy)}
.form-control,.selectize-input{border-radius:10px !important;border-color:var(--slx-line) !important;box-shadow:none !important}
.selectize-input.focus,.form-control:focus{border-color:var(--slx-blue) !important;box-shadow:0 0 0 3px rgba(59,125,216,.15) !important}
.btn{border-radius:999px;font-weight:500;padding:7px 16px}
.btn-primary{background:var(--slx-blue);border-color:var(--slx-blue)}
.btn-primary:hover,.btn-primary:focus{background:var(--slx-blue-dark);border-color:var(--slx-blue-dark)}
.btn-default{background:#fff;border:1px solid var(--slx-line);color:var(--slx-ink)}
.btn-default:hover{background:#EEF3FB;border-color:#CDDAEE;color:var(--slx-blue-dark)}
.btn-lg{padding:10px 24px;font-size:16px}
.shiny-plot-output{background:#fff;border:1px solid var(--slx-line);border-radius:14px;overflow:hidden}
.dataTables_wrapper{background:#fff;border:1px solid var(--slx-line);border-radius:14px;padding:10px 12px;margin:6px 0 12px}
table.dataTable{font-size:13.5px;font-variant-numeric:tabular-nums}
hr{border-color:var(--slx-line);opacity:1}

/* ---- messages: colour carries meaning, not decoration ---- */
.box{background:#fff;border:1px solid var(--slx-line);border-left:4px solid var(--slx-blue);border-radius:12px;padding:12px 16px;margin:12px 0}
.sugbox{background:#EEF8F1;border:1px solid #CBE9D4;border-left:4px solid #2F9E5B;border-radius:12px;padding:10px 14px;margin:10px 0}
.warn{background:#FFF7EA;border:1px solid #F3DEB8;border-left:4px solid #E09A2C;border-radius:12px;padding:10px 14px;margin:10px 0}
.err{background:#FDEEEC;border:1px solid #F3CBC6;border-left:4px solid #C0392B;border-radius:12px;padding:10px 14px;margin:10px 0}
.note{font-size:12.5px;color:var(--slx-muted);margin:4px 0 14px 0;line-height:1.5}
.authors{font-size:12.5px;color:var(--slx-muted)}
.appfoot{position:fixed;right:12px;bottom:8px;font-size:10.5px;color:var(--slx-muted);
  background:rgba(255,255,255,.92);padding:3px 10px;border-radius:999px;z-index:1000;border:1px solid var(--slx-line)}

/* ---- tables of the analysis: numbers that line up ---- */
table.doe{border-collapse:collapse;margin:10px 0 6px 0;font-size:13.5px;background:#fff;font-variant-numeric:tabular-nums}
table.doe th,table.doe td{border:1px solid #D6E0EE;padding:6px 11px;text-align:right}
table.doe th{background:#EEF3FA;color:var(--slx-navy);font-weight:600;text-align:center}
table.doe td:first-child,table.doe th:first-child{text-align:left}
table.doe caption{caption-side:top;text-align:left;font-weight:600;padding:6px 0;color:var(--slx-navy)}
table.doe tfoot td{background:#FAFCFF;font-size:12.5px}
td.cdrow{text-align:left !important;background:#FAFCFF;font-size:12.5px}
span.sig{color:#C0392B;font-weight:600}
span.ns{color:#7A889E}
sup{color:var(--slx-blue-dark);font-weight:600}

/* ---- About: the one landing page ---- */
.slx-hero{text-align:center;padding:44px 16px 36px;margin:0 0 28px;border-radius:24px;
  background:radial-gradient(900px 380px at 20% 0%,#E4EEFD 0%,rgba(228,238,253,0) 65%),
             radial-gradient(800px 360px at 100% 100%,#EFE7FD 0%,rgba(239,231,253,0) 60%),#FBFCFF;
  border:1px solid var(--slx-line)}
.slx-chip{display:inline-flex;align-items:center;gap:7px;padding:5px 12px;border-radius:999px;
  background:#fff;border:1px solid var(--slx-line);font-size:13px;color:var(--slx-ink);font-weight:500}
.slx-dot{width:8px;height:8px;border-radius:50%;background:#2F9E5B;box-shadow:0 0 0 3px rgba(47,158,91,.18)}
.slx-h1{font-size:clamp(30px,5vw,52px);font-weight:800;letter-spacing:-.025em;line-height:1.08;margin:18px auto 14px;max-width:880px;color:var(--slx-navy)}
.slx-grad{background:linear-gradient(90deg,#3B7DD8,#6C63FF 55%,#7C4DFF);-webkit-background-clip:text;background-clip:text;color:transparent}
.slx-lead{font-size:16.5px;color:var(--slx-muted);max-width:720px;margin:0 auto 22px;line-height:1.6}
.slx-chips{display:flex;flex-wrap:wrap;gap:8px;justify-content:center}
.slx-section{background:#fff;border:1px solid var(--slx-line);border-radius:16px;padding:18px 22px;margin:0 0 16px}
.slx-section h3{font-size:19px;margin-top:0}
"

## The choices for the transformation menus. This is a function, not a stored
## object: R sources the files of a package in alphabetical order, so TRANS (in
## assumptions.R) does not yet exist while this file is being loaded.
trans_choices <- function()
  stats::setNames(names(TRANS), vapply(TRANS, `[[`, character(1), "lab"))

## The brand at the left of the navigation bar: the logo, the platform's
## name and the package that powers it
nav_brand <- function()
  tagList(tags$img(src = LOGO_URI, alt = ""),
          tags$span(tags$span(class = "slx-word", HTML("StatLab<span class='slx-x'>X</span>")),
                    tags$span(class = "slx-sub", sprintf("powered by %s %s", APP_NAME, APP_VERSION))))

#' The DOEpro user interface
#'
#' Builds the Shiny UI object. Called by \code{\link{run_DOEpro}}; you should not
#' normally need to call it yourself.
#'
#' @return A Shiny UI definition.
#' @keywords internal
doepro_ui <- function() navbarPage(
  title = nav_brand(), windowTitle = "StatLabX",
  ## bslib's precompiled Bootstrap 5: a theme with changed variables would be
  ## compiled from Sass in the browser on every visit (5 s natively), so the
  ## look is set in APP_CSS instead
  theme = bslib::bs_theme(version = 5),
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
        uiOutput("dqOut"),
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
    uiOutput("asmText"),
    uiOutput("sugbox"),
    fluidRow(column(6, plotOutput("bcPlot", height = "300px"), uiOutput("bcText")),
             column(6, plotOutput("mvPlot", height = "300px"), uiOutput("mvText"))),
    tags$hr(), h4("Residual diagnostics"),
    fluidRow(column(6, plotOutput("diagFit", height = "300px"), uiOutput("diagFitText")),
             column(6, plotOutput("diagQQ", height = "300px"), uiOutput("diagQQText"))),
    fluidRow(column(6, plotOutput("diagHist", height = "300px"), uiOutput("diagHistText")),
             column(6, plotOutput("diagScale", height = "300px"), uiOutput("diagScaleText")))),

  ## --------------------------------------------------------------- posthoc --
  tabPanel("5. Post-hoc",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("aRespUI2"), uiOutput("phEffectUI"),
        selectInput("phMethod", "Test", stats::setNames(PH_METHODS, PH_LABELS[PH_METHODS])),
        ## a control means something only to Dunnett's test
        conditionalPanel("input.phMethod == 'Dunnett'",
          uiOutput("phControlUI"),
          radioButtons("phAlt", "Compare each treatment with the control",
            c("Does it differ? (two-sided)" = "two.sided", "Is it higher? (one-sided)" = "greater",
              "Is it lower? (one-sided)" = "less"))),
        actionButton("dl_ph", "Groups (CSV)", icon = icon("download")),
        tags$br(), tags$br(),
        actionButton("dl_ph_pairs", "Pairwise (CSV)", icon = icon("download"))),
      mainPanel(width = 9,
        uiOutput("phNote"),
        h4("Treatment groups"), DTOutput("phTab"), uiOutput("phText"),
        h4("Test parameters"), DTOutput("phStats"),
        uiOutput("phRangesUI"),
        h4("Pairwise comparisons"),
        div(class = "note", paste(
          "Every pair of means: their difference, the standard error of that difference,",
          "the critical value and critical difference it is judged against, and the verdict.")),
        DTOutput("phPairs"), uiOutput("phPairsText"))
    )),

  ## ----------------------------------------------------------------- plots --
  tabPanel("6. Plots",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("aRespUI3"), uiOutput("plEffectUI"), uiOutput("plXUI"),
        radioButtons("plType", "Plot type",
          c("Bar chart" = "bar", "Interaction lines" = "line",
            "Heat map" = "heat", "Box plot" = "box")),
        checkboxInput("plLetters", "Show grouping letters", TRUE),
        downloadButton("dl_plot", "Plot (PNG)")),
      mainPanel(width = 9, plotOutput("mainPlot", height = "560px"), uiOutput("mainPlotText")))
    ),

  ## -------------------------------------------------------------- interpret --
  tabPanel("7. Interpretation & Report",
    fluidRow(
      column(4, actionButton("dl_html", "Download report (HTML)",
                             icon = icon("download"), class = "btn-primary")),
      column(6, uiOutput("pdfBtn"))),
    tags$hr(),
    uiOutput("interpOut")),

  ## ---------------------------------------------------------------- explore --
  ## analyses that describe and explore the data without a design; the
  ## numbered tabs keep the analysis-of-variance workflow in order
  navbarMenu("Explore",
    tabPanel("Descriptive statistics",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "A summary of each numeric column, and plots of how its values are spread.",
            "Entries that are not plain numbers (such as 5,6 or 12a) count as missing here",
            "until they are corrected on the Data tab.")),
          uiOutput("descUI"),
          actionButton("dl_desc", "Table (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("descTab"),
          uiOutput("descText"),
          tags$hr(),
          h4(textOutput("descPlotTitle", inline = TRUE)),
          fluidRow(
            column(6, plotOutput("descHist", height = "320px"), uiOutput("descHistText")),
            column(6, plotOutput("descDens", height = "320px"), uiOutput("descDensText"))),
          fluidRow(
            column(6, plotOutput("descBox", height = "320px"), uiOutput("descBoxText")),
            column(6, plotOutput("descQQ", height = "320px"), uiOutput("descQQText"))))
      )),
    tabPanel("Correlation",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "How each pair of numeric columns varies together. Each pair uses every row with both values;",
            "entries that are not plain numbers count as missing until they are corrected on the Data tab.")),
          uiOutput("corUI"),
          radioButtons("corMethod", "Coefficient",
            c("Pearson's r (straight-line association)" = "pearson",
              "Spearman's rho (ranks)" = "spearman",
              "Kendall's tau (ranks, pairs of rows)" = "kendall")),
          selectInput("corAlpha", "Significance level", c(0.05, 0.01), selected = 0.05),
          uiOutput("corPairUI"),
          actionButton("dl_cor", "Pairs (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("corMatrix"),
          uiOutput("corText"),
          uiOutput("corPairs"),
          tags$hr(),
          fluidRow(
            column(7, plotOutput("corHeat", height = "420px"), uiOutput("corHeatText")),
            column(5, plotOutput("corScatter", height = "420px"), uiOutput("corScatterText"))))
      )),
    tabPanel("Regression",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "A straight-line equation for predicting one numeric column from one or more others, fitted by least",
            "squares. Only rows with every value are used; entries that are not plain numbers count as missing",
            "until they are corrected on the Data tab.")),
          uiOutput("regUI"),
          selectInput("regAlpha", "Significance level", c(0.05, 0.01), selected = 0.05),
          uiOutput("regShowUI"),
          actionButton("dl_reg", "Coefficients (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("regEquation"),
          uiOutput("regCoef"),
          uiOutput("regModel"),
          uiOutput("regText"),
          uiOutput("regAnova"),
          tags$hr(),
          fluidRow(
            column(7, plotOutput("regMain", height = "440px")),
            column(5, uiOutput("regMainText"))),
          tags$hr(),
          h4("Checking the fit"),
          uiOutput("regChecks"),
          fluidRow(
            column(4, plotOutput("regResid", height = "340px"), uiOutput("regResidText")),
            column(4, plotOutput("regQQ", height = "340px"), uiOutput("regQQText")),
            column(4, plotOutput("regObs", height = "340px"), uiOutput("regObsText"))))
      )),
    tabPanel("Principal components",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "Principal component analysis: a few new variables, the components, that carry as much as possible of",
            "how the chosen columns vary together. Only rows with every value are used; entries that are not plain",
            "numbers count as missing until they are corrected on the Data tab.")),
          uiOutput("pcaUI"),
          radioButtons("pcaScale", "Analyse",
            c("Correlations: each variable standardised (when units differ)" = "cor",
              "Covariances: variables as measured (same units only)" = "cov")),
          uiOutput("pcaAxesUI"),
          uiOutput("pcaGroupUI"),
          actionButton("dl_pca_load", "Loadings (CSV)", icon = icon("download")),
          actionButton("dl_pca_scores", "Scores (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("pcaEigen"),
          uiOutput("pcaText"),
          uiOutput("pcaLoadings"),
          tags$hr(),
          fluidRow(
            column(6, plotOutput("pcaScree", height = "380px"), uiOutput("pcaScreeText")),
            column(6, plotOutput("pcaScores", height = "380px"), uiOutput("pcaScoresText"))),
          fluidRow(
            column(6, plotOutput("pcaLoadPlot", height = "420px"), uiOutput("pcaLoadText")),
            column(6, plotOutput("pcaBiplot", height = "420px"), uiOutput("pcaBiplotText"))),
          uiOutput("pcaScoreTable"))
      )),
    tabPanel("Cluster analysis",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "Hierarchical clustering: rows, or the means of each level of a grouping column (varieties by their",
            "trait means), joined step by step into a tree and cut into clusters of items alike in the chosen columns.",
            "Only rows with every value are used.")),
          uiOutput("clUI"),
          uiOutput("clGroupUI"),
          checkboxInput("clScale", "Standardise each variable (when units differ)", TRUE),
          selectInput("clLink", "Linkage", c("Ward's method" = "ward", "Complete linkage" = "complete",
                                             "Average linkage (UPGMA)" = "average", "Single linkage" = "single")),
          conditionalPanel("input.clLink != 'ward'",
            selectInput("clDist", "Distance", c("Euclidean" = "euclidean", "Manhattan" = "manhattan"))),
          conditionalPanel("input.clLink == 'ward'",
            div(class = "note", "Ward's method works on Euclidean distances.")),
          uiOutput("clKUI"),
          actionButton("dl_cl", "Clusters (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("clText"),
          uiOutput("clMembers"),
          uiOutput("clMeans"),
          tags$hr(),
          fluidRow(
            column(7, plotOutput("clDendro", height = "440px")),
            column(5, uiOutput("clDendroText"))),
          fluidRow(
            column(6, plotOutput("clPcs", height = "400px"), uiOutput("clPcsText")),
            column(6, plotOutput("clSil", height = "400px"), uiOutput("clSilText"))),
          uiOutput("clItems"))
      )),
    tabPanel("Factor analysis",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "Factor analysis (maximum likelihood): a few underlying factors that account for the correlations between",
            "the chosen columns. Only rows with every value are used; entries that are not plain numbers count as",
            "missing until they are corrected on the Data tab.")),
          uiOutput("faUI"),
          uiOutput("faKUI"),
          selectInput("faRot", "Rotation", c("Varimax (factors uncorrelated)" = "varimax",
                                             "Promax (factors may correlate)" = "promax", "None" = "none")),
          selectInput("faAlpha", "Significance level", c(0.05, 0.01), selected = 0.05),
          uiOutput("faAxesUI"),
          actionButton("dl_fa_load", "Loadings (CSV)", icon = icon("download")),
          actionButton("dl_fa_scores", "Scores (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          uiOutput("faText"),
          uiOutput("faAdequacy"),
          uiOutput("faLoadings"),
          uiOutput("faVariance"),
          uiOutput("faPhi"),
          tags$hr(),
          fluidRow(
            column(6, plotOutput("faScree", height = "380px"), uiOutput("faScreeText")),
            column(6, plotOutput("faHeat", height = "380px"), uiOutput("faHeatText"))),
          fluidRow(
            column(6, plotOutput("faLoadPlot", height = "420px"), uiOutput("faLoadText")),
            column(6, uiOutput("faScores"))))
      ))),

  ## ------------------------------------------------------------------ help --
  tabPanel("Help", htmlOutput("help")),

  ## ----------------------------------------------------------------- about --
  tabPanel("About", htmlOutput("about"))
)
