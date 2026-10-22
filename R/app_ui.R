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
.navbar .navbar-brand{cursor:pointer}
.navbar .navbar-brand img.slx-logo{height:30px;width:auto;display:block}
.slx-sub{display:inline-block;padding-left:12px;border-left:1px solid var(--slx-line);font:500 10px/1.25 ui-monospace,SFMono-Regular,Consolas,Menlo,monospace;
  letter-spacing:.12em;text-transform:uppercase;color:var(--slx-muted)}
@media (max-width:575.98px){.slx-sub{display:none}}
.navbar .navbar-nav{gap:2px;flex-wrap:wrap}
.navbar .nav-link{color:var(--slx-ink) !important;border-radius:999px;padding:7px 13px !important;
  font-weight:500;font-size:14px;border:0 !important}
.navbar .nav-link:hover,.navbar .nav-link:focus{background:#EEF3FB;color:var(--slx-blue-dark) !important}
.navbar .nav-link.active,.navbar .nav-item.show>.nav-link{background:var(--slx-blue);color:#fff !important}
.navbar .dropdown-menu{border-radius:14px;border:1px solid var(--slx-line);box-shadow:0 12px 32px rgba(20,40,75,.12);padding:6px}
.navbar .dropdown-item{border-radius:10px;padding:7px 12px;font-size:14px}
.navbar .dropdown-item.active,.navbar .dropdown-item:active{background:var(--slx-blue);color:#fff}
.navbar .dropdown-header{font-size:11px;letter-spacing:.08em;text-transform:uppercase;color:var(--slx-muted);padding:8px 12px 4px}
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

/* ---- cards that group a step of the work ---- */
.slx-card{background:#fff;border:1px solid var(--slx-line);border-radius:16px;padding:16px 18px;margin:16px 0;
  box-shadow:0 1px 2px rgba(20,40,75,.04)}
.slx-card>h4:first-child,.slx-card-h{margin:0 0 4px;font-size:17px;color:var(--slx-navy);font-weight:650}
.slx-card .slx-lead{font-size:13px;color:var(--slx-muted);margin:0 0 12px}
.slx-step{display:inline-flex;align-items:center;justify-content:center;width:22px;height:22px;border-radius:50%;
  background:var(--slx-blue);color:#fff;font-size:12px;font-weight:700;margin-right:8px;vertical-align:2px}
.slx-map-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:4px 14px}
.slx-map-grid .form-group{margin-bottom:8px}
.slx-ctx{display:flex;flex-wrap:wrap;gap:6px;margin:0 0 14px}
.slx-ctx span{background:#fff;border:1px solid var(--slx-line);border-radius:999px;padding:3px 11px;font-size:12.5px;color:var(--slx-ink)}
.slx-ctx span b{color:var(--slx-blue-dark);font-weight:600}
.slx-demo{background:#F3F0FF;border:1px solid #DCD3FF;border-left:4px solid var(--slx-violet);border-radius:12px;
  padding:10px 14px;margin:10px 0;font-size:13.5px}
.slx-resp{margin-top:22px;padding-top:10px;border-top:1px solid var(--slx-line)}
.slx-resp:first-child{border-top:0;padding-top:0;margin-top:6px}
table.slx-settings{width:100%;border-collapse:separate;border-spacing:0 6px}
table.slx-settings th{font-size:12.5px;color:var(--slx-muted);font-weight:600;padding:0 8px}
table.slx-settings td{vertical-align:top;padding:0 8px}
table.slx-settings .form-group{margin-bottom:0}
details.slx-fold{margin:8px 0 14px}
details.slx-fold>summary{cursor:pointer;color:var(--slx-blue-dark);font-weight:600;padding:4px 0}

/* ---- graphs: a small download menu in the corner of each ---- */
.slx-plot{position:relative;margin-bottom:6px}
.slx-plot-dl{position:absolute;top:8px;right:8px;display:flex;gap:4px;opacity:.55;transition:opacity .15s;z-index:5}
.slx-plot:hover .slx-plot-dl,.slx-plot-dl:focus-within{opacity:1}
.slx-dl{border:1px solid var(--slx-line);background:rgba(255,255,255,.95);color:var(--slx-blue-dark);border-radius:999px;
  font-size:11.5px;font-weight:600;padding:3px 9px;line-height:1.4;cursor:pointer}
.slx-dl:hover,.slx-dl:focus{background:var(--slx-blue);border-color:var(--slx-blue);color:#fff;outline:none}

/* ---- messages: colour carries meaning, not decoration ---- */
.box{background:#fff;border:1px solid var(--slx-line);border-left:4px solid var(--slx-blue);border-radius:12px;padding:12px 16px;margin:12px 0}
.sugbox{background:#EEF8F1;border:1px solid #CBE9D4;border-left:4px solid #2F9E5B;border-radius:12px;padding:10px 14px;margin:10px 0}
.warn{background:#FFF7EA;border:1px solid #F3DEB8;border-left:4px solid #E09A2C;border-radius:12px;padding:10px 14px;margin:10px 0}
.err{background:#FDEEEC;border:1px solid #F3CBC6;border-left:4px solid #C0392B;border-radius:12px;padding:10px 14px;margin:10px 0}
.note{font-size:12.5px;color:var(--slx-muted);margin:4px 0 14px 0;line-height:1.5}
ul.slx-why{font-size:13px;margin:6px 0 10px;padding-left:20px}
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
table.doe-summary td,table.doe-structure td,table.doe-screen td{text-align:left;vertical-align:top}
table.doe-summary td{font-size:13px}
.slx-scroll{overflow-x:auto;max-width:100%}
td.cdrow{text-align:left !important;background:#FAFCFF;font-size:12.5px}
span.sig{color:#C0392B;font-weight:600}
span.ns{color:#7A889E}
sup{color:var(--slx-blue-dark);font-weight:600}
.slx-kind{display:inline-block;margin-top:3px;padding:1px 8px;border-radius:999px;font-size:11px;font-weight:600;
  background:#EEF3FA;color:var(--slx-blue-dark)}
.slx-kind-designrequirement{background:#FFF4E0;color:#8A5A00}
.slx-kind-visualcheck{background:#EEF8F1;color:#1E7A44}
.slx-kind-declared{background:#F3F0FF;color:#5B3CC4}

/* ---- About: the one landing page ---- */
.slx-hero{text-align:center;padding:44px 16px 36px;margin:0 0 28px;border-radius:24px;
  background:radial-gradient(900px 380px at 20% 0%,#E4EEFD 0%,rgba(228,238,253,0) 65%),
             radial-gradient(800px 360px at 100% 100%,#EFE7FD 0%,rgba(239,231,253,0) 60%),#FBFCFF;
  border:1px solid var(--slx-line)}
.slx-chip{display:inline-flex;align-items:center;gap:7px;padding:5px 12px;border-radius:999px;
  background:#fff;border:1px solid var(--slx-line);font-size:13px;color:var(--slx-ink);font-weight:500}
.slx-dot{width:8px;height:8px;border-radius:50%;background:#2F9E5B;box-shadow:0 0 0 3px rgba(47,158,91,.18)}
.slx-hero-logo{height:64px;width:auto;margin:0 auto 22px;display:block}
.slx-h1{font:400 clamp(40px,7vw,84px)/.95 Anton,Impact,Haettenschweiler,'Arial Narrow Bold',sans-serif;text-transform:uppercase;
  letter-spacing:.005em;margin:20px auto 16px;max-width:880px;color:#0B1530}
.slx-grad{background:linear-gradient(100deg,#0A2477 0%,#1E4FD7 36%,#3B82F6 68%,#7CB7FF 100%);-webkit-background-clip:text;background-clip:text;color:transparent}
.slx-lead{font-size:16.5px;color:var(--slx-muted);max-width:720px;margin:0 auto 22px;line-height:1.6}
.slx-chips{display:flex;flex-wrap:wrap;gap:8px;justify-content:center}
.slx-section{background:#fff;border:1px solid var(--slx-line);border-radius:16px;padding:18px 22px;margin:0 0 16px}
.slx-section h3{font-size:19px;margin-top:0}

/* ---- guides folded into the tabs ---- */
.slx-guide{background:#fff;border:1px solid var(--slx-line);border-radius:12px;padding:0 16px;margin:12px 0}
.slx-guide>summary{cursor:pointer;padding:10px 0;font-weight:600;color:var(--slx-blue-dark)}
.slx-guide[open]>summary{border-bottom:1px solid var(--slx-line);margin-bottom:10px}
.slx-guide>*:last-child{margin-bottom:12px}
.slx-guide table.doe{font-size:13px}
.slx-guide table.doe th,.slx-guide table.doe td{text-align:left}
"

## The scripts of the app's own page: saving a file the server sends (as text
## or as base64 bytes), the download menu of every graph, printing the report
## from a hidden frame (no file is saved first), and, in the browser build,
## handing the About tab and the logo back to the website.
APP_JS <- "
(function () {
  function save(blob, name) {
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url; a.download = name;
    document.body.appendChild(a); a.click();
    setTimeout(function () { document.body.removeChild(a); URL.revokeObjectURL(url); }, 1500);
  }
  Shiny.addCustomMessageHandler('doepro_save', function (m) {
    try { save(new Blob([m.content], {type: m.mime || 'text/html'}), m.filename); }
    catch (e) { alert('Download failed: ' + e.message); }
  });
  Shiny.addCustomMessageHandler('doepro_save_b64', function (m) {
    try {
      var bin = atob(m.b64), n = bin.length, bytes = new Uint8Array(n);
      for (var i = 0; i < n; i++) bytes[i] = bin.charCodeAt(i);
      save(new Blob([bytes], {type: m.mime}), m.filename);
    } catch (e) { alert('Download failed: ' + e.message); }
  });
  Shiny.addCustomMessageHandler('doepro_print', function (m) {
    var old = document.getElementById('slx-print-frame');
    if (old) old.parentNode.removeChild(old);
    var f = document.createElement('iframe');
    f.id = 'slx-print-frame'; f.setAttribute('aria-hidden', 'true'); f.setAttribute('tabindex', '-1');
    f.style.cssText = 'position:fixed;right:0;bottom:0;width:0;height:0;border:0;';
    f.onload = function () {
      setTimeout(function () {
        try { f.contentWindow.focus(); f.contentWindow.print(); }
        catch (e) { alert('The print dialog could not be opened here. Download the HTML report and print it from your browser.'); }
      }, 300);
    };
    document.body.appendChild(f);
    f.srcdoc = m.html;
  });
  document.addEventListener('click', function (e) {
    var b = e.target.closest ? e.target.closest('.slx-dl') : null;
    if (!b) return;
    var id = b.getAttribute('data-plot'), el = document.getElementById(id);
    Shiny.setInputValue('plot_dl', {id: id, fmt: b.getAttribute('data-fmt'),
      w: el ? el.clientWidth : 900, h: el ? el.clientHeight : 560, n: Date.now()}, {priority: 'event'});
  });
  document.addEventListener('click', function (e) {
    var a = e.target.closest ? e.target.closest('.navbar a[data-value=about], .navbar .navbar-brand') : null;
    if (!a) return;
    var shell = null;
    try { shell = window.parent !== window ? window.parent.StatLabX : null; } catch (err) { shell = null; }
    if (shell && shell.home) { e.preventDefault(); e.stopPropagation(); shell.home(); }
  }, true);
})();
"

## The choices for the transformation menus. This is a function, not a stored
## object: R sources the files of a package in alphabetical order, so TRANS (in
## assumptions.R) does not yet exist while this file is being loaded.
trans_choices <- function()
  stats::setNames(names(TRANS), vapply(TRANS, `[[`, character(1), "lab"))

## The brand at the left of the navigation bar: the StatLabX wordmark and
## the package that powers it
nav_brand <- function()
  tagList(tags$img(src = LOGO_URI, alt = "StatLabX", class = "slx-logo"),
          tags$span(class = "slx-sub", sprintf("powered by %s %s", APP_NAME, APP_VERSION)))

## A graph with its download menu in the upper-right corner: PNG at 300 dpi,
## or SVG, of the graph as drawn, with the platform named at its foot
plot_card <- function(id, height = "320px") {
  div(class = "slx-plot",
      plotOutput(id, height = height),
      div(class = "slx-plot-dl", role = "group", `aria-label` = "Download this graph",
          tags$button(type = "button", class = "slx-dl", `data-plot` = id, `data-fmt` = "png",
                      title = "Download this graph as a PNG image (300 dpi)", icon("download"), "PNG"),
          tags$button(type = "button", class = "slx-dl", `data-plot` = id, `data-fmt` = "svg",
                      title = "Download this graph as an SVG (vector) image", "SVG")))
}

## the line of chips at the top of an analysis tab: what is being analysed
ctx_strip <- function(id) uiOutput(id)

## a step of the work on the Data tab, as a card
step_card <- function(n, title, lead, ...)
  div(class = "slx-card",
      div(class = "slx-card-h", span(class = "slx-step", n), title),
      if (!is.null(lead)) p(class = "slx-lead", lead),
      ...)

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
                             tags$script(HTML(APP_JS))),
                   tags$div(class = "appfoot", CREDIT_SHORT)),

  ## ----------------------------------------------------------------- about --
  ## first, so that the app opens on it; in the browser build the website
  ## (site/) takes its place, and choosing it there brings the website back
  tabPanel("About", value = "about", htmlOutput("about")),

  ## ------------------------------------------------------------------ data --
  tabPanel("1. Data", value = "data",
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
        selectInput("demo", NULL, DEMO_CHOICES),
        actionButton("loaddemo", "Load example"),
        div(class = "note", style = "margin-top:8px",
            "The examples are demonstration data, built for illustration; they are not from real experiments.")),
      mainPanel(width = 8,
        uiOutput("dataNote"),
        tab_guide("How to start", GUIDE_START),
        uiOutput("dqOut"),
        h4("Data (click a cell to edit)"),
        DTOutput("tbl"),
        step_card("2", "Design and variables",
          "Choose the design, the response variable(s) and the column that plays each role. Everything below and on the next tabs updates as you choose.",
          fluidRow(
            column(7, selectInput("design", "Experimental design", DESIGNS, selected = "RCBD", width = "100%")),
            column(5, selectInput("alpha", "Significance level", c("0.05 (5%)" = "0.05", "0.01 (1%)" = "0.01"),
                                  selected = "0.05", width = "100%"))),
          conditionalPanel("input.design == 'FCRD' || input.design == 'FRCBD' || input.design == 'POOLFRCBD' || input.design == 'POOLFCRD'",
            sliderInput("nfac", "Number of treatment factors", 2, 4, 2, step = 1)),
          uiOutput("mapUI"),
          uiOutput("mapNote"),
          tab_guide("The designs and their error terms", GUIDE_DESIGNS),
          conditionalPanel("['POOLRCBD', 'POOLCRD', 'POOLFRCBD', 'POOLFCRD'].indexOf(input.design) >= 0",
            tab_guide("Pooled (combined) analysis over environments", GUIDE_POOLED))),
        step_card("3", "Quick descriptive statistics",
          "A first look at each response chosen above, before any analysis.",
          uiOutput("quickOut"),
          actionButton("dl_quick", "Summary (CSV)", icon = icon("download"), class = "btn-default btn-sm")),
        step_card("4", "Automatic screening",
          "A preliminary check of each response chosen above, from the model for the design and before any transformation. The full checks, with their plots, are on the Assumptions & Diagnostics tab.",
          uiOutput("screenOut")))
    )),

  ## ------------------------------------------------- exploration and diagnostics --
  ## analyses that describe and explore the data without a design, straight
  ## after the data and before the analysis of variance
  navbarMenu("2. Exploration & Diagnostics", menuName = "explore",
    "Exploratory visualisations",
    tabPanel("Distributions and descriptive graphs", value = "explore_dist",
      sidebarLayout(
        sidebarPanel(width = 3,
          div(class = "note", paste(
            "Plots of how each numeric column's values are spread, with a fuller summary that can be split by a",
            "grouping column. Entries that are not plain numbers (such as 5,6 or 12a) count as missing here",
            "until they are corrected on the Data tab.")),
          uiOutput("descUI"),
          actionButton("dl_desc", "Table (CSV)", icon = icon("download"))),
        mainPanel(width = 9,
          h4(textOutput("descPlotTitle", inline = TRUE)),
          fluidRow(
            column(6, plot_card("descHist"), uiOutput("descHistText")),
            column(6, plot_card("descDens"), uiOutput("descDensText"))),
          fluidRow(
            column(6, plot_card("descBox"), uiOutput("descBoxText")),
            column(6, plot_card("descQQ"), uiOutput("descQQText"))),
          tags$hr(),
          uiOutput("descTab"),
          uiOutput("descText"))
      )),
    "----",
    "Data structure & quality",
    tabPanel("Data structure and quality", value = "explore_structure",
      uiOutput("structOut")),
    "----",
    "Additional exploratory analysis",
    tabPanel("Correlation", value = "explore_cor",
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
            column(7, plot_card("corHeat", "420px"), uiOutput("corHeatText")),
            column(5, plot_card("corScatter", "420px"), uiOutput("corScatterText"))))
      )),
    tabPanel("Regression", value = "explore_reg",
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
            column(7, plot_card("regMain", "440px")),
            column(5, uiOutput("regMainText"))),
          tags$hr(),
          h4("Checking the fit"),
          uiOutput("regChecks"),
          fluidRow(
            column(4, plot_card("regResid", "340px"), uiOutput("regResidText")),
            column(4, plot_card("regQQ", "340px"), uiOutput("regQQText")),
            column(4, plot_card("regObs", "340px"), uiOutput("regObsText"))))
      )),
    tabPanel("Principal components", value = "explore_pca",
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
            column(6, plot_card("pcaScree", "380px"), uiOutput("pcaScreeText")),
            column(6, plot_card("pcaScores", "380px"), uiOutput("pcaScoresText"))),
          fluidRow(
            column(6, plot_card("pcaLoadPlot", "420px"), uiOutput("pcaLoadText")),
            column(6, plot_card("pcaBiplot", "420px"), uiOutput("pcaBiplotText"))),
          uiOutput("pcaScoreTable"))
      )),
    tabPanel("Cluster analysis", value = "explore_cl",
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
            column(7, plot_card("clDendro", "440px")),
            column(5, uiOutput("clDendroText"))),
          fluidRow(
            column(6, plot_card("clPcs", "400px"), uiOutput("clPcsText")),
            column(6, plot_card("clSil", "400px"), uiOutput("clSilText"))),
          uiOutput("clItems"))
      )),
    tabPanel("Factor analysis", value = "explore_fa",
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
            column(6, plot_card("faScree", "380px"), uiOutput("faScreeText")),
            column(6, plot_card("faHeat", "380px"), uiOutput("faHeatText"))),
          fluidRow(
            column(6, plot_card("faLoadPlot", "420px"), uiOutput("faLoadText")),
            column(6, uiOutput("faScores"))))
      ))),

  ## ----------------------------------------------------------- assumptions --
  ## before the analysis of variance: the checks decide the scale it is on
  tabPanel("3. Assumptions & Diagnostics", value = "assumptions",
    ctx_strip("ctxAssum"),
    div(class = "slx-card",
      div(class = "slx-card-h", "The scale of each response"),
      p(class = "slx-lead", paste(
        "Say what kind of measurement each response is, and choose the scale it is analysed on. The ANOVA, the means",
        "and the plots all use the scale chosen here; the values as entered are used until you choose otherwise.")),
      uiOutput("transUI"),
      actionButton("applysug", "Apply every suggested transformation", class = "btn-default btn-sm")),
    fluidRow(column(4, uiOutput("aRespUI"))),
    uiOutput("asmSummary"),
    uiOutput("asmText"),
    uiOutput("sugbox"),
    tags$details(class = "slx-fold", tags$summary("The tests in detail"), uiOutput("assumtxt")),
    tab_guide("Choosing a transformation", GUIDE_TRANSFORM),
    h4("Residual diagnostics"),
    div(class = "note", "Residuals are the observed values minus those the analysis fits; the plots show whether they behave as the F-tests assume."),
    fluidRow(column(6, plot_card("diagFit", "300px"), uiOutput("diagFitText")),
             column(6, plot_card("diagQQ", "300px"), uiOutput("diagQQText"))),
    fluidRow(column(6, plot_card("diagHist", "300px"), uiOutput("diagHistText")),
             column(6, plot_card("diagScale", "300px"), uiOutput("diagScaleText"))),
    fluidRow(column(6, plot_card("diagCook", "300px"), uiOutput("diagCookText"))),
    h4("Choosing a transformation"),
    fluidRow(column(6, plot_card("bcPlot", "300px"), uiOutput("bcText")),
             column(6, plot_card("mvPlot", "300px"), uiOutput("mvText")))),

  ## ------------------------------------------------------------------ anova --
  tabPanel("4. ANOVA", value = "anova",
    ctx_strip("ctxAnova"),
    fluidRow(column(12, actionButton("dl_anova", "ANOVA tables (CSV)", icon = icon("download"), class = "btn-default btn-sm"))),
    uiOutput("runNote"),
    uiOutput("anovaOut")),

  ## ---------------------------------------------------- means and comparisons --
  tabPanel("5. Means & Comparisons", value = "means",
    ctx_strip("ctxMeans"),
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("cmpRespUI"),
        selectInput("phMethod", "Compare the means by", CMP_CHOICES, selected = "none"),
        ## a control means something only to Dunnett's test
        conditionalPanel("input.phMethod == 'Dunnett'",
          uiOutput("phControlUI"),
          radioButtons("phAlt", "Compare each treatment with the control",
            c("Does it differ? (two-sided)" = "two.sided", "Is it higher? (one-sided)" = "greater",
              "Is it lower? (one-sided)" = "less"))),
        uiOutput("phEffectUI"),
        uiOutput("phByUI"),
        numericInput("digits", "Decimal places", 2, 0, 5, 1),
        checkboxInput("detailed", "Show detailed tables (every effect)", FALSE),
        actionButton("dl_means", "Means (CSV)", icon = icon("download"), class = "btn-default btn-sm"),
        tags$br(), tags$br(),
        actionButton("dl_ph", "Comparisons (CSV)", icon = icon("download"), class = "btn-default btn-sm"),
        tags$br(), tags$br(),
        actionButton("dl_ph_pairs", "Every pair (CSV)", icon = icon("download"), class = "btn-default btn-sm")),
      mainPanel(width = 9,
        uiOutput("meansOut"),
        uiOutput("phOut"),
        tab_guide("How SE(m), SE(d) and the C.D. are worked out", GUIDE_ERRORS),
        tab_guide("Which comparison to choose", GUIDE_POSTHOC))
    )),

  ## ----------------------------------------------------------------- plots --
  tabPanel("6. Plots", value = "plots",
    ctx_strip("ctxPlots"),
    tabsetPanel(id = "plotTabs", type = "pills",
      tabPanel("Means", value = "plot_means",
        sidebarLayout(
          sidebarPanel(width = 3,
            uiOutput("aRespUI3"), uiOutput("plEffectUI"), uiOutput("plXUI"),
            radioButtons("plType", "Plot type",
              c("Bar chart" = "bar", "Points with error bars" = "line",
                "Heat map" = "heat", "Box plot" = "box")),
            uiOutput("plLettersUI")),
          mainPanel(width = 9, plot_card("mainPlot", "540px"), uiOutput("mainPlotText")))),
      tabPanel("Interaction plots", value = "plot_inter",
        sidebarLayout(
          sidebarPanel(width = 3,
            uiOutput("ipRespUI"), uiOutput("ipEffectUI"), uiOutput("ipXUI"),
            checkboxInput("ipCI", "Show confidence intervals of the means (where they can be given)", TRUE)),
          mainPanel(width = 9, plot_card("interPlot", "540px"), uiOutput("interPlotText")))))),

  ## -------------------------------------------------------------- interpret --
  tabPanel("7. Interpretation & Report", value = "report",
    ctx_strip("ctxReport"),
    div(class = "slx-card",
      div(class = "slx-card-h", "The report"),
      p(class = "slx-lead", paste(
        "The full analysis as one document: the ANOVA, the means with the comparison chosen on the Means &",
        "Comparisons tab, the checks and the interpretation, with the StatLabX credit and the date.")),
      actionButton("print_report", "Print / Save as PDF", icon = icon("print"), class = "btn-primary"),
      actionButton("dl_html", "Download HTML", icon = icon("download"), class = "btn-default"),
      uiOutput("pdfBtn", inline = TRUE),
      div(class = "note", style = "margin-top:10px", paste(
        "Print / Save as PDF opens your browser's print dialog: choose 'Save as PDF' as the printer to keep a PDF.",
        "Tables are kept whole and wide tables print on a landscape page where your browser allows."))),
    uiOutput("interpOut"))
)
