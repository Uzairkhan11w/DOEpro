###############################################################################
##  CHARTS IN THE BROWSER
###############################################################################
## The app's graphs are drawn in the browser, as SVG in the platform's own
## style, from the plots this package builds with ggplot2. ggplot2 still
## decides what is drawn: every bar, interval, point, line, label and axis
## comes from ggplot_build(), so the readings beside each graph, worked out
## from the same plots, stay true. The browser only draws it, crisp at any
## size, with tooltips, and saves it as PNG or SVG exactly as it is shown
## (APP_CHART_JS). The plot functions keep returning ggplot objects for use
## outside the app.

## each geom as the browser draws it; anything else is left out, and the
## tests check that no plot of the app uses one
CHART_GEOMS <- c(GeomCol = "bar", GeomBar = "bar", GeomRect = "rect", GeomTile = "tile",
                 GeomPoint = "point", GeomLine = "line", GeomPath = "path",
                 GeomErrorbar = "errorbar", GeomLinerange = "errorbar",
                 GeomHline = "hline", GeomVline = "vline", GeomAbline = "abline",
                 GeomText = "text", GeomLabel = "text", GeomSegment = "segment",
                 GeomBoxplot = "box", GeomRibbon = "ribbon", GeomArea = "area",
                 GeomSmooth = "smooth", GeomRug = "rug")

## the columns each kind of mark needs
CHART_COLS <- list(
  bar = c("xmin", "xmax", "ymin", "ymax", "x", "y", "fill", "colour", "alpha", "group"),
  rect = c("xmin", "xmax", "ymin", "ymax", "fill", "colour", "alpha", "group"),
  tile = c("xmin", "xmax", "ymin", "ymax", "x", "y", "fill", "colour", "alpha", "group"),
  point = c("x", "y", "colour", "fill", "shape", "size", "alpha", "group"),
  line = c("x", "y", "group", "colour", "linewidth", "linetype", "alpha"),
  path = c("x", "y", "group", "colour", "linewidth", "linetype", "alpha"),
  errorbar = c("x", "xmin", "xmax", "ymin", "ymax", "colour", "linewidth", "group"),
  hline = c("yintercept", "colour", "linewidth", "linetype", "alpha"),
  vline = c("xintercept", "colour", "linewidth", "linetype", "alpha", "group"),
  abline = c("slope", "intercept", "colour", "linewidth", "linetype", "alpha"),
  text = c("x", "y", "label", "colour", "size", "hjust", "vjust", "angle", "fontface", "alpha"),
  segment = c("x", "y", "xend", "yend", "colour", "linewidth", "linetype", "alpha"),
  box = c("x", "xmin", "xmax", "ymin", "lower", "middle", "upper", "ymax", "outliers", "fill", "colour", "group"),
  ribbon = c("x", "ymin", "ymax", "group", "fill", "colour", "alpha"),
  area = c("x", "y", "ymin", "ymax", "group", "fill", "colour", "alpha"),
  smooth = c("x", "y", "ymin", "ymax", "group", "colour", "fill", "alpha", "linewidth"),
  rug = c("x", "y", "colour", "alpha"))

## colours as #RRGGBB, so the browser and the tests compare like with like
hex_col <- function(x) {
  out <- rep(NA_character_, length(x))
  x <- as.character(x)
  ok <- !is.na(x) & nzchar(x) & x != "NA"
  if (any(ok)) {
    m <- grDevices::col2rgb(x[ok])
    out[ok] <- sprintf("#%02X%02X%02X", m[1, ], m[2, ], m[3, ])
  }
  out
}

## line types by name: solid, dashed, dotted, ...
LINETYPES <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
lty_name <- function(x) {
  x <- as.character(x)
  i <- suppressWarnings(as.integer(x))
  ifelse(!is.na(i) & i >= 0 & i <= 6, LINETYPES[pmin(pmax(i, 0), 6) + 1L], x)
}

## text justification as a number from 0 to 1
just_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  v <- suppressWarnings(as.numeric(x))
  words <- c(left = 0, bottom = 0, center = 0.5, centre = 0.5, middle = 0.5, right = 1, top = 1)
  v[is.na(v)] <- unname(words[x[is.na(v)]])
  v[is.na(v)] <- 0.5
  v
}

## One layer's marks: the kind, the columns it needs, colours as hex and the
## arrows of segments. NULL for a layer that draws nothing.
chart_layer <- function(layer, d) {
  cls <- class(layer$geom)
  hit <- cls[cls %in% names(CHART_GEOMS)][1]
  if (is.na(hit) || !nrow(d)) return(NULL)
  kind <- CHART_GEOMS[[hit]]
  ## a histogram's bars stand edge to edge
  if (kind == "bar" && inherits(layer$stat, "StatBin")) kind <- "hist"
  cols <- CHART_COLS[[if (kind == "hist") "bar" else kind]]
  out <- d[, intersect(c("PANEL", cols), names(d)), drop = FALSE]
  out$PANEL <- as.integer(as.character(out$PANEL))
  for (cc in intersect(c("colour", "fill"), names(out))) out[[cc]] <- hex_col(out[[cc]])
  if ("linetype" %in% names(out)) out$linetype <- lty_name(out$linetype)
  for (cc in intersect(c("hjust", "vjust"), names(out))) out[[cc]] <- just_num(out[[cc]])
  if ("label" %in% names(out)) out$label <- as.character(out$label)
  if ("shape" %in% names(out)) out$shape <- as.character(out$shape)
  for (cc in setdiff(names(out), c("PANEL", "colour", "fill", "linetype", "label", "shape", "outliers")))
    if (!is.numeric(out[[cc]])) out[[cc]] <- suppressWarnings(as.numeric(as.character(out[[cc]])))
  if ("outliers" %in% names(out)) out$outliers <- lapply(out$outliers, function(z) as.numeric(z))
  rownames(out) <- NULL
  arrow <- layer$geom_params$arrow
  ## the grouping letters of a plot of means (plot_main() labels them .let),
  ## which the browser draws as pills; any other text stays plain text
  lab <- layer$mapping$label
  lets <- kind == "text" && !is.null(lab) && identical(rlang::as_label(lab), ".let")
  list(kind = kind, geom = sub("^Geom", "", hit), data = out,
       arrow = if (inherits(arrow, "arrow")) list(ends = c("first", "last", "both")[arrow$ends[1]],
                                                  closed = identical(as.integer(arrow$type[1]), 2L)) else NULL,
       se = if (kind == "smooth") isTRUE(layer$stat_params$se) else NULL,
       letters = if (lets) TRUE else NULL)
}

## The kinds of mark that show aesthetic `a` in its legend: the layers that
## map it and do not leave the legend out. The browser draws each key as
## those marks: a swatch for bars, boxes and tiles, a point, a line, or both.
legend_kinds <- function(p, a) {
  out <- character(0)
  for (l in p$layers) {
    sl <- l$show.legend
    if (isFALSE(sl) || (!is.null(names(sl)) && a %in% names(sl) && isFALSE(sl[[a]]))) next
    m <- names(l$mapping)
    if (isTRUE(l$inherit.aes)) m <- c(m, names(p$mapping))
    if (!a %in% m) next
    hit <- intersect(class(l$geom), names(CHART_GEOMS))[1]
    if (!is.na(hit)) out <- c(out, CHART_GEOMS[[hit]])
  }
  unique(out)
}

## an axis of a panel: its range and the breaks with their labels, where a
## discrete axis puts its levels at 1, 2, 3, ...
chart_axis <- function(sc, range) {
  br <- sc$get_breaks()
  lab <- sc$get_labels()
  disc <- isTRUE(sc$is_discrete())
  pos <- if (disc) attr(br, "pos") %||% tryCatch(sc$map(br), error = function(e) seq_along(br)) else br
  pos <- suppressWarnings(as.numeric(pos))
  lab <- as.character(lab)
  if (length(lab) != length(pos)) lab <- rep_len(lab, length(pos))
  keep <- !is.na(pos) & pos >= min(range) - 1e-9 & pos <= max(range) + 1e-9
  list(range = as.numeric(range), breaks = pos[keep], labels = lab[keep], discrete = disc)
}

## the labels of a plot as it is drawn, set ones and those from mappings alike
chart_labs <- function(p) {
  f <- tryCatch(getExportedValue("ggplot2", "get_labs"), error = function(e) NULL)
  l <- if (is.function(f)) tryCatch(suppressMessages(f(p)), error = function(e) NULL) else NULL
  l %||% p$labels
}

## Every legend the plot shows: a discrete one as items (label and the
## fill, colour, shape and line type of its key) and the kinds of mark its
## keys stand for, a colour bar as its colour stops. Legends with the same
## title and labels (colour and shape of one factor) are merged.
chart_legends <- function(p, labs) {
  gd <- tryCatch(getExportedValue("ggplot2", "get_guide_data"), error = function(e) NULL)
  if (!is.function(gd)) return(list())
  out <- list()
  for (a in c("fill", "colour", "shape", "linetype")) {
    g <- tryCatch(suppressMessages(gd(p, a)), error = function(e) NULL)
    if (is.null(g) || !is.data.frame(g) || !nrow(g) || !".label" %in% names(g)) next
    ttl <- labs[[a]]
    ttl <- if (is.null(ttl) || is.function(ttl) || !length(ttl)) "" else as.character(ttl)[1]
    if (is.numeric(g$.value) && a %in% c("fill", "colour")) {
      v <- g$.value
      out[[length(out) + 1L]] <- list(type = "bar", title = ttl, aes = a,
        stops = data.frame(at = (v - min(v)) / max(diff(range(v)), 1e-12), colour = hex_col(g[[a]]),
                           label = as.character(g$.label), stringsAsFactors = FALSE))
      next
    }
    items <- data.frame(label = as.character(g$.label), stringsAsFactors = FALSE)
    items[[a]] <- if (a %in% c("fill", "colour")) hex_col(g[[a]]) else if (a == "linetype") lty_name(g[[a]]) else as.character(g[[a]])
    hit <- 0L
    for (k in seq_along(out))
      if (identical(out[[k]]$type, "items") && identical(out[[k]]$title, ttl) &&
          identical(out[[k]]$items$label, items$label)) hit <- k
    if (hit) {
      out[[hit]]$items[[a]] <- items[[a]]
      out[[hit]]$keys <- union(out[[hit]]$keys, legend_kinds(p, a))
    } else out[[length(out) + 1L]] <- list(type = "items", title = ttl, items = items, keys = legend_kinds(p, a))
  }
  out
}

## The chart the browser draws for plot `p`: titles, panels with their
## axes, layers of marks, legends, the aspect ratio when the plot fixes one,
## and the name of the file it saves to.
chart_spec <- function(p, what = "graph", resp = NULL) {
  if (is.null(p)) return(NULL)
  b <- suppressMessages(ggplot2::ggplot_build(p))
  labs <- chart_labs(p)
  lay <- b$layout$layout
  fv <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))
  panels <- lapply(seq_len(nrow(lay)), function(k) {
    pp <- b$layout$panel_params[[k]]
    list(panel = as.integer(as.character(lay$PANEL[k])), row = as.integer(lay$ROW[k]), col = as.integer(lay$COL[k]),
         strip = if (length(fv)) paste(vapply(fv, function(v) as.character(lay[[v]][k]), ""), collapse = ", ") else NULL,
         x = chart_axis(pp$x, pp$x.range), y = chart_axis(pp$y, pp$y.range))
  })
  layers <- Filter(Negate(is.null), lapply(seq_along(b$data), function(i) chart_layer(p$layers[[i]], b$data[[i]])))
  txt <- function(x) if (is.null(x) || is.function(x) || !length(x) || is.na(x[1])) NULL else as.character(x)[1]
  ## an aspect ratio the plot fixes (coord_equal), and axis labels it hides
  ratio <- p$coordinates$ratio
  blank <- function(el) any(grepl("element_blank", class(p$theme[[el]]), fixed = TRUE))
  list(title = txt(labs$title), subtitle = txt(labs$subtitle), caption = txt(labs$caption),
       xlab = txt(labs$x), ylab = txt(labs$y),
       ratio = if (is.numeric(ratio) && length(ratio) == 1L && is.finite(ratio)) ratio else NULL,
       hide = list(x = blank("axis.text.x") || blank("axis.text"), y = blank("axis.text.y") || blank("axis.text")),
       panels = panels, layers = layers, legends = chart_legends(p, labs),
       file = sub("[.]$", "", plot_file(what, resp, "")))
}

## The output that holds a chart, with its download menu: PNG at 300 dpi,
## or SVG, of the chart as it is shown, with the platform named at its foot
chart_output <- function(id, height = "320px") {
  div(class = "slx-plot",
      div(id = id, class = "slx-chart", style = sprintf("height:%s", height), role = "img"),
      div(class = "slx-plot-dl", role = "group", `aria-label` = "Download this graph",
          tags$button(type = "button", class = "slx-dl", `data-plot` = id, `data-fmt` = "png",
                      title = "Download this graph as a PNG image (300 dpi)", icon("download"), "PNG"),
          tags$button(type = "button", class = "slx-dl", `data-plot` = id, `data-fmt` = "svg",
                      title = "Download this graph as an SVG (vector) image", "SVG")))
}

## the render function of a chart output: `fun` returns a ggplot (or NULL)
render_chart <- function(fun, what = "graph", resp = NULL) {
  shiny::createRenderFunction(function() {
    p <- fun()
    chart_spec(p, if (is.function(what)) what() else what,
               if (is.function(resp)) tryCatch(resp(), error = function(e) NULL) else resp)
  }, function(value, session, name, ...) value, outputFunc = chart_output)
}

## The browser's half: draws the charts chart_spec() describes, in the
## platform's style, and saves them as PNG (2400 pixels wide, marked 300 dpi)
## or SVG. Plain JavaScript, no library: every byte here is paid for on the
## first visit. Kept as a raw string, so it reads as the JavaScript it is.
APP_CHART_JS <- r"---(/* StatLabX charts: the app's graphs drawn in the browser as SVG, from the
   chart descriptions R sends (chart_spec()). R decides what is drawn; this
   only draws it, in the platform's style, and saves it as PNG or SVG. */
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg";
  var FONT = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif";
  var INK = "#14284B", TXT = "#3D4B63", MUTED = "#6B7A93", GRID = "#E3E9F2", AXIS = "#C9D3E2";
  var BRAND_TOP = "#9CC3FF", BRAND = "#3B7DD8";
  var uid = 0;
  var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  /* ------------------------------------------------------------ helpers */
  function S(tag, a, parent) {
    var e = document.createElementNS(NS, tag);
    if (a) for (var k in a) if (a[k] !== undefined && a[k] !== null && a[k] !== false) e.setAttribute(k, a[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  var mctx = document.createElement("canvas").getContext("2d");
  function tw(s, size, weight) { mctx.font = (weight || 400) + " " + size + "px " + FONT; return mctx.measureText(String(s)).width; }
  function str(x) { return typeof x === "string" ? x : ""; }
  function arr(v) { return v === undefined || v === null ? [] : (Array.isArray(v) ? v : [v]); }
  function nrows(d) { var n = 0; for (var k in d) n = Math.max(n, arr(d[k]).length); return n; }
  function rows(d) {
    var n = nrows(d), keys = Object.keys(d), cols = {}, out = [], i, k;
    for (k = 0; k < keys.length; k++) cols[keys[k]] = arr(d[keys[k]]);
    for (i = 0; i < n; i++) { var r = {}; for (k = 0; k < keys.length; k++) r[keys[k]] = cols[keys[k]][i]; out.push(r); }
    return out;
  }
  function num(v) { return typeof v === "number" && isFinite(v); }
  /* a figure's minus sign as a true minus, which a hyphen is too short to
     read as: -2.5 shows as \u22122.5. Only labels that are wholly a figure. */
  function minus(s) {
    s = String(s);
    return /^-\d*\.?\d+(e[+-]?\d+)?\**$/i.test(s) ? "\u2212" + s.slice(1) : s;
  }
  function fnum(v) {
    if (!num(v)) return "-";
    var a = Math.abs(v);
    if (a !== 0 && (a < 0.001 || a >= 1e6)) return minus(v.toExponential(2));
    return minus(String(+v.toPrecision(4)));
  }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  function rgb(h) {
    h = String(h || "").replace("#", "");
    if (h.length !== 6) return [59, 125, 216];
    return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
  }
  function mix(h, w, t) {
    var a = rgb(h), b = rgb(w);
    return "rgb(" + [0, 1, 2].map(function (i) { return Math.round(a[i] + (b[i] - a[i]) * t); }).join(",") + ")";
  }
  function lum(h) { var c = rgb(h); return (0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2]) / 255; }
  function same(a, b) { return String(a || "").toUpperCase() === String(b || "").toUpperCase(); }
  var DASH = { solid: null, blank: "0 1000", dashed: "6 5", dotted: "1.5 4", dotdash: "1.5 4 6 4", longdash: "11 5", twodash: "7 3 2 3" };

  /* the brand's own blues (bars, histogram, boxes) get the brand gradient */
  function isBrand(c) { return same(c, "#3B7DD8") || same(c, "#BBD3F2") || same(c, "#3B82F6"); }

  /* ------------------------------------------------------------ the chart */
  function render(el, spec, opt) {
    opt = opt || {};
    var W = Math.round(opt.width || el.clientWidth), H = Math.round(opt.height || el.clientHeight);
    while (el.firstChild) el.removeChild(el.firstChild);
    if (!spec || !W || !H) return null;
    var id = "slx" + (++uid), anim = !!opt.animate && !reduce && !opt.exporting;
    var svg = S("svg", { xmlns: NS, width: W, height: H, viewBox: "0 0 " + W + " " + H, "font-family": FONT, role: "img",
                         "aria-label": str(spec.title) || "Chart" }, el);
    S("title", null, svg).textContent = str(spec.title);
    var defs = S("defs", null, svg);
    if (opt.exporting) S("rect", { x: 0, y: 0, width: W, height: H, fill: "#FFFFFF" }, svg);
    var grads = {};
    function grad(top, bottom, vertical) {
      var key = top + "|" + bottom + "|" + vertical;
      if (grads[key]) return grads[key];
      var gid = id + "g" + Object.keys(grads).length;
      var g = S("linearGradient", vertical === false ? { id: gid, x1: 0, y1: 0, x2: 1, y2: 0 } : { id: gid, x1: 0, y1: 0, x2: 0, y2: 1 }, defs);
      S("stop", { offset: 0, "stop-color": top }, g);
      S("stop", { offset: 1, "stop-color": bottom }, g);
      return (grads[key] = "url(#" + gid + ")");
    }
    function fillOf(c, invert) {
      if (isBrand(c)) return invert ? grad(BRAND, BRAND_TOP) : grad(BRAND_TOP, BRAND);
      return invert ? grad(c, mix(c, "#FFFFFF", 0.32)) : grad(mix(c, "#FFFFFF", 0.32), c);
    }
    var markers = {};
    function marker(c, closed) {
      var key = c + closed;
      if (markers[key]) return markers[key];
      var mid = id + "m" + Object.keys(markers).length;
      var m = S("marker", { id: mid, viewBox: "0 0 10 10", refX: 9, refY: 5, markerWidth: 7, markerHeight: 7, orient: "auto-start-reverse" }, defs);
      S("path", closed ? { d: "M0,1 L10,5 L0,9 Z", fill: c } : { d: "M1,1 L9,5 L1,9", fill: "none", stroke: c, "stroke-width": 1.6, "stroke-linecap": "round", "stroke-linejoin": "round" }, m);
      return (markers[key] = "url(#" + mid + ")");
    }

    var padX = 18, top = 16, right = opt.exporting ? padX : 132;
    /* titles */
    var y = top;
    /* words into lines: the first line may be narrower (beside the download
       buttons), and the last line kept ends in an ellipsis if text is left */
    function wrapText(t, size, weight, maxW, firstW, most) {
      var words = String(t).replace(/\s+/g, " ").trim().split(" "), lines = [], cur = "";
      words.forEach(function (w) {
        var next = cur ? cur + " " + w : w, lim = lines.length ? maxW : (firstW || maxW);
        if (tw(next, size, weight) > lim && cur) { lines.push(cur); cur = w; } else cur = next;
      });
      if (cur) lines.push(cur);
      if (most && lines.length > most) {
        lines = lines.slice(0, most);
        var last = lines[most - 1];
        while (last && tw(last + "\u2026", size, weight) > maxW) last = last.replace(/\s*\S+$/, "");
        lines[most - 1] = last + "\u2026";
      }
      return lines;
    }
    var title = str(spec.title), sub = str(spec.subtitle);
    if (title) {
      wrapText(title, 15, 700, W - 2 * padX, W - padX - right, 3).forEach(function (ln) {
        y += 18; S("text", { x: padX, y: y, "font-size": 15, "font-weight": 700, fill: INK }, svg).textContent = ln;
      });
    }
    if (sub) wrapText(sub, 12, 400, W - 2 * padX, title ? 0 : W - padX - right, 2).forEach(function (ln, i) {
      y += i ? 15 : 17; S("text", { x: padX, y: y, "font-size": 12, fill: MUTED }, svg).textContent = ln;
    });

    /* legends, as pills under the title */
    var legends = arr(spec.legends), lx = padX, ly = y + (title || sub ? 12 : 0), rowH = 24, colourOf = {};
    function newRow() { lx = padX; ly += rowH + 6; }
    /* a legend's title as written: upper case would turn Pearson's r into R */
    function legendTitle(ttl, extra) {
      var w0 = tw(ttl, 11.5, 600);
      if (lx + w0 + extra > W - padX && lx > padX) newRow();
      S("text", { x: lx, y: ly + 16, "font-size": 11.5, "font-weight": 600, fill: TXT }, svg).textContent = ttl;
      lx += w0 + 10;
    }
    legends.forEach(function (lg) {
      var ttl = str(lg.title);
      if (lg.type === "bar") {
        var stops = rows(lg.stops || {}), bw = 132;
        if (lx + (ttl ? tw(ttl, 11.5, 600) + 10 : 0) + bw + 22 > W - padX && lx > padX) newRow();
        if (ttl) legendTitle(ttl, bw + 22);
        var gid = id + "lg" + uid++;
        var g = S("linearGradient", { id: gid, x1: 0, y1: 0, x2: 1, y2: 0 }, defs);
        stops.forEach(function (s) { S("stop", { offset: s.at, "stop-color": s.colour }, g); });
        S("rect", { x: lx, y: ly + 7, width: bw, height: 9, rx: 4.5, fill: "url(#" + gid + ")" }, svg);
        if (stops.length) {
          S("text", { x: lx, y: ly + 30, "font-size": 10.5, fill: MUTED }, svg).textContent = minus(stops[0].label);
          S("text", { x: lx + bw, y: ly + 30, "font-size": 10.5, fill: MUTED, "text-anchor": "end" }, svg).textContent = minus(stops[stops.length - 1].label);
        }
        lx += bw + 22; return;
      }
      /* each key drawn as the marks it stands for: a swatch for bars, boxes
         and tiles, a point, a line, or a line through a point */
      var items = rows(lg.items || {}), keys = arr(lg.keys);
      var has = function (list) { return keys.some(function (k) { return list.indexOf(k) >= 0; }); };
      var areaKey = has(["bar", "hist", "rect", "tile", "box", "area", "ribbon"]),
          lineKey = has(["line", "path", "smooth", "segment", "errorbar", "abline", "hline", "vline"]),
          pointKey = has(["point"]);
      if (ttl) legendTitle(ttl, 60);
      items.forEach(function (it) {
        var lab = minus(it.label), w = tw(lab, 11.5, 500) + 34;
        if (lx + w > W - padX && lx > padX) newRow();
        var g = S("g", null, svg);
        S("rect", { x: lx, y: ly + 1, width: w, height: 22, rx: 11, fill: "#F5F8FD", stroke: "#E2E8F2" }, g);
        var c = it.colour || it.fill || INK, sx0 = lx + 9, sy0 = ly + 12;
        var swatch = it.fill && (areaKey || (!keys.length && !it.linetype));
        if (swatch) S("rect", { x: sx0, y: sy0 - 5, width: 10, height: 10, rx: 3, fill: fillOf(it.fill) }, g);
        else if (pointKey && !lineKey) shape(g, sx0 + 5, sy0, 4, it.shape || "16", it.fill || c, c);
        else if ((lineKey && !pointKey) || (it.linetype && !it.shape))
          S("line", { x1: sx0 - 1, x2: sx0 + 12, y1: sy0, y2: sy0, stroke: c, "stroke-width": 2, "stroke-dasharray": DASH[it.linetype] || null, "stroke-linecap": "round" }, g);
        else {
          S("line", { x1: sx0 - 2, x2: sx0 + 12, y1: sy0, y2: sy0, stroke: c, "stroke-width": 2, "stroke-linecap": "round" }, g);
          shape(g, sx0 + 5, sy0, 3.6, it.shape || "16", it.fill || c, c);
        }
        S("text", { x: lx + 26, y: ly + 16, "font-size": 11.5, "font-weight": 500, fill: TXT }, g).textContent = lab;
        if (it.fill) colourOf[String(it.fill).toUpperCase()] = lab;
        if (it.colour) colourOf[String(it.colour).toUpperCase()] = lab;
        lx += w + 6;
      });
      lx += 10;
    });
    if (legends.length) ly += rowH + 4;
    var plotTop = Math.max(y + (title || sub ? 14 : 4), legends.length ? ly + 6 : 0);

    /* the caption, and the platform's name on a saved chart */
    var capLines = str(spec.caption) ? wrapText(spec.caption, 11, 400, W - 2 * padX) : [];
    var bottom = H - 10 - capLines.length * 15 - (opt.exporting ? 16 : 0);
    capLines.forEach(function (ln, i) {
      S("text", { x: padX, y: bottom + 18 + i * 15, "font-size": 11, fill: MUTED }, svg).textContent = ln;
    });
    if (opt.exporting)
      S("text", { x: W - padX, y: H - 9, "font-size": 10.5, fill: "#8A97AD", "text-anchor": "end", "font-weight": 600 }, svg).textContent = "Generated with StatLabX";

    /* the panels */
    var panels = arr(spec.panels);
    if (!panels.length) return svg;
    var ncol = Math.max.apply(null, panels.map(function (p) { return p.col || 1; }));
    var nrow = Math.max.apply(null, panels.map(function (p) { return p.row || 1; }));
    var facet = panels.length > 1 || !!str(panels[0].strip);
    var ylab = str(spec.ylab), xlab = str(spec.xlab);
    var hide = spec.hide || {}, yw = 0;
    if (!hide.y) panels.forEach(function (p) { arr(p.y.labels).forEach(function (l) { yw = Math.max(yw, tw(l, 11.5, 400)); }); });
    var left = padX + (ylab ? 22 : 0) + yw + 10;
    var gap = 18, stripH = facet ? 22 : 0;
    var availW = W - left - padX, panW = (availW - gap * (ncol - 1)) / ncol;
    /* x labels: rotate when they would overlap */
    var rot = false, xh = hide.x ? 2 : 16;
    if (!hide.x) panels.forEach(function (p) {
      var labs = arr(p.x.labels), br = arr(p.x.breaks), span = p.x.range[1] - p.x.range[0];
      if (labs.length > 1) {
        var room = panW * Math.min.apply(null, br.slice(1).map(function (b, i) { return Math.abs(b - br[i]); })) / span;
        var widest = Math.max.apply(null, labs.map(function (l) { return tw(l, 11.5, 400); }));
        if (widest + 8 > room) rot = true;
      }
    });
    if (rot) {
      var wmax = 0;
      panels.forEach(function (p) { arr(p.x.labels).forEach(function (l) { wmax = Math.max(wmax, tw(l, 11.5, 400)); }); });
      xh = Math.min(110, wmax * 0.64 + 14);
    }
    var bottomAxis = xh + 8 + (xlab ? 22 : 0);
    var availH = bottom - plotTop - bottomAxis, panH = (availH - (gap + stripH) * (nrow - 1) - stripH) / nrow;
    if (panW < 40 || panH < 40) return svg;

    var ratio = num(spec.ratio) ? spec.ratio : null;
    var tipEl = opt.exporting ? null : tooltip(el);
    var allBoxes = [], bx0 = Infinity, bx1 = -Infinity;
    panels.forEach(function (p, pi) {
      var col = (p.col || 1) - 1, row = (p.row || 1) - 1;
      var x0 = left + col * (panW + gap), y0 = plotTop + stripH + row * (panH + gap + stripH);
      var pw = panW, ph = panH;
      var xr = p.x.range, yr = p.y.range;
      if (ratio) {
        var ux = xr[1] - xr[0], uy = yr[1] - yr[0], s = Math.min(pw / ux, ph / (uy * ratio));
        var nw = s * ux, nh = s * uy * ratio;
        x0 += (pw - nw) / 2; y0 += (ph - nh) / 2; pw = nw; ph = nh;
      }
      bx0 = Math.min(bx0, x0); bx1 = Math.max(bx1, x0 + pw);
      var sx = function (v) { return x0 + (v - xr[0]) / (xr[1] - xr[0]) * pw; };
      var sy = function (v) { return y0 + ph - (v - yr[0]) / (yr[1] - yr[0]) * ph; };
      var xLabel = function (v) {
        if (!p.x.discrete) return fnum(v);
        var br = arr(p.x.breaks), labs = arr(p.x.labels), best = -1, d = 1e9;
        br.forEach(function (b, i) { if (Math.abs(b - v) < d) { d = Math.abs(b - v); best = i; } });
        return best >= 0 && d <= 0.5 ? labs[best] : fnum(v);
      };
      var yLabel = function (v) {
        if (!p.y.discrete) return fnum(v);
        var br = arr(p.y.breaks), labs = arr(p.y.labels), best = -1, d = 1e9;
        br.forEach(function (b, i) { if (Math.abs(b - v) < d) { d = Math.abs(b - v); best = i; } });
        return best >= 0 && d <= 0.5 ? labs[best] : fnum(v);
      };
      if (facet && str(p.strip)) {
        S("rect", { x: x0, y: y0 - stripH, width: pw, height: stripH - 4, rx: 8, fill: "#EEF3FB" }, svg);
        S("text", { x: x0 + pw / 2, y: y0 - stripH / 2 - 2, "font-size": 11, "font-weight": 600, fill: INK, "text-anchor": "middle", "dominant-baseline": "central" }, svg).textContent = p.strip;
      }
      /* grid and axes */
      var gG = S("g", null, svg);
      arr(p.y.breaks).forEach(function (b, i) {
        var yy = sy(b);
        S("line", { x1: x0, x2: x0 + pw, y1: yy, y2: yy, stroke: GRID, "stroke-dasharray": "3 4" }, gG);
        if (col === 0 && !hide.y) S("text", { x: x0 - 9, y: yy, "font-size": 11.5, fill: MUTED, "text-anchor": "end", "dominant-baseline": "central" }, gG).textContent = minus(arr(p.y.labels)[i]);
      });
      arr(p.x.breaks).forEach(function (b, i) {
        var xx = sx(b);
        if (!p.x.discrete) S("line", { x1: xx, x2: xx, y1: y0, y2: y0 + ph, stroke: GRID, "stroke-dasharray": "3 4", opacity: 0.8 }, gG);
        var lab = minus(arr(p.x.labels)[i]);
        if (!hide.x && (row === nrow - 1 || !panels.some(function (q) { return q.col === p.col && q.row === p.row + 1; }))) {
          if (rot) S("text", { x: xx, y: y0 + ph + 12, "font-size": 11.5, fill: MUTED, "text-anchor": "end", transform: "rotate(-38 " + xx + " " + (y0 + ph + 12) + ")" }, gG).textContent = lab;
          else S("text", { x: xx, y: y0 + ph + 18, "font-size": 11.5, fill: MUTED, "text-anchor": "middle" }, gG).textContent = lab;
        }
      });
      S("line", { x1: x0, x2: x0 + pw, y1: y0 + ph + 0.5, y2: y0 + ph + 0.5, stroke: AXIS, "stroke-width": 1 }, gG);
      if (p.x.discrete === false && p.y.discrete === false && !ratio) { /* scatter-like: a light left axis too */
        S("line", { x1: x0 - 0.5, x2: x0 - 0.5, y1: y0, y2: y0 + ph, stroke: AXIS, "stroke-width": 1, opacity: 0.7 }, gG);
      }
      /* marks, clipped to the panel: bars, lines and bands at its edge; points,
         intervals and labels a little beyond it, so that one on the edge is
         not cut in half. Reference lines thus sit under the points. */
      var cid = id + "c" + pi, cid2 = id + "k" + pi;
      S("rect", { x: x0 - 2, y: y0 - 2, width: pw + 4, height: ph + 4 }, S("clipPath", { id: cid }, defs));
      S("rect", { x: x0 - 12, y: y0 - 14, width: pw + 24, height: ph + 26 }, S("clipPath", { id: cid2 }, defs));
      var gM = S("g", { "clip-path": "url(#" + cid + ")" }, svg), gP = S("g", { "clip-path": "url(#" + cid2 + ")" }, svg);
      var ctx = { svg: svg, g: gM, gP: gP, sx: sx, sy: sy, x0: x0, y0: y0, pw: pw, ph: ph, xr: xr, anim: anim, panel: p.panel, xLabel: xLabel, yLabel: yLabel,
                  colourOf: colourOf, xlab: xlab, ylab: ylab, tip: tipEl, fillOf: fillOf, marker: marker, boxes: allBoxes, tiles: [], placed: [] };
      arr(spec.layers).forEach(function (L, li) { layer(ctx, L, li); });
    });
    /* axis titles */
    if (ylab) {
      var cy = plotTop + (bottom - bottomAxis - plotTop) / 2, yx = Math.max(padX + 8, bx0 - yw - 24);
      S("text", { x: yx, y: cy, "font-size": 12, "font-weight": 600, fill: TXT, "text-anchor": "middle", transform: "rotate(-90 " + yx + " " + cy + ")" }, svg).textContent = ylab;
    }
    if (xlab) S("text", { x: isFinite(bx0) ? (bx0 + bx1) / 2 : left + availW / 2, y: bottom - 4, "font-size": 12, "font-weight": 600, fill: TXT, "text-anchor": "middle" }, svg).textContent = xlab;
    return svg;
  }

  /* point shapes, by ggplot2's numbers */
  function shape(g, x, y, r, sh, fill, stroke, extra) {
    sh = String(sh);
    if (sh === "3" || sh === "4" || sh === "8") {   /* plus, cross, asterisk */
      var q = r * 0.78, dd = "";
      if (sh !== "4") dd += "M" + (x - r) + "," + y + "H" + (x + r) + "M" + x + "," + (y - r) + "V" + (y + r);
      if (sh !== "3") dd += "M" + (x - q) + "," + (y - q) + "L" + (x + q) + "," + (y + q) + "M" + (x - q) + "," + (y + q) + "L" + (x + q) + "," + (y - q);
      var ex = S("path", { d: dd, fill: "none", stroke: stroke, "stroke-width": 2.2, "stroke-linecap": "round" }, g);
      for (var k0 in (extra || {})) ex.setAttribute(k0, extra[k0]);
      return ex;
    }
    var open = { "0": "square", "1": "circle", "2": "triangle", "5": "diamond" }[sh];
    var solid = { "15": "square", "16": "circle", "17": "triangle", "18": "diamond", "19": "circle", "20": "circle", "21": "circle", "22": "square", "23": "diamond", "24": "triangle", "25": "triangle" }[sh] || open || "circle";
    var filled = !open, withFill = ["21", "22", "23", "24", "25"].indexOf(sh) >= 0;
    var f = open ? "#FFFFFF" : (withFill ? (fill || "#FFFFFF") : stroke), s = open || withFill ? stroke : "#FFFFFF", sw = open || withFill ? 1.6 : 1.3;
    var a = extra || {}, e;
    if (solid === "circle") e = S("circle", { cx: x, cy: y, r: r }, g);
    else if (solid === "square") e = S("rect", { x: x - r * 0.88, y: y - r * 0.88, width: r * 1.76, height: r * 1.76, rx: 1.5 }, g);
    else if (solid === "triangle") e = S("path", { d: "M" + x + "," + (y - r * 1.1) + "L" + (x + r * 1.05) + "," + (y + r * 0.75) + "L" + (x - r * 1.05) + "," + (y + r * 0.75) + "Z" }, g);
    else e = S("path", { d: "M" + x + "," + (y - r * 1.15) + "L" + (x + r * 1.05) + "," + y + "L" + x + "," + (y + r * 1.15) + "L" + (x - r * 1.05) + "," + y + "Z" }, g);
    e.setAttribute("fill", f); e.setAttribute("stroke", s); e.setAttribute("stroke-width", sw);
    for (var k in a) e.setAttribute(k, a[k]);
    if (!filled) e.setAttribute("fill-opacity", 1);
    return e;
  }

  function lineCol(c) { return !c || lum(c) < 0.33 ? INK : c; }

  /* one layer of marks in one panel */
  function layer(c, L, li) {
    var d = rows(L.data || {}).filter(function (r) { return r.PANEL === undefined || r.PANEL === c.panel; });
    if (!d.length) return;
    var k = L.kind, sx = c.sx, sy = c.sy;
    var g = S("g", null, ["point", "text", "errorbar", "box", "segment", "rug"].indexOf(k) >= 0 ? c.gP : c.g);
    function tip(e, html) { if (c.tip) { e.setAttribute("class", "slx-mark"); e._tip = html; c.tip.attach(e); } }
    function grow(e, i, fromTop) {
      if (!c.anim) return;
      e.setAttribute("style", "transform-box:fill-box;transform-origin:50% " + (fromTop ? "0%" : "100%") +
        ";animation:slx-grow .75s cubic-bezier(.2,.8,.2,1) both;animation-delay:" + Math.min(0.6, i * 0.035).toFixed(3) + "s");
    }
    if (k === "bar" || k === "hist" || k === "rect") {
      d.forEach(function (r, i) {
        if (![r.xmin, r.xmax, r.ymin, r.ymax].every(num)) return;
        var xa = sx(r.xmin), xb = sx(r.xmax), ya = sy(Math.max(r.ymin, r.ymax)), yb = sy(Math.min(r.ymin, r.ymax));
        var below = r.ymax <= 0 && r.ymin <= 0 && Math.min(r.ymin, r.ymax) < 0;
        if (k === "hist") { xa += 1; xb -= 1; }
        var w = Math.max(xb - xa, 0.5), h = Math.max(yb - ya, 0);
        var rad = Math.min(k === "hist" ? 4 : 7, w / 3, h / 2), e;
        if (k === "rect") e = S("rect", { x: xa, y: ya, width: w, height: h, fill: r.fill || BRAND, "fill-opacity": num(r.alpha) ? r.alpha : 1 }, g);
        else if (below) e = S("path", { d: "M" + xa + "," + ya + "H" + (xa + w) + "V" + (yb - rad) + "Q" + (xa + w) + "," + yb + " " + (xa + w - rad) + "," + yb + "H" + (xa + rad) + "Q" + xa + "," + yb + " " + xa + "," + (yb - rad) + "Z", fill: c.fillOf(r.fill || BRAND, true) }, g);
        else e = S("path", { d: "M" + xa + "," + yb + "V" + (ya + rad) + "Q" + xa + "," + ya + " " + (xa + rad) + "," + ya + "H" + (xa + w - rad) + "Q" + (xa + w) + "," + ya + " " + (xa + w) + "," + (ya + rad) + "V" + yb + "Z", fill: c.fillOf(r.fill || BRAND) }, g);
        if (num(r.alpha) && k !== "rect") e.setAttribute("fill-opacity", r.alpha);
        grow(e, i, below);
        var series = r.fill ? c.colourOf[String(r.fill).toUpperCase()] : null;
        if (k === "hist") tip(e, "<b>" + fnum(r.xmin) + " to " + fnum(r.xmax) + "</b><br>" + fnum(r.ymax) + " " + esc(c.ylab ? c.ylab.toLowerCase() : "observations"));
        else tip(e, "<b>" + esc(c.xLabel(num(r.x) ? r.x : (r.xmin + r.xmax) / 2)) + (series ? " &middot; " + esc(series) : "") + "</b><br>" + esc(c.ylab || "Value") + ": " + fnum(num(r.y) ? r.y : r.ymax));
      });
    } else if (k === "tile") {
      d.forEach(function (r) {
        if (![r.xmin, r.xmax, r.ymin, r.ymax].every(num)) return;
        var xa = sx(r.xmin) + 1.5, xb = sx(r.xmax) - 1.5, ya = sy(r.ymax) + 1.5, yb = sy(r.ymin) - 1.5;
        var plain = same(r.fill, "#FFFFFF");
        c.tiles.push({ xmin: r.xmin, xmax: r.xmax, ymin: r.ymin, ymax: r.ymax, fill: plain ? "#F6F8FC" : (r.fill || BRAND) });
        var e = S("rect", { x: xa, y: ya, width: Math.max(xb - xa, 1), height: Math.max(yb - ya, 1), rx: 8, fill: plain ? "#F6F8FC" : (r.fill || BRAND) }, g);
        if (c.anim) e.setAttribute("style", "animation:slx-fade .6s ease both");
        tip(e, "<b>" + esc(c.xLabel(r.x)) + " &middot; " + esc(c.yLabel(r.y)) + "</b>");
      });
    } else if (k === "point") {
      var many = d.length > 300;
      d.forEach(function (r, i) {
        if (!num(r.x) || !num(r.y)) return;
        var col = r.colour || BRAND, rad = 2.6 + (num(r.size) ? r.size : 1.5) * 0.95;
        var e = shape(g, sx(r.x), sy(r.y), rad, r.shape || "19", r.fill, col,
                      num(r.alpha) ? { "fill-opacity": r.alpha } : null);
        if (c.anim && !many) e.setAttribute("style", "transform-box:fill-box;transform-origin:50% 50%;animation:slx-pop .45s cubic-bezier(.2,.8,.2,1) both;animation-delay:" + Math.min(0.7, 0.15 + i * 0.012).toFixed(3) + "s");
        var series = c.colourOf[String(col).toUpperCase()];
        tip(e, (series ? "<b>" + esc(series) + "</b><br>" : "") + esc(c.xlab || "x") + ": " + esc(c.xLabel(r.x)) + "<br>" + esc(c.ylab || "y") + ": " + esc(c.yLabel(r.y)));
      });
    } else if (k === "line" || k === "path") {
      var groups = {}, order = [];
      d.forEach(function (r) { var key = String(r.group); if (!groups[key]) { groups[key] = []; order.push(key); } groups[key].push(r); });
      order.forEach(function (key, gi) {
        var pts = groups[key].filter(function (r) { return num(r.x) && num(r.y); });
        if (pts.length < 2) return;
        var r0 = pts[0], col = r0.colour || BRAND;
        var dd = pts.map(function (r, i) { return (i ? "L" : "M") + sx(r.x).toFixed(2) + "," + sy(r.y).toFixed(2); }).join("");
        var wd = Math.max(1.3, (num(r0.linewidth) ? r0.linewidth : 0.6) * 2.2);
        var e = S("path", { d: dd, fill: "none", stroke: col, "stroke-width": wd, "stroke-linejoin": "round", "stroke-linecap": "round",
                            "stroke-dasharray": DASH[r0.linetype] || null, "stroke-opacity": num(r0.alpha) ? r0.alpha : null }, g);
        if (c.anim && !DASH[r0.linetype]) {
          var len = 0;
          for (var i = 1; i < pts.length; i++) len += Math.hypot(sx(pts[i].x) - sx(pts[i - 1].x), sy(pts[i].y) - sy(pts[i - 1].y));
          e.setAttribute("stroke-dasharray", len.toFixed(1));
          e.setAttribute("style", "stroke-dashoffset:" + len.toFixed(1) + ";animation:slx-draw 1s cubic-bezier(.3,.7,.2,1) " + (0.1 + gi * 0.08).toFixed(2) + "s forwards");
        }
      });
    } else if (k === "errorbar") {
      d.forEach(function (r) {
        if (!num(r.ymin) || !num(r.ymax)) return;
        var x = sx(num(r.x) ? r.x : (r.xmin + r.xmax) / 2), xa = num(r.xmin) ? sx(r.xmin) : x - 4, xb = num(r.xmax) ? sx(r.xmax) : x + 4;
        var col = lineCol(r.colour), op = col === INK ? 0.6 : 0.9;
        var path = "M" + x + "," + sy(r.ymin) + "V" + sy(r.ymax) + "M" + xa + "," + sy(r.ymin) + "H" + xb + "M" + xa + "," + sy(r.ymax) + "H" + xb;
        var e = S("path", { d: path, stroke: col, "stroke-width": 1.5, "stroke-opacity": op, fill: "none", "stroke-linecap": "round" }, g);
        if (c.anim) e.setAttribute("style", "animation:slx-fade .5s ease .55s both");
      });
    } else if (k === "hline" || k === "vline" || k === "abline") {
      d.forEach(function (r) {
        var col = r.colour || MUTED, wd = Math.max(1.1, (num(r.linewidth) ? r.linewidth : 0.5) * 1.8), a;
        if (k === "hline" && num(r.yintercept)) a = { x1: c.x0, x2: c.x0 + c.pw, y1: sy(r.yintercept), y2: sy(r.yintercept) };
        else if (k === "vline" && num(r.xintercept)) a = { x1: sx(r.xintercept), x2: sx(r.xintercept), y1: c.y0, y2: c.y0 + c.ph };
        else if (k === "abline" && num(r.slope) && num(r.intercept))
          a = { x1: c.x0, x2: c.x0 + c.pw, y1: sy(r.intercept + r.slope * c.xr[0]), y2: sy(r.intercept + r.slope * c.xr[1]) };
        if (!a) return;
        a.stroke = col; a["stroke-width"] = wd; a["stroke-dasharray"] = DASH[r.linetype] || null; a["stroke-linecap"] = "round";
        if (num(r.alpha)) a["stroke-opacity"] = r.alpha;
        S("line", a, g);
      });
    } else if (k === "text") {
      d.forEach(function (r, i) {
        if (!num(r.x) || !num(r.y) || r.label === null || r.label === undefined || r.label === "") return;
        var px = sx(r.x), py = sy(r.y), lab = minus(r.label);
        var fs = Math.max(9.5, (num(r.size) ? r.size : 3.9) * 3.1), bold = r.fontface === 2 || r.fontface === "bold";
        var hj = num(r.hjust) ? r.hjust : 0.5, vj = num(r.vjust) ? r.vjust : 0.5, ang = num(r.angle) ? r.angle : 0;
        var cy = py - (0.5 - vj) * fs * 1.15;
        var anchor = hj < 0.25 ? "start" : (hj > 0.75 ? "end" : "middle");
        var col = r.colour || INK;
        /* grouping letters, as pills */
        if (L.letters && !ang) {
          var w = tw(lab, fs, 700) + 12, h = fs + 7, xx = anchor === "start" ? px : (anchor === "end" ? px - w : px - w / 2);
          var gL = S("g", null, g);
          S("rect", { x: xx, y: cy - h / 2 - 3, width: w, height: h, rx: h / 2, fill: "#EEF4FF", stroke: "#D3E2FF" }, gL);
          S("text", { x: xx + w / 2, y: cy - 3, "font-size": fs, "font-weight": 700, fill: "#1E4FD7", "text-anchor": "middle", "dominant-baseline": "central" }, gL).textContent = lab;
          if (c.anim) gL.setAttribute("style", "animation:slx-fade .5s ease " + (0.6 + Math.min(0.5, i * 0.03)).toFixed(2) + "s both");
          c.placed.push({ x0: xx, x1: xx + w, y0: cy - h / 2 - 3, y1: cy + h / 2 - 3 });
          return;
        }
        if (!ang) {
          var wl = tw(lab, fs, bold ? 700 : 500), hh = fs * 0.6;
          /* a label that would run out of the panel turns back into it: from
             the side, or to the other side of its point */
          if (anchor === "start" && px + wl > c.x0 + c.pw) { anchor = "end"; px = Math.min(px, c.x0 + c.pw) - 4; }
          else if (anchor === "end" && px - wl < c.x0) { anchor = "start"; px = Math.max(px, c.x0) + 4; }
          else if (anchor === "middle") px = Math.min(Math.max(px, c.x0 + wl / 2 + 2), c.x0 + c.pw - wl / 2 - 2);
          if (cy - hh < c.y0 && cy < py) cy = 2 * py - cy;
          else if (cy + hh > c.y0 + c.ph && cy > py) cy = 2 * py - cy;
          cy = Math.min(Math.max(cy, c.y0 + hh), c.y0 + c.ph - hh);
          /* and one that would cover another label moves up or down off it
             (not the figures in a heat map's tiles, which stay in their tiles) */
          var bx = anchor === "start" ? px : (anchor === "end" ? px - wl : px - wl / 2);
          var hit = function (yc) { return c.placed.some(function (q) { return bx < q.x1 && bx + wl > q.x0 && yc - hh < q.y1 && yc + hh > q.y0; }); };
          if (!c.tiles.length && hit(cy)) {
            var step = 2 * hh + 2, best = null;
            for (var s = 1; s <= 4 && best === null; s++)
              [cy - s * step, cy + s * step].forEach(function (yc) { if (best === null && yc - hh >= c.y0 && yc + hh <= c.y0 + c.ph && !hit(yc)) best = yc; });
            if (best !== null) cy = best;
          }
          c.placed.push({ x0: bx, x1: bx + wl, y0: cy - hh, y1: cy + hh });
        }
        /* on a tile the figure takes the colour that reads on it: white on a
           dark tile, dark on a light one */
        var under = null;
        c.tiles.forEach(function (tl) { if (r.x >= tl.xmin && r.x <= tl.xmax && r.y >= tl.ymin && r.y <= tl.ymax) under = tl; });
        if (under) col = lum(under.fill) < 0.55 ? "#FFFFFF" : (lum(col) > 0.75 ? INK : col);
        var t = S("text", { x: px, y: cy, "font-size": fs, "font-weight": bold ? 700 : 500, fill: col, "text-anchor": anchor, "dominant-baseline": "central",
                            transform: ang ? "rotate(" + (-ang) + " " + px + " " + cy + ")" : null }, g);
        t.textContent = lab;
        if (!under && lum(col) < 0.75) { t.setAttribute("stroke", "#FFFFFF"); t.setAttribute("stroke-width", 3); t.setAttribute("paint-order", "stroke"); t.setAttribute("stroke-linejoin", "round"); }
      });
    } else if (k === "segment") {
      d.forEach(function (r) {
        if (![r.x, r.y, r.xend, r.yend].every(num)) return;
        var col = r.colour || INK, wd = Math.max(1.4, (num(r.linewidth) ? r.linewidth : 0.5) * 2.8);
        var a = { x1: sx(r.x), y1: sy(r.y), x2: sx(r.xend), y2: sy(r.yend), stroke: col, "stroke-width": wd, "stroke-linecap": "round",
                  "stroke-dasharray": DASH[r.linetype] || null };
        if (L.arrow) {
          var m = c.marker(col, !!L.arrow.closed);
          if (L.arrow.ends === "last" || L.arrow.ends === "both") a["marker-end"] = m;
          if (L.arrow.ends === "first" || L.arrow.ends === "both") a["marker-start"] = m;
        }
        S("line", a, g);
      });
    } else if (k === "box") {
      d.forEach(function (r, i) {
        if (![r.lower, r.upper, r.middle].every(num)) return;
        var x = sx(r.x), xa = sx(r.xmin) + 2, xb = sx(r.xmax) - 2, col = lineCol(r.colour);
        var gB = S("g", null, g);
        S("path", { d: "M" + x + "," + sy(r.ymin) + "V" + sy(r.lower) + "M" + x + "," + sy(r.upper) + "V" + sy(r.ymax) +
                       "M" + (x - (xb - xa) / 4) + "," + sy(r.ymin) + "H" + (x + (xb - xa) / 4) + "M" + (x - (xb - xa) / 4) + "," + sy(r.ymax) + "H" + (x + (xb - xa) / 4),
                    stroke: col, "stroke-width": 1.4, "stroke-opacity": 0.75, fill: "none", "stroke-linecap": "round" }, gB);
        var light = c.fillOf(r.fill || BRAND);
        var box = S("rect", { x: xa, y: sy(r.upper), width: Math.max(xb - xa, 1), height: Math.max(sy(r.lower) - sy(r.upper), 1), rx: 7,
                              fill: light, "fill-opacity": 0.55, stroke: col, "stroke-width": 1.3 }, gB);
        S("line", { x1: xa + 1, x2: xb - 1, y1: sy(r.middle), y2: sy(r.middle), stroke: col, "stroke-width": 2.4, "stroke-linecap": "round" }, gB);
        arr(r.outliers).forEach(function (o) { if (num(o)) S("circle", { cx: x, cy: sy(o), r: 3.6, fill: "#C0392B", stroke: "#FFFFFF", "stroke-width": 1.2 }, gB); });
        if (c.anim) gB.setAttribute("style", "transform-box:fill-box;transform-origin:50% 100%;animation:slx-grow .7s cubic-bezier(.2,.8,.2,1) both;animation-delay:" + Math.min(0.5, i * 0.05).toFixed(2) + "s");
        tip(box, "<b>" + esc(c.xLabel(r.x)) + "</b><br>Median " + fnum(r.middle) + "<br>Quartiles " + fnum(r.lower) + " to " + fnum(r.upper));
      });
    } else if (k === "ribbon" || k === "area" || k === "smooth") {
      var gs = {}, ord = [];
      d.forEach(function (r) { var key = String(r.group); if (!gs[key]) { gs[key] = []; ord.push(key); } gs[key].push(r); });
      ord.forEach(function (key) {
        var pts = gs[key].filter(function (r) { return num(r.x); }).sort(function (a, b) { return a.x - b.x; });
        if (pts.length < 2) return;
        var r0 = pts[0];
        var hasBand = pts.every(function (r) { return num(r.ymin) && num(r.ymax); });
        if (hasBand && (k !== "smooth" || L.se)) {
          var up = pts.map(function (r, i) { return (i ? "L" : "M") + sx(r.x).toFixed(2) + "," + sy(r.ymax).toFixed(2); }).join("");
          var dn = pts.slice().reverse().map(function (r) { return "L" + sx(r.x).toFixed(2) + "," + sy(r.ymin).toFixed(2); }).join("");
          var fc = r0.fill || r0.colour || BRAND;
          var gid = id2(c, fc, k);
          var e = S("path", { d: up + dn + "Z", fill: gid, stroke: "none" }, g);
          if (c.anim) e.setAttribute("style", "animation:slx-fade .8s ease both");
        }
        var lineY = k === "area" ? "ymax" : "y";
        var col = k === "ribbon" ? null : (r0.colour || BRAND);
        if (col) {
          var dd = pts.filter(function (r) { return num(r[lineY]); }).map(function (r, i) { return (i ? "L" : "M") + sx(r.x).toFixed(2) + "," + sy(r[lineY]).toFixed(2); }).join("");
          S("path", { d: dd, fill: "none", stroke: col, "stroke-width": k === "smooth" ? 2.4 : 2, "stroke-linejoin": "round", "stroke-linecap": "round" }, g);
        }
      });
    } else if (k === "rug") {
      d.forEach(function (r) {
        if (num(r.x)) S("line", { x1: sx(r.x), x2: sx(r.x), y1: c.y0 + c.ph, y2: c.y0 + c.ph - 8, stroke: r.colour || INK, "stroke-opacity": 0.45, "stroke-width": 1.2 }, g);
      });
    }
  }
  /* a soft vertical wash for bands and areas */
  function id2(c, colour, kind) {
    var top = kind === "area" ? 0.5 : 0.22, bottom = kind === "area" ? 0.06 : 0.16;
    var key = "band" + colour + kind;
    if (!c.svg._bands) c.svg._bands = {};
    if (c.svg._bands[key]) return c.svg._bands[key];
    var defs = c.svg.querySelector("defs"), gid = "b" + (++uid);
    var g = S("linearGradient", { id: gid, x1: 0, y1: 0, x2: 0, y2: 1 }, defs);
    var base = isBrand(colour) ? BRAND : colour;
    S("stop", { offset: 0, "stop-color": base, "stop-opacity": top }, g);
    S("stop", { offset: 1, "stop-color": base, "stop-opacity": bottom }, g);
    return (c.svg._bands[key] = "url(#" + gid + ")");
  }
  /* ------------------------------------------------------------ tooltips */
  function tooltip(el) {
    var t = document.createElement("div");
    t.className = "slx-tip";
    el.appendChild(t);
    function move(ev) {
      var r = el.getBoundingClientRect();
      t.style.left = Math.min(Math.max(ev.clientX - r.left, 70), r.width - 70) + "px";
      t.style.top = (ev.clientY - r.top) + "px";
    }
    return {
      attach: function (e) {
        e.addEventListener("mouseenter", function (ev) { t.innerHTML = e._tip; t.classList.add("on"); move(ev); });
        e.addEventListener("mousemove", move);
        e.addEventListener("mouseleave", function () { t.classList.remove("on"); });
      }
    };
  }

  /* ------------------------------------------------------------ in Shiny */
  function draw(el) {
    if (!el._spec || !el.clientWidth || !el.clientHeight) return;
    render(el, el._spec, { animate: !el._drawn });
    el._drawn = true;
    el._w = el.clientWidth;
  }
  var ro = window.ResizeObserver ? new ResizeObserver(function (entries) {
    entries.forEach(function (en) {
      var el = en.target, w = Math.round(en.contentRect.width);
      if (!w || w === el._w || !el._spec) return;
      clearTimeout(el._t);
      el._t = setTimeout(function () { draw(el); }, el._drawn ? 120 : 0);
    });
  }) : null;
  if (window.Shiny && Shiny.OutputBinding && window.jQuery) {
    var binding = new Shiny.OutputBinding();
    jQuery.extend(binding, {
      find: function (scope) { return jQuery(scope).find(".slx-chart"); },
      renderValue: function (el, data) {
        el._spec = data || null; el._drawn = false;
        if (!data) { while (el.firstChild) el.removeChild(el.firstChild); return; }
        if (ro && !el._observed) { ro.observe(el); el._observed = true; }
        draw(el);
      },
      /* a message in place of the chart: the old chart must not come back
         over it when the window is resized */
      renderError: function (el, err) {
        el._spec = null;
        Shiny.OutputBinding.prototype.renderError.call(this, el, err);
      }
    });
    Shiny.outputBindings.register(binding, "statlabx.chart");
  }

  /* ------------------------------------------------------------ saving */
  var CRC = null;
  function crc32(b) {
    if (!CRC) { CRC = []; for (var n = 0; n < 256; n++) { var c = n; for (var k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1; CRC[n] = c >>> 0; } }
    var crc = 0xFFFFFFFF;
    for (var i = 0; i < b.length; i++) crc = CRC[(crc ^ b[i]) & 0xFF] ^ (crc >>> 8);
    return (crc ^ 0xFFFFFFFF) >>> 0;
  }
  /* a PNG that says it is 300 dpi: a pHYs chunk after the header */
  function withDpi(blob, dpi, done) {
    if (!blob || !blob.arrayBuffer) return done(blob);
    blob.arrayBuffer().then(function (buf) {
      var src = new Uint8Array(buf), at = 8;
      while (at + 8 <= src.length) {
        var len = (src[at] << 24 | src[at + 1] << 16 | src[at + 2] << 8 | src[at + 3]) >>> 0;
        var type = String.fromCharCode(src[at + 4], src[at + 5], src[at + 6], src[at + 7]);
        if (type === "pHYs") return done(blob);
        if (type === "IDAT" || type === "IEND") break;
        at += 12 + len;
      }
      var ppm = Math.round(dpi / 0.0254), ch = new Uint8Array(21), dv = new DataView(ch.buffer);
      dv.setUint32(0, 9); ch.set([112, 72, 89, 115], 4); dv.setUint32(8, ppm); dv.setUint32(12, ppm); ch[16] = 1;
      dv.setUint32(17, crc32(ch.subarray(4, 17)));
      var out = new Uint8Array(src.length + 21);
      out.set(src.subarray(0, 33)); out.set(ch, 33); out.set(src.subarray(33), 54);
      done(new Blob([out], { type: "image/png" }));
    }, function () { done(blob); });
  }
  function download(id, fmt) {
    var el = document.getElementById(id);
    if (!el || !el._spec) { alert("This graph is not drawn yet."); return; }
    var w = 1000, h = Math.round(w * el.clientHeight / Math.max(el.clientWidth, 1));
    h = Math.max(560, Math.min(h, 920));
    var holder = document.createElement("div");
    holder.style.cssText = "position:fixed;left:-30000px;top:0;width:" + w + "px;height:" + h + "px;";
    document.body.appendChild(holder);
    var svg = render(holder, el._spec, { width: w, height: h, exporting: true });
    var xml = svg ? new XMLSerializer().serializeToString(svg) : "";
    document.body.removeChild(holder);
    if (!xml) { alert("This graph could not be saved."); return; }
    var name = el._spec.file || "StatLabX_graph", save = window.slxSave;
    if (fmt === "svg") { save(new Blob(['<?xml version="1.0" encoding="UTF-8"?>' + String.fromCharCode(10) + xml], { type: "image/svg+xml" }), name + ".svg"); return; }
    var img = new Image();
    img.onload = function () {
      var k = 2400 / w, cv = document.createElement("canvas");
      cv.width = Math.round(w * k); cv.height = Math.round(h * k);
      var g = cv.getContext("2d");
      g.fillStyle = "#FFFFFF"; g.fillRect(0, 0, cv.width, cv.height);
      g.scale(k, k); g.drawImage(img, 0, 0, w, h);
      cv.toBlob(function (b) { withDpi(b, 300, function (b2) { save(b2, name + ".png"); }); }, "image/png");
    };
    img.onerror = function () { alert("The PNG could not be made here; download the SVG instead."); };
    img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(xml);
  }
  window.SLXChart = { render: render, download: download };
})();)---"
