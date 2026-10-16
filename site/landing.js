/* StatLabX website: the page people land on, shown at once while R starts
   underneath in the app's frame (#root). build_app.R puts this into
   docs/index.html after every shinylive export, and fills in FILES with the
   size of every file a first visit downloads (R, its built-in library and
   the packages), read from docs/ itself, and the facts block (#slx-facts)
   with figures from the package: the worked example's ANOVA and means.

   Loading. webR downloads its files from its own worker, where the page's
   fetch cannot see them; the addition to shinylive's service worker
   (sw-progress.js) counts the bytes of each file as it arrives and reports
   them on the "statlabx-loading" channel. Progress is those bytes against
   the total; no percentage is printed and nothing is estimated. The stages
   are read from which files have arrived: R itself, R starting (webR asks
   for the package list once R runs), the packages, R installing and loading
   them, then the app's page in its frame, which ends when Shiny connects.
   The words say "loading", not "downloading": later visits take the same
   bytes from the browser's copy, and the page cannot tell the two apart.

   Opening. "Start analysing" shows the app as soon as Shiny has connected,
   or a small sheet with the progress until then. The app's own About tab and
   logo bring this page back (window.StatLabX.home). #app in the address
   opens the app directly, and the Back button returns here. */
(function () {
  "use strict";
  var FILES = /*FILES*/{};
  var doc = document, body = doc.body;
  var site = doc.getElementById("slx-site");
  if (!site) return;
  doc.documentElement.classList.add("slx-js");
  body.classList.add("slx-shell");
  var FACTS = {};
  try { FACTS = JSON.parse(doc.getElementById("slx-facts").textContent); } catch (e) { FACTS = {}; }

  // shinylive reloads the page once on a first visit, to put its service
  // worker in charge; the intro then plays only the first time
  try {
    if (sessionStorage.getItem("slx-intro")) site.classList.add("no-intro");
    sessionStorage.setItem("slx-intro", "1");
  } catch (e) { /* storage blocked: the intro plays again */ }

  function $(id) { return doc.getElementById(id); }
  var root = $("root");
  // the page decides where it scrolls when the app closes, not the browser
  try { if ("scrollRestoration" in history) history.scrollRestoration = "manual"; } catch (e) { /* older browsers */ }

  // ------------------------------------------------ the hero's worked example
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  function heroCards() {
    var stage = $("s-stage");
    if (!stage || !FACTS.means) return;
    var m = FACTS.means, n = m.length, W = 340, H = 172, pad = 6, top = 26, base = H - 22;
    var max = 0, i;
    for (i = 0; i < n; i++) max = Math.max(max, m[i].mean + m[i].se);
    var step = (W - 2 * pad) / n, bw = Math.min(34, step * 0.56);
    var bars = "";
    for (i = 0; i < n; i++) {
      var x = pad + step * i + (step - bw) / 2, h = (base - top) * m[i].mean / max, y = base - h;
      var e = (base - top) * m[i].se / max, cx = x + bw / 2;
      bars += '<rect class="bar' + (m[i].best ? " top" : "") + '" style="animation-delay:' + (0.7 + i * 0.07).toFixed(2) +
        's" x="' + x.toFixed(1) + '" y="' + y.toFixed(1) + '" width="' + bw.toFixed(1) + '" height="' + h.toFixed(1) + '" rx="6"/>' +
        '<path class="err" d="M' + cx.toFixed(1) + " " + (y - e).toFixed(1) + "V" + (y + e).toFixed(1) + "M" + (cx - 4).toFixed(1) + " " +
        (y - e).toFixed(1) + "h8M" + (cx - 4).toFixed(1) + " " + (y + e).toFixed(1) + 'h8"/>' +
        '<text class="let" x="' + cx.toFixed(1) + '" y="' + (y - e - 6).toFixed(1) + '">' + esc(m[i].letter) + "</text>" +
        '<text class="lab" x="' + cx.toFixed(1) + '" y="' + (H - 6) + '">' + esc(m[i].lab) + "</text>";
    }
    var svg = '<svg class="s-bars" viewBox="0 0 ' + W + " " + H + '" role="img" aria-label="Bar chart of the worked example\'s means">' +
      '<defs><linearGradient id="s-bar-g" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#9CC3FF"/><stop offset="1" stop-color="#3B82F6"/></linearGradient>' +
      '<linearGradient id="s-bar-top" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#3B82F6"/><stop offset="1" stop-color="#0A2477"/></linearGradient></defs>' +
      '<line class="base" x1="0" x2="' + W + '" y1="' + base + '" y2="' + base + '"/>' + bars + "</svg>";
    var aov = "";
    (FACTS.anova || []).forEach(function (r) {
      aov += "<tr><td>" + esc(r.src) + "</td><td>" + esc(r.df) + "</td><td>" + esc(r.ms) + "</td><td>" + esc(r.f) +
        (r.mark ? ' <span class="' + (r.mark === "NS" ? "ns" : "sig") + '">' + esc(r.mark) + "</span>" : "") + "</td></tr>";
    });
    stage.innerHTML =
      '<div class="s-card s-card-means" style="--d:0"><div class="s-card-head"><div><p class="s-card-k">Table of means</p>' +
      '<p class="s-card-t">' + esc(FACTS.response) + " by " + esc(FACTS.factor) + "</p></div>" +
      '<span class="s-chip-cd">C.D. (' + esc(FACTS.level) + ") " + esc(FACTS.cd) + "</span></div>" + svg +
      '<p class="s-card-note">Means sharing a letter do not differ significantly at the ' + esc(FACTS.level) + " level.</p></div>" +
      '<div class="s-card s-card-anova" style="--d:1"><p class="s-card-k">Analysis of variance &middot; ' + esc(FACTS.design) + "</p>" +
      '<table class="s-aov"><thead><tr><th>Source</th><th>df</th><th>MS</th><th>F</th></tr></thead><tbody>' + aov + "</tbody></table></div>" +
      '<div class="s-card s-card-checks" style="--d:2"><p class="s-card-k">Assumptions</p>' +
      '<p class="s-ok">Normal residuals <b>p = ' + esc(FACTS.sw) + "</b></p>" +
      '<p class="s-ok">Equal variances <b>p = ' + esc(FACTS.hov) + "</b></p>" +
      '<p class="s-ok">CV ' + esc(FACTS.cv) + "%, " + esc(FACTS.cv_band) + "</p></div>";
  }
  heroCards();

  // the hero's cards lean gently towards the pointer
  (function () {
    var card = doc.querySelector(".s-hero-card"), stage = $("s-stage"), raf = 0, px = 0, py = 0;
    if (!card || !stage || !window.matchMedia || !matchMedia("(pointer: fine)").matches) return;
    if (matchMedia("(prefers-reduced-motion: reduce)").matches) return;
    card.addEventListener("pointermove", function (e) {
      var r = card.getBoundingClientRect();
      px = ((e.clientX - r.left) / r.width - 0.5) * 2; py = ((e.clientY - r.top) / r.height - 0.5) * 2;
      if (!raf) raf = requestAnimationFrame(function () { raf = 0; stage.style.setProperty("--px", px.toFixed(3)); stage.style.setProperty("--py", py.toFixed(3)); });
    });
    card.addEventListener("pointerleave", function () { stage.style.setProperty("--px", 0); stage.style.setProperty("--py", 0); });
  })();

  // --------------------------------------------------------- reveal, counts
  (function () {
    var els = doc.querySelectorAll("[data-reveal]");
    function count(el) {
      var b = el.querySelector("[data-count]");
      if (!b || b.dataset.done) return;
      b.dataset.done = "1";
      var to = +b.getAttribute("data-count"), t0 = performance.now();
      if (!(to > 0)) return;
      (function tick(t) {
        var k = Math.min(1, (t - t0) / 900);
        b.textContent = Math.round(to * (1 - Math.pow(1 - k, 3)));
        if (k < 1) requestAnimationFrame(tick);
      })(t0);
    }
    if (!("IntersectionObserver" in window)) { els.forEach(function (el) { el.classList.add("is-in"); }); return; }
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (!en.isIntersecting) return;
        en.target.classList.add("is-in"); count(en.target); io.unobserve(en.target);
      });
    }, { rootMargin: "0px 0px -8% 0px", threshold: 0.08 });
    els.forEach(function (el, i) { el.style.transitionDelay = ((i % 4) * 0.07) + "s"; io.observe(el); });
  })();

  // --------------------------------------------------------------- the nav
  var nav = $("s-nav"), links = $("s-links"), burger = $("s-burger");
  function onScroll() { if (nav) nav.classList.toggle("is-scrolled", window.scrollY > 8); }
  window.addEventListener("scroll", onScroll, { passive: true }); onScroll();
  if (burger) burger.addEventListener("click", function () {
    var open = links.classList.toggle("is-open");
    burger.setAttribute("aria-expanded", open ? "true" : "false");
  });
  doc.addEventListener("click", function (e) {
    var a = e.target.closest ? e.target.closest("[data-goto]") : null;
    if (!a || !site.contains(a)) return;
    e.preventDefault();
    if (links) links.classList.remove("is-open");
    if (burger) burger.setAttribute("aria-expanded", "false");
    var id = a.getAttribute("data-goto"), target = id === "top" ? null : $(id);
    if (id === "top") window.scrollTo({ top: 0, behavior: "smooth" });
    else if (target) window.scrollTo({ top: target.getBoundingClientRect().top + window.scrollY - 96, behavior: "smooth" });
  });

  // ----------------------------------------------------------- copy buttons
  doc.addEventListener("click", function (e) {
    var b = e.target.closest ? e.target.closest("[data-copy]") : null;
    if (!b) return;
    var src = $(b.getAttribute("data-copy")); if (!src) return;
    var text = src.innerText.trim(), label = b.textContent;
    function done() { b.textContent = "Copied"; setTimeout(function () { b.textContent = label; }, 1600); }
    if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(text).then(done, function () {});
    else {
      var ta = doc.createElement("textarea"); ta.value = text; doc.body.appendChild(ta); ta.select();
      try { doc.execCommand("copy"); done(); } catch (err) { /* nothing more to try */ }
      doc.body.removeChild(ta);
    }
  });

  // ============================================== R loading, as it happens
  var total = 0, f;
  for (f in FILES) total += FILES[f];
  var got = {}, ended = {}, heard = false, appStarting = false, ready = false, failed = false, unknown = false;
  var started = Date.now(), lastNews = Date.now();
  var STAGES = ["Loading R", "Starting R in your browser", "Loading the packages the analyses use",
                "Installing and loading the packages", "Starting StatLabX"];
  var view = { stage: 0, fraction: 0, working: false, amount: "" };

  function mb(bytes) { return (bytes / 1048576).toFixed(1); }
  // R itself and its built-in library sit directly in shinylive/webr/ and
  // arrive before R starts; the package list (metadata.rds) is asked for
  // once R runs, and the packages and R's other libraries (vfs/) after it
  function isCore(file) { return /^shinylive\/webr\/[^/]+$/.test(file); }
  function isList(file) { return /\/packages\/metadata\.rds$/.test(file); }

  function compute() {
    if (ready) { view = { stage: 5, fraction: 1, working: false, amount: mb(total) + " MB of R and its packages in place" }; return; }
    if (unknown || !heard) {
      view = { stage: appStarting ? 4 : 0, fraction: 1, working: true, amount: "" };
      if (unknown && !appStarting) view.label = "Loading R and its packages";
      return;
    }
    var received = 0, coreDone = true, laterDone = true, listSeen = false;
    for (f in FILES) {
      if (got[f] !== undefined) received += Math.min(got[f], FILES[f]);
      if (isList(f)) listSeen = got[f] !== undefined;
      else if (isCore(f)) { if (!ended[f]) coreDone = false; }
      else if (!ended[f]) laterDone = false;
    }
    if (coreDone && listSeen && laterDone) {
      view = { stage: appStarting ? 4 : 3, fraction: 1, working: true, amount: mb(total) + " MB of R and its packages in place" };
      return;
    }
    view = { stage: !coreDone ? 0 : (!listSeen ? 1 : 2), fraction: received / total, working: false,
             amount: mb(received) + " of " + mb(total) + " MB" };
  }

  function bar(el, fill) {
    if (!el || !fill) return;
    fill.style.width = (view.working ? 100 : Math.max(2, 100 * view.fraction)).toFixed(1) + "%";
    el.classList.toggle("is-working", !!view.working && !ready);
  }
  function stageText() {
    if (failed) return "R could not start here";
    if (ready) return "Ready";
    return view.label || STAGES[view.stage];
  }
  function render() {
    compute();
    site.classList.toggle("is-ready", ready);
    site.classList.toggle("is-failed", failed && !ready);
    var chip = $("s-status-text");
    if (chip) chip.textContent = ready ? "R is ready in your browser" :
      failed ? "R could not start here; open the app to see why" : stageText();
    var sf = $("s-status-fill"); if (sf) sf.style.width = (view.working ? 35 : Math.max(4, 100 * view.fraction)).toFixed(1) + "%";
    var hs = $("s-hud-steps");
    if (hs) Array.prototype.forEach.call(hs.children, function (li, i) {
      li.classList.toggle("is-done", ready || i < view.stage);
      li.classList.toggle("is-active", !ready && !failed && i === view.stage);
    });
    var st = $("s-hud-state"); if (st) st.textContent = ready ? "Ready" : failed ? "Stopped" : "Loading";
    bar(doc.querySelector(".s-hud-bar"), $("s-hud-fill"));
    var amt = $("s-hud-amount"); if (amt) amt.textContent = view.amount || " ";
    var ss = $("s-sheet-stage"); if (ss) ss.textContent = stageText();
    bar(doc.querySelector(".s-sheet-bar"), $("s-sheet-fill"));
    var sa = $("s-sheet-amount"); if (sa) sa.textContent = view.amount || " ";
    var fin = $("s-final-note");
    if (fin) fin.textContent = ready ? "R is ready: the app opens at once." :
      failed ? "R could not start in this browser; the app shows what went wrong." :
      "R is loading in the background while you read.";
  }

  var channel = null;
  if (typeof BroadcastChannel === "function" && total > 0) {
    channel = new BroadcastChannel("statlabx-loading");
    channel.onmessage = function (e) {
      var m = e.data || {};
      if (!m.file || !(m.file in FILES)) return;
      heard = true;
      got[m.file] = Math.max(got[m.file] || 0, m.bytes || 0);
      if (m.done) ended[m.file] = true;
      lastNews = Date.now();
      render();
    };
  } else unknown = true;
  render();

  // ================================================= the app and the page
  var pending = null, appOpen = false, toTop = false;
  function frame() { return doc.querySelector("#root iframe"); }
  function appWin() { var fr = frame(); try { return fr && fr.contentWindow; } catch (e) { return null; } }
  function appDoc() { var w = appWin(); try { return w && w.document; } catch (e) { return null; } }
  function connected() {
    var w = appWin();
    try { return !!(w && w.Shiny && w.Shiny.shinyapp && w.Shiny.shinyapp.isConnected()); } catch (e) { return false; }
  }
  function setInert(el, on) { if (!el) return; if (on) el.setAttribute("inert", ""); else el.removeAttribute("inert"); }
  setInert(root, true);

  function tab(d, value) { var a = d.querySelector('.navbar a[data-value="' + value + '"]'); if (a) a.click(); }
  function waitFor(test, ms, then) {
    var t0 = Date.now();
    (function poll() {
      var ok = false; try { ok = test(); } catch (e) { ok = false; }
      if (ok) return then(true);
      if (Date.now() - t0 > ms) return then(false);
      setTimeout(poll, 150);
    })();
  }
  // where the app opens: the Data tab the first time (it starts on About),
  // the tab the visitor left otherwise; the worked example loads the RCBD
  // example, runs it and shows the analysis of variance
  function route(target) {
    var d = appDoc(); if (!d) return;
    var active = d.querySelector(".navbar .nav-link.active");
    var onAbout = !active || active.getAttribute("data-value") === "about";
    if (target !== "example") { if (onAbout) tab(d, "data"); return; }
    var demo = FACTS.demo || "RCBD";
    tab(d, "data");
    // data the visitor has loaded are never replaced: the app just opens
    function rows() { return d.querySelectorAll("#tbl tbody tr td:not(.dataTables_empty)").length; }
    if (rows() > 0) return;
    var sel = d.getElementById("demo");
    try { if (sel && sel.selectize) sel.selectize.setValue(demo); } catch (e) { return; }
    setTimeout(function () {
      var load = d.getElementById("loaddemo"); if (load) load.click();
      waitFor(function () { return rows() > 0; }, 9000, function (ok) {
        if (!ok) return;
        // the column mapping is drawn only once its tab is showing
        tab(d, "design");
        waitFor(function () {
          var r = d.getElementById("resp"), de = d.getElementById("design");
          return r && de && r.value === FACTS.response && de.value === demo;
        }, 9000, function (ok2) {
          if (!ok2) return;
          setTimeout(function () { var run = d.getElementById("run"); if (run) run.click(); }, 300);
        });
      });
    }, 250);
  }

  var sheet = $("s-sheet"), sheetClose = $("s-sheet-close"), lastFocus = null;
  function showSheet() {
    if (!sheet || !sheet.hidden) return;
    lastFocus = doc.activeElement; sheet.hidden = false;
    if (sheetClose) sheetClose.focus();
  }
  function hideSheet() {
    if (!sheet || sheet.hidden) return;
    sheet.hidden = true;
    if (lastFocus && lastFocus.focus) lastFocus.focus();
  }
  if (sheetClose) sheetClose.addEventListener("click", function () { pending = null; hideSheet(); });
  doc.addEventListener("keydown", function (e) { if (e.key === "Escape" && sheet && !sheet.hidden) { pending = null; hideSheet(); } });

  function showApp(target) {
    hideSheet(); pending = null; appOpen = true;
    if (links) links.classList.remove("is-open");
    body.classList.add("slx-app-open");
    setInert(site, true); setInert(root, false);
    if (location.hash !== "#app") history.pushState({ slx: "app" }, "", "#app");
    route(target);
    var fr = frame(); if (fr) try { fr.focus(); } catch (e) { /* the frame takes focus when clicked */ }
  }
  function showSite() {
    appOpen = false;
    body.classList.remove("slx-app-open");
    setInert(site, false); setInert(root, true);
    if (toTop) { toTop = false; window.scrollTo(0, 0); }
  }
  function open(target) {
    target = target || "data";
    if (ready || failed) return showApp(target);
    pending = target; showSheet(); render();
  }
  function home() {
    toTop = true;
    if (history.state && history.state.slx === "app") history.back();
    else { history.replaceState(null, "", location.pathname + location.search); showSite(); }
  }
  window.addEventListener("popstate", function () {
    if (location.hash === "#app") { if (!appOpen) open("keep"); }
    else if (appOpen) showSite();
  });
  doc.addEventListener("click", function (e) {
    var b = e.target.closest ? e.target.closest("[data-open]") : null;
    if (!b || !site.contains(b)) return;
    e.preventDefault(); open(b.getAttribute("data-open"));
  });
  // the app's About tab and logo call this from inside its frame
  window.StatLabX = { home: home, open: open };
  if (location.hash === "#app") open("keep");

  // ready once Shiny in the app frame has connected
  var timer = setInterval(function () {
    var fr = frame();
    if (!ready && connected()) {
      ready = true; render();
      if (channel) { channel.close(); channel = null; }
      if (pending) showApp(pending);
      clearInterval(timer);
      return;
    }
    // the app's page is given its address once R has started the app
    if (!appStarting && fr && /app_/.test(fr.getAttribute("src") || "")) { appStarting = true; render(); }
    // shinylive's error panel must be seen
    if (!failed && doc.querySelector(".loading-wrapper-error")) { failed = true; render(); if (pending) showApp(pending); }
    if (!heard && !unknown && !appStarting && Date.now() - started > 15000) { unknown = true; render(); }
    var slow = $("s-sheet-slow"); if (slow) slow.hidden = Date.now() - lastNews < 45000;
  }, 250);
})();
