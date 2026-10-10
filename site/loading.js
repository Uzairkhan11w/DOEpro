/* StatLabX loading screen. build_app.R puts this into docs/index.html after
   every shinylive export, and fills in FILES with the size of every file the
   first visit downloads (R, its built-in library and the packages), read
   from docs/ itself, so the total is the real one for this build.

   webR downloads those files from its own worker, where the page's fetch
   cannot see them. The service worker sees every request, so the addition
   to it (sw-progress.js) counts the bytes of each file as they arrive and
   reports them on the "statlabx-loading" channel. The bar shows those bytes
   against the total; no percentage is printed, and nothing is estimated. The
   stages are read from which files have arrived: R itself, then R starting
   (webR asks for the package list once R runs), then the packages, then R
   installing and loading them, then the app's own page loading in its
   frame, which ends when Shiny connects. The words say "loading", not
   "downloading": on later visits the same bytes come from the browser's
   copy, and the screen cannot tell the two apart. */
(function () {
  "use strict";
  var FILES = /*FILES*/{};
  var total = 0, f;
  for (f in FILES) total += FILES[f];

  var screen = document.getElementById("slx-loading");
  if (!screen) return;
  var bar = document.getElementById("slx-bar");
  var fill = document.getElementById("slx-fill");
  var status = document.getElementById("slx-status");
  var amount = document.getElementById("slx-amount");
  var slow = document.getElementById("slx-slow");

  var got = {}, ended = {}, heard = false, appStarting = false, lastNews = Date.now(), shown = "";

  function mb(bytes) { return (bytes / 1048576).toFixed(1); }
  // R itself and its built-in library sit directly in shinylive/webr/ and
  // arrive before R starts; the package list (metadata.rds) is asked for
  // once R runs, and the packages and R's other libraries (vfs/) after it
  function isCore(file) { return /^shinylive\/webr\/[^/]+$/.test(file); }
  function isList(file) { return /\/packages\/metadata\.rds$/.test(file); }

  function say(text) {
    if (text !== shown) { shown = text; status.textContent = text; lastNews = Date.now(); }
  }

  // the bar and the line under it, from the bytes counted so far
  function draw() {
    var received = 0, coreDone = true, laterDone = true, listSeen = false;
    for (f in FILES) {
      if (got[f] !== undefined) received += Math.min(got[f], FILES[f]);
      if (isList(f)) listSeen = got[f] !== undefined;
      else if (isCore(f)) { if (!ended[f]) coreDone = false; }
      else if (!ended[f]) laterDone = false;
    }
    if (coreDone && listSeen && laterDone) {
      fill.style.width = "100%";
      bar.classList.add("slx-working");
      bar.removeAttribute("aria-valuenow");
      amount.textContent = mb(total) + " MB of R and its packages in place";
      say(appStarting ? "Starting StatLabX" : "Installing and loading the packages");
      return;
    }
    fill.style.width = (100 * received / total).toFixed(2) + "%";
    bar.setAttribute("aria-valuenow", Math.floor(100 * received / total));
    amount.textContent = mb(received) + " of " + mb(total) + " MB";
    if (!coreDone) say("Loading R");
    else if (!listSeen) say("Starting R in your browser");
    else say("Loading the packages the analyses use");
  }

  // without the channel, or if no file is reported (an old service worker
  // still in charge on the first load after an update), the bar says only
  // that work is going on
  function unknown() {
    fill.style.width = "100%";
    bar.classList.add("slx-working");
    bar.removeAttribute("aria-valuenow");
    amount.textContent = "";
    say("Loading R and its packages");
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
      draw();
    };
  } else unknown();

  function finish() {
    clearInterval(timer);
    if (channel) channel.close();
    screen.classList.add("slx-done");
    setTimeout(function () { if (screen.parentNode) screen.parentNode.removeChild(screen); }, 400);
  }

  var started = Date.now();
  var timer = setInterval(function () {
    // the app is ready once Shiny in the app frame has connected
    var frame = document.querySelector("#root iframe");
    var ready = false;
    try {
      var w = frame && frame.contentWindow;
      ready = !!(w && w.Shiny && w.Shiny.shinyapp && w.Shiny.shinyapp.isConnected());
    } catch (err) { ready = false; }
    if (ready) return finish();
    // the app's page is given its address once R has started the app
    if (!appStarting && frame && /app_/.test(frame.getAttribute("src") || "")) {
      appStarting = true;
      if (heard) draw(); else say("Starting StatLabX");
    }
    // shinylive's error panel must be seen, so the screen steps aside
    if (document.querySelector(".loading-wrapper-error")) return finish();
    if (!heard && !appStarting && Date.now() - started > 15000) unknown();
    if (Date.now() - lastNews > 45000) slow.hidden = false;
  }, 250);
})();
