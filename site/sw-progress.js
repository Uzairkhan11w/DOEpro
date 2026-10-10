
// ---------------------------------------------------------------------------
// StatLabX: download progress for the loading screen (site/loading.js).
// Appended to shinylive's service worker by build_app.R after every export.
// webR fetches R and its packages from its own worker, which the page cannot
// watch; this worker sees every request, so for each file under
// shinylive/webr/ it counts the bytes as they arrive and reports them on the
// "statlabx-loading" channel. The files themselves pass through unchanged.
// Requests shinylive answers itself (the app's own paths, and requests for
// cross-origin isolation) are left to it, so the two never both respond.
// ---------------------------------------------------------------------------
const slxChannel = typeof BroadcastChannel === "function" ? new BroadcastChannel("statlabx-loading") : null;
const slxBase = self.location.pathname.replace(/[^/]*$/, "");
self.addEventListener("fetch", (event) => {
  if (!slxChannel) return;
  const request = event.request;
  if (request.method !== "GET") return;
  const url = new URL(request.url);
  if (url.origin !== self.location.origin) return;
  if (!url.pathname.startsWith(slxBase + "shinylive/webr/")) return;
  if (/\/app_[^/]+\//.test(url.pathname)) return;
  if (url.searchParams.get("coi") === "1" || request.referrer.includes("coi=1")) return;
  const file = url.pathname.slice(slxBase.length);
  event.respondWith((async () => {
    const response = await fetch(request);
    if (!response.body) {
      slxChannel.postMessage({ file, bytes: 0, done: true });
      return response;
    }
    const reader = response.body.getReader();
    let bytes = 0, last = 0;
    slxChannel.postMessage({ file, bytes: 0, done: false });
    const body = new ReadableStream({
      async pull(controller) {
        try {
          const { done, value } = await reader.read();
          if (done) {
            slxChannel.postMessage({ file, bytes, done: true });
            controller.close();
            return;
          }
          bytes += value.byteLength;
          const now = Date.now();
          if (now - last > 100) { last = now; slxChannel.postMessage({ file, bytes, done: false }); }
          controller.enqueue(value);
        } catch (err) {
          controller.error(err);
        }
      },
      cancel(reason) { return reader.cancel(reason); }
    });
    return new Response(body, { status: response.status, statusText: response.statusText, headers: response.headers });
  })());
});
