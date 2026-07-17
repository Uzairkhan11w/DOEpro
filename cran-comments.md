## Test environments

* local Windows 11, R 4.6.0 — 0 errors | 0 warnings | 0 notes
* win-builder, R-devel (2026-07-16 r90264) — 1 note (see below)
* GitHub Actions:
  * Windows Server 2022, R release
  * macOS, R release
  * Ubuntu, R devel, release and oldrel-1

  all: 0 errors | 0 warnings | 0 notes

## R CMD check results

One note on win-builder, with three parts:

**1. New submission**

```
Maintainer: 'Uzair Javid Khan <uzairkhan11w@gmail.com>'
New submission
```

This is the first submission of DOEpro.

**2. Possibly misspelled word in DESCRIPTION**

```
Possibly misspelled words in DESCRIPTION:
  Satterthwaite (18:54)
```

This is spelled correctly. It is the surname of Franklin E. Satterthwaite, whose
approximation to the degrees of freedom of a linear combination of mean squares
the package uses for the two mixed comparisons of a split-plot design
(Satterthwaite, 1946, Biometrics Bulletin 2(6), 110-114,
<doi:10.2307/3002019>).

**3. Possibly invalid URL**

```
Found the following (possibly) invalid URLs:
  URL: https://doepro.pages.dev
    Status: Error
    Message: SSL connect error [doepro.pages.dev]:
      Recv failure: Connection was reset
```

The URL is valid and the site is live. It is where the application runs in the
browser, and it is served by Cloudflare Pages.

We were unable to reproduce the failure. The address answers normally over
HTTPS/2 from every client we tried, including R's own URL checker:

```r
> curlGetHeaders("https://doepro.pages.dev", verify = TRUE)[1]
[1] "HTTP/2 200 \r\n"
```

and `curl -I` returns `HTTP/2 200` with a valid certificate, under HTTP/1.1 and
HTTP/2, under TLS 1.2 and 1.3, and over IPv4. We believe the check machine's
connection to Cloudflare's edge was reset in passing, rather than the URL being
wrong.

If you would prefer the DESCRIPTION not to carry a URL that the checker cannot
always reach, we are happy to drop it and keep only the GitHub address; please
let us know.

## Downstream dependencies

There are none: this is a new package.
