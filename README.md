# USASpending PRI / LQ / Entropy Explorer

Interactive county-level dashboard for the Arclight/Feldman USASpending geographic
variation project — Procurement Readiness Index (PRI), location quotient, and
spending-diversification entropy, with a per-capita-by-year layer.

## What this needs to run

Three small pre-aggregated CSVs, produced by the project's Rmd pipeline (not included in
this repo — see `data/README.md`):

| File | Produced by |
|---|---|
| `pri_county_v01.csv` | `usaspending_county_aggregates_pri.Rmd` §3.3 |
| `q7_lq_entropy_typology_*.csv` | `usaspending_requests.Rmd` §9.5 |
| `county_year_percap*.csv` | `usaspending_requests.Rmd` (panels chunk) |

`app.R`'s `dir_proj` config block points at these on the developer's machine by default.
For deployment, point it at a `data/` folder shipped alongside `app.R` instead (see
**Deploying** below) — the app doesn't need live DuckDB/Parquet access, only these three
CSVs plus a first-run county-geometry fetch (cached after that).

## Local setup

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "sf", "dplyr", "tidyr", "readr", "stringr",
  "glue", "plotly", "scales", "tigris", "rmapshaper"
))
```

Edit the `dir_proj` path near the top of `app.R` to match your machine, then:

```r
shiny::runApp("app.R")
```

First run downloads + simplifies county geometry (~30–60s) and caches it to
`data/cache/counties_simplified.rds`; subsequent launches are instant.

## Deploying to a public URL

**shinyapps.io (recommended — free tier, fastest path to a link you can embed on the
Arclight site):**

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<your-account>", token = "...", secret = "...")
# (get these from shinyapps.io → Account → Tokens, after creating a free account)

rsconnect::deployApp(appDir = ".", appName = "usaspending-pri-explorer")
```

Before deploying, swap the `dir_proj` config block in `app.R` for a relative path (e.g.
`dir_agg <- "data/aggregated"`, `dir_tab <- "data/tables"`) and copy the three CSVs into
those folders inside this repo — `rsconnect::deployApp()` bundles everything in the app
directory, but it can't reach your Dropbox path. Once deployed you'll get a URL like
`https://<account>.shinyapps.io/usaspending-pri-explorer/` — link or iframe that from the
Arclight site.

**Alternative:** [Posit Connect Cloud](https://connect.posit.cloud) — similar free-tier
workflow, custom domains on paid plans. Self-hosting via Docker + Shiny Server is the
other option if you want a fully custom domain with no `shinyapps.io` branding at all;
ask if you want a Dockerfile for that route.

## Repo structure

```
app.R              — the dashboard
data/aggregated/   — pri_county_v01.csv (gitignored by default — see data/README.md)
data/tables/        — q7_lq_entropy_typology_*.csv, county_year_percap*.csv (same)
data/cache/         — counties_simplified.rds, regenerated on first run
.gitignore
README.md
```

## Notes on the data

PRI, LQ, and entropy are pooled over the full FY2008–2024 analysis window (growth
requires multiple years; LQ/entropy are computed once on total spending) — they don't
have a per-year value. Only the per-capita-by-year layer varies by fiscal year; the app's
year slider is disabled for every other field rather than implying a per-year PRI that
doesn't exist.
