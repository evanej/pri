# =============================================================================
# USASpending PRI / LQ / Entropy Dashboard
# -----------------------------------------------------------------------------
# Reads three pre-built CSVs (produced by the project's Rmds — nothing here
# touches DuckDB or Parquet live, so the app stays fast):
#   1. data/aggregated/pri_county_v01.csv        <- usaspending_county_aggregates_pri.Rmd (§3.3)
#   2. output/tables/q7_lq_entropy_typology_*.csv <- usaspending_requests.Rmd (§9.5)
#   3. output/tables/county_year_percap*.csv      <- usaspending_requests.Rmd (§9 "panels" chunk)
#
# PRI / RICH / GROW / OPP / LQ / entropy are pooled over the full analysis
# window (see the Rmds for why — growth needs multiple years, LQ/entropy are
# computed once on total spending). Only the "Per-capita obligations (by
# year)" map field is genuinely year-varying. The year slider is disabled for
# every other field rather than implying a per-year PRI that doesn't exist.
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(glue)
library(plotly)
library(scales)
library(tigris)
library(rmapshaper)
options(tigris_use_cache = TRUE)

# ---- Config: local Dropbox path if present, else the repo's own data/ folders ----
# On the developer's machine, the full project (with the live DuckDB/Parquet pipeline)
# lives under Dropbox and this points there so the dashboard always reads the latest
# exports. When deployed from GitHub (Posit Connect Cloud, shinyapps.io, etc.), that
# Dropbox path doesn't exist on the build machine — the app falls back to the small
# committed CSVs under this repo's own data/aggregated/ and data/tables/ instead.
dir_proj_local <- if (.Platform$OS.type == "windows") {
  "D:/EVAN/EEJC Dropbox/Evan Johnson/US spending Evan & Maryann"
} else {
  "/Users/arclight/Library/CloudStorage/Dropbox-EEJC/Evan Johnson/US spending Evan & Maryann"
}

if (dir.exists(dir_proj_local)) {
  dir_agg <- file.path(dir_proj_local, "data/aggregated")
  dir_tab <- file.path(dir_proj_local, "output/tables")
} else {
  dir_agg <- "data/aggregated"   # relative to app.R — i.e. this repo's own data/
  dir_tab <- "data/tables"
}
dir_cache <- "data/cache"
dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)

f_pri       <- file.path(dir_agg, "pri_county_v01.csv")
f_typology  <- list.files(dir_tab, "^q7_lq_entropy_typology_.*\\.csv$", full.names = TRUE)[1]
f_percap_yr <- list.files(dir_tab, "^county_year_percap.*\\.csv$",      full.names = TRUE)[1]

stopifnot(
  "pri_county_v01.csv not found — run usaspending_county_aggregates_pri.Rmd \u00a73.3 first" =
    file.exists(f_pri),
  "q7_lq_entropy_typology_*.csv not found — run usaspending_requests.Rmd \u00a79.5 first" =
    length(f_typology) == 1 && !is.na(f_typology),
  "county_year_percap*.csv not found — run usaspending_requests.Rmd's panels chunk first" =
    length(f_percap_yr) == 1 && !is.na(f_percap_yr)
)

# ---- Load & join --------------------------------------------------------
pri       <- read_csv(f_pri,       show_col_types = FALSE) |> mutate(fips = str_pad(fips, 5, pad = "0"))
typology  <- read_csv(f_typology,  show_col_types = FALSE) |> mutate(fips = str_pad(fips, 5, pad = "0"))
percap_yr <- read_csv(f_percap_yr, show_col_types = FALSE) |> mutate(fips = str_pad(fips, 5, pad = "0"))

# state abbreviation from the FIPS state prefix — no shapefile needed for this lookup
data(fips_codes, package = "tigris")
state_lookup <- fips_codes |> distinct(state_code, state) |> rename(st_fips = state_code, state_abb = state)

pri <- pri |> mutate(st_fips = str_sub(fips, 1, 2)) |> left_join(state_lookup, by = "st_fips")
typology <- typology |> mutate(st_fips = str_sub(fips, 1, 2)) |> left_join(state_lookup, by = "st_fips")
percap_yr <- percap_yr |> mutate(st_fips = str_sub(fips, 1, 2)) |> left_join(state_lookup, by = "st_fips")

# ---------------------------------------------------------------------------
# EDIT HERE to change what shows on MAP HOVER: this reference table gets
# left-joined into the map polygons purely for the tooltip — it doesn't
# affect what field is actually colored/mapped. Add or remove columns here,
# then update the `lbl <- glue_data(...)` template a few lines below
# `output$map`/the map-redraw `observe()` block to match.
county_info <- pri |>
  select(fips, pop, PRI, RICH, GROW, OPP) |>
  left_join(typology |> select(fips, lq_fam, H, typology), by = "fips")
# ---------------------------------------------------------------------------

# ---- County geometry, cached locally after first run (tigris download is slow) ----
f_geo <- file.path(dir_cache, "counties_simplified.rds")
if (file.exists(f_geo)) {
  counties_sf <- readRDS(f_geo)
} else {
  NON_CONUS <- c("02", "15", "60", "66", "69", "72", "78")
  counties_sf <- counties(cb = TRUE, year = 2023) |>
    filter(!STATEFP %in% NON_CONUS) |>
    st_transform(4326) |>
    rmapshaper::ms_simplify(keep = 0.15, keep_shapes = TRUE) |>   # 0.05 was aggressive enough
    st_make_valid() |>                                            # guard against self-intersections
    select(fips = GEOID, county_name = NAMELSAD, STUSPS)
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  saveRDS(counties_sf, f_geo)
}

# Plain (non-spatial) name lookup, for tooltips on the quadrant scatter (which plots
# pri/typology directly and has no geometry to carry a name column of its own).
county_names <- counties_sf |> st_drop_geometry() |> select(fips, county_name, STUSPS)

state_choices <- c("All states", sort(unique(na.omit(pri$state_abb))))
year_choices  <- sort(unique(percap_yr$fy))

# Fields selectable for the primary map. `pooled = TRUE` fields ignore the year slider.
map_fields <- tibble::tribble(
  ~key,             ~label,                                  ~pooled, ~palette,  ~reverse_pal,
  "PRI",            "PRI composite (percentile)",             TRUE,  "inferno", TRUE,
  "RICH",           "RICH subindex (current intensity)",       TRUE,  "inferno", TRUE,
  "GROW",           "GROW subindex (growth + entrants)",       TRUE,  "inferno", TRUE,
  "OPP",            "OPP subindex (opportunity)",              TRUE,  "inferno", TRUE,
  "lq_fam",         "Location quotient (focal PSC family)",    TRUE,  "viridis", FALSE,
  "H",              "Entropy (spending diversification)",      TRUE,  "viridis", FALSE,
  "oblig_pos_pc_yr","Per-capita obligations (by year)",        FALSE, "magma",   FALSE
)

# ---------------------------------------------------------------------------
# EDIT HERE to change the plain-language tooltip text shown next to the
# field selector and the quadrant toggle. Each name below must match a
# `key` in map_fields (for field_descriptions) or a `choices` value in the
# quad_view radioButtons (for quad_descriptions) — see the UI section.
field_descriptions <- c(
  PRI = "The Procurement Readiness Index (PRI) is a single 0-1 score summarizing three things about a county: how much federal contracting is already happening there, how fast that's been growing, and how much unclaimed opportunity remains. Higher = already active and still has room to grow.",
  RICH = "How much federal procurement spending a county already receives per person, relative to other counties \u2014 the 'current intensity' piece of the PRI. Higher = more federal contracting dollars already flowing here.",
  GROW = "How fast a county's federal contracting has grown recently, plus how many new (first-time) contractors have shown up. Higher = momentum \u2014 this county's federal presence is expanding, not just large.",
  OPP = "An estimate of unclaimed opportunity: counties with capacity relevant to federal contracting (skilled workforce, related industries) that haven't yet captured much federal spending. Higher = looks primed for more federal activity than it currently has.",
  lq_fam = "Location Quotient (LQ) compares how concentrated one category of federal spending is in this county versus the nation as a whole. LQ = 1 means the county matches the national average; above 1 means the county specializes in that category more than most places do.",
  H = "Entropy measures how spread out a county's federal contracts are across different types of spending, rather than concentrated in one or two categories. Higher = more diversified (many kinds of federal work); lower = more specialized (most dollars going to one type of work).",
  oblig_pos_pc_yr = "Total federal obligations (net of cancellations), divided by population, for the fiscal year selected below \u2014 a simple measure of how much federal spending activity is happening relative to a county's size."
)
quad_descriptions <- c(
  pri = "Each dot is a county, positioned by RICH (current federal-contracting intensity) and OPP (untapped opportunity). 'Rich & Open' counties score high on both; 'Underserved' counties score low on both; the other two quadrants are strong on just one dimension.",
  typology = "Each dot is a county, positioned by LQ (how specialized its spending is in the focal category) and Entropy (how diversified its overall federal spending mix is). Shows whether a county's federal base is narrow and concentrated, or broad and diversified."
)
# ---------------------------------------------------------------------------

# =============================================================================
# UI
# =============================================================================
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "USASpending — PRI / LQ / Entropy Explorer",
  padding = 0,
  tags$style(HTML("
    /* EDIT HERE to change the dashboard's font. Arial isn't a web font — no download
       needed, it's just declared first and the browser uses it if installed (true on
       basically every Mac/Windows machine), falling back to Helvetica/sans-serif. */
    html, body, .controls-col, .bottom-pane, .leaflet-container, h1, h2, h3, h4, h5, h6,
    label, button, select, input {
      font-family: Arial, Helvetica, \"Helvetica Neue\", sans-serif !important;
    }
    html, body { height: 100%; margin: 0; }
    .app-wrap { display: flex; flex-direction: column; height: 100vh; }
    .map-pane  { flex: 2 1 0; min-height: 0; }           /* ~2/3 of the screen */
    .bottom-pane {                                        /* ~1/3 of the screen, full width */
      flex: 1 1 0; min-height: 0; overflow-y: auto;
      display: flex; gap: 28px; align-items: stretch;
      padding: 14px 26px; border-top: 1px solid #ddd; background: #fff;
    }
    .controls-col { flex: 0 0 250px; overflow-y: auto; padding-top: 4px; }
    .plot-col { flex: 1 1 auto; min-width: 0; display: flex; flex-direction: column; }
    .plot-col .plotly { flex: 1 1 auto; }
  ")),
  tags$div(class = "app-wrap",
    tags$div(class = "map-pane", leafletOutput("map", width = "100%", height = "100%")),
    tags$div(class = "bottom-pane",
      tags$div(class = "controls-col",
        h5("Map field",
           tooltip(
             tags$span(icon("circle-info"),
                      style = "color:#888; cursor:help; margin-left:6px; font-size:0.75em;"),
             "Select a field to see what it means.",   # placeholder — updated server-side on load/change
             id = "field_tip", placement = "right"
           )),
        selectInput("field", NULL, choices = setNames(map_fields$key, map_fields$label),
                    selected = "PRI"),

        h5("Filters"),
        selectInput("state", "State", choices = state_choices, selected = "All states"),
        conditionalPanel(
          condition = "output.field_is_pooled == false",
          sliderInput("year", "Fiscal year", min = min(year_choices), max = max(year_choices),
                      value = max(year_choices), step = 1, sep = "")
        ),
        conditionalPanel(
          condition = "output.field_is_pooled == true",
          helpText(em("Pooled over the full window — doesn't vary by year."))
        ),

        hr(),
        h5("Quadrants",
           tooltip(
             tags$span(icon("circle-info"),
                      style = "color:#888; cursor:help; margin-left:6px; font-size:0.75em;"),
             "Select a view to see what it means.",
             id = "quad_tip", placement = "right"
           )),
        radioButtons("quad_view", NULL,
                     choices = c("PRI: Rich \u00d7 Opportunity" = "pri",
                                 "Specialization: LQ \u00d7 Diversity" = "typology"),
                     selected = "pri")
      ),
      tags$div(class = "plot-col",
        plotlyOutput("quad_plot", height = "100%"),
        textOutput("quad_note")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # EDIT HERE (or edit the text itself up in field_descriptions/quad_descriptions near
  # map_fields) to change what the info-icon tooltips say. ignoreInit = FALSE so the
  # right text is already showing for the default selection on first load, not just
  # after the user changes something.
  observeEvent(input$field, {
    update_tooltip("field_tip", field_descriptions[[input$field]])
  }, ignoreInit = FALSE)

  observeEvent(input$quad_view, {
    update_tooltip("quad_tip", quad_descriptions[[input$quad_view]])
  }, ignoreInit = FALSE)

  field_is_pooled <- reactive({
    map_fields$pooled[map_fields$key == input$field]
  })
  output$field_is_pooled <- reactive({ field_is_pooled() })
  outputOptions(output, "field_is_pooled", suspendWhenHidden = FALSE)

  # ---- Data for the currently selected map field ----
  map_data <- reactive({
    if (input$field == "oblig_pos_pc_yr") {
      d <- percap_yr |> filter(fy == input$year) |>
        transmute(fips, state_abb, value = oblig_pos_pc)
    } else if (input$field %in% c("lq_fam", "H")) {
      d <- typology |> transmute(fips, state_abb, value = .data[[input$field]])
    } else {
      d <- pri |> transmute(fips, state_abb, value = .data[[input$field]])
    }
    if (input$state != "All states") d <- d |> filter(state_abb == input$state)
    d
  })

  field_meta <- reactive({ map_fields |> filter(key == input$field) })

  map_sf <- reactive({
    counties_sf |> inner_join(map_data(), by = "fips")
  })

  # EDIT HERE to change which fields get an inverted color scale (dark = high value) —
  # add/remove a field from `reverse_pal` in the map_fields table above rather than here.
  pal <- reactive({
    d <- map_data()$value
    colorNumeric(palette = field_meta()$palette, domain = d, na.color = "#e8e8e8",
                reverse = field_meta()$reverse_pal)
  })

  # ---- Base map, drawn once; polygons + legend updated via leafletProxy ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      setView(lng = -96, lat = 38.5, zoom = 4)
  })

  observe({
    req(nrow(map_data()) > 0)   # avoid a blank/errored render if a filter combo yields nothing
    m <- map_sf() |> left_join(county_info, by = "fips")   # brings in PRI/RICH/GROW/OPP/lq_fam/H/pop for the tooltip
    p <- pal()

    # -------------------------------------------------------------------
    # EDIT HERE to change MAP HOVER TOOLTIP content. `m` has one row per
    # visible county with: county_name, STUSPS, value (the currently-mapped
    # field), plus everything pulled in from `county_info` above (pop, PRI,
    # RICH, GROW, OPP, lq_fam, H, typology). Add a line, reference any of
    # those columns as m$<column>, wrap in fmt(...) for consistent number
    # formatting.
    fmt <- label_number(accuracy = 0.01)
    lbl <- glue(
      "<b>{m$county_name}, {m$STUSPS}</b><br/>",
      "<b>{field_meta()$label}: {fmt(m$value)}</b><br/>",
      "Population (avg.): {label_comma()(round(m$pop))}<br/>",
      "PRI composite: {fmt(m$PRI)} &nbsp;",
      "(RICH {fmt(m$RICH)} / GROW {fmt(m$GROW)} / OPP {fmt(m$OPP)})<br/>",
      "LQ (focal PSC family): {fmt(m$lq_fam)} &nbsp; Entropy: {fmt(m$H)}"
    )
    # -------------------------------------------------------------------

    leafletProxy("map") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data = m, fillColor = ~p(value), fillOpacity = 0.85, color = "white", weight = 0.3,
        label = lapply(lbl, HTML),
        highlightOptions = highlightOptions(weight = 1.5, color = "#333", bringToFront = TRUE)
      ) |>
      addLegend(position = "bottomleft", pal = p, values = m$value[!is.na(m$value)],
               title = field_meta()$label, opacity = 0.9)
  }) |> bindEvent(input$field, input$state, input$year, ignoreNULL = FALSE)

  # ---- Bottom-panel quadrant scatter ----
  quad_data <- reactive({
    if (input$quad_view == "pri") {
      d <- pri |> filter(pop >= 25000) |>
        mutate(quadrant = case_when(
          RICH >= .5 & OPP >= .5 ~ "Rich & Open",
          RICH >= .5 & OPP <  .5 ~ "Established, saturated",
          RICH <  .5 & OPP >= .5 ~ "Emerging opportunity",
          TRUE                   ~ "Underserved"
        ))
    } else {
      # typology has no pop column of its own — bring it in from county_info (built
      # near the top of the file for the map tooltips) so dot sizing works here too.
      d <- typology |> left_join(county_info |> select(fips, pop), by = "fips")
    }
    if (input$state != "All states") d <- d |> filter(state_abb == input$state)

    # EDIT HERE to change dot sizing: log(pop) compresses the huge population range
    # (a few counties in the millions, most in the thousands) so size differences stay
    # subtle rather than a few metros dwarfing everything else. `to = c(4, 14)` is the
    # rendered pixel-diameter range — widen it for more dramatic size variation.
    d |>
      left_join(county_names, by = "fips") |>   # adds county_name, STUSPS for tooltips
      mutate(dot_size = scales::rescale(log(pmax(pop, 1)), to = c(4, 14)))
  })

  # Short legend labels for the typology view — the full strings ("High-LQ (A), high-diversity
  # — \"Diversified specialist\"") are informative but too long for a legend at any panel size;
  # kept in the hover tooltip instead via `typology_full`.
  typology_short <- function(x) {
    case_when(
      str_detect(x, "high-diversity")            & str_detect(x, "High-LQ") ~ "High-LQ, diversified",
      str_detect(x, "low-diversity")              & str_detect(x, "High-LQ") ~ "High-LQ, concentrated",
      str_detect(x, "^Low-LQ, high-diversity")                              ~ "Low-LQ, diversified",
      str_detect(x, "^Low-LQ, low-diversity")                               ~ "Low-LQ, concentrated",
      TRUE ~ x
    )
  }

  output$quad_plot <- renderPlotly({
    d <- quad_data()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))

    # -------------------------------------------------------------------
    # EDIT HERE to change QUADRANT SCATTER HOVER TOOLTIP content. `d` has
    # every column from pri/typology plus county_name/STUSPS from the join
    # above — reference any of them inside glue(). PRI view and typology
    # view build separate `text =` strings since they're different data.
    if (input$quad_view == "pri") {
      p <- plot_ly(d, x = ~RICH, y = ~OPP, color = ~quadrant,
                   type = "scatter", mode = "markers",
                   text = ~glue(
                     "<b>{county_name}, {STUSPS}</b><br>",
                     "Quadrant: {quadrant}<br>",
                     "RICH: {round(RICH,2)} &nbsp; OPP: {round(OPP,2)}<br>",
                     "PRI: {round(PRI,2)} &nbsp; GROW: {round(GROW,2)}<br>",
                     "Population (avg.): {label_comma()(round(pop))}"
                   ), hoverinfo = "text",
                   marker = list(size = ~dot_size, opacity = 0.65)) |>
        layout(xaxis = list(title = "RICH (current intensity)", range = c(0, 1)),
               yaxis = list(title = "OPP (opportunity)", range = c(0, 1)),
               shapes = list(
                 list(type = "line", x0 = .5, x1 = .5, y0 = 0, y1 = 1, line = list(dash = "dot", color = "grey")),
                 list(type = "line", x0 = 0, x1 = 1, y0 = .5, y1 = .5, line = list(dash = "dot", color = "grey"))
               ),
               margin = list(t = 10, b = 45, l = 60, r = 150),
               font = list(family = "Arial, Helvetica, sans-serif"),
               legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle",
                            font = list(size = 10)))
    } else {
      p <- plot_ly(d, x = ~lq_fam, y = ~H, color = ~typology_short(typology),
                   type = "scatter", mode = "markers",
                   text = ~glue(
                     "<b>{county_name}, {STUSPS}</b><br>",
                     "{typology}<br>",
                     "LQ: {round(lq_fam,2)} &nbsp; Entropy: {round(H,2)} ",
                     "({round(H_norm,2)} normalized)<br>",
                     "Categories present: {n_cat} &nbsp; Total obligations: ",
                     "{label_currency(scale_cut = cut_short_scale())(total)}"
                   ), hoverinfo = "text",
                   marker = list(size = ~dot_size, opacity = 0.65)) |>
        layout(xaxis = list(title = "LQ (focal PSC family)"),
               yaxis = list(title = "Entropy (diversification)"),
               margin = list(t = 10, b = 45, l = 60, r = 160),
               font = list(family = "Arial, Helvetica, sans-serif"),
               legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle",
                            font = list(size = 10)))
    }
    p |> config(displayModeBar = FALSE)
  })

  output$quad_note <- renderText({
    if (input$quad_view == "pri")
      glue("{nrow(quad_data())} counties shown (pop \u2265 25k). Color = policy quadrant.")
    else
      glue("{nrow(quad_data())} counties shown. Color = specialization \u00d7 diversity quadrant.")
  })
}

shinyApp(ui, server)
