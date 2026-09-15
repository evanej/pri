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

state_choices <- c("All states", sort(unique(na.omit(pri$state_abb))))
year_choices  <- sort(unique(percap_yr$fy))

# Fields selectable for the primary map. `pooled = TRUE` fields ignore the year slider.
map_fields <- tibble::tribble(
  ~key,             ~label,                                  ~pooled, ~palette,
  "PRI",            "PRI composite (percentile)",             TRUE,  "inferno",
  "RICH",           "RICH subindex (current intensity)",       TRUE,  "inferno",
  "GROW",           "GROW subindex (growth + entrants)",       TRUE,  "inferno",
  "OPP",            "OPP subindex (opportunity)",              TRUE,  "inferno",
  "lq_fam",         "Location quotient (focal PSC family)",    TRUE,  "viridis",
  "H",              "Entropy (spending diversification)",      TRUE,  "viridis",
  "oblig_pos_pc_yr","Per-capita obligations (by year)",        FALSE, "magma"
)

# =============================================================================
# UI
# =============================================================================
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "USASpending — PRI / LQ / Entropy Explorer",
  tags$style(HTML("
    #map { height: 100vh !important; }
    .corner-panel {
      background: rgba(255,255,255,0.96); padding: 16px 18px; border-radius: 10px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.25); width: 460px; max-height: 90vh; overflow-y: auto;
    }
  ")),
  tags$div(style = "position: relative;",
    leafletOutput("map", height = "100vh"),
    absolutePanel(
      top = 16, right = 16, width = 400, class = "corner-panel", draggable = TRUE,
      style = "z-index: 500;",

      h5("Map field"),
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
        helpText(em("This field is pooled over the full analysis window and doesn't vary by year."))
      ),

      hr(),
      h5("Quadrants"),
      radioButtons("quad_view", NULL,
                   choices = c("PRI: Rich \u00d7 Opportunity" = "pri",
                               "Specialization: LQ \u00d7 Diversity" = "typology"),
                   selected = "pri"),
      plotlyOutput("quad_plot", height = "420px"),
      textOutput("quad_note")
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

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

  pal <- reactive({
    d <- map_data()$value
    opt <- field_meta()$palette
    colorNumeric(palette = opt, domain = d, na.color = "#e8e8e8")
  })

  # ---- Base map, drawn once; polygons + legend updated via leafletProxy ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      setView(lng = -96, lat = 38.5, zoom = 4)
  })

  observe({
    req(nrow(map_data()) > 0)   # avoid a blank/errored render if a filter combo yields nothing
    m <- map_sf()
    p <- pal()
    lbl <- glue("<b>{m$county_name}</b><br/>{field_meta()$label}: {label_number(accuracy = 0.01)(m$value)}")

    leafletProxy("map") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data = m, fillColor = ~p(value), fillOpacity = 0.85, color = "white", weight = 0.3,
        label = lapply(lbl, HTML),
        highlightOptions = highlightOptions(weight = 1.5, color = "#333", bringToFront = TRUE)
      ) |>
      addLegend(position = "bottomleft", pal = p, values = m$value,
               title = field_meta()$label, opacity = 0.9)
  }) |> bindEvent(input$field, input$state, input$year, ignoreNULL = FALSE)

  # ---- Corner quadrant scatter ----
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
      d <- typology
    }
    if (input$state != "All states") d <- d |> filter(state_abb == input$state)
    d
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

    if (input$quad_view == "pri") {
      p <- plot_ly(d, x = ~RICH, y = ~OPP, color = ~quadrant, size = ~pop,
                   type = "scatter", mode = "markers",
                   text = ~glue("{fips} ({state_abb})"), hoverinfo = "text",
                   marker = list(sizemode = "area", sizeref = max(d$pop, na.rm = TRUE) / 900,
                                opacity = 0.65)) |>
        layout(xaxis = list(title = "RICH (current intensity)", range = c(0, 1)),
               yaxis = list(title = "OPP (opportunity)", range = c(0, 1)),
               shapes = list(
                 list(type = "line", x0 = .5, x1 = .5, y0 = 0, y1 = 1, line = list(dash = "dot", color = "grey")),
                 list(type = "line", x0 = 0, x1 = 1, y0 = .5, y1 = .5, line = list(dash = "dot", color = "grey"))
               ),
               margin = list(t = 10, b = 60, l = 60, r = 20),
               legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 10)))
    } else {
      p <- plot_ly(d, x = ~lq_fam, y = ~H, color = ~typology_short(typology),
                   type = "scatter", mode = "markers",
                   text = ~glue("{fips} ({state_abb})<br>{typology}"), hoverinfo = "text",
                   marker = list(opacity = 0.65, size = 8)) |>
        layout(xaxis = list(title = "LQ (focal PSC family)"),
               yaxis = list(title = "Entropy (diversification)"),
               margin = list(t = 10, b = 70, l = 60, r = 20),
               legend = list(orientation = "h", x = 0, y = -0.32, font = list(size = 10)))
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
