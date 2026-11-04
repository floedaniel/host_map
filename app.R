# =============================================================================
#  Host Map: host plants of a plant pest and their documentation in Norway
#  Run with:  shiny::runApp()   (from the project folder)
#  Deployable on shinyapps.io: single R process, no background workers.
# =============================================================================

# ---- CONFIGURATION (maintainers only; not exposed in the app) ---------------
# EPPO Global Database API key (free registration at https://data.eppo.int/).
# Set the EPPO_API_KEY environment variable (~/.Renviron locally; an .Renviron
# file deployed with the app on shinyapps.io). Do not commit a real key to git.
EPPO_API_KEY <- Sys.getenv("EPPO_API_KEY", unset = "")

# Uncertainty classification: FIXED, chosen by the statistical validation in
# 5_outputs/REPORT.md (do not change without re-running 4_scripts/02_*.R).
#   records counted = all basisOfRecord except LIVING_SPECIMEN and FOSSIL_SPECIMEN
#   0 = not documented | 1-12 = high | 13-37 = moderate | >= 38 = low uncertainty
#   (12 and 37 are where P(host really present in Norway) reaches 0.95 and 0.99)
UNCERTAINTY_T1 <- 12L
UNCERTAINTY_T2 <- 37L

# Map
MAP_MAX_UNCERTAINTY_M <- 1000     # drop points with coordinate uncertainty above this
MAP_KEEP_MISSING_UNCERTAINTY <- TRUE
MAP_MAX_POINTS <- 1000            # per host layer (kept low for shinyapps.io)
MAP_DEFAULT_N_HOSTS <- 5          # hosts mapped when no table rows are selected
MAP_DEFAULT_CATEGORIES <- "Major host"
# Base maps: free, no API key, loaded by URL (tested in the browser; CARTO and
# Stadia now demand keys). Default: Esri World Light Gray Base, a light grey
# canvas without labels, roads or land use.
ESRI_ATTR <- "Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors, and the GIS user community"
MAP_TILES <- list(
  grey    = list(name = "Light grey (minimal)", maxzoom = 16,
                 url = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
                 attr = ESRI_ATTR),
  terrain = list(name = "Terrain (soft)", maxzoom = 13,
                 url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}",
                 attr = "Tiles &copy; Esri &mdash; Source: USGS, Esri, TANA, DeLorme, and NPS"),
  osm     = list(name = "OpenStreetMap", maxzoom = 19,
                 url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
                 attr = "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors"))

# Performance: concurrent GBIF requests from this one R process
GBIF_MAX_ACTIVE <- 4
# Cache for API calls (GBIF, EPPO, Artsdatabanken). On shinyapps.io this is
# local to the running instance; it is simply rebuilt when an instance restarts.
CACHE_DIR <- file.path(getwd(), "2_processed_data", "cache")
CACHE_MAX_AGE_DAYS <- 30
# Artsdatabanken reference (Red List 2021 + Alien Species List 2023), shipped with the app
REFERENCE_FILE <- file.path(getwd(), "2_processed_data", "artsdatabanken_reference.rds")
# ---- END CONFIGURATION ------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(leaflet)
source("4_scripts/hostmap_functions.R")
source("4_scripts/theme.R")
hm_init_cache(CACHE_DIR, CACHE_MAX_AGE_DAYS)

REFERENCE <- tryCatch({
  if (file.exists(REFERENCE_FILE)) readRDS(REFERENCE_FILE) else {
    dir.create(dirname(REFERENCE_FILE), showWarnings = FALSE, recursive = TRUE)
    build_reference(path = REFERENCE_FILE)
  }
}, error = function(e) { warning("Artsdatabanken reference unavailable: ", conditionMessage(e)); NULL })

BASE_GROUPS <- unname(vapply(MAP_TILES, `[[`, "", "name"))

# ---- UI ---------------------------------------------------------------------
ui <- page(
  theme = hm_theme(),
  tags$head(tags$link(rel = "stylesheet", href = "vkm.css")),
  vkm_header("Host Map", "FinnPRIO · Host plants of plant pests and their documentation in Norway"),
  div(class = "vkm-navbar", navset_bar(id = "tabs", bg = VKM_COLOURS$background, fillable = FALSE,
  sidebar = sidebar(width = 300, bg = VKM_COLOURS$background,
    textInput("pest", "Pest species (scientific name or EPPO code)", "", placeholder = "Species"),
    actionButton("go", "Search", class = "btn-primary w-100"),
    checkboxGroupInput("cats", "EPPO host categories", choices = NULL),
    downloadButton("dl_csv", "Download CSV", class = "btn-secondary w-100 mb-2"),
    downloadButton("dl_xlsx", "Download Excel", class = "btn-secondary w-100")
  ),
  header = tagList(uiOutput("messages"), uiOutput("summary_boxes")),
    nav_panel("Host plants",
      p(class = "small",
        "Uncertainty class refers to ", strong("documented occurrence"), " in GBIF records from Norway ",
        sprintf("(0 records: not documented; 1–%d: high; %d–%d: moderate; ≥%d: low uncertainty). ",
                UNCERTAINTY_T1, UNCERTAINTY_T1 + 1L, UNCERTAINTY_T2, UNCERTAINTY_T2 + 1L),
        "“Status in Norwegian nature” is a separate concept, taken from Artsdatabanken's Red List 2021 ",
        "and Alien Species List 2023, and says whether the plant is ", strong("established"), " in Norway."),
      DTOutput("tbl")),
    nav_panel("Map",
      layout_columns(col_widths = c(4, 3, 2, 3),
        checkboxGroupInput("map_cats", "EPPO categories on map", choices = NULL, inline = TRUE),
        radioButtons("colour_by", "Colour points by",
                     c("Uncertainty class" = "uncertainty_class", "EPPO category" = "eppo_category"), inline = TRUE),
        actionButton("load_map", "Update map", class = "btn-secondary mt-3"),
        p(class = "small mt-2", "Rows selected in the host table are mapped. With no rows selected, the ",
          MAP_DEFAULT_N_HOSTS, " hosts in the ticked map categories with the most records are mapped.")),
      uiOutput("map_msg"),
      leafletOutput("hostmap", height = 620)),
    nav_panel("Method",
      includeMarkdown("3_metadata/method.md"))
  )),
  vkm_footer()
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(pest = NULL, messages = character(0), map_msg = NULL)
  map_trigger <- reactiveVal(0)
  observeEvent(input$load_map, map_trigger(map_trigger() + 1))

  # Resolve pest -> EPPO hosts -> GBIF summaries (cached)
  observeEvent(input$go, {
    rv$messages <- character(0); rv$pest <- NULL
    if (!nzchar(trimws(input$pest))) { rv$messages <- "Enter a species name (e.g. Erwinia amylovora) or an EPPO code (e.g. ERWIAM)."; return() }
    if (!nzchar(EPPO_API_KEY)) {
      rv$messages <- "EPPO API key is missing. Set the EPPO_API_KEY environment variable. Register at https://data.eppo.int/."
      return()
    }
    res <- withProgress(message = "Resolving pest name", value = 0, {
      tryCatch(get_pest_hosts(input$pest, EPPO_API_KEY),
               error = function(e) list(error = conditionMessage(e)))
    })
    if (!is.null(res$error)) { rv$messages <- res$error; return() }
    rv$messages <- res$messages
    if (is.null(res$hosts) || !nrow(res$hosts)) return()
    hosts <- res$hosts
    tab <- withProgress(message = sprintf("Querying GBIF for %d hosts", nrow(hosts)), value = 0, {
      tryCatch({
        prefetch_gbif(hosts$prefname, max_active = GBIF_MAX_ACTIVE,
                      progress = function(done, n, what) setProgress(if (what == "names") 0.4 * done / n else 0.4 + 0.5 * done / n,
                                                                     detail = sprintf("%s %d / %d", what, done, n)))
        summarise_hosts(hosts, basis_keep = GBIF_BASIS_COUNTED, t1 = UNCERTAINTY_T1, t2 = UNCERTAINTY_T2, ref = REFERENCE,
                        progress = function(i, n, nm) setProgress(0.9 + 0.1 * i / n, detail = nm))
      }, error = function(e) { rv$messages <- c(rv$messages, paste("GBIF lookup failed:", conditionMessage(e))); NULL })
    })
    if (is.null(tab)) return()
    n_unres <- sum(is.na(tab$acceptedKey))
    if (n_unres) rv$messages <- c(rv$messages, sprintf("%d host name(s) could not be resolved to a GBIF taxon (see the 'Match note' column).", n_unres))
    rv$pest <- res
    rv$base <- tab
    cats <- levels(droplevels(tab$eppo_category))
    updateCheckboxGroupInput(session, "cats", choices = cats, selected = cats)
    rv$map_cats_default <- if (any(cats %in% MAP_DEFAULT_CATEGORIES)) intersect(cats, MAP_DEFAULT_CATEGORIES) else cats
    updateCheckboxGroupInput(session, "map_cats", choices = cats, selected = rv$map_cats_default, inline = TRUE)
    rv$map_auto <- TRUE
    map_trigger(map_trigger() + 1)   # draw the default map straight after a search
  })

  results <- reactive({ req(rv$base); rv$base })

  filtered <- reactive({
    tab <- results()
    if (length(input$cats)) tab[tab$eppo_category %in% input$cats, , drop = FALSE] else tab[0, ]
  })

  output$messages <- renderUI({
    if (!length(rv$messages)) return(NULL)
    tagList(lapply(rv$messages, function(m) div(class = "alert alert-warning py-2 mb-2", m)))
  })

  output$summary_boxes <- renderUI({
    req(rv$pest)
    tab <- filtered(); ep <- rv$pest$eppo; gb <- rv$pest$gbif
    layout_columns(fill = FALSE,
      value_box("Pest (EPPO)", tags$i(ep$prefname %||% input$pest),
                p(if (!is.na(ep$norwegian %||% NA)) paste0(ep$norwegian, " · ") else NULL,
                  sprintf("EPPO: %s | GBIF key: %s", ep$code, gb$acceptedKey %||% "-"))),
      value_box("Hosts (filtered)", nrow(tab)),
      value_box("Documented in Norway (≥1 record)", sum(tab$n_records > 0, na.rm = TRUE)),
      value_box("Established in Norway (Artsdatabanken)", sum(tab$ref_status == "Established", na.rm = TRUE)))
  })

  display_table <- reactive({
    tab <- filtered()
    data.frame(
      `Host plant (EPPO)` = tab$host_plant,
      `EPPO category` = droplevels(tab$eppo_category),
      `GBIF name` = tab$gbif_name,
      `Match note` = tab$note,
      `Records in Norway` = tab$n_records,
      `Distinct years` = tab$n_years,
      `Counties` = tab$n_counties,
      `Uncertainty class (documented occurrence)` = tab$uncertainty_class,
      `Status in Norwegian nature (Artsdatabanken)` = as.character(tab$ref_status),
      GBIF = ifelse(is.na(tab$gbif_link), "", sprintf('<a href="%s" target="_blank">GBIF</a>', tab$gbif_link)),
      Artskart = sprintf('<a href="%s" target="_blank">Artskart</a>', tab$artskart_link),
      check.names = FALSE)
  })

  output$tbl <- renderDT({
    d <- display_table()
    datatable(d, escape = setdiff(seq_along(d), match(c("GBIF", "Artskart"), names(d))),
              filter = "top", rownames = FALSE, selection = "multiple",
              class = "stripe hover", options = list(pageLength = 25, scrollX = TRUE, order = list(list(4, "desc")))) |>
      formatStyle("Uncertainty class (documented occurrence)", fontWeight = "bold",
                  backgroundColor = styleEqual(names(uncertainty_fill()), unname(uncertainty_fill())))
  })

  export_table <- reactive({
    tab <- filtered()
    tab$bibref <- NULL
    cbind(pest_eppo_name = rv$pest$eppo$prefname %||% NA, pest_eppo_code = rv$pest$eppo$code %||% NA, tab)
  })
  output$dl_csv <- downloadHandler(
    filename = function() sprintf("hosts_%s_%s.csv", gsub("\\W+", "_", rv$pest$eppo$prefname %||% input$pest), Sys.Date()),
    content = function(file) utils::write.csv(export_table(), file, row.names = FALSE, na = ""))
  output$dl_xlsx <- downloadHandler(
    filename = function() sprintf("hosts_%s_%s.xlsx", gsub("\\W+", "_", rv$pest$eppo$prefname %||% input$pest), Sys.Date()),
    content = function(file) {
      meta <- data.frame(
        setting = c("pest (EPPO preferred name)", "EPPO code", "date", "records counted", "uncertainty classes", "method"),
        value = c(rv$pest$eppo$prefname %||% input$pest, rv$pest$eppo$code %||% "", as.character(Sys.time()),
                  paste(GBIF_BASIS_COUNTED, collapse = "; "),
                  sprintf("0 not documented; 1-%d high; %d-%d moderate; >=%d low", UNCERTAINTY_T1, UNCERTAINTY_T1 + 1L, UNCERTAINTY_T2, UNCERTAINTY_T2 + 1L),
                  "Thresholds calibrated so that P(present in Norway) = 0.95 / 0.99 at the class boundaries (see REPORT.md)"))
      writexl::write_xlsx(list(hosts = export_table(), settings = meta), file)
    })

  # ---- Map ----
  output$hostmap <- renderLeaflet({
    m <- leaflet()
    for (t in MAP_TILES) m <- addTiles(m, t$url, attribution = t$attr, group = t$name,
                                       options = tileOptions(maxNativeZoom = t$maxzoom))
    m |> addLayersControl(baseGroups = BASE_GROUPS, options = layersControlOptions(collapsed = TRUE)) |>
      showGroup(BASE_GROUPS[1]) |> hideGroup(BASE_GROUPS[-1]) |>
      setView(lng = 14, lat = 65, zoom = 4)
  })
  outputOptions(output, "hostmap", suspendWhenHidden = FALSE)
  output$map_msg <- renderUI({ if (length(rv$map_msg)) div(class = "alert alert-secondary py-2", HTML(paste(rv$map_msg, collapse = "<br>"))) })

  map_points <- eventReactive(map_trigger(), ignoreInit = TRUE, {
    # Right after a search the category inputs have not been updated yet: use the
    # unfiltered results and the default map categories.
    auto <- isTRUE(rv$map_auto); rv$map_auto <- FALSE
    tab <- if (auto) results() else filtered()
    shiny::validate(shiny::need(nrow(tab) > 0, "Run a search first."))
    map_cats <- if (auto) rv$map_cats_default else input$map_cats
    sel <- if (auto) NULL else input$tbl_rows_selected
    hosts <- if (length(sel)) tab[sel, ] else {
      t2 <- tab[!is.na(tab$n_records) & tab$n_records > 0 & tab$eppo_category %in% map_cats, ]
      head(t2[order(-t2$n_records), ], MAP_DEFAULT_N_HOSTS)
    }
    hosts <- hosts[!is.na(hosts$acceptedKey) & hosts$n_records > 0, ]
    msgs <- character(0)
    pts <- withProgress(message = "Fetching occurrence points", value = 0, {
      lapply(seq_len(nrow(hosts)), function(i) {
        setProgress(i / nrow(hosts), detail = hosts$host_plant[i])
        p <- tryCatch(gbif_points(hosts$acceptedKey[i], MAP_MAX_POINTS, GBIF_BASIS_COUNTED), error = function(e) NULL)
        if (is.null(p)) { msgs <<- c(msgs, sprintf("Failed to fetch points for %s.", hosts$host_plant[i])); return(NULL) }
        d <- clean_points(p$data, MAP_MAX_UNCERTAINTY_M, MAP_KEEP_MISSING_UNCERTAINTY, MAP_MAX_POINTS)
        if (p$n_available > MAP_MAX_POINTS)
          msgs <<- c(msgs, sprintf("<i>%s</i>: %s georeferenced records available; the first %d were fetched, %d shown after de-duplication and precision filter.",
                                   hosts$host_plant[i], format(p$n_available, big.mark = " "), MAP_MAX_POINTS, NROW(d)))
        if (is.null(d) || !nrow(d)) return(NULL)
        d$host <- hosts$host_plant[i]; d$eppo_category <- as.character(hosts$eppo_category[i])
        d$uncertainty_class <- as.character(hosts$uncertainty_class[i])
        d
      })
    })
    rv$map_msg <- if (!nrow(hosts)) "No hosts with Norwegian records to map." else msgs
    dplyr::bind_rows(pts)
  })

  observe({
    d <- map_points()
    proxy <- leafletProxy("hostmap") |> clearMarkers() |> clearControls()
    if (is.null(d) || !nrow(d)) return()
    by <- input$colour_by
    pal <- if (by == "uncertainty_class") hm_viridis(COUNT_CLASS_LEVELS) else
      hm_viridis(intersect(union(EPPO_CATEGORY_LEVELS, unique(d$eppo_category)), unique(d$eppo_category)))
    groups <- unique(d$host)
    proxy |>
      addCircleMarkers(data = d, lng = ~decimalLongitude, lat = ~decimalLatitude, group = ~host,
                       radius = 4, stroke = TRUE, weight = 0.5, color = "black", opacity = 0.6,
                       fillOpacity = 0.8, fillColor = pal(d[[by]]),
                       popup = ~sprintf("<b><i>%s</i></b><br>%s<br>Year: %s<br>%s<br>Uncertainty: %s m",
                                        host, eppo_category, year, basisOfRecord, coordinateUncertaintyInMeters)) |>
      addLegend("bottomright", pal = pal, values = d[[by]], title = if (by == "uncertainty_class") "Uncertainty class" else "EPPO category") |>
      addLayersControl(baseGroups = BASE_GROUPS, overlayGroups = groups,
                       options = layersControlOptions(collapsed = length(groups) > 6))
  })
}

shinyApp(ui, server)
