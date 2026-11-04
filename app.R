# =============================================================================
#  Host Map: host plants of a plant pest and their documentation in Norway
#  Run with:  shiny::runApp()   (from the project folder)
# =============================================================================

# ---- CONFIGURATION (edit here) ----------------------------------------------
# EPPO Global Database API key (free registration at https://data.eppo.int/).
# Either paste it here, or leave as-is and set the EPPO_API_KEY environment
# variable (e.g. in ~/.Renviron). Do not commit a real key to git.
EPPO_API_KEY <- Sys.getenv("EPPO_API_KEY", unset = "")

# Count thresholds for the uncertainty classes (records in Norway).
#   0 = not documented, 1..T1 = high, T1+1..T2 = moderate, > T2 = low uncertainty
DEFAULT_T1 <- 10
DEFAULT_T2 <- 50

# Classification rule: "count" (original rule: count only) or "revised"
# (count + temporal and spatial spread; see REPORT.md for the validation).
DEFAULT_RULE <- "revised"
DEFAULT_MIN_YEARS <- 10      # revised rule: distinct years with records
DEFAULT_MIN_COUNTIES <- 2    # revised rule: distinct counties (GADM level 1)

# basisOfRecord values counted by default. LIVING_SPECIMEN (botanic gardens)
# and FOSSIL_SPECIMEN are excluded because they do not document occurrence in
# Norwegian nature.
DEFAULT_BASIS <- c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "OCCURRENCE",
                   "MACHINE_OBSERVATION", "MATERIAL_SAMPLE", "OBSERVATION",
                   "MATERIAL_CITATION")

# Map: maximum coordinate uncertainty (m) and max points fetched per host layer
DEFAULT_MAX_UNCERTAINTY_M <- 1000
DEFAULT_KEEP_MISSING_UNCERTAINTY <- TRUE
DEFAULT_MAX_POINTS <- 2000
MAX_POINTS_HARD_LIMIT <- 5000
MAP_DEFAULT_N_HOSTS <- 10    # hosts mapped when no table rows are selected
MAP_DEFAULT_CATEGORIES <- "Major host"   # EPPO categories ticked on the map by default
# Base maps: free, no API key. "relief" (default) is shaded terrain with no
# roads, labels or land use; "osm" and "topo" are OpenStreetMap-based.
MAP_TILES <- list(
  relief = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}",
                attr = "Tiles &copy; Esri &mdash; Source: Esri"),
  osm    = list(url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
                attr = "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors"),
  topo   = list(url = "https://tile.opentopomap.org/{z}/{x}/{y}.png",
                attr = "&copy; OpenStreetMap contributors, SRTM | style &copy; <a href='https://opentopomap.org'>OpenTopoMap</a> (CC-BY-SA)"))

# Performance: parallel GBIF workers for long host lists (1 = sequential)
GBIF_WORKERS <- 4
# Persistent cache for all API calls (GBIF, EPPO, Artsdatabanken)
CACHE_DIR <- file.path(getwd(), "2_processed_data", "cache")
CACHE_MAX_AGE_DAYS <- 30
# Artsdatabanken reference (Red List 2021 + Alien Species List 2023)
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


# ---- UI ---------------------------------------------------------------------
ui <- page(
  theme = hm_theme(),
  tags$head(tags$link(rel = "stylesheet", href = "vkm.css")),
  vkm_header("Host Map", "Host plants of plant pests and their documentation in Norway"),
  div(class = "vkm-navbar", navset_bar(id = "tabs", bg = VKM_COLOURS$background, fillable = FALSE,
  sidebar = sidebar(width = 340, bg = VKM_COLOURS$background,
    textInput("pest", "Pest species (scientific name or EPPO code)", "Erwinia amylovora"),
    actionButton("go", "Search", class = "btn-primary w-100"),
    hr(),
    checkboxGroupInput("cats", "EPPO host categories", choices = NULL),
    accordion(open = FALSE,
      accordion_panel("Configuration",
        radioButtons("rule", "Classification rule",
                     c("Revised (count + years + counties)" = "revised", "Count only" = "count"),
                     selected = DEFAULT_RULE),
        numericInput("t1", "Upper limit, high uncertainty (records)", DEFAULT_T1, min = 1),
        numericInput("t2", "Upper limit, moderate uncertainty (records)", DEFAULT_T2, min = 2),
        conditionalPanel("input.rule == 'revised'",
          numericInput("min_years", "Revised: min. distinct years for low uncertainty", DEFAULT_MIN_YEARS, min = 1),
          numericInput("min_counties", "Revised: min. counties for low uncertainty", DEFAULT_MIN_COUNTIES, min = 1)),
        checkboxGroupInput("basis", "basisOfRecord counted", GBIF_BASIS_ALL, selected = DEFAULT_BASIS),
        numericInput("max_unc", "Map: max coordinate uncertainty (m)", DEFAULT_MAX_UNCERTAINTY_M, min = 1),
        checkboxInput("keep_na_unc", "Map: keep points without stated uncertainty", DEFAULT_KEEP_MISSING_UNCERTAINTY),
        numericInput("max_pts", "Map: max points per host layer", DEFAULT_MAX_POINTS, min = 100, max = MAX_POINTS_HARD_LIMIT)
      )
    ),
    hr(),
    downloadButton("dl_csv", "Download CSV", class = "btn-secondary w-100 mb-2"),
    downloadButton("dl_xlsx", "Download Excel", class = "btn-secondary w-100")
  ),
  header = tagList(uiOutput("messages"), uiOutput("summary_boxes")),
    nav_panel("Host plants",
      p(class = "small",
        "Uncertainty class refers to ", strong("documented occurrence"), " in GBIF records from Norway. ",
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
  rv <- reactiveValues(pest = NULL, summaries = list(), messages = character(0), map_msg = NULL)
  map_trigger <- reactiveVal(0)
  observeEvent(input$load_map, map_trigger(map_trigger() + 1))

  # Step 1: resolve pest -> EPPO hosts -> GBIF summaries (heavy, cached)
  observeEvent(input$go, {
    rv$messages <- character(0); rv$pest <- NULL
    if (!nzchar(EPPO_API_KEY)) {
      rv$messages <- "EPPO API key is missing. Set EPPO_API_KEY at the top of app.R or as an environment variable. Register at https://data.eppo.int/."
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
    withProgress(message = sprintf("Querying GBIF for %d hosts", nrow(hosts)), value = 0, {
      if (GBIF_WORKERS > 1 && nrow(hosts) > 20) {
        prefetch_gbif(hosts$prefname, CACHE_DIR, workers = GBIF_WORKERS,
                      progress = function(done, n) setProgress(0.8 * done / n, detail = sprintf("%d / %d (parallel)", done, n)))
      }
      tab <- tryCatch(summarise_hosts(hosts, basis_keep = GBIF_BASIS_ALL, ref = REFERENCE,
                        progress = function(i, n, nm) setProgress(if (GBIF_WORKERS > 1 && n > 20) 0.8 + 0.2 * i / n else i / n, detail = nm)),
                      error = function(e) { rv$messages <- c(rv$messages, paste("GBIF lookup failed:", conditionMessage(e))); NULL })
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

  # Step 2: re-count by basisOfRecord and re-classify (cheap, from cache)
  results <- reactive({
    req(rv$base)
    tab <- rv$base
    basis <- if (length(input$basis)) input$basis else DEFAULT_BASIS
    t1 <- max(1, input$t1 %||% DEFAULT_T1); t2 <- max(t1 + 1, input$t2 %||% DEFAULT_T2)
    tab$n_records <- vapply(seq_len(nrow(tab)), function(i) {
      if (is.na(tab$acceptedKey[i])) return(NA_integer_)
      s <- tryCatch(gbif_no_summary(tab$acceptedKey[i]), error = function(e) NULL)
      if (is.null(s)) NA_integer_ else as.integer(sum(s$basis[names(s$basis) %in% basis]))
    }, 0L)
    tab$count_class <- classify_count(tab$n_records, t1, t2)
    tab$uncertainty_class <- if (identical(input$rule, "count")) tab$count_class else
      classify_revised(tab$n_records, tab$n_years, tab$n_counties, t1, t2,
                       input$min_years %||% DEFAULT_MIN_YEARS, input$min_counties %||% DEFAULT_MIN_COUNTIES)
    tab
  })

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
    tab <- filtered(); gb <- rv$pest$gbif
    layout_columns(fill = FALSE,
      value_box("Pest", tags$i(gb$gbif_canonical %||% input$pest),
                p(sprintf("EPPO: %s | GBIF key: %s", rv$pest$eppo$code, gb$acceptedKey %||% "-"))),
      value_box("Hosts (filtered)", nrow(tab)),
      value_box("Documented in Norway (≥1 record)", sum(tab$n_records > 0, na.rm = TRUE)),
      value_box("Established in Norway (Artsdatabanken)", sum(tab$ref_status == "Established", na.rm = TRUE)))
  })

  display_table <- reactive({
    tab <- filtered()
    data.frame(
      `Host plant` = tab$host_plant,
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
              options = list(pageLength = 25, scrollX = TRUE, order = list(list(4, "desc"))))
  })

  export_table <- reactive({
    tab <- filtered()
    tab$bibref <- NULL
    tab
  })
  output$dl_csv <- downloadHandler(
    filename = function() sprintf("hosts_%s_%s.csv", gsub("\\W+", "_", input$pest), Sys.Date()),
    content = function(file) utils::write.csv(export_table(), file, row.names = FALSE, na = ""))
  output$dl_xlsx <- downloadHandler(
    filename = function() sprintf("hosts_%s_%s.xlsx", gsub("\\W+", "_", input$pest), Sys.Date()),
    content = function(file) {
      meta <- data.frame(setting = c("pest", "eppo_code", "date", "rule", "t1", "t2", "min_years", "min_counties", "basisOfRecord"),
                         value = c(input$pest, rv$pest$eppo$code %||% "", as.character(Sys.time()), input$rule,
                                   input$t1, input$t2, input$min_years, input$min_counties, paste(input$basis, collapse = ";")))
      writexl::write_xlsx(list(hosts = export_table(), settings = meta), file)
    })

  # ---- Map ----
  output$hostmap <- renderLeaflet({
    # Key-free tiles loaded by URL (no provider plugin). Default: shaded relief
    # only (no roads, labels or land use); OSM-based alternatives selectable.
    leaflet() |>
      addTiles(MAP_TILES$relief$url, attribution = MAP_TILES$relief$attr, group = "Terrain (relief only)",
               options = tileOptions(maxNativeZoom = 13)) |>
      addTiles(MAP_TILES$osm$url, attribution = MAP_TILES$osm$attr, group = "OpenStreetMap") |>
      addTiles(MAP_TILES$topo$url, attribution = MAP_TILES$topo$attr, group = "OpenTopoMap (OSM terrain)",
               options = tileOptions(maxNativeZoom = 17)) |>
      addLayersControl(baseGroups = c("Terrain (relief only)", "OpenStreetMap", "OpenTopoMap (OSM terrain)"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = 12, lat = 65, zoom = 4)
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
    max_pts <- min(MAX_POINTS_HARD_LIMIT, input$max_pts %||% DEFAULT_MAX_POINTS)
    basis <- if (length(input$basis)) input$basis else DEFAULT_BASIS
    msgs <- character(0)
    pts <- withProgress(message = "Fetching occurrence points", value = 0, {
      lapply(seq_len(nrow(hosts)), function(i) {
        setProgress(i / nrow(hosts), detail = hosts$host_plant[i])
        p <- tryCatch(gbif_points(hosts$acceptedKey[i], max_pts, basis), error = function(e) NULL)
        if (is.null(p)) { msgs <<- c(msgs, sprintf("Failed to fetch points for %s.", hosts$host_plant[i])); return(NULL) }
        d <- clean_points(p$data, input$max_unc, input$keep_na_unc, max_pts)
        if (p$n_available > max_pts)
          msgs <<- c(msgs, sprintf("<i>%s</i>: %s georeferenced records available; at most %d fetched per basisOfRecord, %d shown after de-duplication and precision filter.",
                                   hosts$host_plant[i], format(p$n_available, big.mark = " "), max_pts, NROW(d)))
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
                       radius = 4, stroke = FALSE, fillOpacity = 0.7, color = pal(d[[by]]),
                       popup = ~sprintf("<b><i>%s</i></b><br>%s<br>Year: %s<br>%s<br>Uncertainty: %s m",
                                        host, eppo_category, year, basisOfRecord, coordinateUncertaintyInMeters)) |>
      addLegend("bottomright", pal = pal, values = d[[by]], title = if (by == "uncertainty_class") "Uncertainty class" else "EPPO category") |>
      addLayersControl(baseGroups = c("Terrain (relief only)", "OpenStreetMap", "OpenTopoMap (OSM terrain)"),
                       overlayGroups = groups, options = layersControlOptions(collapsed = length(groups) > 6))
  })
}

shinyApp(ui, server)
