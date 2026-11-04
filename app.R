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
#   0 = not documented | 1-15 = high | 16-40 = moderate | >= 41 = low uncertainty
#   (calibrated 12.8 / 37.7 records for P(present) = 0.95 / 0.99, rounded to 15 / 40)
UNCERTAINTY_T1 <- 15L
UNCERTAINTY_T2 <- 40L

# Map
MAP_MAX_UNCERTAINTY_M <- 1000     # drop points with coordinate uncertainty above this
MAP_KEEP_MISSING_UNCERTAINTY <- TRUE
MAP_MAX_POINTS <- 1000            # per host layer (kept low for shinyapps.io)
MAP_DEFAULT_N_HOSTS <- 5          # hosts mapped by default (most records first)
MAP_MAX_HOSTS <- 10               # max host species on the map at once (distinct colours)
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
  vkm_header("Host Map", "Host plants of plant pests and how well they are documented in Norway"),
  div(class = "vkm-navbar", navset_bar(id = "tabs", bg = VKM_COLOURS$paper, fillable = FALSE,
  sidebar = sidebar(width = 300, bg = VKM_COLOURS$paper,
    textInput("pest", "Pest species (scientific name or EPPO code)", "", placeholder = "Species"),
    actionButton("go", "Search", class = "btn-primary w-100"),
    checkboxGroupInput("cats", "EPPO host categories", choices = NULL),
    downloadButton("dl_csv", "Download CSV", class = "btn-secondary w-100 mb-2"),
    downloadButton("dl_xlsx", "Download Excel", class = "btn-secondary w-100")
  ),
  header = tagList(uiOutput("messages"), uiOutput("summary_boxes")),
    nav_panel("Host plants",
      p(class = "hm-note",
        "Uncertainty class refers to ", strong("documented occurrence"), " in GBIF records from Norway ",
        sprintf("(0 records: not documented; 1–%d: high; %d–%d: moderate; ≥%d: low uncertainty). ",
                UNCERTAINTY_T1, UNCERTAINTY_T1 + 1L, UNCERTAINTY_T2, UNCERTAINTY_T2 + 1L),
        "“Status in Norwegian nature” is a separate concept, taken from Artsdatabanken's Red List 2021 ",
        "and Alien Species List 2023, and says whether the plant is ", strong("established"), " in Norway."),
      DTOutput("tbl")),
    nav_panel("Map",
      layout_columns(col_widths = c(3, 5, 2, 2),
        checkboxGroupInput("map_cats", "EPPO categories", choices = NULL, inline = TRUE),
        selectizeInput("map_hosts", sprintf("Host species on map (max %d; type to search)", MAP_MAX_HOSTS),
                       choices = NULL, multiple = TRUE, width = "100%",
                       options = list(maxItems = MAP_MAX_HOSTS, plugins = list("remove_button"),
                                      placeholder = "Hosts with the most records")),
        radioButtons("colour_by", "Colour points by",
                     c("Host species" = "host", "Uncertainty class" = "uncertainty_class", "EPPO category" = "eppo_category")),
        actionButton("load_map", "Update map", class = "btn-primary mt-4 w-100")),
      p(class = "hm-note", "Only hosts with Norwegian records are listed, most records first. By default the ",
        MAP_DEFAULT_N_HOSTS, " best-documented hosts in the ticked categories are shown."),
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
        summarise_hosts(hosts, basis_keep = GBIF_BASIS_COUNTED, t1 = UNCERTAINTY_T1, t2 = UNCERTAINTY_T2, ref = REFERENCE, api_key = EPPO_API_KEY,
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
    ch <- host_choices(tab, rv$map_cats_default)
    rv$map_hosts_default <- unname(head(ch, MAP_DEFAULT_N_HOSTS))
    rv$skip_cat_update <- TRUE
    updateSelectizeInput(session, "map_hosts", choices = ch, selected = rv$map_hosts_default, server = TRUE)
    rv$map_auto <- TRUE
    map_trigger(map_trigger() + 1)   # draw the default map straight after a search
  })

  results <- reactive({ req(rv$base); rv$base })

  # Hosts with Norwegian records in the given categories, most records first,
  # labelled "Name (n records)" for the map picker.
  host_choices <- function(tab, cats) {
    t <- tab[!is.na(tab$n_records) & tab$n_records > 0 & !is.na(tab$acceptedKey) & tab$eppo_category %in% cats, ]
    t <- t[order(-t$n_records), ]
    t <- t[!duplicated(t$host_plant), ]
    stats::setNames(t$host_plant, sprintf("%s (%s records)", t$host_plant, formatC(t$n_records, big.mark = " ", format = "d")))
  }
  # Changing the map categories refreshes the picker with that category's top hosts
  observeEvent(input$map_cats, ignoreNULL = FALSE, ignoreInit = TRUE, {
    req(rv$base)
    if (isTRUE(rv$skip_cat_update)) { rv$skip_cat_update <- FALSE; return() }
    ch <- host_choices(rv$base, input$map_cats)
    updateSelectizeInput(session, "map_hosts", choices = ch, selected = unname(head(ch, MAP_DEFAULT_N_HOSTS)), server = TRUE)
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
    tab <- filtered(); ep <- rv$pest$eppo; gb <- rv$pest$gbif
    hm_pest_heading(ep$prefname %||% input$pest, ep$norwegian %||% NA, ep$code %||% NA, gb$acceptedKey %||% NA,
                    n_hosts = nrow(tab), n_doc = sum(tab$n_records > 0, na.rm = TRUE),
                    n_est = sum(tab$ref_status == "Established", na.rm = TRUE))
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
      class_rank = as.integer(tab$uncertainty_class),   # hidden: sort key in class order
      check.names = FALSE)
  })

  output$tbl <- renderDT({
    d <- display_table()
    rank_col <- which(names(d) == "class_rank") - 1L      # 0-based for DataTables
    rec_col <- which(names(d) == "Records in Norway") - 1L
    class_col <- which(names(d) == "Uncertainty class (documented occurrence)") - 1L
    datatable(d, escape = setdiff(seq_along(d), match(c("GBIF", "Artskart"), names(d))),
              filter = "top", rownames = FALSE, selection = "multiple",
              class = "stripe hover",
              options = list(pageLength = 25, scrollX = TRUE,
                             # sorted by uncertainty class (low -> not documented), then by records
                             order = list(list(rank_col, "desc"), list(rec_col, "desc")),
                             columnDefs = list(list(visible = FALSE, targets = rank_col),
                                               list(orderData = list(rank_col, rec_col), targets = class_col)))) |>
      formatStyle("Uncertainty class (documented occurrence)", fontWeight = "bold",
                  backgroundColor = styleEqual(names(uncertainty_fill()), unname(uncertainty_fill())))
  })

  export_table <- reactive({
    tab <- filtered()
    tab$bibref <- NULL; tab$candidates <- NULL
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
                  "Thresholds: calibrated 12.8 / 37.7 records (P(present in Norway) = 0.95 / 0.99), rounded to 15 / 40 (see REPORT.md). Records, years and counties exclude living and fossil specimens."))
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
  output$map_msg <- renderUI({ if (length(rv$map_msg)) p(class = "hm-note", HTML(paste(rv$map_msg, collapse = "<br>"))) })

  map_points <- eventReactive(map_trigger(), ignoreInit = TRUE, {
    # Right after a search the category inputs have not been updated yet: use the
    # unfiltered results and the default map categories.
    auto <- isTRUE(rv$map_auto); rv$map_auto <- FALSE
    tab <- results()
    shiny::validate(shiny::need(nrow(tab) > 0, "Run a search first."))
    picked <- if (auto) rv$map_hosts_default else input$map_hosts
    if (!length(picked)) picked <- unname(head(host_choices(tab, input$map_cats %||% rv$map_cats_default), MAP_DEFAULT_N_HOSTS))
    picked <- head(picked, MAP_MAX_HOSTS)
    hosts <- tab[match(picked, tab$host_plant), , drop = FALSE]
    hosts <- hosts[!is.na(hosts$acceptedKey) & !is.na(hosts$n_records) & hosts$n_records > 0, ]
    hosts <- hosts[order(-hosts$n_records), ]
    msgs <- character(0); capped <- character(0)
    pts <- withProgress(message = "Fetching occurrence points", value = 0, {
      lapply(seq_len(nrow(hosts)), function(i) {
        setProgress(i / nrow(hosts), detail = hosts$host_plant[i])
        p <- tryCatch(gbif_points(hosts$acceptedKey[i], MAP_MAX_POINTS, GBIF_BASIS_COUNTED), error = function(e) NULL)
        if (is.null(p)) { msgs <<- c(msgs, sprintf("Failed to fetch points for %s.", hosts$host_plant[i])); return(NULL) }
        d <- clean_points(p$data, MAP_MAX_UNCERTAINTY_M, MAP_KEEP_MISSING_UNCERTAINTY, MAP_MAX_POINTS)
        if (p$n_available > MAP_MAX_POINTS)
          capped <<- c(capped, sprintf("<i>%s</i> (%s)", hosts$host_plant[i], formatC(p$n_available, big.mark = " ", format = "d")))
        if (is.null(d) || !nrow(d)) return(NULL)
        d$host <- hosts$host_plant[i]; d$eppo_category <- as.character(hosts$eppo_category[i])
        d$uncertainty_class <- as.character(hosts$uncertainty_class[i])
        d
      })
    })
    if (length(capped))
      msgs <- c(msgs, sprintf("The map shows at most %s points per host (after removing duplicates and imprecise coordinates). More georeferenced records exist for: %s.",
                              formatC(MAP_MAX_POINTS, big.mark = " ", format = "d"), paste(capped, collapse = ", ")))
    rv$map_msg <- if (!nrow(hosts)) "No hosts with Norwegian records to map." else msgs
    dplyr::bind_rows(pts)
  })

  observe({
    d <- map_points()
    proxy <- leafletProxy("hostmap") |> clearMarkers() |> clearControls()
    if (is.null(d) || !nrow(d)) return()
    by <- input$colour_by %||% "host"
    groups <- unique(d$host)                       # ordered by number of records
    lv <- switch(by,
      host = groups,
      uncertainty_class = intersect(COUNT_CLASS_LEVELS, unique(d$uncertainty_class)),
      eppo_category = intersect(union(EPPO_CATEGORY_LEVELS, unique(d$eppo_category)), unique(d$eppo_category)))
    cols <- if (by == "host") hm_species_colours(length(lv)) else hm_viridis_colours(length(lv))
    fill <- cols[match(d[[by]], lv)]
    n_pts <- table(factor(d[[by]], levels = lv))
    nfmt <- formatC(as.integer(n_pts), big.mark = " ", format = "d")
    labels <- if (by == "host") sprintf("<i>%s</i>", htmltools::htmlEscape(lv)) else htmltools::htmlEscape(lv)
    title <- switch(by, host = "Host species (points)", uncertainty_class = "Uncertainty class (points)",
                    eppo_category = "EPPO category (points)")
    for (g in groups) {
      i <- d$host == g
      proxy <- proxy |>
        addCircleMarkers(data = d[i, ], lng = ~decimalLongitude, lat = ~decimalLatitude, group = g,
                         radius = 4.5, stroke = TRUE, weight = 0.6, color = "black", opacity = 0.7,
                         fillOpacity = 0.85, fillColor = fill[i],
                         popup = ~sprintf("<b><i>%s</i></b><br>%s<br>%s<br>Year: %s<br>%s<br>Coordinate uncertainty: %s m",
                                          host, eppo_category, uncertainty_class, year, basisOfRecord, coordinateUncertaintyInMeters))
    }
    proxy |>
      addControl(html = hm_map_legend(title, cols, labels, nfmt), position = "bottomright", className = "hm-legend") |>
      addLayersControl(baseGroups = BASE_GROUPS, overlayGroups = groups,
                       options = layersControlOptions(collapsed = TRUE))
  })
}

shinyApp(ui, server)
