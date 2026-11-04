# =============================================================================
# theme.R: look of the Host Map app. All UI colours live here, in one place.
#
# Scheme: clean white + a single accent blue (productivity-tool style).
#   * white surfaces, dark grey text (not black), hairline borders
#   * ONE blue, and only on things you can click: buttons, active tab,
#     checkboxes/radios, links, focus rings
#   * very light blue only for hover and selection
#   * data colours are separate: a grey gradient for the uncertainty column,
#     amber for warnings, viridis for map points
# www/vkm.css holds the colour-free layout and type rules.
# =============================================================================

HM_COLOURS <- list(
  paper     = "#FFFFFF",
  text      = "#2B2F36",   # dark grey body text
  muted     = "#6B7280",   # labels, secondary text
  hairline  = "#E3E6EA",   # borders, dividers
  subtle    = "#F7F7F7",   # faint row striping (neutral grey, no blue)
  blue      = "#2160D6",   # the accent: interactive elements only (white text on it: 5.3:1)
  blue_soft = "#EAF1FD",   # hover / selection
  amber     = "#B7791F",   # warnings (status colour, not an accent)
  # uncertainty column: grey gradient, darker = better documented
  grey_1    = "#EFEFEF",   # high uncertainty
  grey_2    = "#DEDEDE",   # moderate uncertainty
  grey_3    = "#C9C9C9"    # low uncertainty
)
VKM_COLOURS <- HM_COLOURS   # backwards-compatible name used in app.R

hm_theme <- function(col = HM_COLOURS) {
  rules <- sprintf("
    body, .card, .accordion-item, .sidebar, .bslib-sidebar-layout > .sidebar,
    .vkm-header, .vkm-navbar, .vkm-navbar .navbar { background-color: %1$s !important; color: %2$s; }
    .vkm-sep { background: %4$s; }
    .vkm-subtitle, .hm-meta, .hm-stat span, .sidebar .control-label, .hm-note, .small { color: %3$s !important; }

    /* navigation: grey text, blue for the active tab, light blue on hover */
    .vkm-navbar .navbar { border-bottom: 1px solid %4$s; }
    .vkm-navbar .nav-link { color: %3$s !important; }
    .vkm-navbar .nav-link:hover { background-color: %7$s; color: %2$s !important; }
    .vkm-navbar .nav-link.active, .vkm-navbar .nav-link.active:focus { color: %6$s !important; box-shadow: inset 0 -2px 0 %6$s; font-weight: 600 !important; }

    .bslib-sidebar-layout > .sidebar { border-right: 1px solid %4$s; }
    .form-control, .form-select, .selectize-input { background: %1$s; border: 1px solid %4$s; color: %2$s; }
    .form-control:focus, .selectize-input.focus { border-color: %6$s; box-shadow: 0 0 0 3px %7$s; }
    input[type=checkbox], input[type=radio] { accent-color: %6$s; }
    .shiny-input-container input[type=checkbox]:checked, .shiny-input-container input[type=radio]:checked,
    .form-check-input:checked { background-color: %6$s !important; border-color: %6$s !important; }
    .selectize-control.multi .selectize-input > div { background: %7$s; color: %2$s; border-radius: 4px; }
    .selectize-dropdown .active { background: %7$s; color: %2$s; }

    /* buttons: blue = the action; secondary actions are quiet with blue text */
    .btn.btn-primary { background-color: %6$s; border: 1px solid %6$s; color: #FFFFFF !important; font-weight: 600; }
    .btn.btn-primary:hover, .btn.btn-primary:focus { background-color: %6$s; filter: brightness(0.92); color: #FFFFFF !important; }
    .btn.btn-secondary, .btn-default:not(.btn-primary) { background-color: %1$s; border: 1px solid %4$s; color: %6$s !important; }
    .btn.btn-secondary:hover, .btn-default:not(.btn-primary):hover { background-color: %7$s; border-color: %7$s; }
    .btn:focus-visible, .nav-link:focus-visible, a:focus-visible { outline: 2px solid %6$s; outline-offset: 2px; }
    a { color: %6$s; }

    /* warnings: white with a thin amber edge */
    .alert { background-color: %1$s; border: 1px solid %4$s; border-left: 3px solid %3$s; border-radius: 4px; color: %2$s !important; }
    .alert-warning { border-left-color: %8$s; }

    /* table */
    table.dataTable thead th { background-color: %1$s; color: %3$s; font-weight: 600; border-bottom: 1px solid %4$s !important; }
    table.dataTable tbody td { border-top: 1px solid %4$s !important; }
    table.dataTable { --bs-table-striped-bg: %5$s; --bs-table-hover-bg: %7$s; --bs-table-active-bg: %7$s; --dt-row-stripe: 0, 0, 0; }
    table.dataTable.stripe tbody tr.odd > *, table.dataTable.display tbody tr.odd > *,
    table.dataTable.table-striped > tbody > tr:nth-of-type(odd) > * { box-shadow: inset 0 0 0 9999px %5$s; --bs-table-accent-bg: %5$s; }
    table.dataTable tbody tr.even > * { box-shadow: none; }
    table.dataTable tbody tr:hover > * { box-shadow: inset 0 0 0 9999px %7$s !important; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px %7$s !important; color: %2$s !important; }
    .dataTables_wrapper .pagination .page-item.active .page-link { background-color: %6$s; border-color: %6$s; color: #FFFFFF; }
    .dataTables_wrapper .pagination .page-link { color: %6$s; }

    .hm-progress-bar { background: %4$s; } .hm-progress-fill { background: %6$s; }
    .vkm-footer { background: %1$s; border-top: 1px solid %4$s; color: %3$s; }
    .vkm-footer a { color: %6$s; }
    .leaflet-container { background: %1$s; }
  ", col$paper, col$text, col$muted, col$hairline, col$subtle, col$blue, col$blue_soft, col$amber)

  bslib::bs_theme(
    version = 5, bg = col$paper, fg = col$text,
    primary = col$blue, secondary = col$muted,
    info = col$blue, warning = col$amber, danger = col$amber,
    light = col$subtle, dark = col$text,
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
  ) |>
    bslib::bs_add_variables(
      "border-color" = col$hairline, "card-bg" = col$paper, "card-cap-bg" = col$paper,
      "link-color" = col$blue, "link-hover-color" = col$blue, "text-muted" = col$muted,
      "navbar-light-color" = col$muted, "navbar-light-active-color" = col$blue,
      .where = "declarations"
    ) |>
    bslib::bs_add_rules(rules)
}

#' White header: VKM logo (if present in www/), hairline divider, app name.
vkm_header <- function(title, subtitle = NULL, www = "www") {
  logo <- c(file.path(www, "vkm_logo.svg"), file.path(www, "vkm_logo.png"))
  logo <- logo[file.exists(logo)][1]
  shiny::div(class = "vkm-header",
    if (!is.na(logo)) shiny::tags$img(src = basename(logo), class = "vkm-logo",
                                      alt = "VKM – Vitenskapskomiteen for mat og miljø"),
    if (!is.na(logo)) shiny::div(class = "vkm-sep"),
    shiny::div(class = "vkm-title",
      shiny::div(class = "vkm-t", title),
      if (!is.null(subtitle)) shiny::div(class = "vkm-subtitle", subtitle)))
}

vkm_footer <- function(project = "FinnPRIO", name = "Daniel Flø", email = "Daniel.Flo@vkm.no") {
  shiny::div(class = "vkm-footer",
    shiny::span(sprintf("Host Map is part of the %s project. Questions: %s, ", project, name),
                shiny::tags$a(href = paste0("mailto:", email), email)),
    shiny::span("VKM"))
}

#' Pest heading: the species name is the page's headline, then one quiet line of
#' identifiers and one line of counts.
hm_pest_heading <- function(name, norwegian = NA, code = NA, gbif_key = NA, n_hosts, n_doc, n_est) {
  meta <- c(if (!is.na(norwegian)) paste0("Norwegian: ", norwegian),
            if (!is.na(code)) paste0("EPPO code ", code),
            if (!is.na(gbif_key)) paste0("GBIF key ", gbif_key))
  stat <- function(n, label) shiny::div(class = "hm-stat", shiny::tags$b(formatC(n, big.mark = " ", format = "d")), shiny::span(label))
  shiny::div(class = "hm-pest",
    shiny::tags$h1(class = "hm-pest-name", name),
    shiny::div(class = "hm-meta", paste(meta, collapse = ", ")),
    shiny::div(class = "hm-stats",
      stat(n_hosts, "host plants in EPPO"),
      stat(n_doc, "documented in Norway"),
      stat(n_est, "established in Norway (Artsdatabanken)")))
}

#' Uncertainty column: one grey gradient. Darker grey = better documented;
#' not documented = no fill. (Blue is kept for interactive elements.)
uncertainty_fill <- function(col = HM_COLOURS) {
  c("Not documented in Norway" = col$paper,
    "High uncertainty"         = col$grey_1,
    "Moderate uncertainty"     = col$grey_2,
    "Low uncertainty"          = col$grey_3)
}

#' HTML legend for the map: one row per class with a colour dot, label and count.
hm_map_legend <- function(title, colours, labels, counts) {
  rows <- paste0(sprintf('<div class="hm-leg-row"><span class="hm-leg-dot" style="background:%s"></span><span class="hm-leg-lab">%s</span><span class="hm-leg-n">%s</span></div>',
                         colours, labels, counts), collapse = "")
  sprintf('<div class="hm-leg-title">%s</div>%s', htmltools::htmlEscape(title), rows)
}

#' Discrete palettes for data (map points), never the UI palette.
hm_viridis_colours <- function(n) viridisLite::viridis(max(1, n), end = 0.95)
#' Host species: viridis "turbo" (option H) separates up to ~10 classes clearly.
hm_species_colours <- function(n) if (n <= 1) viridisLite::turbo(3)[2] else viridisLite::turbo(n, begin = 0.05, end = 0.95)
hm_viridis <- function(levels) leaflet::colorFactor(hm_viridis_colours(length(levels)), levels = levels, ordered = TRUE)
