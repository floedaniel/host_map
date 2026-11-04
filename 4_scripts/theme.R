# =============================================================================
# theme.R: VKM look for the Host Map app (matches the "FinnPRIO – VKM Shiny
# eksempel v2" mockup). All UI colours are defined here, in one place.
# White background and plenty of space; colour only as details: green line
# under the navbar and on the main button, orange only for warnings and the
# active tab. Black text; secondary text in muted olive-grey.
# www/vkm.css holds the colour-free layout/spacing rules.
# =============================================================================

VKM_COLOURS <- list(
  primary    = "#86B29C",   # navbar line, main button, footer line
  accent     = "#F39800",   # warnings and active tab only
  neutral    = "#CFCFCA",   # thin borders and dividers
  light      = "#E9EBE8",   # key-figure tiles, footer
  muted      = "#626250",   # secondary text (labels, inactive tabs, subtitles)
  background = "#FFFFFF",
  text       = "#000000"
)

hm_theme <- function(col = VKM_COLOURS) {
  rules <- sprintf("
    body { background: %1$s; color: %5$s; }
    .vkm-header, .vkm-navbar, .vkm-navbar .navbar, .sidebar, .bslib-sidebar-layout > .sidebar,
    .card, .accordion-item, .accordion-button { background-color: %1$s !important; }

    /* header: logo | divider | title */
    .vkm-sep { background: %2$s; }
    .vkm-subtitle { color: %6$s; }

    /* navbar: white, green line under, orange under active tab */
    .vkm-navbar .navbar { border-bottom: 3px solid %7$s; }
    .vkm-navbar .nav-link { color: %6$s !important; border-bottom: 3px solid transparent; }
    .vkm-navbar .nav-link.active, .vkm-navbar .nav-link.active:focus { color: %5$s !important; font-weight: 700; box-shadow: inset 0 -3px 0 %3$s; }

    /* sidebar */
    .bslib-sidebar-layout > .sidebar { border-right: 1px solid %2$s; }
    .sidebar .control-label, .sidebar label.form-label { color: %6$s; }
    .form-control, .form-select { background: %1$s; border: 1px solid %2$s; }

    /* buttons */
    .btn.btn-primary { background-color: %7$s; border: 1px solid %5$s; color: %5$s; font-weight: 700; }
    .btn.btn-primary:hover, .btn.btn-primary:focus { background-color: %7$s; filter: brightness(0.95); color: %5$s; }
    .btn.btn-secondary, .btn-default:not(.btn-primary) { background-color: %1$s; border: 1px solid %2$s; color: %5$s; }
    .btn.btn-secondary:hover, .btn-default:not(.btn-primary):hover { background-color: %4$s; color: %5$s; }

    /* warnings / notes */
    .alert { background-color: %1$s; border: 1px solid %2$s; border-left: 6px solid %2$s; border-radius: 6px; }
    .alert-warning { border-color: %3$s; border-left-color: %3$s; }

    /* key-figure tiles and cards */
    .bslib-value-box.card { background-color: %4$s !important; border: 0 !important; border-radius: 8px; box-shadow: none; }
    .bslib-value-box .value-box-title { color: %6$s; }
    .card { border: 1px solid %2$s; border-radius: 8px; box-shadow: none; }
    .accordion-button:not(.collapsed) { box-shadow: inset 0 -1px 0 %2$s; }

    /* tables and form controls */
    table.dataTable thead th { background-color: %1$s; border-bottom: 2px solid %2$s !important; }
    table.dataTable.stripe tbody tr.odd > *, table.dataTable.display tbody tr.odd > *,
    .table-striped > tbody > tr:nth-of-type(odd) > * { box-shadow: inset 0 0 0 9999px %4$s; --bs-table-accent-bg: %4$s; }
    table.dataTable tbody tr:hover > * { box-shadow: inset 0 0 0 9999px %2$s !important; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px %4$s !important; color: %5$s !important; }
    .form-check-input:checked { background-color: %7$s; border-color: %5$s; }
    .small, small, .text-muted { color: %6$s !important; }

    /* footer */
    .vkm-footer { background: %4$s; border-top: 3px solid %7$s; }
    .leaflet-container { background: %1$s; }
  ", col$background, col$neutral, col$accent, col$light, col$text, col$muted, col$primary)

  bslib::bs_theme(
    version = 5, bg = col$background, fg = col$text,
    primary = col$primary, secondary = col$neutral,
    success = col$primary, info = col$neutral,
    warning = col$accent, danger = col$accent,
    light = col$light, dark = col$text,
    base_font = bslib::font_collection("Calibri", "Segoe UI", "Tahoma", "sans-serif"),
    font_scale = 0.95
  ) |>
    bslib::bs_add_variables(
      "card-bg" = col$background, "card-cap-bg" = col$background, "border-color" = col$neutral,
      "navbar-light-color" = col$muted, "navbar-light-active-color" = col$text,
      "link-color" = col$text, "link-hover-color" = col$text,
      "btn-color" = col$text, "text-muted" = col$muted,
      .where = "declarations"
    ) |>
    bslib::bs_add_rules(rules)
}

#' White header: VKM logo (if present in www/), thin divider, title + subtitle.
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
    shiny::span(sprintf("Host Map is part of the %s project. For more information, contact %s ", project, name),
                shiny::tags$a(href = paste0("mailto:", email), paste0("<", email, ">"))),
    shiny::span("VKM"))
}

#' Cell highlight for the uncertainty classes: palette colours only, softened
#' with transparency so they read as details. Black text on all of them.
uncertainty_fill <- function(col = VKM_COLOURS) {
  c("Not documented in Norway" = col$background,
    "High uncertainty"         = paste0(col$accent, "59"),
    "Moderate uncertainty"     = col$light,
    "Low uncertainty"          = paste0(col$primary, "8C"))
}

#' Discrete viridis palette for map points / plots (never the UI palette).
hm_viridis <- function(levels) leaflet::colorFactor(viridisLite::viridis(max(1, length(levels)), end = 0.95),
                                                    levels = levels, ordered = TRUE)
