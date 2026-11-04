# =============================================================================
# theme.R: VKM look for the Host Map app (VKM profile 2025). All UI colours live
# here, in one place.
#
# Principle: white paper, black ink, very few colours. Colour only marks what
# needs attention:
#   green   #86B29C  VKM green (PMS 624c): rule under the pest name, active tab, primary action
#   caution #F39800  warnings only
# The uncertainty column is a single green gradient: the better documented a
# host is in Norway, the stronger the green.
# Everything else is neutral greys from the VKM palette.
# www/vkm.css holds the colour-free layout and type rules.
# =============================================================================

VKM_COLOURS <- list(
  paper     = "#FFFFFF",
  ink       = "#000000",
  grey      = "#626250",   # VKM logo grey: labels, secondary text, controls
  hairline  = "#CFCFCA",   # borders, dividers
  mist      = "#E9EBE8",   # table striping, footer
  accent    = "#86B29C",   # VKM green (PMS 624c): brand accent + primary action
  accent30  = "#DAE7E1",   # VKM 30 % tint of the green: documentation gradient
  caution   = "#F39800"    # warnings only
)

hm_theme <- function(col = VKM_COLOURS) {
  rules <- sprintf("
    body, .card, .accordion-item, .sidebar, .bslib-sidebar-layout > .sidebar,
    .vkm-header, .vkm-navbar, .vkm-navbar .navbar { background-color: %1$s !important; color: %2$s; }

    .vkm-sep { background: %4$s; }
    .hm-rule { background: %6$s; }
    .vkm-subtitle, .hm-meta, .hm-stat span, .sidebar .control-label, .small { color: %3$s !important; }

    /* navigation: hairline under the bar, green under the active tab */
    .vkm-navbar .navbar { border-bottom: 1px solid %4$s; }
    .vkm-navbar .nav-link { color: %2$s !important; }
    .vkm-navbar .nav-link:hover { background-color: %5$s; }
    .vkm-navbar .nav-link.active, .vkm-navbar .nav-link.active:focus { box-shadow: inset 0 -3px 0 %6$s; font-weight: 700 !important; }

    .bslib-sidebar-layout > .sidebar { border-right: 1px solid %4$s; }
    .form-control, .form-select, .selectize-input { background: %1$s; border: 1px solid %4$s; }
    .form-control:focus, .selectize-input.focus { border-color: %3$s; box-shadow: 0 0 0 2px %5$s; }
    .form-check-input:checked { background-color: %3$s !important; border-color: %3$s !important; }
    .form-check-input:focus { box-shadow: 0 0 0 2px %5$s; border-color: %3$s; }
    input[type=checkbox], input[type=radio] { accent-color: %3$s; }
    .shiny-input-container input[type=checkbox]:checked, .shiny-input-container input[type=radio]:checked { background-color: %3$s !important; border-color: %3$s !important; }
    .selectize-control.multi .selectize-input > div { background: %5$s; color: %2$s; border-radius: 4px; }

    /* actions: green = the one thing to press; everything else is quiet */
    .btn.btn-primary { background-color: %6$s; border: 1px solid %2$s; color: %2$s; font-weight: 700; }
    .btn.btn-primary:hover, .btn.btn-primary:focus { background-color: %6$s; filter: brightness(1.06); color: %2$s; }
    .btn.btn-secondary, .btn-default:not(.btn-primary) { background-color: %1$s; border: 1px solid %4$s; color: %2$s; }
    .btn.btn-secondary:hover, .btn-default:not(.btn-primary):hover { background-color: %5$s; color: %2$s; }
    .btn:focus-visible, .nav-link:focus-visible { outline: 2px solid %2$s; outline-offset: 2px; }

    /* warnings: white with a caution edge */
    .alert { background-color: %1$s; border: 1px solid %4$s; border-left: 4px solid %3$s; border-radius: 4px; }
    .alert-warning { border-color: %7$s; border-left-color: %7$s; }

    /* table */
    table.dataTable thead th { background-color: %1$s; border-bottom: 1px solid %2$s !important; }
    table.dataTable.stripe tbody tr.odd > *, table.dataTable.display tbody tr.odd > * { box-shadow: inset 0 0 0 9999px %5$s; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px %4$s !important; color: %2$s !important; }

    .hm-progress-bar { background: %5$s; } .hm-progress-fill { background: %6$s; }
    .vkm-footer { background: %5$s; }
    .leaflet-container { background: %1$s; }
  ", col$paper, col$ink, col$grey, col$hairline, col$mist, col$accent, col$caution)

  bslib::bs_theme(
    version = 5, bg = col$paper, fg = col$ink,
    primary = col$accent, secondary = col$hairline,
    success = col$accent, info = col$hairline, warning = col$caution, danger = col$caution,
    light = col$mist, dark = col$ink,
    base_font = bslib::font_collection("Calibri", "Carlito", "Segoe UI", "sans-serif")
  ) |>
    bslib::bs_add_variables(
      "border-color" = col$hairline, "card-bg" = col$paper, "card-cap-bg" = col$paper,
      "link-color" = col$ink, "link-hover-color" = col$ink, "text-muted" = col$grey,
      "btn-color" = col$ink, "navbar-light-color" = col$ink, "navbar-light-active-color" = col$ink,
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

#' Pest heading: the species name is the page's headline, with a short VKM green
#' rule under it and one quiet line of counts.
hm_pest_heading <- function(name, norwegian = NA, code = NA, gbif_key = NA, n_hosts, n_doc, n_est) {
  meta <- c(if (!is.na(norwegian)) paste0("Norwegian: ", norwegian),
            if (!is.na(code)) paste0("EPPO code ", code),
            if (!is.na(gbif_key)) paste0("GBIF key ", gbif_key))
  stat <- function(n, label) shiny::div(class = "hm-stat", shiny::tags$b(formatC(n, big.mark = " ", format = "d")), shiny::span(label))
  shiny::div(class = "hm-pest",
    shiny::tags$h1(class = "hm-pest-name", name),
    shiny::div(class = "hm-meta", paste(meta, collapse = ", ")),
    shiny::div(class = "hm-rule"),
    shiny::div(class = "hm-stats",
      stat(n_hosts, "host plants in EPPO"),
      stat(n_doc, "documented in Norway"),
      stat(n_est, "established in Norway (Artsdatabanken)")))
}

#' Uncertainty highlighting in the table: one green gradient from VKM green and
#' its 30 % tint. Stronger green = better documented; not documented = no fill.
uncertainty_fill <- function(col = VKM_COLOURS) {
  c("Not documented in Norway" = col$paper,
    "High uncertainty"         = paste0(col$accent30, "99"),   # 60 % of the tint
    "Moderate uncertainty"     = col$accent30,
    "Low uncertainty"          = paste0(col$accent, "B3"))     # 70 % of VKM green
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
