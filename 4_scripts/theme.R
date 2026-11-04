# =============================================================================
# theme.R: VKM look for the Host Map app. All UI colours are defined here.
# Design: white background, generous white space; palette colours are used as
# details (borders, accents, indicators), not as large surfaces. Black text only.
# Change a colour in VKM_COLOURS and it applies app-wide (bs_theme + Sass rules).
# www/vkm.css only forces black text and holds spacing rules (no colours).
# =============================================================================

VKM_COLOURS <- list(
  primary    = "#86B29C",
  secondary  = "#B88EB4",
  accent     = "#F39800",   # warnings, key call-to-action, active nav indicator
  neutral    = "#CFCFCA",   # thin borders and dividers
  light      = "#E9EBE8",   # subtle hover / selected rows only
  background = "#FFFFFF",
  text       = "#000000"
)

hm_theme <- function(col = VKM_COLOURS) {
  rules <- sprintf("
    body, .card, .bslib-value-box, .accordion-item, .accordion-button,
    .sidebar, .bslib-sidebar-layout > .sidebar, .vkm-header, .vkm-footer,
    .vkm-navbar, .vkm-navbar .navbar { background-color: %1$s !important; }
    .vkm-navbar .navbar { border-bottom: 1px solid %2$s; }
    .vkm-navbar .nav-link.active { border-bottom: 3px solid %3$s; }
    .bslib-sidebar-layout > .sidebar { border-right: 1px solid %2$s; }
    .card, .bslib-value-box { border: 1px solid %2$s; box-shadow: none; }
    .bslib-value-box.card { border: 1px solid %2$s !important; border-left: 4px solid %6$s !important; }
    .accordion-button:not(.collapsed) { box-shadow: inset 0 -1px 0 %2$s; }
    .vkm-footer { border-top: 1px solid %2$s; }
    .btn.btn-primary   { background-color: %1$s; border: 2px solid %6$s; color: %5$s; }
    .btn.btn-secondary { background-color: %1$s; border: 1px solid %7$s; color: %5$s; }
    .btn.btn-primary:hover   { background-color: %6$s; color: %5$s; }
    .btn.btn-secondary:hover { background-color: %7$s; color: %5$s; }
    .btn-default:not(.btn-primary):not(.btn-secondary) { background-color: %1$s; border: 1px solid %2$s; color: %5$s; }
    .alert { background-color: %1$s; border: 1px solid %2$s; border-left: 4px solid %7$s; }
    .alert-warning { border-left-color: %3$s; }
    table.dataTable thead th { background-color: %1$s; border-bottom: 2px solid %2$s !important; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px %4$s !important; color: %5$s !important; }
    .form-check-input:checked { background-color: %6$s; border-color: %6$s; }
    .leaflet-container { background: %1$s; }
  ", col$background, col$neutral, col$accent, col$light, col$text, col$primary, col$secondary)

  bslib::bs_theme(
    version = 5, bg = col$background, fg = col$text,
    primary = col$primary, secondary = col$secondary,
    success = col$primary, info = col$secondary,
    warning = col$accent, danger = col$accent,
    light = col$light, dark = col$text
  ) |>
    bslib::bs_add_variables(
      "card-bg" = col$background, "card-cap-bg" = col$background, "border-color" = col$neutral,
      "navbar-light-color" = col$text, "navbar-light-active-color" = col$text,
      "link-color" = col$text, "link-hover-color" = col$text,
      "btn-color" = col$text,
      .where = "declarations"
    ) |>
    bslib::bs_add_rules(rules)
}

#' White header bar with the VKM logo (if present in www/) and the app title.
vkm_header <- function(title, subtitle = NULL, www = "www") {
  logo <- c(file.path(www, "vkm_logo.svg"), file.path(www, "vkm_logo.png"))
  logo <- logo[file.exists(logo)][1]
  shiny::div(class = "vkm-header",
    if (!is.na(logo)) shiny::tags$img(src = basename(logo), class = "vkm-logo",
                                      alt = "VKM – Vitenskapskomiteen for mat og miljø"),
    shiny::div(class = "vkm-title",
      shiny::h1(title),
      if (!is.null(subtitle)) shiny::div(class = "vkm-subtitle", subtitle)))
}

vkm_footer <- function(contact = "Contact for the Host Map project: Daniel Flø") {
  shiny::div(class = "vkm-footer", shiny::span(contact), shiny::span("VKM"))
}

#' Discrete viridis palette for map points / plots (never the UI palette).
hm_viridis <- function(levels) leaflet::colorFactor(viridisLite::viridis(max(1, length(levels)), end = 0.95),
                                                    levels = levels, ordered = TRUE)
