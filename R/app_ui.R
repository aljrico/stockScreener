list_companies <- function(){
  all_symbols <- rfinance::get_symbols_list()
  # complete_list <- rfinance::get_company_profile(all_symbols)
  all_symbols
}

navigation_bar <- function() {
  bs4Dash::bs4DashNavbar(
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = "bars",
    controlbarIcon = "th",
    fixed = FALSE
  )
}

autocomplete_searchbar <- function(
  id, label, options, value = "", width = NULL, placeholder = 'Search...',
  max_options = 0, hide_values = FALSE, create = FALSE
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is needed to convert list of options into json!")
  }
  value <- shiny::restoreInput(id = id, default = value)
  js_opts <- jsonlite::toJSON(as.list(options), auto_unbox = TRUE)
  width <- shiny::validateCssUnit(width)
  if (length(value) == 0L) value <- ""
  shiny::div(
    class = "form-group shiny-input-container searchbar",
    shiny::tags$input(
      id = id, type = "text", class = "search_input", result = value,
      value = value, placeholder = placeholder, "data-options" = js_opts,
      "data-max" = max_options, "data-hide" = tolower(isTRUE(hide_values)),
      "data-create" = tolower(isTRUE(create))
    ),
    tags$a(href="#", class="search_icon", tags$i(class="fas fa-search")),
    htmltools::htmlDependency(
      "autocomplete", "0.0.1", c(href = "aljrico"),
      script = "js/autocomplete"
    )
  )
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    bs4Dash::bs4DashPage(
      old_school = FALSE,
      sidebar_collapsed = TRUE,
      controlbar_collapsed = TRUE,
      controlbar_overlay = TRUE,
      title = "Basic Dashboard",
      body = bs4Dash::bs4DashBody(
        fluidRow(
          column(4),
          column(4, autocomplete_searchbar('searchbar', "", companies_list, max_options = 5, hide_values = TRUE)),
          column(4)
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "stockScreener"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
