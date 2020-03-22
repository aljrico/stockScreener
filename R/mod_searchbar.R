#' searchbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id Module ID
#' @param label
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_searchbar_ui <- function(id, label, options, value = "", width = NULL, placeholder = 'Search...',
                             max_options = 0, hide_values = FALSE, create = FALSE){
  ns <- NS(id)

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
