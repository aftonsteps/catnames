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
    # Your application UI logic
    fluidPage(
      actionButton("browser", "browser"),
      tags$script("$('#browser').hide();"),
      h1("Cat Name Generator"),
      mod_cat_name_picker_ui("cat_name_picker_1"),
      HTML("<a href='https://www.freepik.com/vectors/ornament'>Ornament vector created by rawpixel.com - www.freepik.com</a>")
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'catnames'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

