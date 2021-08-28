#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  observeEvent(input$browser,{
    browser()
  })

  mod_cat_name_picker_server("cat_name_picker_1")
}
