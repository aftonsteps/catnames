#' cat_name_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cat_name_picker_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("breed_picker")),
    uiOutput(outputId = ns("color_picker")),
    actionButton(inputId = ns("get_name"),
                 label = "Generate Name!"),
    textOutput(outputId = ns("name_text"))
  )
}

#' cat_name_picker Server Functions
#'
#' @noRd
mod_cat_name_picker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$breed_picker <-
      renderUI({
        avail_choices <- cat_names$breed
        breeds <- selectInput(inputId = "breed_picker",
                              label = "Breed",
                              choices = avail_choices,
                              selectize = FALSE)
        return(breeds)
      })

    output$color_picker <-
      renderUI({
        avail_choices <- cat_names$color
        colors <- selectInput(inputId = "color_picker",
                              label = "Color",
                              choices = avail_choices,
                              selectize = FALSE)
        return(colors)
      })

    chosen_name_reac <-
      eventReactive(eventExpr = input$get_name, valueExpr = {
        sel_color <- input$color_picker
        sel_breed <- input$breed_picker

        avail_names <-
          cat_names

        if (!is.null(sel_color)) {
          avail_names <-
            avail_names %>%
            dplyr::filter(color == sel_color)
        }

        if (!is.null(sel_breed)) {
          avail_names <-
            avail_names %>%
            dplyr::filter(breed == sel_breed)
        }

        chosen_name <-
          paste0(sample(x = avail_names$name, size = 1),
                 " is the best name!")

        return(chosen_name)
      })

    output$name_text <-
      renderText({
        req(chosen_name_reac())

        return(chosen_name_reac())
      })
  })
}

## To be copied in the UI
# mod_cat_name_picker_ui("cat_name_picker_ui_1")

## To be copied in the server
# mod_cat_name_picker_server("cat_name_picker_ui_1")
