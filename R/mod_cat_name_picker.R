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
    textOutput(outputId = ns("name_text")),
    plotOutput(outputId = ns("plot_image"))
  )
}

#' cat_name_picker Server Functions
#'
#' @noRd
mod_cat_name_picker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    breed_filtered_cat_names <-
      eventReactive(input$breed_picker, {
        sel_breed <- input$breed_picker

        avail_names <- cat_names

        if (!is.null(sel_breed) && sel_breed != "ALL") {
          avail_names <-
            avail_names %>%
            dplyr::filter(breed == sel_breed)
        }

        return(avail_names)
      })

    filtered_cat_names <-
      eventReactive(input$color_picker, {
        sel_color <- input$color_picker

        avail_names <-
          breed_filtered_cat_names()

        if (!is.null(sel_color) && sel_color != "ALL") {
          avail_names <-
            avail_names %>%
            dplyr::filter(color == sel_color)
        }

        return(avail_names)
      })

    output$breed_picker <-
      renderUI({
        avail_choices <- c("ALL", unique(cat_names$breed))
        breeds <- selectInput(inputId = ns("breed_picker"),
                              label = "Breed",
                              choices = avail_choices,
                              selectize = TRUE)
        return(breeds)
      })

    output$color_picker <-
      renderUI({
        req(breed_filtered_cat_names())

        avail_choices <- c("ALL", unique(breed_filtered_cat_names()$color))
        colors <- selectInput(inputId = ns("color_picker"),
                              label = "Color",
                              choices = avail_choices,
                              selectize = FALSE)
        return(colors)
      })

    chosen_name_reac <-
      eventReactive(eventExpr = input$get_name, valueExpr = {
        req(filtered_cat_names())

        filtered_cat_names <- filtered_cat_names()

        if (nrow(filtered_cat_names) == 0) { return("No cats like that!")}

        chosen_name <-
          paste0(sample(x = filtered_cat_names$name, size = 1),
                 " is the best name!")

        return(chosen_name)
      })

    output$name_text <-
      renderText({
        req(chosen_name_reac())

        return(chosen_name_reac())
      })

    output$plot_image <-
      renderImage({
        return(list(src = "inst/nyan.gif",
                    width = "100%"))
      })

  })
}
