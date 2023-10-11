#' rotation_cycle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rotation_cycle_ui <- function(id){
  ns <- NS(id)
  tagList(
    #plotOutput(ns("rotation_cycle"))
    verbatimTextOutput(ns("rotation_cycle"))
  )
}
    
#' rotation_cycle Server Functions
#'
#' @noRd 
#' 
#' @import ggplot2
mod_rotation_cycle_server <- function(id){ # site needs to be added at some point
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observeEvent(site, {
    #
    #
    # })

    # output$rotation_cycle <- renderPlot({
    #   ggplot(data = data.frame(a=1:10, b=seq(2,20, 2)), aes(x=a, y=b)) + geom_point()
    # })

    output$rotation_cycle <- renderText({
      result <- paste("Placeholder")
    })

  })
}
    
## To be copied in the UI
# mod_rotation_cycle_ui("rotation_cycle_1")
    
## To be copied in the server
# mod_rotation_cycle_server("rotation_cycle_1")
