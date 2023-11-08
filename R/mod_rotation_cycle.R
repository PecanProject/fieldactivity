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
    # Header for rotation cycle
    h5(textOutput("rotation_cycle_title")),
    #plotOutput(ns("rotation_cycle"))
    verbatimTextOutput(ns("rotation_cycle"))
  )
}
    
#' rotation_cycle Server Functions
#'
#' @noRd 
#' 
#' @import ggplot2
mod_rotation_cycle_server <- function(id, rotation, site, block){ # site needs to be added at some point
  
  stopifnot(is.reactive(rotation))
  stopifnot(is.reactive(site))
  stopifnot(is.reactive(block))
  #stopifnot(is.reactive(rotation_status))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    
    rotation_status <- reactiveVal(FALSE)
    
    # observeEvent(site, {
    #
    #
    # })

    # output$rotation_cycle <- renderPlot({
    #   ggplot(data = data.frame(a=1:10, b=seq(2,20, 2)), aes(x=a, y=b)) + geom_point()
    # })

    #observeEvent( block(), {
      if (!isTruthy(site())) { return() }
      rotation <- read_json_file(site(), block())$rotation
      rotation_status <- ifelse(!is.null(rotation), TRUE, FALSE)
      print(rotation_status)
      print(block())

      if( length(rotation) != 0 ){
        output$rotation_cycle <- renderText({
          result <- paste("Rotation info")
        })
        # output$rotation_status <- renderPrint({
        #   rotation_in_place()
        # })
        
      } else {
        output$rotation_cycle <- renderText({
          result <- paste("Rotation information not added for this block")
        })
      }
    
    #})

    return(rotation_status)
    
  })
}
    
## To be copied in the UI
# mod_rotation_cycle_ui("rotation_cycle_1")
    
## To be copied in the server
# mod_rotation_cycle_server("rotation_cycle_1")
