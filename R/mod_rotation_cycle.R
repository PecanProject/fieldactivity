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
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (dp()) message("Check the crop rotation options")
    rotation_status <- reactiveVal(FALSE)

      if (!isTruthy(site())) { return() }
      rotation <- read_json_file(site(), block())$rotation
      # Rotation status based on if there is rotation information on json -file
      rotation_status <- ifelse(length(rotation) != 0, TRUE, FALSE)

      if( length(rotation) != 0 ){
        output$rotation_cycle <- renderText({
          result <- paste("Rotation info")
        })
        
      } else {
        output$rotation_cycle <- renderText({
          result <- paste("Rotation information not added for this block")
        })
        if (dp()) message("Crop rotation information not found from this site and block")
      }
    
    # Return true/false value
    return(rotation_status)
    
  })
}