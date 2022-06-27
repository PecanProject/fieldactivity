####


mod_download_ui <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("report"), label)
  )
}


#' download Server Function
#'
#' @noRd
mod_download_serverxx <- function(input, output, session){
  ns <- session$ns
  data_xi <- "string"
  
  output$report <- downloadHandler(
    
    filename = function(){
      paste("sitemxt", "csv", sep = ".")
    },
    
    content = function(file){
      write.csv(data_xi, file)
    }
  )
}



mod_download_server <- function(input, output, session) {
  ns <- session$ns

  output$report <- downloadHandler(
    filename = "instructions.html",
    content = function(file) {
      params <- list(n = input$n)

      id <- showNotification(
        "Rendering report...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      rmarkdown::render("./inst/user_doc/user_instructions.md",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}