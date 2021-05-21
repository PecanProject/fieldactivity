## R Shiny App for Management Data Input 
## Field Observatory
# Otto Kuusela 2021

library(shiny)
library(jsonlite)

# make helper functions available
source("json_file_helpers.R")

# read the csv file containing the sites 
sites <- read.csv(file = 'data/FOsites.csv')$site

# what are the possible activities that can be submitted?
possible_activities <- c("planting", "fertilizer", "irrigation", "tillage", 
                         "organic_material", "harvest", "bed_prep", 
                         "inorg_mulch", "Inorg_mul_rem", "chemicals", "mowing",
                         "observation", "weeding", "puddling", "flood_level", 
                         "other")

# Define UI for the application
ui <- fluidPage(

    # Application title
    titlePanel("Input Management Data"),

    # create a sidebar layout
    sidebarLayout(
        
        # the sidebar contains the selectors for the farm, activity type and date
        sidebarPanel(
            
            # adding "" to the choices makes the default choice empty
            selectInput("site", label = "Select the site:", 
                        choices = c("", sites)),
            
            selectInput("block", label = "Select the block:", 
                        choice = c("", "0", "1")),
            
            selectInput("activity", label = "Select the activity:", 
                        choices = c("", possible_activities)),
            
            dateInput("date", label = "Select the date when the activity was performed:"),
            
            textAreaInput("notes", label = "Notes (optional):", 
                          placeholder = paste("Anything related to the event,",
                          "e.g. yield amount, seeding or tillage depth,", 
                          "products spread, machine type, etc.", sep = ""), 
                          resize = "vertical"),
            
            actionButton("submit", label = "Save")
        ),

        # the main panel will in the future enable the input of more precise data
        # as well as seeing previous submissions
        mainPanel(
           
        ),
    )
)


# Define server logic incl. save button action
server <- function(input, output) {
    
    # save input to a file
    observeEvent(input$submit, {
        
        # TODO: fix weird date picker problem ("18977")
        
        append_to_json_file(input$site, input$block, input$date,
                            input$activity, input$notes)
        
        # TODO: clear some of the fields?
        
        showNotification("Data saved!", type = "message")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
