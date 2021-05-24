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
            
            # setting max disallows inputting future events
            dateInput("date", format = "dd/mm/yyyy", 
                      label = "Select the date when the activity was performed:",
                      max = Sys.Date()),
            
            textAreaInput("notes", label = "Notes (optional):", 
                          placeholder = paste("Anything related to the event,",
                          "e.g. yield amount, seeding or tillage depth,", 
                          "products spread, machine type, etc."), 
                          resize = "vertical"),
            
            actionButton("submit", label = "Save")
        ),

        # the main panel will in the future enable the input of more precise data
        # as well as seeing previous submissions
        mainPanel(
            
            # text for testing purposes
            # textOutput("test_text"),
            
            # table for showing already supplied information
            dataTableOutput("mgmt_events_table")
            
        ),
    )
)


# Define server logic incl. save button action
server <- function(input, output) {
    
    # this is where we access the data to display in the data table.
    # initially NULL because reactive expressions aren't allowed here.
    # we store the data in a reactiveValues object so that if the data is 
    # updated when the button is clicked, the data table automatically updates
    tabledata <- reactiveValues(events = NULL)
    
    output$mgmt_events_table <- renderDataTable({
        
        # when input$site or input$block changes, update.
        # this is also updated if tabledata$events changes, which happens
        # when the submit button is pressed
        tabledata$events <- retrieve_json_info(input$site, input$block) 
        tabledata$events

    },
    # order by date in descending order 
    # we have dates for ordering in the 4th (hidden) column, so the index
    # is 3
    options = list(order = list(3, 'desc'), 
                   columnDefs = list(list(visible=FALSE, targets=c(3))))
    )
    
    # save input to a file
    observeEvent(input$submit, {
        
        # format the date to be displayed nicely (otherwise will use default
        # yyyy-mm-dd formatting of the Date object)
        formatted_date <- format(input$date, "%d/%m/%Y")
        
        # this saves the data to the json file.
        append_to_json_file(input$site, input$block, formatted_date,
                            input$activity, input$notes)
        
        # TODO: clear some of the fields?
        
        showNotification("Data saved!", type = "message")
        
        # set tabledata$events to NULL. This makes the renderDataTable 
        # expression run, which reads the latest data from the json file
        # and updates the table
        tabledata$events <- NULL
    })
    
    # the following is only for testing
    # output$test_text <- renderText({
    #     as.character(input$date)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
