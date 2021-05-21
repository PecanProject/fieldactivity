## R Shiny App for Management Data Input 
## Field Observatory

library(shiny)

# read the csv file containing the sites 
sites <- read.csv(file = 'data/FOsites.csv')$site

# what are the possible activities that can be submitted?
possible_activities <- c("planting", "fertilizer", "irrigation", "tillage", "organic_material", "harvest", "bed_prep", "inorg_mulch", "Inorg_mul_rem", "chemicals", "mowing", "observation", "weeding", "puddling", "flood_level", "other")

# Define UI for the application
ui <- fluidPage(

    # Application title
    titlePanel("Input Management Data"),

    # create a sidebar layout
    sidebarLayout(
        
        # the sidebar contains the selectors for the farm, activity type and date
        sidebarPanel(
            
            # adding "" to the choices makes the default choice empty
            selectInput("site", label = "Select the site:", choices = c("", sites)),
            
            selectInput("block", label = "Select the block:", choice = c("", "0", "1")),
            
            selectInput("activity", label = "Select the activity:", choices = c("", possible_activities)),
            
            dateInput("date", label = "Select the date when the activity was performed:"),
            
            textAreaInput("notes", label = "Notes (optional):", placeholder = "Anything related to the event, e.g. yield amount, seeding or tillage depth, products spread, machine type, etc."),
            
            submitButton(text = "Save")
        ),

        # the main panel will in the future contain the options to input more precise data
        mainPanel(
           
        ),
    )
)


# Define server logic TODO
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
