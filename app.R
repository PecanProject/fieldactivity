## R Shiny App for Management Data Input 
## Field Observatory
# Otto Kuusela 2021

library(shiny)
library(jsonlite)
library(shinyjs) # shinyjs is used for e.g. disabling action buttons

# make helper functions available
source("display_name_helpers.R")
source("json_file_helpers.R")
source("ui_builder.R")

# read the csv file containing the sites 
sites_file_path <- "data/FOsites.csv"
sites <- read.csv(sites_file_path)

# converts block info from csv (e.g. "[0;1]") to vectors of strings ("0" "1")
blocks_to_vector <- function(x) strsplit(substr(x, start = 2, stop = nchar(x)-1), ";")
sites$blocks <- sapply(sites$blocks, blocks_to_vector)

# options for UI languages
# language_codes match the name of columns in display_names.csv
# when you give a named vector as the choices for selectInput, the names
# rather than the values will be displayed
languages <- c("English 🇬🇧", "Finnish 🇫🇮")
language_codes <- c("disp_name_eng", "disp_name_fin")
names(language_codes) <- languages

# Define UI for the application
# some of the UI (esp. additional options for activities) will be generated
# by create_ui in activit
ui <- fluidPage(
    useShinyjs(),  # enable shinyjs
    
    selectInput("language", choices = language_codes, label = "", width = "120px"),
    
    # set web page title
    titlePanel("", windowTitle = "Field Observatory"),
    
    # title to be displayed on the page
    h1(textOutput("window_title")),
    
    # create a sidebar layout
    sidebarLayout(
        # the sidebar contains the selectors for the farm, activity type and date
        sidebarPanel(
            # adding "" to the choices makes the default choice empty
            selectInput(
                "site",
                label = "Select the site:",
                choices = c("", sites$site)
            ),
            
            selectInput("block", label = "Select the block:",
                        choice = c("", "0", "1")),
            
            selectInput(
                "activity",
                label = "Select the activity (hint: choose planting):",
                choices = c(
                    "",
                    get_category_names("activity_choice", language_codes[1])
                )
            ),
            
            # show a detailed options panel for the different activities
            # activity_options is defined in ui_builder.R
            create_ui(activity_options, language = language_codes[1], 
                      create_border = FALSE),
            
            # setting max disallows inputting future events
            dateInput(
                "date",
                format = "dd/mm/yyyy",
                label = "Select the date when the activity was performed:",
                max = Sys.Date(),
                value = Sys.Date()
            ),
            
            
            textAreaInput(
                "notes",
                label = "Notes (optional):",
                placeholder = paste(
                    "Anything related to the event,",
                    "e.g. yield amount, seeding or tillage depth,",
                    "products spread, machine type, etc."
                ),
                resize = "vertical"
            ),
            
            actionButton("save", label = "Save")
        ),
        
        mainPanel(
            
            # table for showing already supplied information
            dataTableOutput("mgmt_events_table")
            
        )
    )
)


# Define server logic incl. save button action
server <- function(input, output, session) {
    
    # this is where we access the data to display in the data table.
    # initially NULL because reactive expressions aren't allowed here.
    # we store the data in a reactiveValues object so that if the data is 
    # updated when the button is clicked, the data table automatically updates
    tabledata <- reactiveValues(events = NULL)
    
    output$mgmt_events_table <- renderDataTable({
        # when input$site or input$block changes, update.
        # this is also updated if tabledata$events changes, which happens
        # when the save button is pressed
        tabledata$events <-
            retrieve_json_info(input$site, input$block, input$language)
        tabledata$events
        
    },
    # order by date in descending order
    # we have dates for ordering in the 4th (hidden) column, so the index
    # is 3
    options = list(order = list(3, 'desc'),
                   columnDefs = list(list(
                       visible = FALSE, targets = c(3)
                   ))))
    
    # save input to a file when save button is pressed
    observeEvent(input$save, {
        
        # format the date to be displayed nicely (otherwise will use default
        # yyyy-mm-dd formatting of the Date object)
        formatted_date <- format(input$date, "%d/%m/%Y")
        
        # this saves the data to the json file.
        append_to_json_file(input$site, input$block, formatted_date,
                            input$activity, input$notes)
        
        # clear the selected activity and notes
        updateSelectInput(session, "activity", selected = "")
        updateTextAreaInput(session, "notes", value = "")
        
        showNotification("Data saved!", type = "message")
        
        # set tabledata$events to NULL. This makes the renderDataTable 
        # expression run, which reads the latest data from the json file
        # and updates the table
        tabledata$events <- NULL
    })
    
    # disable the save button if not all necessary info has been filled
    observe({
        
        # if the UI has not been initialised yet, skip
        #if (is.null(input$site)) {
        #    return()
        #}
        
        if (input$site == "" | input$block == "" | input$activity == "") {
            shinyjs::disable("save")
        } else {
            shinyjs::enable("save")
        }
    })
    
    # change available blocks depending on the site
    observeEvent(input$site, {
        if (is.null(input$site) | input$site == "") {
            shinyjs::disable("block")
        } else {
            shinyjs::enable("block")
            block_choices <- subset(sites, site == input$site)$blocks[[1]]
            updateSelectInput(session, 
                              "block", 
                              choices = c("", block_choices))
        }
    })
    

    
    # change language when user requests it
    observeEvent(input$language, {
        
        # change textOutputs when the language is changed
        # one has to use lapply here, for loop does not work! See
        # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
        lapply(text_output_code_names, function(text_output_code_name) {
            output[[text_output_code_name]] <- 
                renderText(get_disp_name(text_output_code_name, input$language))
        })
        
        # get a list of all input elements which we have to relabel
        input_element_names <- names(isolate(reactiveValuesToList(input)))
        
        for (code_name in input_element_names) {
            
            # rlapply, structure and code_name_checker are defined in
            # activity_option_builder.R
            # going through the structure like this for each element
            # is inefficient, but since the structure will never be very
            # large, it should not be an issue
            element <- rlapply(
                structure,
                code_name_checker,
                code_name = code_name)
            
            if (is.null(element$type)) next
            
            if (element$type == "selectInput") {
                
                # the code name for the label is stored in label
                label_code_name <- element$label
                
                # the choices for a selectInput element can be stored in
                # three ways: 
                # 1) the code names of the choices are given as a vector
                # 2) for site and block selectors, there is IGNORE:
                # this means that the choices should not be updated here
                # 3) the category name for the choices is given
                # in the following if-statement, these are handled
                # in this same order
                selector_choices <- NULL
                ignore_choices <- FALSE
                
                if (length(element$choices) > 1) {
                    selector_choices <- c("", element$choices)
                    names(selector_choices) <- c("", get_disp_name(
                        element$choices,
                        language = input$language))
                    
                } else if (element$choices == "IGNORE") {
                    ignore_choices = TRUE
                } else {
                    # get_category_names returns both display names and 
                    # code names
                    selector_choices <- c(
                        "",
                        get_category_names(element$choices,
                                                   language = input$language)
                    )
                }
                
                
                current_value <- isolate(input[[code_name]])
                
                if (ignore_choices) {
                    updateSelectInput(session, 
                                      code_name,
                                      label = get_disp_name(
                                          label_code_name, input$language),
                                      selected = current_value) 
                } else {
                    updateSelectInput(session, 
                                      code_name,
                                      label = get_disp_name(
                                          label_code_name, input$language),
                                      choices = selector_choices,
                                      selected = current_value)
                }
                
                
            } else if (element$type == "dateInput") {
                updateDateInput(session, 
                                code_name, 
                                label = get_disp_name(element$label,
                                                      input$language))
            } else if (element$type == "textArea") {
                updateTextAreaInput(session,
                                    code_name,
                                    label = get_disp_name(element$label,
                                                          input$language),
                                    placeholder = 
                                        get_disp_name(
                                            element$placeholder, 
                                            input$language))
            } else if (element$type == "actionButton") {
                updateActionButton(session,
                                   code_name,
                                   label = get_disp_name(element$label,
                                                         input$language))
            } else if (element$type == "checkboxInput") {
                updateCheckboxInput(session,
                                    code_name,
                                    label = get_disp_name(element$label,
                                                          input$language))
            }

        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)