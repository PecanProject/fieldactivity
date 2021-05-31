## R Shiny App for Management Data Input 
## Field Observatory
# Otto Kuusela 2021

library(shiny)
library(jsonlite)
library(shinyjs) # shinyjs is used for e.g. disabling action buttons
library(shinymanager) # for user authentication
library(shinythemes) # change theme for login UI (and possibly rest of app)
library(keyring) # for interacting with system credential store to store db key
library(DT) # fancier data table

#### AUTHENTICATION STUFF

# failsafe: ask for the db key only if we really want to. Has to be set by hand
set_db_key <- FALSE
# if the database encryption key is not found and we want to set the key,
# we ask the user to define it
if (nrow(key_list("FO-mgmt-events-key")) == 0) {
    # throw exception if there is no key and we don't want to define it
    stopifnot(set_db_key)
    
    key_set("FO-mgmt-events-key", "FO-mgmt-events-user") 
}

# define some credentials
#credentials <- data.frame(
#    user = c("shiny", "shinymanager"), # mandatory
#    password = c("azerty", "12345"), # mandatory
#    admin = c(FALSE, TRUE),
#    comment = "Simple and secure authentification mechanism 
#    for single ‘Shiny’ applications.",
#    stringsAsFactors = FALSE
#)

# Init the database
#create_db(
#    credentials_data = credentials,
#    sqlite_path = "data/database.sqlite", # will be created
#    passphrase = key_get("FO-mgmt-events-key", "FO-mgmt-events-user")
#    # passphrase = "passphrase_wihtout_keyring"
#)

#### / AUTHENTICATION STUFF

date_format <- "%d/%m/%Y"

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
# languages match the names of columns in display_names.csv
# when you give a named vector as the choices for selectInput, the names
# rather than the values will be displayed
languages <- c("English 🇬🇧" = "disp_name_eng",
               "suomi 🇫🇮" = "disp_name_fin")

# function for updating a UI element when its type is not determined
# TODO: move to an appropriate file
update_ui_element <- function(session, code_name, value, ...) {
    # find the element from the UI json file
    # rlapply, structure and code_name_checker are defined in
    # ui_builder.R
    # going through the structure like this for each element
    # is inefficient, but since the structure will never be very
    # large, it should not be an issue 
    # (and we are not reading the json file each time)
    element <- rlapply(
        structure,
        code_name_checker,
        code_name = code_name)
    
    # didn't find the element corresponding to code_name
    # this should not happen if the element is in 
    # sidebar_ui_structure.json
    if (is.null(element$type)) return()
    
    if (element$type == "selectInput") {
        updateSelectInput(session, code_name, selected = value,  ...)
    } else if (element$type == "dateInput") {
        updateDateInput(session, code_name, 
                        value = as.Date(value, format = date_format), ...)
    } else if (element$type == "textAreaInput") {
        updateTextAreaInput(session, code_name, value = value,  ...)
    } else if (element$type == "checkboxInput") {
        updateCheckboxInput(session, code_name, value = value, ...)
    } else if (element$type == "actionButton") {
        updateActionButton(session, code_name, ...)
    }
}

# element is the list returned by rlapply
get_selectInput_choices <- function(code_name, element) {
    selector_choices <- NULL
    
    if (length(element$choices) > 1) {
        selector_choices <- c("", element$choices)
        names(selector_choices) <- c("", get_disp_name(
            element$choices,
            language = input$language))
        
    } else if (element$choices == "IGNORE") {
        selector_choices <- NULL
    } else {
        # get_category_names returns both display names and 
        # code names
        selector_choices <- c(
            "",
            get_category_names(element$choices,
                               language = input$language)
        )
    }
}

# Define UI for the application
# some of the UI (esp. additional options for activities) will be generated
# by create_ui in ui_builder.R
ui <- fluidPage(theme = shinytheme("lumen"),
    useShinyjs(),  # enable shinyjs
    
    selectInput("language", choices = languages, width = "120px", label = ""),
    
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
            
            # in general the choices don't have to be defined for selectInputs,
            # as they will be populated when the language is changed (which
            # also happens when the app starts)
            
            selectInput("block", label = "Select the block:",
                        choice = c("")),
            
            selectInput(
                "mgmt_operations_event",
                label = "Select the activity (hint: choose planting):",
                choices = c("")
            ),
            
            # show a detailed options panel for the different activities
            # activity_options is defined in ui_builder.R
            create_ui(activity_options, language = default_language, 
                      create_border = FALSE),
            
            # setting max disallows inputting future events
            dateInput(
                "mgmt_event_date",
                format = "dd/mm/yyyy",
                label = "Select the date when the activity was performed:",
                max = Sys.Date(),
                value = Sys.Date()
            ),
            
            textAreaInput(
                "mgmt_event_notes",
                label = "Notes (optional):",
                placeholder = "",
                resize = "vertical"
            ),
            
            actionButton("save", label = "Save")
        ),
        
        mainPanel(
            
            # table for showing already supplied information
            DT::dataTableOutput("mgmt_events_table")
            
        )
    )
)

# Wrap your UI with secure_app
ui <- secure_app(ui, 
                 # language selector for login page
                 tags_bottom = selectInput("login_language", 
                                           label = "" , 
                                           choices = languages),
                 tags_top = tagList(
                     p("EXAMPLE USER site: ruukki, password: Ruukki1"),
                     p("ADMIN site: shinymanager, password: 12345")),
                 theme = shinytheme("lumen"),
                 enable_admin = TRUE)

# Define server logic incl. save button action
server <- function(input, output, session) {

    # initialise in the normal (non-edit) mode
    session$userData$edit_mode = FALSE
    
    # check_credentials returns a function to authenticate users
    # might have to use the hand-typed passphrase option for now when deploying
    # to shinyapps.io
    credential_checker <- check_credentials(
        "data/database.sqlite",
        passphrase = key_get("FO-mgmt-events-key", "FO-mgmt-events-user")
        # passphrase = "salasana"
    )
    
    observeEvent(input$login_language, {

        # yes we are overwriting the English language. This is by far
        # the simplest method
        
        if (input$login_language == "disp_name_fin") {
            set_labels(
                language = "en",
                "Please authenticate" = "Kirjaudu syöttääksesi tapahtumia",
                "Username:" = "Sijainti",
                "Password:" = "Salasana",
                "Login" = "Kirjaudu",
                "Logout" = "Kirjaudu ulos"
            )
        } else if (input$login_language == "disp_name_eng") {
            set_labels(
                language = "en",
                "Please authenticate" = "Log in to enter management events",
                "Username:" = "Site",
                "Password:" = "Password",
                "Login" = "Login",
                "Logout" = "Logout"
            )
        }
        
        # TODO: make the language setting communicate to main app
        # PROBLEM: session userData doesn't seem to be saved after
        # logging in is complete
        
        # change the value of the session-specific variable default_language to
        # match the login language selector
        # session$userData$default_language <- input$login_language
        
        # showNotification(paste("From input$login_language", session$userData$default_language))
        
        # this seems to refresh the authentication UI
        auth_result <- secure_server(check_credentials = credential_checker)
        
    })
    
    # runs when logged in
    observeEvent(auth_result$user, {
        
        if (auth_result$admin == "FALSE") {
            updateSelectInput(session, "site", selected = auth_result$user)
            shinyjs::disable("site")
        } else {
            shinyjs::enable("site")
        }

        # here would be good to somehow fetch the language selection from
        # login UI, but it's difficult
        
    })
    
    # enable editing of old entries
    # TODO: don't clear fields if row selected and language changed
    observe({
        
        row_index <- input$mgmt_events_table_rows_selected
        
        if (is.null(row_index)) {
            
            if (session$userData$edit_mode) {
                # no rows selected anymore, 
                # so clear the fields and set edit mode off
                print("Clearing fields")
                session$userData$edit_mode = FALSE
            }
            
            return()
        } 
        
        # we have selected a row. Let's fetch the table with code names
        data_with_code_names <- retrieve_json_info(input$site, 
                                                   input$block,
                                                   language = NULL)
        selected_data <- data_with_code_names[row_index,]
        
        # populate the input controls with the values corresponding to the row
        
        for (col_name in names(selected_data)) {
            
            # try updating the element corresponding to column name with the
            # value in that column. If no element is found corresponding to 
            # that name, update_ui_element does nothing
            update_ui_element(session, col_name,
                              value = selected_data[1, col_name])
            
        }
        
        # set edit mode on
        session$userData$edit_mode = TRUE
    })
    
    # call the server part of shinymanager
    # weird observation: this has to be after the previous observeEvent block
    # which observes the auth_result$user. If it isn't the site selectInput
    # selection is not updated to match the username.
    auth_result <- secure_server(check_credentials = credential_checker)
    
    # this is where we access the data to display in the data table.
    # initially NULL because reactive expressions aren't allowed here.
    # we store the data in a reactiveValues object so that if the data is 
    # updated when the button is clicked, the data table automatically updates
    tabledata <- reactiveValues(events = NULL)
    
    output$mgmt_events_table <- DT::renderDataTable({
        # when input$site, input$block or input$language changes, update.
        # this is also updated if tabledata$events changes, which happens
        # when the save button is pressed
        tabledata$events <- retrieve_json_info(input$site,
                                               input$block,
                                               input$language)
        n_cols <- ncol(tabledata$events)
        datatable(tabledata$events, selection = "single", 
                  rownames = FALSE, # hide row numbers
                  colnames = c(names(
                      get_category_names("table_col_name",
                                         language = input$language)), 
                      "date_ordering"),
                  options = list(dom = 't', # hide unnecessary controls
                                 # TODO: check whether a long list is entirely
                                 # visible
                                 # order chronologically by hidden column
                                 # autoWidth = TRUE,
                                 order = list(n_cols - 1, 'desc'), 
                                 columnDefs = list(
                                     # hide date_ordering column
                                     list(visible = FALSE, targets = 
                                              c(n_cols - 1)),
                                     # hide sorting arrows
                                     list(orderable = FALSE, targets = 
                                              0:(n_cols - 2)))
                                 ))
        
    })
    
    # save input to a file when save button is pressed
    observeEvent(input$save, {
        
        # format the date to be displayed nicely (otherwise will use default
        # yyyy-mm-dd formatting of the Date object)
        formatted_date <- format(input$date, date_format)
        
        # this saves the data to the json file.
        append_to_json_file(input$site, input$block, formatted_date,
                            input$mgmt_operations_event, input$mgmt_event_notes)
        
        # clear the selected activity and notes
        updateSelectInput(session, "mgmt_operations_event", selected = "")
        updateTextAreaInput(session, "mgmt_event_notes", value = "")
        
        showNotification("Data saved!", type = "message")
        
        # set tabledata$events to NULL. This makes the renderDataTable 
        # expression run, which reads the latest data from the json file
        # and updates the table
        tabledata$events <- NULL
    })
    
    # disable the save button if not all necessary info has been filled
    observe({

        # is.Truthy essentially checks whether input$site is empty or null        
        if (!isTruthy(input$site) | !isTruthy(input$block) | 
            !isTruthy(input$mgmt_operations_event)) {
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
        
        # we have to handle input and output elements in different ways
        
        # OUTPUT ELEMENTS:
        
        # change textOutputs when the language is changed
        # one has to use lapply here, for-loop does not work! See
        # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
        lapply(text_output_code_names, function(text_output_code_name) {
            output[[text_output_code_name]] <- 
                renderText(get_disp_name(text_output_code_name, input$language))
        })
        
        # INPUT ELEMENTS:
        
        # get a list of all input elements which we have to relabel
        input_element_names <- names(isolate(reactiveValuesToList(input)))
        
        for (code_name in input_element_names) {
            
            # TODO: update to use the update_ui_element function
            
            # rlapply, structure and code_name_checker are defined in
            # ui_builder.R
            # going through the structure like this for each element
            # is inefficient, but since the structure will never be very
            # large, it should not be an issue 
            # (and we are not reading the json file each time)
            element <- rlapply(
                structure,
                code_name_checker,
                code_name = code_name)
            
            # didn't find the element corresponding to code_name
            # this should not happen if the element is in 
            # sidebar_ui_structure.json
            if (is.null(element$type)) next
            
            if (element$type == "selectInput") {
                
                # the code name for the label is stored in label
                label_code_name <- element$label
                
                # the choices for a selectInput element can be stored in
                # three ways: 
                # 1) the code names of the choices are given as a vector
                # 2) for site and block selectors, there is IGNORE:
                # this means that the choices should not be updated here
                # 3) the category name for the choices is given.
                # in the following if-statement, these are handled
                # in this same order
                selector_choices <- NULL
                
                if (length(element$choices) > 1) {
                    selector_choices <- c("", element$choices)
                    names(selector_choices) <- c("", get_disp_name(
                        element$choices,
                        language = input$language))
                    
                } else if (element$choices == "IGNORE") {
                    selector_choices <- NULL
                } else {
                    # get_category_names returns both display names and 
                    # code names
                    selector_choices <- c(
                        "",
                        get_category_names(element$choices,
                                                   language = input$language)
                    )
                }
                
                # make sure we don't change the selected value
                current_value <- isolate(input[[code_name]])
                
                if (is.null(selector_choices)) {
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
            } else if (element$type == "textAreaInput") {
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