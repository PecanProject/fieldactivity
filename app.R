## R Shiny App for Management Data Input 
## Field Observatory
# Otto Kuusela 2021

library(shiny)
library(jsonlite)
library(shinyjs) # shinyjs is used for e.g. disabling action buttons
library(shinymanager) # for user authentication
library(shinythemes) # change theme for login UI (and possibly rest of app)
#library(keyring) # for interacting with system credential store to store db key
library(DT) # fancier data table

#### AUTHENTICATION STUFF

# failsafe: ask for the db key only if we really want to. Has to be set by hand
set_db_key <- FALSE
# if the database encryption key is not found and we want to set the key,
# we ask the user to define it
#if (nrow(key_list("FO-mgmt-events-key")) == 0) {
#    # throw exception if there is no key and we don't want to define it
#    stopifnot(set_db_key)
#    
#    key_set("FO-mgmt-events-key", "FO-mgmt-events-user") 
#}

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

# missing value in the ICASA standard
missingval <- "-99.0"
date_format <- "%d/%m/%Y"

#### / AUTHENTICATION STUFF

# make helper functions available
source("display_name_helpers.R")
source("ui_builder.R")
source("json_file_helpers.R")

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
update_ui_element <- function(session, code_name, value, ...) {
    # find the element from the UI structure lookup list, which has been
    # generated in ui_builder.R 
    element <- structure_lookup_list[[code_name]]
    
    # didn't find the element corresponding to code_name
    # this should not happen if the element is in 
    # sidebar_ui_structure.json
    if (is.null(element$type)) return()

    # replace missingvals with empty strings
    if (is.na(value) || value == missingval) { value <- "" }
    
    if (element$type == "selectInput") {
        # if value is a list (e.g. multiple crops selected in harvest_crop)
        # turn it into a character vector
        if (is.list(value)) {
            print("List was turned to vector when updating selectInput")
            value <- value[[1]]
        }
        updateSelectInput(session, code_name, selected = value,  ...)
    } else if (element$type == "dateInput") {
        if (value == "") {
            formatted_date <- Sys.Date()
        } else {
            formatted_date <- as.Date(value, format = date_format)
        }
        updateDateInput(session, code_name, value = formatted_date, ...)
    } else if (element$type == "textAreaInput") {
        updateTextAreaInput(session, code_name, value = value,  ...)
    } else if (element$type == "checkboxInput") {
        updateCheckboxInput(session, code_name, value = value, ...)
    } else if (element$type == "actionButton") {
        updateActionButton(session, code_name, ...)
    } else if (element$type == "textInput") {
        updateTextInput(session, code_name, value = value, ...)
    } else if (element$type == "numericInput") {
        # if we are given a non-numeric value, we don't want to start converting
        # it. Let's replace it with an empty string (the default value)
        if (!is.numeric(value)) { value <- "" }
        updateNumericInput(session, code_name, value = value, ...)
    }
}

# sets the specified input fields to their default states
reset_input_fields <- function(session, input, fields_to_clear, 
                               exceptions = c("")) {
    
    # we never want to clear the site or block
    exceptions <- c(exceptions, "site", "block")
    
    for (code_name in fields_to_clear) {
        if (code_name %in% exceptions) next
        update_ui_element(session, code_name, value = "")
    }
    
    # if the frontpage_block selector is set to a specific block, mirror that
    # value in input$block. Otherwise don't change the block widget value
    if (input$frontpage_block != "block_choice_all") {
        update_ui_element(session, "block", input$frontpage_block)
    }
}

# takes a list of events and makes a data table with given variables in columns.
# also adds a column with the complete event list and a final column for
# ordering the list by date.
# The function doesn't replace code names with display names. That is done
# separately so that when the app language is switched, we can change the
# table display names without having to create it again.
get_data_table <- function(events, variable_names) {
    # initialise table
    display_data_table <- data.frame()
    for (variable_name in variable_names) {
        
        # get corresponding element and determine whether the column type should 
        # be list or character
        element <- structure_lookup_list[[variable_name]]
        if (!is.null(element$multiple)) {
            display_data_table[[variable_name]] <- list()
        } else {
            display_data_table[[variable_name]] <- character()
        }

    }
    # the event column will hold the complete event information as a list
    display_data_table$event <- list()
    # the date_ordering column will hold dates for ordering table
    display_data_table$date_ordering <- character()
    
    row_number <- 1
    for (event in events) {
        
        for (variable_name in variable_names) {
            value <- event[[variable_name]]
            if (is.null(value)) {
                value <- ""
            }
            display_data_table[[row_number, variable_name]] <- value
        }
        
        # double brackets allow saving a list nicely
        display_data_table[[row_number, "event"]] <- event
        
        display_data_table[row_number, "date_ordering"] <- as.Date(
            event$date, 
            format = date_format)
        
        row_number <- row_number + 1
    }
    return(display_data_table)
}

# loads data from all the json files corresponding to a site and stores it in
# separate data frames in session$userData$event_lists
load_json_data <- function(session, site1) {
    # clear possible previous data
    session$userData$event_lists <- list()
    
    # find all blocks on this site
    site_blocks <- subset(sites, site == site1)$blocks[[1]]
    
    # go through the blocks and save events from the corresponding json file
    # to session$userData$event_lists
    for (block in site_blocks) {
        session$userData$event_lists[[block]] <- 
            retrieve_json_info(site1, block)
    }
}

# find the index corresponding to the given event in the list of events.
# this is used when editing events
find_event_index <- function(event, event_list) {

    # sort the items in the lists to the same order (alphabetical)
    event <- event[order(names(event))]
    
    # go through all rows in the event list and check if any of them match
    for (i in 1:length(event_list)) {
        
        list_event <- event_list[[i]]
        list_event <- list_event[order(names(list_event))]
        
        if (identical(event, list_event)) {
            return(i)
        }

    }

    str(event_list)
    #stop("DID NOT FIND EVENT IN THE LIST")
    return(NULL)
}

# this function is used to update the various texts in the app into the correct
# language etc.
# TODO: incorporate into update_ui_element?
text_output_handler <- function(text_output_code_name, session, input, output) {
    text_to_show <- get_disp_name(text_output_code_name, input$language)
    
    # get element from the UI structure lookup list
    element <- structure_lookup_list[[text_output_code_name]]
    # if the text should be updated dynamically, do that
    if (!is.null(element$dynamic)) {
        
        # there are currently two modes of dynamic text
        if (element$dynamic$mode == "input") {
            # the -1 removes the mode element, we don't want it
            patterns <- names(element$dynamic)[-1]
            for (pattern in patterns) {
                replacement <- input[[ element$dynamic[[pattern]] ]]
                replacement <- get_disp_name(replacement, input$language)
                
                if (replacement == "") {
                    text_to_show <- ""
                    break
                }
                
                text_to_show <- gsub(pattern, replacement, text_to_show)
            }
            
        } else if (element$dynamic$mode == "session$userData$edit_mode") {
            
            if (session$userData$edit_mode) {
                text_to_show <- element$dynamic[["TRUE"]]
            } else {
                text_to_show <- element$dynamic[["FALSE"]]
            }
            text_to_show <- get_disp_name(text_to_show, input$language)
            
        }
    }
    
    # render text
    output[[text_output_code_name]] <- renderText(text_to_show)
}

# this function fills the editing table depending on the choice of block and
# activity.
update_editing_table <- function(session, input, output, block, activity, 
                                 render = TRUE) {

    editing_table_data <- NULL
    editing_table_variables <- c("date", "mgmt_event_notes")
    
    if (!isTruthy(block) | !isTruthy(activity)) {
        event_list <- list()
    } else {
        editing_table_variables <- 
            c(editing_table_variables, 
              unlist(rlapply(activity_options[[activity]], fun = function(x) {
                  if (is.null(x$type) || x$type == "textOutput") {
                      return(NULL)
                  } else {
                      return(x$code_name)
                  }
              })))
        
        # generate a list of events to display
        event_list <- session$userData$event_lists[[block]]
        # filter list to only show events of the given type
        event_list <- rlapply(event_list, fun = function(x) 
            if (x$mgmt_operations_event == activity) {x})
    }
    
    #print("Events to be displayed in the editing table:")
    #str(event_list)
    
    # turn event list into a table to display
    editing_table_data <- get_data_table(event_list, editing_table_variables)
    session$userData$displayed_editing_table_data <- editing_table_data
    
    #print("Table to be displayed:")
    #str(editing_table_data)
    
    if (render) {
        output$editing_table <- DT::renderDataTable({
            
            new_data_to_display <- replace_with_display_names(
                session$userData$displayed_editing_table_data, input$language
            )
            n_cols <- ncol(new_data_to_display)
            datatable(new_data_to_display, 
                      selection = "single", # allow selection of a single row
                      rownames = FALSE, # hide row numbers
                      colnames = get_disp_name(names(new_data_to_display),
                                                 language = input$language,
                                                 is_variable_name = TRUE),
                      options = list(dom = 'tp', # hide unnecessary controls
                                     # order chronologically by hidden column
                                     order = list(n_cols - 1, 'desc'), 
                                     columnDefs = list(
                                         # hide event and date_ordering columns
                                         list(visible = FALSE, targets = 
                                                  (n_cols - 2):(n_cols - 1)),
                                         # hide sorting arrows
                                         list(orderable = FALSE, targets = 
                                                  0:(n_cols - 2))),
                                     pageLength = 25
                      ))
        })
    } else {
        
        # if we know we don't have to render (e.g. when column don't change)
        # only updating the data in the table is sufficient
        new_data_to_display <- replace_with_display_names(
            editing_table_data, input$language
        )
        DTproxy <- DT::dataTableProxy("editing_table", session = session)
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
    }

    # update editing table title
    text_output_handler("editing_table_title", session, input, output)
}

# this function displays the latest data from session$userData$event_tables
# in the frontpage table and (TODO) in the editing table. If changed_blocks
# is not specified, tables will be updated independent of which blocks are 
# displayed (this happens during start up)
update_frontpage_table <- function(session, input, output, 
                                   changed_blocks = NULL, 
                                   clear_selection = "all") {
    # if the blocks to which changes have been made are not specified,
    # fill tables
    frontpage_table_data <- NULL
    frontpage_table_variables <- c("block", 
                                   "mgmt_operations_event", 
                                   "date", 
                                   "mgmt_event_notes")
  
    # generate the data to display on the front page table depending on
    # the farmer's choice
    if (input$frontpage_block == "block_choice_all") {
        event_list <- list()
        for (block_data in session$userData$event_lists) {
            event_list <- c(event_list, block_data)
        }
    } else {
        # if the changed block is not displayed, don't do anything
        if (!is.null(changed_blocks) && 
            !(input$frontpage_block %in% changed_blocks)) {
            return()
        }
        
        event_list <- session$userData$event_lists[[input$frontpage_block]]
    }
    
    # make event list into a table
    frontpage_table_data <- get_data_table(event_list, 
                                           frontpage_table_variables)
    
    # we know now that the displayed data has changed, so update userData
    session$userData$displayed_frontpage_table_data <- frontpage_table_data

    # update currently displayed data
    new_data_to_display <- replace_with_display_names(
        frontpage_table_data, input$language
    )
    DTproxy <- DT::dataTableProxy("mgmt_events_table", session = session)
    DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE, 
                    clearSelection = clear_selection)
    
    #update_editing_table(session, input, output, input$block, input$activity)
}

# exit edit mode
# this is called when saving and when pressing cancel
exit_sidebar_mode <- function(session, input) {
    # reset all input fields
    reset_input_fields(session, input, get_category_names("variable_name"))
    # hide sidebar
    shinyjs::hide("sidebar")
    shinyjs::enable("add_event")
    
    if (session$userData$edit_mode) {
        shinyjs::hide("delete")
        session$userData$edit_mode <- FALSE
        session$userData$event_to_edit <- NULL
        DT::selectRows(proxy = dataTableProxy("mgmt_events_table"), 
                       selected = NULL)
    }
    
    
}

row <- data.frame(eka = as.character(numericInput("moi", label = "muu", value = 3)), 
                  toka = as.character(selectInput("huhuu", label = "jaa",
                                                  choices = c("1", "2"))),
                  kolkki = as.character(textInput("mahtava", label = "joo")))

# Define UI for the application
# some of the UI (esp. additional options for activities) will be generated
# by create_ui in ui_builder.R
ui <- fluidPage(theme = shinytheme("lumen"),
    useShinyjs(),  # enable shinyjs
    
    selectInput("language", choices = languages, width = "120px", label = ""),
    
    # adding "" to the choices makes the default choice empty
    shinyjs::hidden(selectInput("site", label = "", 
                                choices = c("", sites$site))),
    
    # set web page title
    titlePanel("", windowTitle = "Field Observatory"),
    
    # title to be displayed on the page
    h1(textOutput("window_title")),
    
    # show instructions
    textOutput("frontpage_text"),
    
    h2(textOutput("frontpage_table_title")),
    
    selectInput("frontpage_block", label = "", choices = c("")),
    
    # front page data table
    DT::dataTableOutput("mgmt_events_table"),
    
    # add a little space between the elements
    br(),
    
    actionButton("add_event", label = ""), 
    shinyjs::disabled(actionButton("clone_event", label = "")),
    
    br(),
    br(),
    
    # create a sidebar layout
    shinyjs::hidden(div(id = "sidebar", sidebarLayout(
        # the sidebar contains the selectors for entering information
        # about the event
        sidebarPanel(
            
            h3(textOutput("sidebar_title"), 
               style = "margin-bottom = 0px; margin-top = 0px; 
               margin-block-start = 0px"),
            
            # in general the choices and labels don't have to be defined for
            # selectInputs, as they will be populated when the language is
            # changed (which also happens when the app starts)
            
            span(textOutput("required_variables_helptext"), 
                 style = "color:gray"),
            br(),
            
            selectInput("block", label = "", choices = ""),
            
            selectInput("mgmt_operations_event", label = "", choice = ""),
            
            # setting max disallows inputting future events
            dateInput(
                "date",
                format = "dd/mm/yyyy",
                label = "",
                max = Sys.Date(),
                value = Sys.Date()
            ),
            
            # show a detailed options panel for the different activities
            # activity_options is defined in ui_builder.R
            create_ui(activity_options, language = default_language, 
                      create_border = FALSE),
            
            textAreaInput(
                "mgmt_event_notes",
                label = "",
                placeholder = "",
                resize = "vertical"
            ),
            
            actionButton("save", label = "Save"),
            
            actionButton("cancel", label = "Cancel"),
            
            shinyjs::hidden(actionButton("delete", label = "Delete", 
                                         class = "btn-warning"))
        ),
        
        mainPanel(
            h2(textOutput("editing_table_title")),
            br(),
            # table for showing already supplied information
            DT::dataTableOutput("editing_table")
        )
    ))),
    

    
    conditionalPanel("input.harvest_crop.length > 1", DT::datatable(row, escape = FALSE)),
    
    #DT::dataTableOutput("test_table")
    #uiOutput("test_table")
)


# wrap the ui with the secure_app function which hides the app contents
# until login is successful
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
    
    observeEvent(input$huhuu, {
        showNotification(input$huhuu)
    })
    
    # go through all fields and set maxLength if requested in ui_structure.json
    for (element in structure_lookup_list) {
        if (!is.null(element$maxlength)) {
            js_message <- "$('##code_name').attr('maxlength', #maxlength)"
            js_message <- gsub("#code_name", element$code_name, js_message)
            js_message <- gsub("#maxlength", element$maxlength, js_message)
            #print(js_message)
            shinyjs::runjs(js_message)
        }
    }
    
    # initialise in the normal (non-edit) mode
    session$userData$edit_mode <- FALSE
    
    # check_credentials returns a function to authenticate users
    # might have to use the hand-typed passphrase option for now when deploying
    # to shinyapps.io
    credential_checker <- check_credentials(
        "data/database.sqlite",
        # passphrase = key_get("FO-mgmt-events-key", "FO-mgmt-events-user")
        passphrase = "salasana"
    )
    
    # change login form language when requested
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
            shinyjs::show("site")
        }

        # here would be good to somehow fetch the language selection from
        # login UI, but it's difficult
    })
    
    # enable editing of old entries
    # this runs only when some rows are selected, not when selection is cleared
    # (if you want that, set ignoreNULL = FALSE)
    observeEvent(input$mgmt_events_table_rows_selected, ignoreNULL = FALSE, {
        
        row_index <- input$mgmt_events_table_rows_selected
        
        if (is.null(row_index)) {
            shinyjs::disable("clone_event")
            return()
        } else {
            shinyjs::enable("clone_event")
        }
        
        # fetch the event data of the selected row
        selected_event_data <- 
            session$userData$displayed_frontpage_table_data[[row_index,"event"]]
        
        # populate the input controls with the values corresponding to the row
        for (variable_name in names(selected_event_data)) {
            # Try updating the UI elements corresponding to the variable names
            # to hold the data of the event. If no element is found 
            # corresponding to that name, update_ui_element does nothing
            update_ui_element(session, variable_name,
                              value = selected_event_data[[variable_name]])
        }
        
        # set edit mode on
        session$userData$edit_mode <- TRUE
        # save the event we want to edit so that it is preserved even if front
        # page table view is changed
        session$userData$event_to_edit <- selected_event_data
        # update sidebar title (either add or edit)
        text_output_handler("sidebar_title", session, input, output)
        shinyjs::show("delete")
        shinyjs::show("sidebar")
        # enable add event button in case we were adding an event
        shinyjs::enable("add_event")
    })
    
    # cancel means we exit edit mode and hide sidebar controls
    observeEvent(input$cancel, {
        exit_sidebar_mode(session, input)
    })
    
    # call the server part of shinymanager
    # weird observation: this has to be after the observeEvent block
    # which observes the auth_result$user. If it isn't the site selectInput
    # selection is not updated to match the username.
    auth_result <- secure_server(check_credentials = credential_checker)
    
    # when block changes, update table
    observeEvent(input$frontpage_block, {
        update_frontpage_table(session, input, output)
    })
    
    # show add event UI when requested
    observeEvent(input$add_event, {
        
        # clear all input fields
        reset_input_fields(session, input, get_category_names("variable_name"))
        shinyjs::disable("add_event")
        
        # exit edit mode if we were in it
        if (session$userData$edit_mode) {
            shinyjs::hide("delete")
            
            session$userData$edit_mode <- FALSE
            session$userData$event_to_edit <- NULL
        }
        
        # update sidebar title
        text_output_handler("sidebar_title", session, input, output)
        
        # show sidebar
        shinyjs::show("sidebar")
    })
    
    observeEvent(input$clone_event, {
        # fetch the event to be cloned
        event <- session$userData$event_to_edit
        
        block_data <- retrieve_json_info(input$site, event$block)
        
        block_data[[length(block_data) + 1]] <- event
        
        # save changes
        write_json_file(input$site, event$block, block_data)
        showNotification("Cloned successfully.", type = "message")
        
        # update session$userData$event_tables
        session$userData$event_lists[[event$block]] <- block_data
        
        # update tables if necessary
        current_row <- input$mgmt_events_table_rows_selected
        update_frontpage_table(session, input, output,
                               changed_blocks = event$block,
                               clear_selection = "none")
        update_editing_table(session, input, output, block = event$block,
                             activity = event$mgmt_operations_event, 
                             render = FALSE)
        
    })
    
    # save input to a file when save button is pressed
    # we are either creating a new event or editing an older one
    observeEvent(input$save, {
        
        # let's create a list to edit
        if (session$userData$edit_mode) {
            event <- session$userData$event_to_edit
        } else {
            event <- list()
        }
        orig_block <- event$block
        
        
        # if we are editing, find the index of the event in the original 
        # block data list. Also, if the block has been changed, update that 
        # file. If the block has not changed, we will need the index when
        # replacing the old event with the updated one.
        if (session$userData$edit_mode) {
            
            orig_block_data <- retrieve_json_info(input$site, orig_block)
            event_index <- 
                find_event_index(event, orig_block_data)

            if (is.null(event_index)) {
                showNotification("Could not edit entry.", type = "error")
                return()
            }
            
            if (!(orig_block == input$block)) {
                orig_block_data[event_index] <- NULL
                write_json_file(input$site, orig_block, orig_block_data)
                session$userData$event_lists[[orig_block]] <- orig_block_data
            }
            
        }

        # fill out current_event to match new / updated data.
        # find variables that correspond to the selected activity and save
        # only those
        relevant_variables <- unlist(rlapply(
            activity_options[[input$mgmt_operations_event]],
            fun = function(x) x$code_name))
        relevant_variables <- c("block", 
                                "mgmt_operations_event",
                                "date",
                                "mgmt_event_notes",
                                relevant_variables)
        
        # fill / update information
        for (variable_name in get_category_names("variable_name")) {
            
            # if this variable is not relevant, make sure it is not included
            # in the event data
            if (!(variable_name %in% relevant_variables)) {
                event[variable_name] <- NULL
                next
            }
                
            value_to_save <- input[[variable_name]]

            # if the value is not defined or empty, replace with missingval
            if (!isTruthy(value_to_save)) {
                value_to_save <- missingval
            }
            
            # format value to character string if it is a date
            if (class(value_to_save) == "Date") {
                value_to_save <- format(value_to_save, date_format)
            }
            
            # if value has multiple values (e.g. selectInput with possibility
            # of selecting multiple values), then make that into a list so that
            # it saves nicely into the event list
            # if we didn't do this, we'd get an error saying we have too many
            # replacement values
            if (length(value_to_save) > 1) {
                value_to_save <- list(value_to_save)
            }
            
            event[variable_name] <- value_to_save
        }
        
        #print("ALL THE INFO FILLED:")
        #str(event)
        
        # load the json file corresponding to the new block selection (new as in
        # the current input$block value). We load from the file because it might
        # have changed and session$userData$event_lists might be out of date
        
        new_block_data <- retrieve_json_info(input$site, input$block)
        
        # if editing and block didn't change, replace event. 
        # Otherwise append event to the list
        if (session$userData$edit_mode && orig_block == input$block) {
            new_block_data[[event_index]] <- event
        } else {
            new_block_data[[length(new_block_data) + 1]] <- event
        }
        
        # save changes
        write_json_file(input$site, input$block, new_block_data)
        showNotification("Saved successfully.", type = "message")
        
        # update session$userData$event_tables
        session$userData$event_lists[[input$block]] <- new_block_data
        
        # update tables if necessary
        update_frontpage_table(session, input, output,
                      changed_blocks = c(input$block, orig_block), 
                      clear_selection = "none")
        
        # exit sidebar mode
        exit_sidebar_mode(session, input)
    })
    
    # delete entry when delete button is pressed
    observeEvent(input$delete, {
        event <- session$userData$event_to_edit
        
        # retrieve up to date information from the json file
        block_data <- retrieve_json_info(input$site, event$block)
        
        # find the index of the event to be deleted from the event list
        event_index <- find_event_index(event, block_data)
        
        if (is.null(event_index)) {
            showNotification("Could not delete entry.", type = "error")
            return()
        }
        
        # delete
        block_data[event_index] <- NULL
        
        # write changes to json
        write_json_file(input$site, event$block, block_data)
        showNotification("Entry deleted.", type = "message")
        
        # update userData
        session$userData$event_lists[[event$block]] <- block_data
        
        # update tables if necessary
        update_frontpage_table(session, input, output, 
                               changed_blocks = event$block)
        
        # exit sidebar mode
        exit_sidebar_mode(session, input)
    })
    
    # disable the save button if not all necessary info has been filled
    observe({
        # run whenever any of the inputs change. I know this is not ideal, but
        # reactivity to input values doesn't work when we dynamically generate
        # which inputs we want to access
        reactiveValuesToList(input)

        req(auth_result$admin)
        
        if (auth_result$admin == "TRUE") {
            # if we are in admin mode, we don't care about required variables
            return()
        }
        
        for (required_variable in session$userData$required_variables) {
            # is.Truthy essentially checks whether input is empty or null
            if (!isTruthy(input[[required_variable]])) {
                shinyjs::disable("save")
                return()
            }
        }
        
        shinyjs::enable("save")
    })
    
    # change available blocks depending on the site and load the site event
    # data into memory (session$userData$event_lists)
    observeEvent(input$site, {

        if (is.null(input$site) | input$site == "") {
            shinyjs::disable("frontpage_block")
            shinyjs::disable("block")
        } else {
            shinyjs::enable("frontpage_block")
            shinyjs::enable("block")
            
            # update block choices.
            # frontpage_block choices are also updated in the observeEvent for
            # input$language to make the block_choice_all name translate
            block_choices <- subset(sites, site == input$site)$blocks[[1]]
            names_for_frontpage_selector <- c(
                get_disp_name("block_choice_all", input$language),
                block_choices)
            choices_for_frontpage_selector <- c("block_choice_all",
                                                block_choices)
            names(choices_for_frontpage_selector) <-
                names_for_frontpage_selector

            updateSelectInput(session, "frontpage_block",
                              choices = choices_for_frontpage_selector)
            updateSelectInput(session, "block", choices = block_choices)
            
            # load the events corresponding to this site into memory
            load_json_data(session, input$site)
        }
        
    })
    
    # change editing table when input$block is changed
    observeEvent(input$block, {
        update_editing_table(session, input, output, input$block, 
                             input$mgmt_operations_event, render = FALSE)
    })
    
    # change editing table and required variables when activity is changed
    observeEvent(input$mgmt_operations_event, {
        update_editing_table(session, input, output, input$block, 
                             input$mgmt_operations_event)
        
        required_checker <- function(element) {
            if (!is.null(element$required)) {
                return(element$code_name)
            } else {
                return(NULL)
            }
        }
        
        # find the variables that are compulsory for this activity type
        required_variables <- rlapply(
            activity_options[[input$mgmt_operations_event]],
            fun = required_checker)
        required_variables <- c(list("site", 
                                     "block", 
                                     "mgmt_operations_event",
                                     "date"),
                                required_variables)
        # save to userData. The inputs are compared against this list of
        # variables in an observe()
        session$userData$required_variables <- required_variables
    })
    
    # re-render frontpage table when input$language changes
    output$mgmt_events_table <- DT::renderDataTable({
        
        new_data_to_display <- replace_with_display_names(
            session$userData$displayed_frontpage_table_data, input$language)
        n_cols <- ncol(new_data_to_display)
        
        datatable(new_data_to_display, 
                  selection = "single", # allow selection of a single row
                  rownames = FALSE, # hide row numbers
                  colnames = get_disp_name(names(new_data_to_display),
                                             language = input$language,
                                             is_variable_name = TRUE),
                  options = list(dom = 'tp', # hide unnecessary controls
                                 # order chronologically by hidden column
                                 order = list(n_cols - 1, 'desc'), 
                                 columnDefs = list(
                                     # hide all other columns except
                                     # event, date and notes
                                     list(visible = FALSE, targets = 
                                              c(4:(n_cols - 1))),
                                     # hide sorting arrows
                                     list(orderable = FALSE, targets = 
                                              0:(n_cols - 3))),
                                 pageLength = 25
                  ))
    })
    
    # change language when user requests it
    # TODO: change frontpage_block choices' language
    observeEvent(input$language, {
        
        # we have to handle input and output elements in different ways
        
        # OUTPUT ELEMENTS:
        
        # change textOutputs when the language is changed
        # one has to use lapply here, for-loop does not work! See
        # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
        
        # function to render text outputs. Note the pattern matching which
        # is used for the editing table title (shown in text_output_handler)
        lapply(text_output_code_names, text_output_handler, session = session,
               input = input,
               output = output)
        
        # no need to update data tables, their updating is defined in
        # update_tables and they update reactively when language changes
        
        # INPUT ELEMENTS:
        
        # get a list of all input elements which we have to relabel
        input_element_names <- names(reactiveValuesToList(input))
        
        for (code_name in input_element_names) {
            
            # TODO: update to use the update_ui_element function
            
            # find element in the UI structure lookup list
            element <- structure_lookup_list[[code_name]]  
          
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
                current_value <- input[[code_name]]
                
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
            } else if (element$type == "textInput") {
                updateTextInput(session, 
                                code_name, 
                                label = get_disp_name(element$label, 
                                                      input$language),
                                placeholder = 
                                    get_disp_name(
                                        element$placeholder, 
                                        input$language))
            } else if (element$type == "numericInput") {
                updateNumericInput(session,
                                   code_name,
                                   label = get_disp_name(element$label, 
                                                         input$language))
            }

        }
        
        # update frontpage_block selector choices separately
        if (isTruthy(input$site)) {
            block_choices <- subset(sites, site == input$site)$blocks[[1]]
            names_for_frontpage_selector <- c(
                get_disp_name("block_choice_all", input$language), 
                block_choices)
            choices_for_frontpage_selector <- c("block_choice_all", 
                                                block_choices)
            names(choices_for_frontpage_selector) <-
                names_for_frontpage_selector
            
            updateSelectInput(session, "frontpage_block", 
                              choices = choices_for_frontpage_selector)
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)