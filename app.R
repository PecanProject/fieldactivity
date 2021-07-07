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
library(glue) # used for debug printing

#### AUTHENTICATION STUFF

# developer mode. If TRUE, logging in is disabled
dev_mode <- TRUE

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
date_format_json <- "%Y-%m-%d"

#### / AUTHENTICATION STUFF

# make helper functions and modules available
source("display_name_helpers.R")
source("table.R")
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
            formatted_date <- as.Date(value, format = date_format_json)
        }
        updateDateInput(session, code_name, value = formatted_date,...)
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

# sets the specified input fields to their default states.
# this doesn't reset the tables (e.g. harvest_crop_table) -- they reset 
# themselves every time they become hidden
# TODO: make sure this is always used
reset_input_fields <- function(session, input, fields_to_clear, 
                               exceptions = c("")) {
    
    # we never want to clear the site or block
    exceptions <- c(exceptions, "site", "block")
    
    for (code_name in fields_to_clear) {
        if (code_name %in% exceptions) next
        update_ui_element(session, code_name, value = "")
    }
    
    # if the table_block selector is set to a specific block, mirror that
    # value in input$block. Otherwise don't change the block widget value
    if (!is.null(input$table_block) &&
        input$table_block != "block_choice_all") {
        update_ui_element(session, "block", input$table_block)
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
        #if (!is.null(element$multiple)) {
            display_data_table[[variable_name]] <- list()
        #} else {
        #    display_data_table[[variable_name]] <- character()
        #}

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
            # if value is a vector, it will be turned into a single string
            # when the table is converted to a table with display names
        }
        
        # double brackets allow saving a list nicely
        display_data_table[[row_number, "event"]] <- event
        display_data_table[row_number, "date_ordering"] <- 
            as.Date(event$date, format = date_format_json)
        
        row_number <- row_number + 1
    }
    return(display_data_table)
}

# find the (first) index corresponding to the given event in the list of events.
# This is used when editing events
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

    # We didn't find a match, so return NULL
    return(NULL)
}

# if a variable is in a table (e.g. planting_depth is in a table when 
# planted_crop has multiple values), return the code name of that table. 
# Otherwise return NULL.
# only_rows determines whether we seek the variable from the rows only, i.e.
# get_variable_table("harvest_crop", only_rows = TRUE) returns 
# "harvest_crop_table" and 
# get_variable_table("harvest_cut_height", only_rows = TRUE) returns NULL
get_variable_table <- function(variable_name, only_rows = FALSE) {
    
    for (table_code_name in data_table_code_names) {
        table <- structure_lookup_list[[table_code_name]]
        
        names_to_check <- table$rows
        if (!only_rows) {
            names_to_check <- c(names_to_check, table$columns)
        }
        
        if (variable_name %in% names_to_check) {
            return(table_code_name)
        }
    }

    return(NULL)
}

# takes a condition written in javascript notation (visibility conditions
# in ui_structure.json) and evaluates it in R.
# Might not be best coding practice, but works as long as the js_condition
# doesn't have any typos.
# TODO: add error catching
evaluate_condition <- function(js_condition) {
    # substitute dots with dollar signs (fixed = TRUE means we don't use regex)
    condition <- gsub(".", "$", js_condition, fixed = TRUE)
    # parse string into an expression and evaluate it
    eval(parse(text = condition))
}

# this function is used to update the various texts in the app into the correct
# language etc.
# TODO: incorporate into update_ui_element?
# text_output_handler <- function(text_output_code_name, session, input, output) {
#     text_to_show <- get_disp_name(text_output_code_name, input$language)
#     
#     # get element from the UI structure lookup list
#     element <- structure_lookup_list[[text_output_code_name]]
#     # if the text should be updated dynamically, do that
#     if (!is.null(element$dynamic)) {
#         
#         # there are currently two modes of dynamic text
#         if (element$dynamic$mode == "input") {
#             # the -1 removes the mode element, we don't want it
#             patterns <- names(element$dynamic)[-1]
#             for (pattern in patterns) {
#                 replacement <- input[[ element$dynamic[[pattern]] ]]
#                 replacement <- get_disp_name(replacement, input$language)
#                 
#                 if (replacement == "") {
#                     text_to_show <- ""
#                     break
#                 }
#                 
#                 text_to_show <- gsub(pattern, replacement, text_to_show)
#             }
#             
#         } else if (element$dynamic$mode == "session$userData$edit_mode") {
#             
#             if (session$userData$edit_mode) {
#                 text_to_show <- element$dynamic[["TRUE"]]
#             } else {
#                 text_to_show <- element$dynamic[["FALSE"]]
#             }
#             text_to_show <- get_disp_name(text_to_show, input$language)
#             
#         }
#     }
#     
#     # render text
#     #output[[text_output_code_name]] <- renderText(text_to_show)
# }

# this function fills the editing table depending on the choice of block and
# activity.
# update_editing_table <- function(session, input, output, block, activity, 
#                                  render = TRUE) {
# 
#     editing_table_data <- NULL
#     editing_table_variables <- c("date", "mgmt_event_notes")
#     
#     if (!isTruthy(block) | !isTruthy(activity)) {
#         event_list <- list()
#     } else {
#         editing_table_variables <- 
#             c(editing_table_variables, 
#               unlist(rlapply(activity_options[[activity]], fun = function(x) {
#                   if (is.null(x$type) || x$type == "textOutput") {
#                       return(NULL)
#                   } else {
#                       return(x$code_name)
#                   }
#               })))
#         
#         # generate a list of events to display
#         event_list <- events$by_block[[block]]
#         # filter list to only show events of the given type
#         event_list <- rlapply(event_list, fun = function(x) 
#             if (x$mgmt_operations_event == activity) {x})
#     }
#     
#     #print("Events to be displayed in the editing table:")
#     #str(event_list)
#     
#     # turn event list into a table to display
#     editing_table_data <- get_data_table(event_list, editing_table_variables)
#     session$userData$displayed_editing_table_data <- editing_table_data
#     
#     #print("Table to be displayed:")
#     #str(editing_table_data)
#     
#     if (render) {
#         output$editing_table <- DT::renderDataTable({
#             
#             new_data_to_display <- replace_with_display_names(
#                 session$userData$displayed_editing_table_data, input$language
#             )
#             n_cols <- ncol(new_data_to_display)
#             datatable(new_data_to_display, 
#                       selection = "single", # allow selection of a single row
#                       rownames = FALSE, # hide row numbers
#                       colnames = get_disp_name(names(new_data_to_display),
#                                                  language = input$language,
#                                                  is_variable_name = TRUE),
#                       options = list(dom = 'tp', # hide unnecessary controls
#                                      # order chronologically by hidden column
#                                      order = list(n_cols - 1, 'desc'), 
#                                      columnDefs = list(
#                                          # hide event and date_ordering columns
#                                          list(visible = FALSE, targets = 
#                                                   (n_cols - 2):(n_cols - 1)),
#                                          # hide sorting arrows
#                                          list(orderable = FALSE, targets = 
#                                                   0:(n_cols - 2))),
#                                      pageLength = 25
#                       ))
#         })
#     } else {
#         
#         # if we know we don't have to render (e.g. when column don't change)
#         # only updating the data in the table is sufficient
#         new_data_to_display <- replace_with_display_names(
#             editing_table_data, input$language
#         )
#         DTproxy <- DT::dataTableProxy("editing_table", session = session)
#         DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
#     }
# 
# }

# this function displays the latest data from session$userData$event_tables
# in the frontpage table and (TODO) in the editing table. If changed_blocks
# is not specified, tables will be updated independent of which blocks are 
# displayed (this happens during start up)
# update_frontpage_table <- function(session, input, output, 
#                                    changed_blocks = NULL, 
#                                    clear_selection = "all") {
#     # if the blocks to which changes have been made are not specified,
#     # fill tables
#     frontpage_table_data <- NULL
#     frontpage_table_variables <- c("block", 
#                                    "mgmt_operations_event", 
#                                    "date", 
#                                    "mgmt_event_notes")
#   
#     # generate the data to display on the front page table depending on
#     # the farmer's choice
#     if (input$frontpage_block == "block_choice_all") {
#         event_list <- list()
#         for (block_data in events$by_block) {
#             event_list <- c(event_list, block_data)
#         }
#     } else {
#         # if the changed block is not displayed, don't do anything
#         if (!is.null(changed_blocks) && 
#             !(input$frontpage_block %in% changed_blocks)) {
#             return()
#         }
#         
#         event_list <- events$by_block[[input$frontpage_block]]
#     }
#     
#     # make event list into a table
#     frontpage_table_data <- get_data_table(event_list, 
#                                            frontpage_table_variables)
#     
#     # we know now that the displayed data has changed, so update userData
#     session$userData$displayed_frontpage_table_data <- frontpage_table_data
#     
# 
#     # update currently displayed data
#     new_data_to_display <- replace_with_display_names(
#         frontpage_table_data, input$language
#     )
#     DTproxy <- DT::dataTableProxy("mgmt_events_table", session = session)
#     DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE, 
#                     clearSelection = clear_selection)
#     
#     #update_editing_table(session, input, output, input$block, input$activity)
# }

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
    
    selectInput("table_activity", label = "", choices = c("")),
    selectInput("table_block", label = "", choices = c("")),
    selectInput("table_year", label = "", choices = c("")),
    
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
                value = Sys.Date(),
                weekstart = 1
            ),
            
            textAreaInput(
                "mgmt_event_notes",
                label = "",
                placeholder = "",
                resize = "vertical",
                height = "70px"
            ),
            
            # show a detailed options panel for the different activities
            # activity_options is defined in ui_builder.R
            create_ui(activity_options, create_border = FALSE),
            
            actionButton("save", label = "Save"),
            
            actionButton("cancel", label = "Cancel"),
            
            shinyjs::hidden(actionButton("delete", label = "Delete", 
                                         class = "btn-warning"))
        ),
        
        mainPanel(
        #     h2(textOutput("editing_table_title")),
        #     br(),
        #     # table for showing already supplied information
        #     DT::dataTableOutput("editing_table")
        )
    )))

)


# wrap the ui with the secure_app function which hides the app contents
# until login is successful
if (!dev_mode) {
    ui <- secure_app(ui, 
                     # language selector for login page
                     tags_bottom = selectInput("login_language", 
                                               label = "" , 
                                               choices = languages),
                     tags_top = tagList(
                         p("EXAMPLE USER site: ruukki, password: Ruukki1"),
                         p("ADMIN site: shinymanager, password: 12345")),
                     theme = shinytheme("lumen"),
                     enable_admin = TRUE,
                     fab_position = "top-right")
}


# Define server logic incl. save button action
server <- function(input, output, session) {
    
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
        
        # here would be good to somehow fetch the language selection from the
        # login UI, but it's difficult
    })
    
    # call the server part of shinymanager
    # weird observation: this has to be after the observeEvent block
    # which observes the auth_result$user. If it isn't the site selectInput
    # selection is not updated to match the username.
    auth_result <- secure_server(check_credentials = credential_checker)
    
    if (dev_mode) {
        shinyjs::show("site")
    }
    
    # go through all fields and set maxLength if requested in ui_structure.json
    for (element in structure_lookup_list) {
        if (!is.null(element$maxlength)) {
            js_message <- "$('##code_name').attr('maxlength', #maxlength)"
            js_message <- gsub("#code_name", element$code_name, js_message)
            js_message <- gsub("#maxlength", element$maxlength, js_message)
            shinyjs::runjs(js_message)
        }
    }
    
    frontpage_table_data <- reactiveVal()
    editing_table_data <- reactiveVal()
    # initialise in the normal (non-edit) mode
    event_to_edit <- reactiveVal()
    # lists of events by block on the currently viewed site
    # accessed like events$by_block[["0"]]
    events <- reactiveValues(by_block = list())
    
    observeEvent(event_to_edit(), ignoreNULL = FALSE, {
        
        if (is.null(event_to_edit())) {
            # edit mode was disabled
            shinyjs::hide("delete")
            shinyjs::disable("clone_event")
            DT::selectRows(proxy = dataTableProxy("mgmt_events_table"), 
                           selected = NULL)
            exit_sidebar_mode()
        } else {
            # edit mode was enabled, or there was a switch from one event to
            # another
            
            # populate the input controls with the values corresponding to the 
            # event
            for (variable_name in get_category_names("variable_name")) {
                # Update the UI elements corresponding to the variable names
                # to hold the data of the event. If no element is found 
                # corresponding to that name, update_ui_element does nothing.
                # this happens e.g. with date_ordering
                
                # if there is a table corresponding to the variable (it is on
                # the rows of the table), this is its name
                table_code_name <- get_variable_table(variable_name, 
                                                      only_rows = TRUE)
                
                # TODO: change to use reset_input_fields
                # this clears up old values
                if (!(variable_name %in% names(event_to_edit()))) {
                    update_ui_element(session, variable_name, value = "")
                    
                    if (!is.null(table_code_name)) {
                        prefill_values[[table_code_name]](list())
                    }
                    next
                }
                
                # now update all the UI elements to show the event info
                value <- event_to_edit()[[variable_name]]
                
                # if there is a table corresponding to this variable, pre-fill
                # it
                if (!is.null(table_code_name)) {
                    prefill_values[[table_code_name]](event_to_edit())
                }
                
                update_ui_element(session, variable_name, value = value)
            }
            
            shinyjs::show("delete")
            shinyjs::show("sidebar")
            shinyjs::disable("add_event")
            shinyjs::enable("clone_event")
        }
        
    })
    
    # exit edit mode
    # this is called when saving and when pressing cancel
    # TODO: make obsolete
    exit_sidebar_mode <- function() {
        # reset all input fields
        reset_input_fields(session, input, get_category_names("variable_name"))
        # hide sidebar
        shinyjs::hide("sidebar")
        shinyjs::enable("add_event")
    }
    
    # load data from all the json files corresponding to a site and store it in
    # separate lists in events$by_block
    # TODO: move elsewhere, return the list instead of saving it here
    # maybe fuse with retrieve_json_info?
    load_json_data <- function(site1) {
        # clear possible previous data
        events$by_block <- list()
        
        # find all blocks on this site
        site_blocks <- subset(sites, site == site1)$blocks[[1]]
        
        # go through the blocks and save events from the corresponding json file
        # to events$by_block
        for (block in site_blocks) {
            events$by_block[[block]] <- retrieve_json_info(site1, block)
        }
    }
    
    get_table_year_choices <- function() {
        years <- NULL
        
        for (event_list in events$by_block) {
            for (event in event_list) {
                
                # this shouldn't happen
                if (is.null(event$date)) next
                
                year <- format(as.Date(event$date, date_format_json), "%Y")
                
                if (!(year %in% years)) { years <- c(years, year) }
            }
        }
        
        sort(years, decreasing = TRUE)
    }

    # update year choices when events change
    # TODO: this can be sped up by keeping a up-to-date list of event years
    observeEvent(events$by_block, {
        years <- get_table_year_choices()
        
        current_choice <- input$table_year
        if (!isTruthy(current_choice) || !(current_choice %in% years)) {
            current_choice <- "year_choice_all"
        }
        
        table_year_choices <- c("year_choice_all", years)
        names(table_year_choices) <- get_disp_name(table_year_choices, 
                                                   input$language)
        updateSelectInput(session, "table_year", selected = current_choice,
                          choices = table_year_choices)
    })
    
    # update frontpage_table_data and the table itself when necessary
    observe({
        
        frontpage_table_variables <- c("block", 
                                       "mgmt_operations_event", 
                                       "date", 
                                       "mgmt_event_notes")
        
        if (is.null(input$table_block)) {
            event_list <- list()
        } else if (input$table_block == "block_choice_all") {
            event_list <- list()
            for (block_data in events$by_block) {
                event_list <- c(event_list, block_data)
            }
        } else {
            event_list <- events$by_block[[input$table_block]]
        }
        
        # make event list into a table
        data <- get_data_table(event_list, frontpage_table_variables)
        frontpage_table_data(data)
        
        # update currently displayed data
        new_data_to_display <- replace_with_display_names(data, input$language)
        DTproxy <- DT::dataTableProxy("mgmt_events_table", session = session)
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE, 
                        clearSelection = "none")
    })
    
    # we set priority = 1 so that this runs before the editing table rendering
    # runs (which is reactive on input$mgmt_operations_event)
    observe(priority = 1, {
        
        editing_table_variables <- c("date", "mgmt_event_notes")
        
        event_list <- list()
        if (isTruthy(input$block) & isTruthy(input$mgmt_operations_event)) {
            
            # find variables specific to activity
            activity_variables <- unlist(rlapply(
                activity_options[[input$mgmt_operations_event]], 
                fun = function(x) {
                    if (is.null(x$type) || 
                        x$type == "textOutput" || 
                        x$type == "dataTable") {
                        return(NULL)
                    } else {
                        return(x$code_name)
                    }
                }))
            
            editing_table_variables <- c(editing_table_variables, 
                                         activity_variables)
            
            # generate a list of events to display
            event_list <- events$by_block[[input$block]]
            # filter list to only show events of the given type
            event_list <- rlapply(event_list, fun = function(x) 
                if (x$mgmt_operations_event == input$mgmt_operations_event) {x})
        }
        
        #print("Events to be displayed in the editing table:")
        #str(event_list)
        
        # turn event list into a table to display
        data <- get_data_table(event_list, editing_table_variables)
        editing_table_data(data)
        
        #print("Table to be displayed:")
        #str(editing_table_data)
        
        new_data_to_display <- replace_with_display_names(data, input$language)
        DTproxy <- DT::dataTableProxy("editing_table", session = session)
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
    })
    
    # enable editing of old entries
    observeEvent(input$mgmt_events_table_rows_selected, ignoreNULL = FALSE, {
        
        row_index <- input$mgmt_events_table_rows_selected
        
        if (is.null(row_index)) {
            event_to_edit(NULL)
            return()
        }
        
        # fetch the event data of the selected row
        selected_event_data <- frontpage_table_data()[[row_index,"event"]]
        
        # set edit mode on. This saves the event we want to edit so that it is
        # preserved even if front page table view is changed
        event_to_edit(selected_event_data)
        
    })
    
    # cancel means we exit edit mode and hide sidebar controls
    observeEvent(input$cancel, {
        if (is.null(event_to_edit())) {
            exit_sidebar_mode()
        } else {
            event_to_edit(NULL)
        }
    })
    
    # when block changes, update table
    #observeEvent(input$frontpage_block, {
        #update_frontpage_table(session, input, output)
    #})
    
    # show add event UI when requested
    observeEvent(input$add_event, {
        # clear all input fields
        reset_input_fields(session, input, get_category_names("variable_name"))
        shinyjs::disable("add_event")
        
        # exit edit mode if we were in it
        event_to_edit(NULL)
        
        # show sidebar
        shinyjs::show("sidebar")
    })
    
    observeEvent(input$clone_event, {
        # fetch the event to be cloned
        event <- event_to_edit()
        
        block_data <- retrieve_json_info(input$site, event$block)
        
        block_data[[length(block_data) + 1]] <- event
        
        # save changes
        write_json_file(input$site, event$block, block_data)
        showNotification("Cloned successfully.", type = "message")
        
        # update events data
        events$by_block[[event$block]] <- block_data
        
        # update tables if necessary
        #current_row <- input$mgmt_events_table_rows_selected
        #update_frontpage_table(session, input, output,
        #                       changed_blocks = event$block,
        #                       clear_selection = "none")
        #update_editing_table(session, input, output, block = event$block,
        #                     activity = event$mgmt_operations_event, 
        #                     render = FALSE)
        
    })
    
    # save input to a file when save button is pressed
    # we are either creating a new event or editing an older one
    observeEvent(input$save, {
        
        # are we editing an existing event or creating a new one?
        editing <- !is.null(event_to_edit())
        # let's create a list which we will update to match the event info
        event <- if (editing) {
            event_to_edit()
        } else {
            list()
        }
        orig_block <- event$block
        
        # if we are editing, find the index of the event in the original 
        # block data list. Also, if the block has been changed, update that 
        # file. If the block has not changed, we will need the index when
        # replacing the old event with the updated one.
        if (editing) {
            
            orig_block_data <- retrieve_json_info(input$site, orig_block)
            event_index <- find_event_index(event, orig_block_data)

            if (is.null(event_index)) {
                showNotification("Could not edit entry because it was not 
                                 found in the event files.", type = "error")
                return()
            }
            
            # if the block of the event has been changed, delete it from the 
            # original block file
            if (orig_block != input$block) {
                orig_block_data[event_index] <- NULL
                write_json_file(input$site, orig_block, orig_block_data)
                events$by_block[[orig_block]] <- orig_block_data
            }
            
        }
        # fill out current_event to match new / updated data.
        # find variables that correspond to the selected activity and save
        # only those
        # TODO: not all of the variables under an activity option are 
        # necessarily relevant. How to determine the ones that are?
        relevant_variables <- unlist(rlapply(
            activity_options[[input$mgmt_operations_event]],
            fun = function(x) x$code_name))
        relevant_variables <- c("block", 
                                "mgmt_operations_event",
                                "date",
                                "mgmt_event_notes",
                                relevant_variables)
        
        # determine whether we need to read some variables from a table or not
        read_from_table <- NULL
        for (table_code_name in data_table_code_names) {
            if (visible[[table_code_name]]) {
                read_from_table <- structure_lookup_list[[table_code_name]]
            }
        }
        
        # fill / update information
        for (variable_name in get_category_names("variable_name")) {
            
            # if this variable is not relevant, make sure it is not included
            # in the event data
            if (!(variable_name %in% relevant_variables)) {
                event[variable_name] <- NULL
                next
            }
            

            if (!is.null(read_from_table) && 
                variable_name %in% read_from_table$columns) {
        
                value_to_save <- 
                    table_data[[read_from_table$code_name]]()[[variable_name]]
            } else {
                value_to_save <- input[[variable_name]]
            }

            # if the value is not defined or empty, replace with missingval
            if (length(value_to_save) == 0) {
                value_to_save <- missingval
            } else {
                missing_indexes <- is.na(value_to_save) | 
                    trimws(value_to_save) == ""
                if (any(missing_indexes)) {
                    value_to_save[missing_indexes] <- missingval
                }
            }
            
            # format value to character string if it is a date
            if (class(value_to_save) == "Date") {
                value_to_save <- format(value_to_save, date_format_json)
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
        # have changed and events$by_block might be out of date
        new_block_data <- retrieve_json_info(input$site, input$block)
        
        # if editing and block didn't change, replace event. 
        # Otherwise append event to the list
        if (editing && orig_block == input$block) {
            new_block_data[[event_index]] <- event
        } else {
            new_block_data[[length(new_block_data) + 1]] <- event
        }
        
        # save changes
        write_json_file(input$site, input$block, new_block_data)
        showNotification("Saved successfully.", type = "message")
        
        # update events$by_block
        events$by_block[[input$block]] <- new_block_data
        
        # exit sidebar mode
        if (editing) {
            event_to_edit(NULL)
        } else {
            exit_sidebar_mode()
        }
        
    })
    
    # delete entry when delete button is pressed
    observeEvent(input$delete, {
        event <- event_to_edit()
        
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
        
        # update events list
        events$by_block[[event$block]] <- block_data
        
        # exit edit mode
        event_to_edit(NULL)
    })
    
    # change available blocks depending on the site and load the site event
    # data into memory (events$by_block)
    observeEvent(input$site, {

        if (is.null(input$site) | input$site == "") {
            shinyjs::disable("table_block")
            shinyjs::disable("block")
            shinyjs::disable("add_event")
            return()
        } 
        
        shinyjs::enable("table_block")
        shinyjs::enable("block")
        shinyjs::enable("add_event")
        
        # update table_block choices.
        # table_block choices are also updated in the observeEvent for
        # input$language to make the block_choice_all name translate
        block_choices <- subset(sites, site == input$site)$blocks[[1]]
        names_for_block_choices <- c(
            get_disp_name("block_choice_all", input$language), 
            block_choices)
        choices_for_table_block <- c("block_choice_all", block_choices)
        names(choices_for_table_block) <- names_for_block_choices
        
        updateSelectInput(session, "table_block", 
                          choices = choices_for_table_block)
        updateSelectInput(session, "block", choices = block_choices)
        
        # load the events corresponding to this site into memory
        load_json_data(input$site)
        
    })
    
    # change editing table when input$block is changed
    #observeEvent(input$block, {
        #update_editing_table(session, input, output, input$block, 
        #                     input$mgmt_operations_event, render = FALSE)
    #})
    
    required_variables <- reactiveVal(list("site", "block", "date",
                                           "mgmt_operations_event"))
    
    # change required variables when activity is changed
    observeEvent(input$mgmt_operations_event, {
        
        required_checker <- function(element) {
            if (!is.null(element$required)) {
                return(element$code_name)
            } else {
                return(NULL)
            }
        }
        
        # find the variables that are compulsory for this activity type
        variables <- rlapply(
            activity_options[[input$mgmt_operations_event]],
            fun = required_checker)
        variables <- c(list("site", 
                            "block", 
                            "mgmt_operations_event",
                            "date"),
                       variables)
        # save to a reactiveval. The inputs are compared against this list of
        # variables in an observe()
        required_variables(variables)
    })
    
    # disable the save button if not all necessary info has been filled
    observe({
        #if (!dev_mode) {req(auth_result$admin)}
        #req(auth_result$admin)
        
        # run whenever any of the inputs change. I know this is not ideal, but
        # reactivity to input values doesn't work when we dynamically generate
        # which inputs we want to access
        reactiveValuesToList(input)
        
        # if (dev_mode || auth_result$admin == "TRUE") {
            # if we are in admin or dev mode, 
            # we don't care about required variables
            # return()
        # }
        
        for (required_variable in required_variables()) {
            
            table_code_name <- get_variable_table(required_variable)
            
            current_val <- if (!is.null(table_code_name) && 
                               visible[[table_code_name]]) {
                table_data[[table_code_name]]()[[required_variable]]
            } else {
                input[[required_variable]]
            }
            
            # is.Truthy essentially checks whether input is empty or null
            is_filled <- sapply(current_val, isTruthy)
            if (!all(is_filled)) {
                shinyjs::disable("save")
                return()
            }
        }
        
        shinyjs::enable("save")
    })
    
    # (re-)render frontpage table when input$language changes
    # TODO: figure server = FALSE out
    output$mgmt_events_table <- DT::renderDataTable(#server = FALSE,
        {
        new_data_to_display <- replace_with_display_names(
            isolate(frontpage_table_data()), input$language)
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
    
    # render the editing table when language or data changes
    # TODO: figure server = FALSE out
    output$editing_table <- DT::renderDataTable(#server = FALSE, 
        {
            
        # take a dependency on activity, since that is an indicator
        # of when the table needs to be re-rendered
        input$mgmt_operations_event
        
        new_data_to_display <- replace_with_display_names(
            isolate(editing_table_data()), input$language
        )
        n_cols <- ncol(new_data_to_display)
        datatable(new_data_to_display, 
                  selection = "none", # allow selection of a single row
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
    
    # holds boolean values which indicate whether the conditions for the 
    # visibility of data tables are met
    visible <- reactiveValues()
    
    # changing these overrides the values in the table
    prefill_values <- list()
    
    # initialise the table server for each of the dynamically added tables
    # sapply with simplify = FALSE is equivalent to lapply
    table_data <- sapply(data_table_code_names,
                         FUN = function(data_table_code_name) {
        table_structure <- structure_lookup_list[[data_table_code_name]]
        
        # what are the names of the rows? This can either be determined by
        # the choices of selectInput with multiple selections, or a numericInput
        # which represents the number of rows
        row_names <- reactive({
            row_variable <- structure_lookup_list[[table_structure$rows]]
            if (row_variable$type == "numericInput") {
                
                number_of_rows <- input[[row_variable$code_name]]
                
                if (!isTruthy(number_of_rows)) {
                    NULL
                } else {
                    number_of_rows <- max(ceiling(number_of_rows), 1)
                    1:number_of_rows
                }
                
                
            } else if (row_variable$type == "selectInput") {
                input[[row_variable$code_name]]
            }
            
        })
        
        # add observer to visibility condition of table
        # table is visible if the length of the variable presented on the rows
        # of the table is more than 1
        observeEvent(row_names(), ignoreNULL = FALSE, {
            visible[[data_table_code_name]] <- length(row_names()) > 1
            #message(glue("Visibility for {data_table_code_name} is {visible[[data_table_code_name]]}"))
        })
        
        prefill_values[[data_table_code_name]] <<- reactiveVal()
        tableServer(data_table_code_name, row_names, reactive(input$language),
                    visible = reactive(visible[[data_table_code_name]]),
                    override_values = prefill_values[[data_table_code_name]])
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    # observe({
    #     for (value in table_data) {
    #         str(value())
    #     }
    # })
    
    # update each of the text outputs automatically, including language changes
    # and the dynamic updating in editing table title etc. 
    lapply(text_output_code_names, FUN = function(text_output_code_name) {
        
        # render text
        output[[text_output_code_name]] <- renderText({
            
            text_to_show <- get_disp_name(text_output_code_name, input$language)
            
            #get element from the UI structure lookup list
            element <- structure_lookup_list[[text_output_code_name]]
            #if the text should be updated dynamically, do that
            if (!is.null(element$dynamic)) {

                # there are currently two modes of dynamic text
                if (element$dynamic$mode == "input") {
                    # the -1 removes the mode element, we don't want it
                    patterns <- names(element$dynamic)[-1]
                    # use lapply here to get the dependency on input correctly
                    replacements <- lapply(patterns, function(pattern) {
                        replacement <- input[[ element$dynamic[[pattern]] ]]
                        replacement <- get_disp_name(replacement,
                                                     input$language)
                        text_to_show <<- gsub(pattern, replacement, 
                                              text_to_show)
                        replacement
                    })

                    # if one of the replacements is empty, we don't want to
                    # see the text at all
                    if ("" %in% replacements) { text_to_show <- "" }

                } else if (element$dynamic$mode == "edit_mode") {

                    text_to_show <- if (!is.null(event_to_edit())) {
                        element$dynamic[["TRUE"]]
                    } else {
                        element$dynamic[["FALSE"]]
                    }
                    text_to_show <- get_disp_name(text_to_show, input$language)

                }
            }
            text_to_show
        })

    })
    
    # change language when user requests it
    observeEvent(input$language, {
        
        # we have to handle input and output elements in different ways
        
        # OUTPUT ELEMENTS:
        
        # change textOutputs when the language is changed
        # one has to use lapply here, for-loop does not work! See
        # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
        
        # function to render text outputs. Note the pattern matching which
        # is used for the editing table title (shown in text_output_handler)
        #lapply(text_output_code_names, text_output_handler, session = session,
        #       input = input,
        #       output = output)
        
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
            
            label <- get_disp_name(element$label, input$language)
            
            if (element$type == "selectInput") {
                
                # fetch choices for the selectInput
                choices <- get_selectInput_choices(element, input$language)
                
                # make sure we don't change the selected value
                current_value <- input[[code_name]]
                
                if (is.null(choices)) {
                    updateSelectInput(session, 
                                      code_name,
                                      label = label,
                                      selected = current_value) 
                } else {
                    updateSelectInput(session, 
                                      code_name,
                                      label = label,
                                      choices = choices,
                                      selected = current_value)
                }
                
                
            } else if (element$type == "dateInput") {
                #language_code <- if (input$language == "disp_name_fin") {
                #    "fi"
                #} else {
                #    "en"
                #}
                updateDateInput(session, 
                                code_name, 
                                label = label,
                                #language = language_code
                                )
            } else if (element$type == "textAreaInput") {
                updateTextAreaInput(session,
                                    code_name,
                                    label = label,
                                    placeholder = 
                                        get_disp_name(
                                            element$placeholder, 
                                            input$language))
            } else if (element$type == "actionButton") {
                updateActionButton(session,
                                   code_name,
                                   label = label)
            } else if (element$type == "checkboxInput") {
                updateCheckboxInput(session,
                                    code_name,
                                    label = label)
            } else if (element$type == "textInput") {
                updateTextInput(session, 
                                code_name, 
                                label = label,
                                placeholder = 
                                    get_disp_name(
                                        element$placeholder, 
                                        input$language))
            } else if (element$type == "numericInput") {
                updateNumericInput(session,
                                   code_name,
                                   label = label)
            } else if (element$type == "dateRangeInput") {
                updateDateRangeInput(session,
                                     code_name,
                                     label = label)
            }

        }
        
        # update table_block selector choices separately
        if (isTruthy(input$site)) {
            block_choices <- subset(sites, site == input$site)$blocks[[1]]
            names_for_block_choices <- c(
                get_disp_name("block_choice_all", input$language), 
                block_choices)
            choices_for_table_block <- c("block_choice_all", block_choices)
            names(choices_for_table_block) <- names_for_block_choices
            
            updateSelectInput(session, "table_block", 
                              choices = choices_for_table_block)
        }
        
        # update table_activity selector choices separately
        choices_for_table_activity <- 
            c("activity_choice_all", get_category_names(
                "mgmt_operations_event_choice"))
        names(choices_for_table_activity) <- 
            get_disp_name(choices_for_table_activity, input$language)
        updateSelectInput(session, "table_activity", 
                          choices = choices_for_table_activity)
        
        # update table_year selector choices separately
        years <- get_table_year_choices()
        
        current_choice <- input$table_year
        if (!isTruthy(current_choice) || !(current_choice %in% years)) {
            current_choice <- "year_choice_all"
        }
        
        table_year_choices <- c("year_choice_all", years)
        names(table_year_choices) <- get_disp_name(table_year_choices, 
                                                   input$language)
        updateSelectInput(session, "table_year", selected = current_choice,
                          choices = table_year_choices)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)