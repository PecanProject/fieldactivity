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
        
        # if value is a list (e.g. multiple crops selected in harvest_crop)
        # turn it into a character vector
        if (is.list(value)) {
            value <- as.character(value[[1]])
        }
        
        updateSelectInput(session, code_name, selected = value,  ...)
    } else if (element$type == "dateInput") {
        if (value == "") {
            formatted_date <- Sys.Date()
        } else {
            formatted_date <- as.Date(value, format = date_format)
        }
        updateDateInput(session, code_name, 
                        value = formatted_date, ...)
    } else if (element$type == "textAreaInput") {
        updateTextAreaInput(session, code_name, value = value,  ...)
    } else if (element$type == "checkboxInput") {
        updateCheckboxInput(session, code_name, value = value, ...)
    } else if (element$type == "actionButton") {
        updateActionButton(session, code_name, ...)
    } else if (element$type == "textInput") {
        updateTextInput(session, code_name, value = value, ...)
    }
}

# TODO: is this needed anywhere?
get_input_element_names <- function(input) {
    # get a list of all input elements
    input_element_names <- names(reactiveValuesToList(input))
    
    for (code_name in input_element_names) {
        
        element <- rlapply(
            structure,
            code_name_checker,
            code_name = code_name)
        
        # didn't find the element corresponding to code_name, so this
        # is not an input element we care about
        if (is.null(element$type)) {
            input_element_names <- 
                input_element_names[which(!input_element_names == code_name)]
        }
        
    }
    
    return(input_element_names)
}

clear_input_fields <- function(session, fields_to_clear, exceptions = c("site", "block")) {
    
    for (code_name in fields_to_clear) {
        
        if (code_name %in% exceptions) next
        
        update_ui_element(session, code_name, value = "")
        
    }
    
}

get_display_data_table <- function(code_name_data_table, language) {
    # update data table
    display_name_data_table <- replace_with_display_names(
        code_name_data_table, language
    )
    # add a new column for ordering by date
    display_name_data_table$date_ordering <- as.Date(
        display_name_data_table$mgmt_event_date, 
        format = date_format)
    
    return(display_name_data_table)
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
        # the sidebar contains the selectors for entering information
        # about the event
        sidebarPanel(
            
            h2(shinyjs::hidden(textOutput("edit_mode_title")),
               style = "margin-bottom = 0px; margin-top = 0px"),
            
            # adding "" to the choices makes the default choice empty
            selectInput(
                "site",
                label = "",
                choices = c("", sites$site)
            ),
            
            # in general the choices and labels don't have to be defined for
            # selectInputs, as they will be populated when the language is
            # changed (which also happens when the app starts)
            
            selectInput("block", label = "", choice = c("")),
            
            selectInput("mgmt_operations_event", label = "", choices = c("")),
            
            # setting max disallows inputting future events
            dateInput(
                "mgmt_event_date",
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
            
            shinyjs::hidden(actionButton("cancel", label = "Cancel")),
            
            shinyjs::hidden(actionButton("delete", label = "Delete", 
                                         class = "btn-warning"))
        ),
        
        mainPanel(
            # table for showing already supplied information
            DT::dataTableOutput("mgmt_events_table")
        )
    )
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

    # initialise in the normal (non-edit) mode
    session$userData$edit_mode <- FALSE
    # when in editing mode the block has been changed by the user, we return
    # it back to its original state. However, this update is “slow” whereas
    # the value of the edit_mode variable changes quickly. Therefore we need
    # an additional variable to block an unnecessary table update in this case
    session$userData$block_table_update <- FALSE
    
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
        }

        # here would be good to somehow fetch the language selection from
        # login UI, but it's difficult
    })
    
    # enable editing of old entries
    # TODO: don't clear fields if row selected and language changed
    observeEvent(input$mgmt_events_table_rows_selected, ignoreNULL = FALSE, {
        
        row_index <- input$mgmt_events_table_rows_selected
        
        if (is.null(row_index)) {
            
            if (session$userData$edit_mode) {
                
                # no rows selected anymore, 
                # so clear the fields and set edit mode off
                clear_input_fields(session, get_input_element_names(input))
                
                # if block selector was changed, change it back
                if (!(session$userData$original_block == input$block)) {
                    # prevent unnecessary table update
                    session$userData$block_table_update <- TRUE
                    
                    # return block value to original
                    updateSelectInput(session, "block", selected = 
                                          session$userData$original_block)
                }
                
                shinyjs::hide("cancel")
                shinyjs::hide("delete")
                shinyjs::hide("edit_mode_title")
                
                session$userData$edit_mode <- FALSE
                session$userData$original_block <- NULL

            }
            
            return()
        } 
        
        selected_data <- tabledata$events_with_code_names[row_index,]
        
        # populate the input controls with the values corresponding to the row
        
        for (col_name in names(selected_data)) {
            
            # try updating the element corresponding to column name with the
            # value in that column. If no element is found corresponding to 
            # that name, update_ui_element does nothing
            update_ui_element(session, col_name,
                              value = selected_data[1, col_name])
            
        }
        
        # set edit mode on
        session$userData$edit_mode <- TRUE
        shinyjs::show("cancel")
        shinyjs::show("delete")
        shinyjs::show("edit_mode_title")
        
        # save current block value so we can return to it later
        session$userData$original_block <- input$block
    })
    
    DTproxy <- DT::dataTableProxy("mgmt_events_table", session = session)
    
    # canceling editing is equivalent to deselecting the selected row
    observeEvent(input$cancel, {
        DT::selectRows(DTproxy, NULL)
    })
    
    # call the server part of shinymanager
    # weird observation: this has to be after the observeEvent block
    # which observes the auth_result$user. If it isn't the site selectInput
    # selection is not updated to match the username.
    auth_result <- secure_server(check_credentials = credential_checker)
    
    # this is where we access the data that is displayd in the data table.
    # initially NULL because reactive expressions aren't allowed here.
    tabledata <- reactiveValues(events_with_code_names = NULL)
    
    # when block changes, update table
    observeEvent(input$block, {
    
        # if we are editing, don't do anything
        if (session$userData$edit_mode) {
            return()
        }
        
        # the user has changed the block selection during edit mode and now
        # we want to return it back. We don't however want to update the table
        # again unnecessarily, so we block this update
        if (session$userData$block_table_update) {
            session$userData$block_table_update <- FALSE
            return()
        }
        
        req(input$site, input$block, input$language)
        
        # block changed and we are not editing, so load new data
        tabledata$events_with_code_names <- retrieve_json_info(
            input$site, 
            input$block,
            language = NULL)
        
        # add missing columns (with no mentions in json file) to the table
        # so that it is displayed correctly
        for (variable_name in get_category_names("variable_name", NULL)) {
            if (is.null(tabledata$events_with_code_names[[variable_name]])) {
                # creates a column filled with NAs
                tabledata$events_with_code_names[[variable_name]] <- NA
            }
        }
        
        # render data table.
        # this can also run on its own without the entire input$block
        # observer running. This happens when input$language is changed.
        output$mgmt_events_table <- DT::renderDataTable({

            new_data_to_display <- get_display_data_table(
                isolate(tabledata$events_with_code_names), input$language
            )
            n_cols <- ncol(new_data_to_display)
            
            datatable(new_data_to_display, 
                      selection = "single", # allow selection of a single row
                      rownames = FALSE, # hide row numbers
                      colnames = c(names(
                          get_category_names("variable_name",
                                             language = 
                                                 input$language)), 
                          "date_ordering"),
                      options = list(dom = 't', # hide unnecessary controls
                                     # TODO: check whether a long list is
                                     # entirely visible
                                     # order chronologically by hidden column
                                     order = list(n_cols - 1, 'desc'), 
                                     columnDefs = list(
                                         # hide all other columns except
                                         # event, date and notes
                                         list(visible = FALSE, targets = 
                                                  c(3:(n_cols - 1))),
                                         # hide sorting arrows
                                         list(orderable = FALSE, targets = 
                                                  0:(n_cols - 2)))
                      ))
        })
    })
    
    # save input to a file when save button is pressed
    observeEvent(input$save, {
        # we are either creating a new event or editing an older one
        # either way, let's create a new data frame row
        
        if (session$userData$edit_mode) {
            # create an updated row
            selected_row <- input$mgmt_events_table_rows_selected
            new_row <- tabledata$events_with_code_names[selected_row, ]
        } else {
            new_row <- generate_empty_data_frame()
        }
        
        # fill / update information on the row
        for (variable_name in names(new_row)) {
            
            value_to_save <- input[[variable_name]]
            
            # if the value is not defined or empty, replace with missingval
            if (!isTruthy(value_to_save)) {
                value_to_save <- missingval
            }
            
            # format value if it is a date
            if (class(value_to_save) == "Date") {
                value_to_save <- format(value_to_save, date_format)
            }
            
            # if value has multiple values (e.g. selectInput with possibility
            # of selecting multiple values), then make that into a list if
            # necessary
            #if (length(value_to_save) > 1) {
            #    if (typeof(new_row[[variable_name]]) == "character") {
            #        value_to_save <- list(value_to_save)
            #    }
            #}
            
            # double brackets allow saving a list (e.g. multiple selections)
            new_row[[1, variable_name]] <- value_to_save
        }

        # save new information / edited information
        if (session$userData$edit_mode) {
            # if the block has changed, we have to save to a different file
            if (!(session$userData$original_block == input$block)) {
                table_to_save <- retrieve_json_info(input$site,
                                                    input$block,
                                                    NULL)
                table_to_save <- rbind(table_to_save, new_row)
                
                write_json_file(input$site, input$block, table_to_save)
                
                # delete from table to be displayed
                tabledata$events_with_code_names <-
                    tabledata$events_with_code_names[-selected_row, ]
                
            } else {
                # block is the same so just update row
                tabledata$events_with_code_names[selected_row, ] <-
                    new_row
            }
            
            # write changes to file of original block
            write_json_file(input$site,
                            session$userData$original_block,
                            tabledata$events_with_code_names)
            
            showNotification("Modifications saved!", type = "message")
            
        } else {
            
            # add a new event
            tabledata$events_with_code_names <- rbind(
                tabledata$events_with_code_names, new_row)
            
            write_json_file(input$site,
                            input$block,
                            tabledata$events_with_code_names)
            
            # clear the selected activity and notes
            # TODO: change to using clear fields function
            # although do consider date
            updateSelectInput(session, "mgmt_operations_event", selected = "")
            updateTextAreaInput(session, "mgmt_event_notes", value = "")
            
            showNotification("Data saved!", type = "message")
        }
        
        new_data_to_display <- get_display_data_table(
            tabledata$events_with_code_names, input$language)
        # this deselects the row which exits the edit mode
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
        
    })
    
    # delete entry when delete button is pressed
    observeEvent(input$delete, {
      
        selected_row <- input$mgmt_events_table_rows_selected
        
        # delete row 
        tabledata$events_with_code_names <-
            tabledata$events_with_code_names[-selected_row,]
        
        # write to json
        write_json_file(input$site, session$userData$original_block,
                        tabledata$events_with_code_names)
        
        showNotification("Entry deleted!", type = "message")
        
        new_data_to_display <- get_display_data_table(
            tabledata$events_with_code_names, input$language
        )
        
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
        # replacing the data clears the selection, which in turn exits
        # the edit mode, so we are ready
        
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
        
        # update data table to the new language
        new_data_to_display <- get_display_data_table(
            tabledata$events_with_code_names, input$language
        )
        DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE)
        
        # INPUT ELEMENTS:
        
        # get a list of all input elements which we have to relabel
        input_element_names <- names(reactiveValuesToList(input))
        
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
            } else if (element$type == "textInput") {
                updateTextInput(session, 
                                code_name, 
                                label = get_disp_name(element$label, 
                                                      input$language))
            } else if (element$type == "numericInput") {
                updateNumericInput(session,
                                   code_name,
                                   label = get_disp_name(element$label, 
                                                         input$language))
            }

        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)