# The function of the form module is as follows:
# - contains the widgets for entering the actual information about the event
# - shows the correct widgets depending on the user's choices
# - allows prefilling the widgets with the desired values
# - verifies that the information has been supplied correctly
# - returns the values to the main app in the format they will be saved
#   (nested array properties, empty values left out)
# - contains the save, cancel and delete buttons and sends their signals to
#   the main app


#' UI function for the form module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_form_ui <- function(id){
  ns <- NS(id)
  iso <- lang_to_iso(init_lang)
  pr <- mgmt_schema$property_registry

  tagList(
 
    # the form contains the widgets for entering information
    # about the event
    fluidRow(
      column(width = 3,
             h3(textOutput(ns("form_title")), 
                style = "margin-bottom = 0px; margin-top = 0px; 
                   margin-block-start = 0px"),
             
             # in general the choices and labels don't have to be 
             # defined for  selectInputs, as they will be 
             # populated when the language is changed 
             # (which also happens when the app starts)
             
             span(textOutput(ns("required_variables_helptext")), 
                  style = "color:gray"),
             br(),
             
             selectInput(ns("block"), label = get_disp_name("block_label", 
                                                            init_lang),
                         choices = ""),
             
             selectInput(ns("mgmt_operations_event"), 
                         label = schema_get_title(
                           pr$mgmt_operations_event$titles, iso, "event"),
                         choices = build_event_type_choices(mgmt_schema, iso)
                         ),
             
             # setting max disallows inputting future events
             dateInput(
               ns("date"),
               format = "dd/mm/yyyy",
               label = common_field_label(mgmt_schema, "date", NULL, iso),
               max = Sys.Date(),
               value = Sys.Date(),
               weekstart = 1
             ),
             
             textAreaInput(
               ns("mgmt_event_short_notes"),
               label = schema_get_title(
                 pr$mgmt_event_short_notes$titles, iso, "description"),
               placeholder = schema_get_title(
                 pr$mgmt_event_short_notes$placeholders, iso, ""),
               resize = "vertical",
               height = "70px"
             )
      ),
      
      column(width = 9, 
             render_schema_form(mgmt_schema, ns, init_lang)
      )
    ),
    
    # the buttons for saving, canceling and deleting
    fluidRow(
      column(width = 12,
             actionButton(ns("save"), label = "Save"),
             
             actionButton(ns("cancel"), label = "Cancel"),
             
             shinyjs::hidden(actionButton(ns("delete"), label = "Delete", 
                                          class = "btn-warning"))
      )
    )
    
  )
}
    
#' form Server Functions
#'
#' Each schema property has one widget per event type (and subtype) it appears
#' in, identified by the property's registry key (desc$id). The widgets of the
#' selected event type and subtype are the relevant ones: they are validated
#' and their values are saved under the property name (desc$name).
#'
#' @param id The id of the corresponding UI element
#' @param site A reactive expression holding the current site name
#' @param set_values Changing the value of this reactive expression sets the
#'   values in the form
#' @param reset_values A reactive expression. If set to TRUE, clears the values
#'   on the form
#' @param edit_mode A reactive expression holding a boolean value which
#'   indicates whether the app is editing an event (TRUE) or creating a purely
#'   new one (FALSE)
#' @param language A reactive expression holding the current UI language
#'
#' @import shinyvalidate
#' @noRd
mod_form_server <- function(id, site, set_values, reset_values, edit_mode, 
                            language, init_signal) {
  
  stopifnot(is.reactive(site))
  stopifnot(is.reactive(set_values))
  stopifnot(is.reactive(reset_values))
  stopifnot(is.reactive(edit_mode))
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(init_signal))
  
  moduleServer(id, function(input, output, session) {
    if (dp()) message("Initialising form server function")
    
    schema <- mgmt_schema
    er <- schema$event_registry
    pr <- schema$property_registry

    # the widgets of event and subtype properties, and the tables and file
    # (image) fields among them
    event_fields <- Filter(function(desc) desc$event_type != "__common__", pr)
    input_fields <- Filter(function(desc) {
      !desc$type %in% c("dataTable", "fileInput")
    }, event_fields)
    table_fields <- Filter(function(desc) desc$type == "dataTable",
                           event_fields)
    file_fields <- Filter(function(desc) desc$type == "fileInput",
                          event_fields)

    # keys the schema knows about. Other keys of an edited event are kept as
    # they are when it is saved, so that no data is lost
    known_names <- c(vapply(pr, function(desc) desc$name, character(1)),
                     "block", "$schema")
    unknown_values <- reactiveVal(list())
    # the totals currently computed from the tables (see auto-sum below)
    summed_ids <- character(0)

    # the selected event type and subtype
    current_context <- reactive({
      event_type <- input$mgmt_operations_event
      if (!isTruthy(event_type) || is.null(er[[event_type]])) return(NULL)
      list(event_type = event_type,
           subtype = get_current_subtype(input, er[[event_type]]))
    })

    # The fields of the selected event type and subtype that are visible,
    # i.e. whose x-ui condition (if any) is met. These are the ones validated
    # and saved.
    relevant_fields <- reactive({
      context <- current_context()
      if (is.null(context)) return(list())
      fields <- get_relevant_fields(schema, context$event_type,
                                    context$subtype)
      Filter(function(desc) {
        is.null(desc$condition) ||
          isTRUE(evaluate_condition(desc$condition, session))
      }, fields)
    })
    relevant_ids <- reactive({
      vapply(relevant_fields(), function(desc) desc$id, character(1))
    })

    date_required <- reactive({
      context <- current_context()
      !is.null(context) && "date" %in% er[[context$event_type]]$required
    })

    # add input validators
    # each widget has its own validator, which is active whenever that widget
    # is relevant. These individual validators are added as subvalidators to
    # main_iv
    main_iv <- InputValidator$new()
      
    common_iv <- InputValidator$new()
    common_iv$add_rule("mgmt_operations_event",
                       sv_required(message = "Required"))
    common_iv$add_rule("block", sv_required(message = "Required"))
    main_iv$add_validator(common_iv)
      
    date_iv <- InputValidator$new()
    date_iv$add_rule("date", sv_required(message = "Required"))
    date_iv$condition(date_required)
    main_iv$add_validator(date_iv)
        
    for (desc in input_fields) {
      iv <- field_validator(desc)
      if (is.null(iv)) next
      local({
        field_id <- desc$id
        iv$condition(reactive(field_id %in% relevant_ids()))
        })
        main_iv$add_validator(iv)
      }
    # start showing validation messages
    main_iv$enable()
    
    # when site setting is changed, update the block choices on the form
    observeEvent(site(), ignoreNULL = FALSE, {
      
      if (!isTruthy(site())) {
        shinyjs::disable("block")
        shinyjs::disable("save")
        return()
      } 
      
      shinyjs::enable("block")
      shinyjs::enable("save")
      
      # update block choices
      block_choices <- subset(sites, sites$site == site())$blocks[[1]]
      updateSelectInput(session, "block", choices = block_choices)
    })
      
    # Fill the form with the values of an event and clear all other fields, so
    # that nothing is left over from a previously edited event (empty values
    # are not saved, so a missing value means an empty field). A new event is
    # dated today, as when the form is first shown.
    fill_form <- function(values) {
      event_type <- values$mgmt_operations_event
      subtype <- get_subtype_value(values, er)
      values$date <- values$date %||% format(Sys.Date(), date_format_json)

      if (!is.null(values$block)) {
        updateSelectInput(session, "block", selected = values$block)
      }
      for (prop_name in schema$common_properties) {
        update_schema_value(session, pr[[prop_name]], values[[prop_name]])
      }
      event_ids <- if (!is.null(event_type)) {
        vapply(get_relevant_fields(schema, event_type, subtype),
               function(desc) desc$id, character(1))
      }
      for (desc in input_fields) {
        value <- if (desc$id %in% event_ids) values[[desc$name]]
        update_schema_value(session, desc, value)
      }
      for (desc in table_fields) {
        rows <- if (desc$id %in% event_ids) values[[desc$name]]
        tables[[desc$id]]$set_values(rows %||% list())
      }
      for (desc in file_fields) {
        path <- if (desc$id %in% event_ids) values[[desc$name]]
        if (is.null(path)) {
          files[[desc$id]]$reset_path(TRUE)
        } else {
          files[[desc$id]]$set_path(path)
        }
      }

      unknown_values(values[setdiff(names(values), known_names)])
      summed_ids <<- character(0)
    }
    
    # when set_values is changed, update the values in the form
    observeEvent(set_values(), {
      
      if (dp()) message("Filling the form with values")
      fill_form(set_values())
      
      # change set_values back to NULL so that we can catch the next time its
      # value is changed. This doesn't re-trigger this observeEvent as
      # observeEvent ignores NULL values by default
      set_values(NULL)
    })
    
    # when reset_values is signaled, reset the values of all widgets
    observeEvent(reset_values(), {
      if (identical(reset_values(), FALSE)) return()
      if (dp()) message("Resetting form values")
      fill_form(list())
      reset_values(FALSE)
    })
    
    # show Delete button depending on edit mode
    observeEvent(edit_mode(), ignoreNULL = FALSE, {
      shinyjs::toggle("delete", condition = edit_mode())
    })
    
    # update each of the text outputs automatically, including language changes
    # and the dynamic updating in editing table title etc. 
    lapply(text_output_code_names, FUN = function(text_output_code_name) {
      
      # render text
      output[[text_output_code_name]] <- renderText({
        
        if (dp()) message(glue("Rendering text for {text_output_code_name}"))
        
        text_to_show <- get_disp_name(text_output_code_name, language())
        
        #get element from the UI structure lookup list
        element <- structure_lookup_list[[text_output_code_name]]
        #if the text should be updated dynamically, do that
        if (!is.null(element$dynamic)) {
          if (element$dynamic$mode == "edit_mode") {
            text_to_show <- if (edit_mode()) {
              element$dynamic[["TRUE"]]
            } else {
              element$dynamic[["FALSE"]]
            }
            text_to_show <- get_disp_name(text_to_show, language())
            
          }
        }
        text_to_show
      })
      
    })
    
    # the date label depends on the event type (e.g. the start date of
    # grazing) and whether the event type requires a date
    observe({
      updateDateInput(session, "date", label = common_field_label(
        schema, "date", input$mgmt_operations_event, lang_to_iso(language())))
    })

    # Language switching for schema-driven form fields
    observeEvent(language(), ignoreInit = TRUE, {
      iso <- lang_to_iso(language())
      
      # Update event type selector
      updateSelectInput(session, "mgmt_operations_event",
                        label = schema_get_title(
                          pr$mgmt_operations_event$titles, iso, "event"),
                        choices = build_event_type_choices(schema, iso),
                        selected = input$mgmt_operations_event)
      
      # Update short notes
      short_notes_desc <- pr$mgmt_event_short_notes
      updateTextAreaInput(session, "mgmt_event_short_notes",
                          label = schema_get_title(
                            short_notes_desc$titles, iso, "description"),
                          placeholder = schema_get_title(
                            short_notes_desc$placeholders, iso, ""))
        
      # Update all event and subtype fields
      for (desc in input_fields) {
        subtype_choices <- if (desc$is_discriminator) {
          build_subtype_choices(er[[desc$event_type]], iso)
        }
        update_schema_widget(session, desc, iso, input,
                             choices = subtype_choices)
      }
        
      # Update app chrome (block label, save/cancel/delete buttons)
      for (code_name in names(reactiveValuesToList(input))) {
        element <- structure_lookup_list[[code_name]]  
        if (is.null(element$type)) next
        
        label <- get_disp_name(element$label, language())
        
        if (element$type == "selectInput") {
          choices <- get_selectInput_choices(code_name, language())
          current_value <- input[[code_name]]
          
          if (is.null(choices)) {
            updateSelectInput(session, code_name,
                              label = ifelse(is.null(label),"",label),
                              selected = current_value) 
          } else {
            updateSelectInput(session, code_name,
                              label = ifelse(is.null(label),"",label),
                              choices = choices,
                              selected = current_value)
          }
        } else if (element$type == "actionButton") {
          updateActionButton(session, code_name, label = label)
        }
        
      }
      
      
    })
    
    # Table and fileInput module servers are started on first use. The
    # reactiveVals for setting table values exist from the start.
    tables <- lapply(table_fields, function(desc) {
      list(set_values = reactiveVal())
    })
    files <- lapply(file_fields, function(desc) {
      list(set_path = reactiveVal(), reset_path = reactiveVal())
    })
    
    observeEvent(init_signal(), once = TRUE, {
      if (dp()) message("Initialising table and fileInput server functions")
      
      for (desc in table_fields) {
        tables[[desc$id]]$result <<- mod_table_server(
          schema_table_id(desc), desc, language, tables[[desc$id]]$set_values)
            }
      
      for (desc in file_fields) {
        files[[desc$id]]$value <<- mod_fileInput_server(
          desc$id, desc, language, files[[desc$id]]$set_path,
          files[[desc$id]]$reset_path)
      }
    })
      
    # Auto-sum: update total fields from table column values. A total is only
    # computed when the table has values to sum, so a total entered without
    # per-item values (as in many legacy events) is kept.
    observe({
      context <- current_context()
      if (is.null(context)) return()

      for (desc in relevant_fields()) {
        if (is.null(desc$total_of)) next
        table_id <- lookup_property_key(pr, desc$total_of$list_name,
                                        context$event_type, context$subtype)
        table <- tables[[table_id %||% ""]]$result
        if (is.null(table)) next

        prop_to_sum <- desc$total_of$property_name
        nums <- suppressWarnings(as.numeric(unlist(
          lapply(table$values(), function(row) row[[prop_to_sum]] %||% NA))))
        if (all(is.na(nums))) {
          # the values it was computed from have been removed
          if (desc$id %in% summed_ids) {
            updateNumericInput(session, desc$id, value = NA)
            summed_ids <<- setdiff(summed_ids, desc$id)
          }
          shinyjs::enable(desc$id)
          next
        }

        updateNumericInput(session, desc$id, value = sum(nums, na.rm = TRUE))
        shinyjs::disable(desc$id)
        summed_ids <<- union(summed_ids, desc$id)
      }
    })
    
    # when requested, prepare the entered data
    form_data <- reactive({
      
      if (dp()) message("Calculating form data")
      
      fields <- relevant_fields()
      is_table <- vapply(fields, function(desc) desc$type == "dataTable",
                         logical(1))
      is_file <- vapply(fields, function(desc) desc$type == "fileInput",
                        logical(1))
      relevant_tables <- lapply(fields[is_table], function(desc) {
        tables[[desc$id]]$result
      })
      names(relevant_tables) <- vapply(fields[is_table],
                                       function(desc) desc$name, character(1))
      relevant_tables <- Filter(Negate(is.null), relevant_tables)
      
      # check that the form and table validation rules have been met
      tables_valid <- vapply(relevant_tables, function(t) isTRUE(t$valid()),
                             logical(1))
      if (!main_iv$is_valid() || !all(tables_valid)) {
        return(NULL)
      }
      
      event <- list()
      # fill information
      event$mgmt_operations_event <- input$mgmt_operations_event
      event$date <- tryCatch(format(input$date, date_format_json),
                             error = function(cnd) "")
      event$mgmt_event_short_notes <- input$mgmt_event_short_notes
      event$block <- input$block

      for (desc in fields[!is_table & !is_file]) {
        value <- input[[desc$id]]
        
        # format Date value to character string and replace with "" if that
        # fails for some reason
        if (inherits(value, "Date")) {
          value <- tryCatch(format(value, date_format_json),
                            error = function(cnd) "")
        }
        
        event[[desc$name]] <- value
        }
        
      # each table is saved as an array of row objects under its property
      for (array_prop_name in names(relevant_tables)) {
        event[[array_prop_name]] <- relevant_tables[[array_prop_name]]$values()
      }
      
      # trim whitespace from text and leave out empty values
      event <- drop_empty_values(
        rapply(event, trimws, classes = "character", how = "replace"))

      # the state of each file field (list(filepath, new_file)). The main app
      # saves an uploaded file and replaces this with the path of the file
      for (desc in fields[is_file]) {
        event[[desc$name]] <- files[[desc$id]]$value()
      }

      # keep the values of the edited event that the form doesn't know about
      # as they are
      unknown <- unknown_values()
      event[names(unknown)] <- unknown
      event
    })
  
    ################## RETURN VALUE
    
    list(
      data = form_data,
      save = reactive(input$save),
      cancel = reactive(input$cancel),
      delete = reactive(input$delete)
    )
    
  })
  
}

# Helper: label of a common property for the selected event type. An event
# type can retitle a common property (e.g. grazing's date is its start date)
# and require it.
common_field_label <- function(schema, prop_name, event_type, iso) {
  entry <- if (isTruthy(event_type)) schema$event_registry[[event_type]]
  desc <- entry$common_overrides[[prop_name]] %||%
    schema$property_registry[[prop_name]]
  make_required_label(schema_get_title(desc$titles, iso, prop_name),
                      prop_name %in% entry$required)
}

# Helper: get the selected subtype of an event type from input
get_current_subtype <- function(input, event_entry) {
  if (!isTRUE(event_entry$has_subtypes)) return(NULL)
  sub_val <- input[[event_entry$subtype_discriminator_id]]
  if (!isTruthy(sub_val)) return(NULL)
  sub_val
}

# Helper: get subtype from event values (for populating form)
get_subtype_value <- function(values, er) {
  event_type <- values$mgmt_operations_event
  if (is.null(event_type)) return(NULL)
  event_entry <- er[[event_type]]
  if (is.null(event_entry) || !event_entry$has_subtypes) return(NULL)
  disc <- event_entry$subtype_discriminator
  if (is.null(disc)) return(NULL)
  values[[disc]]
}
