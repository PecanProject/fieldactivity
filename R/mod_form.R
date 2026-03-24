# The function of the form module is as follows:
# - contains the widgets for entering the actual information about the event
# - shows the correct widgets depending on the user's choices
# - allows prefilling the widgets with the desired values
# - verifies that the information has been supplied correctly
# - returns the values to the main app in the format they will be saved (e.g. 
#   "" replaced with missingval)
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
  
  tagList(
    fluidRow(
      column(width = 3,
             h3(textOutput(ns("form_title")), 
                style = "margin-bottom = 0px; margin-top = 0px; 
                   margin-block-start = 0px"),
             
             span(textOutput(ns("required_variables_helptext")), 
                  style = "color:gray"),
             br(),
             
             selectInput(ns("block"), label = get_disp_name("block_label", 
                                                            init_lang),
                         choices = ""),
             
             selectInput(ns("mgmt_operations_event"), 
                         label = schema_get_title(
                           lookup_property(mgmt_schema$property_registry, 
                                           "mgmt_operations_event")$titles,
                           iso, "event"),
                         choices = build_event_type_choices(mgmt_schema, iso)
             ),
             
             dateInput(
               ns("date"),
               format = "dd/mm/yyyy",
               label = schema_get_title(
                 lookup_property(mgmt_schema$property_registry, "date")$titles,
                 iso, "date"),
               max = Sys.Date(),
               value = Sys.Date(),
               weekstart = 1
             ),
             
             textAreaInput(
               ns("mgmt_event_short_notes"),
               label = schema_get_title(
                 lookup_property(mgmt_schema$property_registry, 
                                 "mgmt_event_short_notes")$titles,
                 iso, "description"),
               placeholder = schema_get_title(
                 lookup_property(mgmt_schema$property_registry, 
                                 "mgmt_event_short_notes")$placeholders,
                 iso, ""),
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
    ns <- session$ns
    
    if (dp()) message("Initialising form server function")
    
    schema <- mgmt_schema
    er <- schema$event_registry
    pr <- schema$property_registry
    
    main_iv <- InputValidator$new()
    
    all_props <- get_all_schema_properties(schema)
    for (prop_name in all_props) {
      # Try to find descriptor from any event/subtype context
      desc <- find_any_property_desc(pr, prop_name, schema$property_reverse_index)
      if (is.null(desc)) next
      if (desc$type %in% c("const", "dataTable")) next

      iv <- InputValidator$new()
      added_rules <- FALSE
      
      if (isTRUE(desc$required)) {
        iv$add_rule(prop_name, sv_required(message = ""))
        added_rules <- TRUE
      }
      
      if (!is.null(desc$minimum)) {
        iv$add_rule(prop_name, sv_gte(desc$minimum, allow_na = TRUE, 
                                       message_fmt = ""))
        added_rules <- TRUE
      }
      
      if (!is.null(desc$maximum)) {
        iv$add_rule(prop_name, sv_lte(desc$maximum, allow_na = TRUE,
                                       message_fmt = ""))
        added_rules <- TRUE
      }

      if (isTRUE(desc$is_integer)) {
        iv$add_rule(prop_name, function(value) {
          if (is.null(value) || is.na(value)) return(NULL)
          if (value != floor(value)) return("Must be a whole number")
          NULL
        })
        added_rules <- TRUE
      }
      
      if (added_rules) {
        local({
          local_prop <- prop_name
          iv$condition(reactive({ 
            local_prop %in% relevant_variables()$regular 
          }))
        })
        main_iv$add_validator(iv)
      }
    }
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
      
      block_choices <- subset(sites, sites$site == site())$blocks[[1]]
      updateSelectInput(session, "block", choices = block_choices)
    })
    
    # when set_values is changed, update the values in the form
    observeEvent(set_values(), {
      if (dp()) message("Filling the form with values")
      
      values <- set_values()
      iso <- lang_to_iso(language())
      
      relevant <- get_relevant_properties(
        schema, 
        values$mgmt_operations_event %||% "",
        get_subtype_value(values, er)
      )
      
      for (prop_name in relevant$all) {
        desc <- lookup_property(
          pr, prop_name, 
          values$mgmt_operations_event, 
          get_subtype_value(values, er))
        if (is.null(desc)) next
        if (desc$type %in% c("const", "dataTable")) next
        
        value <- values[[prop_name]]
        # Handle legacy property name mapping
        if (is.null(value)) {
          value <- get_legacy_value(values, prop_name)
        }
        
        if (is.null(value)) {
          clear_schema_value(session, prop_name, desc)
        } else {
          update_schema_value(session, prop_name, desc, value)
        }
      }
      
      # Handle array/table data (event-level and subtype-level)
      subtype_val <- get_subtype_value(values, er)
      for (prop_name in c(relevant$event_props, relevant$subtype_props)) {
        desc <- lookup_property(pr, prop_name,
                                              values$mgmt_operations_event,
                                              subtype_val)
        if (!is.null(desc) && desc$type == "dataTable") {
          table_name <- paste0(prop_name, "_table")
          if (!is.null(tables[[table_name]])) {
            tables[[table_name]]$set_values(values)
          }
        }
      }
      
      # Also set common fields explicitly
      if (!is.null(values$block)) {
        updateSelectInput(session, "block", selected = values$block)
      }
      if (!is.null(values$mgmt_operations_event)) {
        updateSelectInput(session, "mgmt_operations_event", 
                          selected = values$mgmt_operations_event)
      }
      if (!is.null(values$date)) {
        date_val <- tryCatch(as.Date(values$date, format = date_format_json),
                             warning = function(cnd) NULL)
        updateDateInput(session, "date", value = date_val)
      }
      if (!is.null(values$mgmt_event_short_notes)) {
        updateTextAreaInput(session, "mgmt_event_short_notes", 
                            value = values$mgmt_event_short_notes)
      }
      # Handle legacy field name for short notes
      if (!is.null(values$mgmt_event_notes) && 
          is.null(values$mgmt_event_short_notes)) {
        updateTextAreaInput(session, "mgmt_event_short_notes", 
                            value = values$mgmt_event_notes)
      }
      
      set_values(NULL)
    })
    
    # when reset_values is signaled, reset the values of all widgets
    observeEvent(reset_values(), {
      if (identical(reset_values(), FALSE)) return()
      if (dp()) message("Resetting form values")
      
      # Reset common properties
      for (pn in schema$common_properties) {
        desc <- lookup_property(pr, pn)
        if (!is.null(desc)) clear_schema_value(session, pn, desc)
      }
      
      # Reset all event properties
      all_props <- get_all_schema_properties(schema)
      for (pn in all_props) {
        desc <- find_any_property_desc(pr, pn, schema$property_reverse_index)
        if (!is.null(desc) && !(desc$type %in% c("const", "dataTable"))) {
          clear_schema_value(session, pn, desc)
        }
      }
      
      # Reset fileInput fields separately
      for (fileInput_code_name in fileInput_code_names) {
        files[[fileInput_code_name]]$reset_path(TRUE)
      }
      
      reset_values(FALSE)
    })
    
    # show Delete button depending on edit mode
    observeEvent(edit_mode(), ignoreNULL = FALSE, {
      shinyjs::toggle("delete", condition = edit_mode())
    })
    
    # text outputs (form_title, required_variables_helptext)
    lapply(text_output_code_names, FUN = function(text_output_code_name) {
      output[[text_output_code_name]] <- renderText({
        if (dp()) message(glue("Rendering text for {text_output_code_name}"))
        
        text_to_show <- get_disp_name(text_output_code_name, language())
        
        element <- structure_lookup_list[[text_output_code_name]]
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
    
    # Language switching for schema-driven form fields
    observeEvent(language(), ignoreInit = TRUE, {
      iso <- lang_to_iso(language())
      
      # Update event type selector
      event_choices <- build_event_type_choices(schema, iso)
      current_event <- input[["mgmt_operations_event"]]
      updateSelectInput(session, "mgmt_operations_event",
                        label = schema_get_title(
                          lookup_property(pr, "mgmt_operations_event")$titles, 
                          iso, "event"),
                        choices = event_choices,
                        selected = current_event)
      
      # Update date label
      updateDateInput(session, "date",
                      label = schema_get_title(
                        lookup_property(pr, "date")$titles, iso, "date"))
      
      # Update short notes
      short_notes_desc <- lookup_property(pr, "mgmt_event_short_notes")
      updateTextAreaInput(session, "mgmt_event_short_notes",
                          label = schema_get_title(
                            short_notes_desc$titles, iso, "description"),
                          placeholder = schema_get_title(
                            short_notes_desc$placeholders, iso, ""))
      
      # Update all event-type properties
      for (ec in names(er)) {
        event_entry <- er[[ec]]
        for (pn in event_entry$property_names) {
          desc <- lookup_property(pr, pn, ec)
          if (is.null(desc)) next
          if (desc$type %in% c("const", "dataTable")) next
          update_schema_widget(session, pn, desc, iso, input)
        }
        
        if (event_entry$has_subtypes) {
          # Update subtype discriminator choices
          disc <- event_entry$subtype_discriminator
          disc_desc <- lookup_property(pr, disc, ec)
          if (!is.null(disc_desc)) {
            sub_choices <- build_subtype_choices(event_entry, iso)
            current_sub <- input[[disc]]
            updateSelectInput(session, disc,
                              label = schema_get_title(
                                disc_desc$titles, iso, disc),
                              choices = sub_choices,
                              selected = current_sub)
          }
          
          for (sc in names(event_entry$subtypes)) {
            sub <- event_entry$subtypes[[sc]]
            for (spn in sub$property_names) {
              sdesc <- lookup_property(pr, spn, ec, sc)
              if (is.null(sdesc)) next
              if (sdesc$type %in% c("const", "dataTable")) next
              update_schema_widget(session, spn, sdesc, iso, input)
            }
          }
        }
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
                              label = ifelse(is.null(label), "", label),
                              selected = current_value) 
          } else {
            updateSelectInput(session, code_name,
                              label = ifelse(is.null(label), "", label),
                              choices = choices,
                              selected = current_value)
          }
        } else if (element$type == "actionButton") {
          updateActionButton(session, code_name, label = label)
        }
      }
    })
    
    # Initialise table and fileInput module servers on first use
    tables <- list()
    files <- sapply(fileInput_code_names, USE.NAMES = TRUE, simplify = FALSE, 
                    FUN = function(fileInput_code_name) {
                      set_path <- reactiveVal()
                      reset_path <- reactiveVal()
                      list(set_path = set_path, reset_path = reset_path)
                    })
    
    # Build the tables list with set_values reactiveVals for each array table
    schema_table_names <- get_schema_table_names(schema)
    for (tn in schema_table_names) {
      set_values_rv <- reactiveVal()
      tables[[tn]] <- list(set_values = set_values_rv)
    }
    
    observeEvent(init_signal(), {
      if (dp()) message("Initialising table and fileInput server functions")
      
      for (tn in schema_table_names) {
        # Find the array property this table corresponds to
        array_prop_name <- sub("_table$", "", tn)
        # Find which event/subtype this belongs to
        found <- FALSE
        for (ec in names(er)) {
          # Check event-level properties
          desc <- lookup_property(pr, array_prop_name, ec)
          if (!is.null(desc) && desc$type == "dataTable") {
            tables[[tn]]$result <<-
              mod_table_server_schema(tn, array_prop_name, desc, schema,
                                      language,
                                      tables[[tn]]$set_values,
                                      input, main_iv, ns)
            found <- TRUE
            break
          }
          # Check subtype-level properties
          if (er[[ec]]$has_subtypes) {
            for (sc in names(er[[ec]]$subtypes)) {
              desc <- lookup_property(pr, array_prop_name, ec, sc)
              if (!is.null(desc) && desc$type == "dataTable") {
                tables[[tn]]$result <<-
                  mod_table_server_schema(tn, array_prop_name, desc, schema,
                                          language,
                                          tables[[tn]]$set_values,
                                          input, main_iv, ns)
                found <- TRUE
                break
              }
            }
          }
          if (found) break
        }
      }
      
      # start server for all fileInput modules
      sapply(fileInput_code_names, FUN = function(fileInput_code_name) {
        files[[fileInput_code_name]]$value <<-  
          mod_fileInput_server(id = fileInput_code_name, 
                               language = language,
                               set_path = files[[fileInput_code_name]]$set_path,
                               reset_path = files[[fileInput_code_name]]$reset_path)
      })
    })
    
    # Auto-sum: update total fields from table column values
    observe({
      event_type <- input[["mgmt_operations_event"]]
      if (!isTruthy(event_type)) return()

      event_entry <- er[[event_type]]
      if (is.null(event_entry)) return()

      # Collect all property names including subtypes
      subtype <- get_current_subtype(input, er)
      all_props <- event_entry$property_names
      if (event_entry$has_subtypes && !is.null(subtype) &&
          !is.null(event_entry$subtypes[[subtype]])) {
        all_props <- c(all_props, event_entry$subtypes[[subtype]]$property_names)
      }

      for (pn in all_props) {
        desc <- lookup_property(pr, pn, event_type, subtype)
        if (is.null(desc) || is.null(desc$total_of)) next

        list_name <- desc$total_of$list_name
        prop_to_sum <- desc$total_of$property_name
        tn <- paste0(list_name, "_table")

        if (is.null(tables[[tn]]$result)) next

        col_values <- tables[[tn]]$result$values()[[prop_to_sum]]
        if (is.null(col_values) || all(is.na(col_values))) {
          total <- NA
        } else {
          nums <- suppressWarnings(as.numeric(col_values))
          total <- sum(nums, na.rm = TRUE)
          if (total == 0 && all(is.na(nums))) total <- NA
        }

        updateNumericInput(session, pn, value = total)
        shinyjs::disable(pn)
      }
    })

    form_data <- reactive({
      if (dp()) message("Calculating form data")
      
      relevant <- relevant_variables()
      
      relevant_table <- NULL
      if (length(relevant$table_name) > 0 && !is.null(tables[[relevant$table_name]])) {
        relevant_table <- tables[[relevant$table_name]]$result
      }
      
      if (!main_iv$is_valid() || 
          (!is.null(relevant_table) && !relevant_table$valid())) {
        return(NULL)
      }
      
      event <- list()
      
      event$mgmt_operations_event <- input[["mgmt_operations_event"]]
      event$date <- tryCatch(
        format(input[["date"]], date_format_json),
        error = function(cnd) ""
      )
      event$mgmt_event_short_notes <- trimws(input[["mgmt_event_short_notes"]] %||% "")
      event$block <- input[["block"]]
      
      for (prop_name in relevant$regular) {
        if (prop_name %in% c("mgmt_operations_event", "date", 
                              "mgmt_event_short_notes", "block")) next
        
        desc <- lookup_property(
          pr, prop_name, 
          input[["mgmt_operations_event"]],
          get_current_subtype(input, er))
        if (is.null(desc)) next
        if (desc$type == "const") next
        
        is_fileInput <- !is.null(structure_lookup_list[[prop_name]]) &&
          identical(structure_lookup_list[[prop_name]]$type, "fileInput")
        
        value <- if (is_fileInput) {
          files[[prop_name]]$value()
        } else {
          input[[prop_name]]
        }
        
        if (is.character(value)) value <- trimws(value)
        
        if (inherits(value, "Date")) {
          value <- tryCatch(format(value, date_format_json),
                            error = function(cnd) "")
        }
        
        if (length(value) == 0) {
          value <- missingval
        } else {
          missing_indexes <- is.na(value) | value == ""
          if (any(missing_indexes)) {
            value[missing_indexes] <- missingval
          }
        }
        
        event[[prop_name]] <- value
      }
      
      if (!is.null(relevant_table)) {
        table_values <- relevant_table$values()
        for (vn in names(table_values)) {
          val <- table_values[[vn]]
          if (length(val) == 0) {
            val <- missingval
          } else {
            missing_indexes <- is.na(val) | val == ""
            if (any(missing_indexes)) val[missing_indexes] <- missingval
          }
          event[[vn]] <- val
        }
      }
      
      # Replace empty common fields
      for (fn in c("mgmt_event_short_notes")) {
        if (is.null(event[[fn]]) || identical(event[[fn]], "")) {
          event[[fn]] <- missingval
        }
      }
      
      event
    })
    
    relevant_variables <- reactive({
      if (dp()) message("Calculating relevant variables")
      
      event_type <- input[["mgmt_operations_event"]]
      if (!isTruthy(event_type)) {
        return(list(regular = character(0), table = character(0), 
                    table_name = character(0)))
      }
      
      event_entry <- er[[event_type]]
      if (is.null(event_entry)) {
        return(list(regular = character(0), table = character(0), 
                    table_name = character(0)))
      }
      
      subtype <- get_current_subtype(input, er)
      relevant <- get_relevant_properties(schema, event_type, subtype)
      
      regular_props <- character(0)
      table_props <- character(0)
      table_name <- character(0)
      
      for (pn in relevant$all) {
        desc <- lookup_property(pr, pn, event_type, subtype)
        if (is.null(desc)) next
        if (desc$type == "const") next
        
        # Skip fields whose x-ui condition evaluates to FALSE
        if (!is.null(desc$xui$condition)) {
          cond_result <- evaluate_condition(desc$xui$condition, session)
          if (is.null(cond_result) || !isTRUE(cond_result)) next
        }

        if (desc$type == "dataTable") {
          tn <- paste0(pn, "_table")
          table_name <- c(table_name, tn)
          # Get the table column names
          if (!is.null(desc$array_columns)) {
            table_props <- c(table_props, names(desc$array_columns))
          }
        } else {
          regular_props <- c(regular_props, pn)
        }
      }
      
      list(
        regular = regular_props,
        table = table_props,
        table_name = if (length(table_name) > 0) table_name[1] else character(0)
      )
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

# Helper: find a property descriptor trying multiple contexts.
# Uses the reverse index (built in load_schema) for O(1) lookup instead of
# scanning every registry key.
find_any_property_desc <- function(pr, prop_name, reverse_index = NULL) {
  # Direct common lookup
  if (!is.null(pr[[prop_name]])) return(pr[[prop_name]])
  # Use reverse index if available (O(1) instead of O(n))
  if (!is.null(reverse_index) && !is.null(reverse_index[[prop_name]])) {
    return(pr[[reverse_index[[prop_name]]]])
  }
  # Fallback: linear scan (for callers without the index)
  for (key in names(pr)) {
    if (startsWith(key, paste0(prop_name, REGISTRY_KEY_SEP))) {
      return(pr[[key]])
    }
  }
  NULL
}

# Helper: get the current subtype from input
get_current_subtype <- function(input, er) {
  event_type <- input[["mgmt_operations_event"]]
  if (!isTruthy(event_type)) return(NULL)
  event_entry <- er[[event_type]]
  if (is.null(event_entry) || !event_entry$has_subtypes) return(NULL)
  disc <- event_entry$subtype_discriminator
  if (is.null(disc)) return(NULL)
  sub_val <- input[[disc]]
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

# Helper: get schema table names
get_schema_table_names <- function(schema) {
  er <- schema$event_registry
  pr <- schema$property_registry
  table_names <- character(0)
  for (ec in names(er)) {
    event_entry <- er[[ec]]
    for (pn in event_entry$property_names) {
      desc <- lookup_property(pr, pn, ec)
      if (!is.null(desc) && desc$type == "dataTable") {
        table_names <- c(table_names, paste0(pn, "_table"))
      }
    }
    # Also check subtype properties (e.g. soil_layer_list in observation_type_soil)
    if (event_entry$has_subtypes) {
      for (sc in names(event_entry$subtypes)) {
        for (spn in event_entry$subtypes[[sc]]$property_names) {
          sdesc <- lookup_property(pr, spn, ec, sc)
          if (!is.null(sdesc) && sdesc$type == "dataTable") {
            table_names <- c(table_names, paste0(spn, "_table"))
          }
        }
      }
    }
  }
  unique(table_names)
}

get_legacy_value <- function(values, schema_prop_name) {
  legacy_names <- names(legacy_name_map)[legacy_name_map == schema_prop_name]
  for (ln in legacy_names) {
    if (!is.null(values[[ln]])) return(values[[ln]])
  }
  NULL
}
