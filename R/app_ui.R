#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # Define the UI of the application
  ui <- fluidPage(#theme = bslib::bs_theme(version = 4, bootswatch = "lumen"),

    selectInput("language", choices = languages, width = "120px", label = ""),
    
    # adding "" to the choices makes the default choice empty
    shinyjs::hidden(selectInput("site", label = "", 
                                choices = c("", sites$site))),
    
    # set web page title
    titlePanel("", windowTitle = "Field Observatory"),
    
    # title to be displayed on the page
    h1(textOutput("window_title")),
    
    # show instructions
    div(style = "max-width:500px;", textOutput("frontpage_text")),
    
    h2(textOutput("frontpage_table_title")),
    
    # selector to filter table data
    div(style="display: inline-block;vertical-align:middle;",
        textOutput("table_filter_text_1", inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput("table_activity", label = "", choices = c(""), 
                    width = "150px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput("table_filter_text_2", inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput("table_block", label = "", choices = c(""),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput("table_filter_text_3", inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput("table_year", label = "", choices = c(""),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;", "."),
    
    # front page data table
    DT::dataTableOutput("mgmt_events_table"),
    
    # add a little space between the elements
    br(),
    
    actionButton("add_event", label = ""), 
    shinyjs::disabled(actionButton("clone_event", label = "")),
    
    br(),
    br(),
    
    # create a sidebar layout
    #shinyjs::hidden(div(id = "sidebar", sidebarLayout(
    shinyjs::hidden(div(id = "sidebar", wellPanel(
      # the sidebar contains the selectors for entering information
      # about the event
      #sidebarPanel(width = 12,
      fluidRow(
        column(width = 3,
               h3(textOutput("sidebar_title"), 
                  style = "margin-bottom = 0px; margin-top = 0px; 
                   margin-block-start = 0px"),
               
               # in general the choices and labels don't have to be 
               # defined for  selectInputs, as they will be 
               # populated when the language is changed 
               # (which also happens when the app starts)
               
               span(textOutput("required_variables_helptext"), 
                    style = "color:gray"),
               br(),
               
               selectInput("block", label = "", choices = ""),
               
               selectInput("mgmt_operations_event", label = "", 
                           choices = ""),
               
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
               )
        ),
        
        column(width = 9, 
               # show a detailed options panel for the 
               # different activities
               # activity_options is defined in ui_builder.R
               create_ui(activity_options, create_border = FALSE)
        )),
      
      fluidRow(
        column(width = 12,
               actionButton("save", label = "Save"),
               
               actionButton("cancel", label = "Cancel"),
               
               shinyjs::hidden(actionButton("delete", label = "Delete", 
                                            class = "btn-warning"))
        ))
    )
    ))
  )
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    ui
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fieldactivity'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),  # enable shinyjs
  )
}

