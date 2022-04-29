#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # Define the UI of the application
  ui <- fluidPage(#theme = bslib::bs_theme(),
    
    # adding the language to the left side and sitename to the right side on top of the application.
    # Also added a few pixels on top, because otherwise sitename would be attached to the head of app.
    fluidRow(
      column(width = 1, offset = 10, div(style = "height:10px;background-color: white;", " "))),
    fluidRow(
      column(width = 2, selectInput("language", choices = languages, width = "120px", label = "")),
      column(width = 2, offset = 8, textInput("uservisible", value = " ", width = "175px",
                                              label = textOutput("uservisible_title")))
      ),
    
    
    # adding "" to the choices makes the default choice empty
    shinyjs::hidden(selectInput("site", 
                                label = get_disp_name("site_label", init_lang), 
                                choices = c("", sites$site))),
    
    # set web page title
    titlePanel("", windowTitle = "Field Observatory"),
    

    # title to be displayed on the page
    h1(textOutput("frontpage_title")),
    
    # show instructions
    div(style = "max-width:500px;", textOutput("frontpage_text")),
    
    h2(textOutput("event_list_title")),
    
    # event list module UI
    mod_event_list_ui("event_list"),
    
    # add a little space between the elements
    br(),
    
    actionButton("add_event", label = get_disp_name("add_event_label", init_lang)), 
    shinyjs::disabled(actionButton("clone_event", 
                             label = get_disp_name("clone_event_label", init_lang))),
    
    br(),
    br(),
    
    # add form for entering and viewing information
    shinyjs::hidden(div(id = "form_panel", wellPanel(
      mod_form_ui("form")
    )))
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
      app_title = 'fieldactivity',
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),  # enable shinyjs
  )
}

