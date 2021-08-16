# library(shinytest)
# 
# # a helper function which creates a new app for testing the table module
# create_table_test_app <- function(table_code_name) {
#   
#   row_variable <- "harvest_crop"
#   
#   ui <- fluidPage(mod_table_ui(id = table_code_name), 
#                   selectInput(row_variable, label = row_variable, 
#                               choices = c("FRG", "WHT", "BAR"), multiple = TRUE),
#                   selectInput("language", label = "lang",
#                               choices = c("disp_name_eng", "disp_name_fin")))
#   
#   server <- function(input, output, session) {
#     
#     override_values <- shiny::reactiveVal()
#     
#     mod_table_server(table_code_name, 
#                      row_variable_value = reactive(input$harvest_crop),
#                      language = reactive(input$language),
#                      override_values = override_values)
#   }
#   
#   shinytest::ShinyDriver$new(shiny::shinyApp(ui, server))
#   
# }
# 
# test_that("harvest_crop_table works", {
#   app <- create_table_test_app("harvest_crop_table")
#   
#   app$setInputs(language = "disp_name_fin", harvest_crop = c("FRG", "WHT"))
# 
#   expect_equal(TRUE, TRUE)
# })
