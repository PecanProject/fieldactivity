test_that("display name file is available", {
  
  file_exists <- file.exists(app_sys("extdata", "display_names.csv"))
  expect_true(file_exists)
  
})

test_that("code names are translated correctly", {
  
  # load display name file
  namedata <- read.csv(app_sys("extdata", "display_names.csv"), 
                       comment.char = "#")
  # take ten random row numbers
  rows <- sample(x = nrow(namedata), size = 10)
  # get code names translations and whether they are variable names or not
  code_names <- namedata[rows, "code_name"]
  finnish <- namedata[rows, "disp_name_fin"]
  english <- namedata[rows, "disp_name_eng"]
  is_variable_name <- (namedata$category == "variable_name")[rows]
  
  for (i in 1:10) {
    # is Finnish correct?
    expect_equal(get_disp_name(code_names[i], 
                               "disp_name_fin", 
                               is_variable_name = is_variable_name[i]),
                 finnish[i])
    # is English correct?
    expect_equal(get_disp_name(code_names[i], 
                               "disp_name_eng", 
                               is_variable_name = is_variable_name[i]),
                 english[i])
    
  }
  
})