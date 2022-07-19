test_that("is download functional", {
  # Check that the size of guide makes sense when it is downloaded
  testServer(mod_download_server_inst, {
    #stopifnot(grepl(".html", output$report))
    print(output$report)
    expect_true(grepl("guideFieldactivity.html", output$report))
    expect_true(file.info(output$report)$size > 10000)
  })
  
  # Check that file exist when downloading csv
  testServer(mod_download_server_table, args = list(user_auth = "qvidja"), {
    expect_true(file.exists(output$eventtable))
    expect_true(file.size(output$eventtable) > 70)
  })
  
  
  # Check that file exist when downloading zip
  testServer(mod_download_server_json, args = list(user_auth = "qvidja"), {
    expect_true(file.exists(output$eventjson))
    expect_true(grepl(".zip", output$eventjson))
    
    
    data <- unzip(output$eventjson, list=TRUE)
    expect_true(length(data) > 1)
  })
})
