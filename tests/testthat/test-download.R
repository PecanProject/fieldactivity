test_that("is download functional", {
  # Check that the size of guide makes sense when it is downloaded
  testServer(mod_download_server_inst, {
    expect_true(grepl("guideFieldactivity.html", output$report))
    expect_true(file.info(output$report)$size > 10000)
  })
})

test_that("csv export has one row per event in the canonical format", {
  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  write_events_file(base_folder, "site", "0", legacy_events[1:2])
  write_events_file(base_folder, "site", "1", legacy_events[5])

  testServer(mod_download_server_table,
             args = list(user_auth = reactive("site"),
                         base_folder = base_folder), {
    table <- utils::read.csv(output$eventtable, colClasses = "character")
    expect_equal(nrow(table), 3)
    expect_equal(names(table)[1], "block")
    expect_equal(table$block, c("0", "0", "1"))
    expect_equal(table$planted_crop[1], "BAR; RCL; ZZ3")
    expect_equal(table$mgmt_event_long_notes[3], "Plowing.")
    expect_false(any(grepl("-99", unlist(table), fixed = TRUE)))
  })
})

test_that("json export zips the events file of each block", {
  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  write_events_file(base_folder, "site", "0", legacy_events[1:2])
  write_events_file(base_folder, "site", "1", legacy_events[5])
  write_events_file(base_folder, "other_site", "0", legacy_events[3])

  testServer(mod_download_server_json,
             args = list(user_auth = reactive("site"),
                         base_folder = base_folder), {
    files <- zip::zip_list(output$eventjson)$filename
    expect_setequal(files, c("json/", "json/events_0.json",
                             "json/events_1.json"))
  })
})

test_that("exports without data explain that there is none", {
  testServer(mod_download_server_table,
             args = list(user_auth = reactive(NULL), base_folder = tempfile()), {
    expect_match(paste(readLines(output$eventtable), collapse = ""),
                 "there isn't any data")
  })
  testServer(mod_download_server_json,
             args = list(user_auth = reactive(NULL), base_folder = tempfile()), {
    expect_equal(zip::zip_list(output$eventjson)$filename,
                 c("json/", "json/Error.csv"))
  })
})
