# Tests for reading and writing events.json files

test_that("saved events can be found again in their events.json file", {
  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  # legacy events have no $schema, which writing adds to every event
  write_events_file(base_folder, "site", "0", legacy_events)
  events <- read_json_file("site", "0", base_folder = base_folder)$events

  # events as the form returns them: without $schema, numbers as doubles and
  # an empty image field as the missing value
  events[[4]] <- list(mgmt_operations_event = "tillage", date = "2023-05-25",
                      block = "0", tillage_operations_depth = 20)
  events[[7]] <- list(mgmt_operations_event = "observation",
                      date = "2023-06-01", block = "0",
                      canopeo_image = missingval)
  saved <- save_block_events("site", "0", events, list(),
                             base_folder = base_folder)

  on_disk <- read_json_file("site", "0", base_folder = base_folder)$events
  expect_length(saved, length(events))
  for (i in seq_along(saved)) {
    expect_identical(find_event_index(saved[[i]], on_disk), i)
  }
})

test_that("numbers are written at full precision", {
  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  dir.create(base_folder)

  event <- list(mgmt_operations_event = "tillage", date = "2023-05-25",
                block = "0", tillage_operations_depth = 12.345678)
  saved <- save_block_events("site", "0", list(event), list(),
                             base_folder = base_folder)
  expect_identical(saved[[1]]$tillage_operations_depth, 12.345678)

  event$tillage_operations_depth <- 0.00004
  saved <- save_block_events("site", "0", list(event), list(),
                             base_folder = base_folder)
  expect_identical(saved[[1]]$tillage_operations_depth, 0.00004)
})
