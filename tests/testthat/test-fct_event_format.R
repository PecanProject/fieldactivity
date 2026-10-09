# Tests for the canonical event format and the legacy upgrade on read
# (fixtures in helper-events.R)

# --- drop_empty_values --------------------------------------------------------

test_that("drop_empty_values removes empty values, rows and lists", {
  x <- list(
    a = "-99.0", b = NA, c = "", d = -99, e = "kept", f = 0,
    items = list(list(x = "-99.0"), list(x = "BAR", y = NA)),
    empty_items = list(list(x = ""))
  )
  expect_equal(drop_empty_values(x),
               list(e = "kept", f = 0, items = list(list(x = "BAR"))))
})

test_that("drop_empty_values keeps vectors that are only partly empty", {
  expect_equal(drop_empty_values(list(v = c("a", "-99.0"))),
               list(v = c("a", "-99.0")))
})

# --- normalize_legacy_event ---------------------------------------------------

test_that("normalize_legacy_event maps legacy notes names", {
  event <- normalize_legacy_event(list(mgmt_event_notes = "hello"))
  expect_equal(event$mgmt_event_short_notes, "hello")
  expect_null(event$mgmt_event_notes)

  for (old_name in c("planting_notes", "harvest_comments", "fertilizer_notes",
                     "tillage_treatment_notes", "chemical_applic_notes",
                     "mowing_notes", "observation_notes", "other_notes")) {
    event <- normalize_legacy_event(stats::setNames(list("notes"), old_name))
    expect_equal(event$mgmt_event_long_notes, "notes", info = old_name)
    expect_null(event[[old_name]])
  }
})

test_that("normalize_legacy_event does not overwrite existing new-name values", {
  event <- normalize_legacy_event(list(
    mgmt_event_notes = "old",
    mgmt_event_short_notes = "new"
  ))
  expect_equal(event$mgmt_event_short_notes, "new")
  expect_equal(event$mgmt_event_notes, "old")
})

test_that("normalize_legacy_event nests parallel vectors into rows", {
  event <- normalize_legacy_event(legacy_events[[1]])

  expect_equal(event$planting_list, list(
    list(planted_crop = "BAR", planting_material_weight = 240),
    list(planted_crop = "RCL", planting_material_weight = 3,
         planting_material_source = "own seeds"),
    list(planted_crop = "ZZ3", planting_material_weight = 20)
  ))
  expect_null(event$planted_crop)
  expect_null(event$planting_depth)
  expect_equal(event$mgmt_event_long_notes, "Mixed grass")
  expect_null(event$mgmt_event_short_notes)
})

test_that("normalize_legacy_event nests single-item scalars into one row", {
  event <- normalize_legacy_event(legacy_events[[2]])

  expect_equal(event$harvest_list, list(
    list(harvest_crop = "ZZ1", harvest_method = "HM008",
         harvest_operat_component = "silage")
  ))
  # event-level properties stay on the event
  expect_equal(event$harvest_yield_harvest_dw_total, 4200)
  expect_null(event$harvest_area)
  expect_null(event$harvest_comments)
})

test_that("normalize_legacy_event maps organic_material to fertilizer", {
  event <- normalize_legacy_event(legacy_events[[3]])
  expect_equal(event$mgmt_operations_event, "fertilizer")
  expect_equal(event$fertilizer_type, "fertilizer_type_organic")
  expect_equal(event$organic_material, "RE003")
  expect_equal(event$mgmt_event_long_notes, "Separator dry fraction")
})

test_that("normalize_legacy_event splits the legacy grazing period", {
  event <- normalize_legacy_event(legacy_events[[6]])
  expect_equal(event$date, "2022-06-01")
  expect_equal(event$end_date, "2022-06-20")
  expect_null(event$grazing_period)

  # without a date, the start of the period is the date
  event <- normalize_legacy_event(list(
    mgmt_operations_event = "grazing",
    grazing_period = c("2022-06-01", "2022-06-20")
  ))
  expect_equal(event$date, "2022-06-01")
  expect_null(event$grazing_period)
})

test_that("normalize_legacy_event keeps a grazing period that starts on another date", {
  event <- normalize_legacy_event(list(
    mgmt_operations_event = "grazing",
    date = "2022-05-30",
    grazing_period = c("2022-06-01", "2022-06-20")
  ))
  expect_equal(event$date, "2022-05-30")
  expect_equal(event$end_date, "2022-06-20")
  expect_equal(event$grazing_period, c("2022-06-01", "2022-06-20"))
})

test_that("normalize_legacy_event keeps keys unknown to the schema", {
  event <- normalize_legacy_event(list(
    mgmt_operations_event = "observation",
    date = "2022-06-01",
    canopeo_image = "canopeo_image/2022-06-01_site_0_canopeo_image_0.jpg"
  ))
  expect_equal(event$canopeo_image,
               "canopeo_image/2022-06-01_site_0_canopeo_image_0.jpg")
})

# --- read / write round trip --------------------------------------------------

test_that("legacy files are written back in the canonical format", {
  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  write_events_file(base_folder, "site", "0", legacy_events)

  events <- read_json_file("site", "0", base_folder = base_folder)$events
  write_json_file("site", "0", events, list(), base_folder = base_folder)

  file_path <- file.path(base_folder, "site", "0", "events.json")
  written <- jsonlite::fromJSON(file_path,
                                simplifyDataFrame = FALSE)$management$events

  expect_length(written, length(legacy_events))
  expect_false(any(grepl("-99", readLines(file_path), fixed = TRUE)))
  expect_length(written[[1]]$planting_list, 3)
  expect_length(written[[2]]$harvest_list, 1)
  expect_equal(written[[3]]$mgmt_operations_event, "fertilizer")
  for (event in written) expect_null(event$block)

  # reading and saving the canonical file again leaves it unchanged
  first_write <- readLines(file_path)
  events <- read_json_file("site", "0", base_folder = base_folder)$events
  write_json_file("site", "0", events, list(), base_folder = base_folder)
  expect_identical(readLines(file_path), first_write)
})

test_that("written events are valid against the management event schema", {
  skip_if_not_installed("jsonvalidate")

  base_folder <- tempfile()
  on.exit(unlink(base_folder, recursive = TRUE))
  write_events_file(base_folder, "site", "0", legacy_events)
  events <- read_json_file("site", "0", base_folder = base_folder)$events
  write_json_file("site", "0", events, list(), base_folder = base_folder)

  schema <- paste(readLines(schema_file_path(), warn = FALSE), collapse = "\n")
  validate <- jsonvalidate::json_validator(schema, engine = "ajv")
  written <- jsonlite::fromJSON(
    file.path(base_folder, "site", "0", "events.json"),
    simplifyDataFrame = FALSE)$management$events

  for (event in written) {
    json <- jsonlite::toJSON(event, auto_unbox = TRUE)
    expect_true(validate(json), info = event$mgmt_operations_event)
  }
})

# --- events_to_table ----------------------------------------------------------

test_that("events_to_table spreads item columns with one row per event", {
  events <- list(
    list(mgmt_operations_event = "planting", date = "2023-05-25",
         planting_list = list(
           list(planted_crop = "BAR", planting_material_weight = 240),
           list(planted_crop = "RCL"))),
    list(mgmt_operations_event = "other", date = "2024-10-09",
         mgmt_event_long_notes = "Plowing, twice",
         `$schema` = "https://example.com/schema.json")
  )
  table <- events_to_table(events)

  expect_equal(nrow(table), 2)
  expect_equal(table$planted_crop, c("BAR; RCL", ""))
  expect_equal(table$planting_material_weight, c("240; ", ""))
  expect_equal(table$mgmt_event_long_notes, c("", "Plowing, twice"))
  expect_false("$schema" %in% names(table))
  expect_false("planting_list" %in% names(table))
})
