# Regression coverage for optional, per-crop sowing density (issue #55).
sowing_rows <- function() {
  list(
    list(planted_crop = "BAR", planting_density = 0),
    list(planted_crop = "OAT", planting_material_weight = 25),
    list(planted_crop = "RCL", planting_material_weight = 5,
         planting_density = 125.5)
  )
}

sowing_event <- function(rows = sowing_rows()) {
  list(mgmt_operations_event = "planting", date = "2022-06-01",
       planting_list = rows)
}

sowing_desc <- function() {
  lookup_property(mgmt_schema$property_registry, "planting_list", "planting")
}

# Model the inputs sent by the browser after the crop table has been bound.
set_sowing_inputs <- function(session, rows, prefix = "") {
  inputs <- list(rendered = TRUE)
  for (i in seq_along(rows)) {
    for (column in names(sowing_desc()$array_columns)) {
      desc <- sowing_desc()$array_columns[[column]]
      value <- rows[[i]][[column]] %||%
        if (desc$type == "numericInput") NA_real_ else ""
      inputs[[paste(column, i, sep = "_")]] <- value
    }
  }
  names(inputs) <- paste0(prefix, names(inputs))
  do.call(session$setInputs, inputs)
}

test_that("sowing density is an optional numeric crop column with translated units", {
  columns <- sowing_desc()$array_columns
  expect_equal(names(columns)[2:3],
               c("planting_material_weight", "planting_density"))
  density <- columns$planting_density
  expect_equal(density$type, "numericInput")
  expect_false(density$required)
  expect_false(density$is_integer)
  expect_equal(density$minimum, 0)
  expect_null(density$maximum)
  expect_equal(density$xui$unit, "m\u207b\u00b2")
  expect_false(columns$planting_material_weight$required)

  titles <- c(en = "Sowing density (plants/m\u00b2)",
              fi = "Kylv\u00f6tiheys (kasveja/m\u00b2)",
              sv = "Uts\u00e4dest\u00e4thet (plantor/m\u00b2)")
  for (iso in names(titles)) {
    expect_equal(schema_get_title(density$titles, iso), unname(titles[[iso]]))
  }
  expect_equal(get_disp_name("planting_density", "disp_name_eng", TRUE),
               unname(titles[["en"]]))
  expect_equal(get_disp_name("planting_density", "disp_name_fin", TRUE),
               unname(titles[["fi"]]))
})

test_that("sowing schema accepts either measurement and rejects invalid density", {
  skip_if_not_installed("jsonvalidate")
  validate <- jsonvalidate::json_validator(schema_file_path(), engine = "ajv")
  valid_rows <- list(
    list(planted_crop = "BAR"),
    list(planted_crop = "BAR", planting_material_weight = 25),
    list(planted_crop = "BAR", planting_density = 0),
    list(planted_crop = "BAR", planting_density = 125.5),
    list(planted_crop = "BAR", planting_material_weight = 5,
         planting_density = 125.5)
  )
  for (row in valid_rows) {
    expect_true(validate(jsonlite::toJSON(sowing_event(list(row)),
                                          auto_unbox = TRUE)))
  }
  for (value in list(-1, "125.5", "not a number")) {
    event <- sowing_event(list(list(planted_crop = "BAR",
                                    planting_density = value)))
    expect_false(validate(jsonlite::toJSON(event, auto_unbox = TRUE)))
  }
})

test_that("sowing table keeps zero and crop alignment across language and row changes", {
  overrides <- reactiveVal(sowing_rows())
  lang <- reactiveVal("disp_name_eng")
  testServer(mod_table_server,
             args = list(desc = sowing_desc(), language = lang,
                         override_values = overrides), {
    session$flushReact()
    cells <- table_data()
    expect_match(cells[1, "planting_density"], 'value="0"', fixed = TRUE)
    expect_match(cells[3, "planting_density"], 'value="125.5"', fixed = TRUE)
    set_sowing_inputs(session, sowing_rows())
    expect_equal(drop_empty_values(session$returned$values()), sowing_rows())

    for (language in c("disp_name_fin", "disp_name_swe", "disp_name_eng")) {
      lang(language)
      session$flushReact()
      cells <- table_data()
      expect_match(cells[1, "planting_density"], 'value="0"', fixed = TRUE)
      expect_match(cells[3, "planting_density"], 'value="125.5"', fixed = TRUE)
      session$setInputs(rendered = TRUE)
      expect_equal(drop_empty_values(session$returned$values()), sowing_rows())
    }

    session$setInputs(add_row = 1L)
    expect_equal(dynamic_rows(), 1:4)
    rows <- c(sowing_rows(), list(list(planted_crop = "BAR", planting_density = 8)))
    set_sowing_inputs(session, rows)
    session$setInputs(remove_row_index = 2L)
    cells <- table_data()
    expect_equal(dynamic_rows(), c(1L, 3L, 4L))
    expect_match(cells[2, "planting_density"], 'value="125.5"', fixed = TRUE)
    set_sowing_inputs(session, rows[-2])
    session$setInputs(planting_density_2 = 130.5)
    rows[[3]]$planting_density <- 130.5
    expect_equal(drop_empty_values(session$returned$values()), rows[-2])
  })
})

test_that("sowing form saves optional densities and prevents negative values", {
  args <- list(site = reactive(NULL), set_values = reactiveVal(),
               reset_values = reactiveVal(), edit_mode = reactive(FALSE),
               language = reactive("disp_name_eng"), init_signal = reactiveVal())
  testServer(mod_form_server, args = args, {
    init_signal(TRUE)
    set_values(sowing_event())
    session$flushReact()
    session$setInputs(mgmt_operations_event = "planting", block = "0",
                      date = as.Date("2022-06-01"))
    prefix <- paste0(schema_table_id(sowing_desc()), "-")
    set_sowing_inputs(session, sowing_rows(), prefix)
    expect_equal(form_data()$planting_list, sowing_rows())
    expect_null(form_data()$planting_density)

    do.call(session$setInputs,
            stats::setNames(list(-1), paste0(prefix, "planting_density_1")))
    expect_null(form_data())
    set_sowing_inputs(session, sowing_rows(), prefix)
    expect_equal(form_data()$planting_list, sowing_rows())

    # Editing an older event must not inherit densities from the previous one.
    old_rows <- list(list(planted_crop = "BAR", planting_material_weight = 25))
    set_values(sowing_event(old_rows))
    session$flushReact()
    set_sowing_inputs(session, old_rows, prefix)
    expect_equal(form_data()$planting_list, old_rows)
  })
})

test_that("saving and reopening sowing events preserves density and old records", {
  base_folder <- tempfile()
  dir.create(base_folder)
  on.exit(unlink(base_folder, recursive = TRUE))
  old_event <- sowing_event(list(list(planted_crop = "OAT",
                                     planting_material_weight = 25)))
  events <- list(sowing_event(), old_event)
  write_json_file("site", "0", events, list(), base_folder = base_folder)
  reopened <- read_json_file("site", "0", base_folder = base_folder)$events
  expect_equal(reopened[[1]]$planting_list, sowing_rows())
  expect_equal(reopened[[2]]$planting_list, old_event$planting_list)
  expect_null(reopened[[1]]$planting_list[[2]]$planting_density)
  reopened[[1]]$planting_list[[3]]$planting_density <- 130.5
  write_json_file("site", "0", reopened, list(), base_folder = base_folder)
  edited <- read_json_file("site", "0", base_folder = base_folder)$events
  expect_equal(edited[[1]]$planting_list[[1]]$planting_density, 0)
  expect_equal(edited[[1]]$planting_list[[3]]$planting_density, 130.5)
  expect_equal(edited[[2]], reopened[[2]])
})

test_that("sowing downloads preserve zero, blank alignment and raw CSV names", {
  base_folder <- tempfile()
  dir.create(base_folder)
  on.exit(unlink(base_folder, recursive = TRUE))
  write_json_file("site", "0", list(sowing_event()), list(),
                  base_folder = base_folder)
  args <- list(user_auth = reactive("site"), base_folder = base_folder)
  testServer(mod_download_server_table, args = args, {
    csv <- utils::read.csv(output$eventtable, colClasses = "character")
    expect_equal(csv$planting_density, "0; ; 125.5")
    expect_equal(csv$planted_crop, "BAR; OAT; RCL")
    expect_equal(csv$planting_material_weight, "; 25; 5")
    expect_false("Sowing density (plants/m\u00b2)" %in% names(csv))
  })
  testServer(mod_download_server_json, args = args, {
    extracted <- tempfile()
    on.exit(unlink(extracted, recursive = TRUE), add = TRUE)
    utils::unzip(output$eventjson, exdir = extracted)
    events <- jsonlite::fromJSON(file.path(extracted, "json", "events_0.json"),
                                 simplifyVector = FALSE)$management$events
    expect_equal(events[[1]]$planting_list, sowing_rows())
  })

  write_json_file("site", "0", list(sowing_event(list(list(planted_crop = "BAR")))),
                  list(), base_folder = base_folder)
  testServer(mod_download_server_table, args = args, {
    expect_false("planting_density" %in% names(utils::read.csv(output$eventtable)))
  })
})

test_that("sowing events with density remain selectable in the existing event list", {
  event <- sowing_event()
  event$block <- "0"
  testServer(mod_event_list_server,
             args = list(events = reactive(list("0" = list(event))),
                         language = reactive("disp_name_eng"),
                         site = reactive(NULL)), {
    session$setInputs(event_list_activity_filter = "planting",
                      event_list_block_filter = "block_choice_all",
                      event_list_year_filter = "year_choice_all")
    expect_equal(nrow(table_data()), 1)
    expect_false(any(c("planting_list", "planting_density") %in% names(table_data())))
    expect_true(is.character(output$table))
    session$setInputs(table_rows_selected = 1L)
    expect_equal(current_event()$planting_list, sowing_rows())
  })
})
