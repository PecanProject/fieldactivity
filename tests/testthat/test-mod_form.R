# Tests for the schema-driven form module

form_module_args <- function() {
  list(site = reactive(NULL), set_values = reactiveVal(),
       reset_values = reactiveVal(), edit_mode = reactive(FALSE),
       language = reactive("disp_name_eng"), init_signal = reactiveVal())
}

field_id <- function(prop_name, event_type, subtype = NULL) {
  lookup_property_key(mgmt_schema$property_registry, prop_name, event_type,
                      subtype)
}

# set the value of a field whose id is only known at run time
set_field <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
}

test_that("an event without an event type is not valid", {
  testServer(mod_form_server, args = form_module_args(), {
    session$setInputs(block = "0", date = Sys.Date())
    expect_null(form_data())
  })
})

test_that("required fields are validated for the selected event type", {
  testServer(mod_form_server, args = form_module_args(), {
    # long notes are required for "other" but not for planting, which has a
    # widget of its own for them
    session$setInputs(mgmt_operations_event = "other", block = "0",
                      date = Sys.Date())
    expect_null(form_data())

    set_field(session, field_id("mgmt_event_long_notes", "other"),
              "  Plowing ")
    event <- form_data()
    expect_equal(event$mgmt_event_long_notes, "Plowing")
    expect_equal(event$mgmt_operations_event, "other")
  })
})

test_that("only the fields of the selected event type are saved", {
  testServer(mod_form_server, args = form_module_args(), {
    set_field(session, field_id("mgmt_event_long_notes", "planting"),
              "planting notes")
    set_field(session, field_id("mgmt_event_long_notes", "weeding"),
              "weeding notes")
    session$setInputs(mgmt_operations_event = "weeding", block = "0",
                      date = Sys.Date())
    event <- form_data()
    expect_equal(event$mgmt_event_long_notes, "weeding notes")
  })
})

test_that("the date is required only when the event type requires it", {
  testServer(mod_form_server, args = form_module_args(), {
    session$setInputs(mgmt_operations_event = "weeding", block = "0")
    expect_null(form_data())

    session$setInputs(mgmt_operations_event = "measurement")
    expect_false(is.null(form_data()))
  })
})

test_that("keys the form doesn't know about are kept when saving", {
  testServer(mod_form_server, args = form_module_args(), {
    set_values(list(mgmt_operations_event = "weeding", date = "2022-06-01",
                    weeding_tool = "hoe"))
    session$flushReact()
    session$setInputs(mgmt_operations_event = "weeding", block = "0",
                      date = as.Date("2022-06-01"))

    expect_equal(form_data()$weeding_tool, "hoe")

    # a new event doesn't inherit them
    reset_values(TRUE)
    session$flushReact()
    expect_null(form_data()$weeding_tool)
  })
})

test_that("tables are filled for the edited event and cleared otherwise", {
  testServer(mod_form_server, args = form_module_args(), {
    harvest_id <- field_id("harvest_list", "harvest")
    planting_id <- field_id("planting_list", "planting")
    rows <- list(list(harvest_crop = "ZZ1"))

    set_values(list(mgmt_operations_event = "harvest", harvest_list = rows))
    session$flushReact()
    expect_equal(tables[[harvest_id]]$set_values(), rows)
    expect_equal(tables[[planting_id]]$set_values(), list())

    reset_values(TRUE)
    session$flushReact()
    expect_equal(tables[[harvest_id]]$set_values(), list())
  })
})

test_that("table rows are saved nested under the array property", {
  testServer(mod_form_server, args = form_module_args(), {
    init_signal(TRUE)
    session$setInputs(mgmt_operations_event = "harvest", block = "0",
                      date = Sys.Date())
    table_id <- schema_table_id(
      lookup_property(mgmt_schema$property_registry, "harvest_list",
                      "harvest"))
    inputs <- list("rendered" = TRUE, "harvest_crop_1" = "ZZ1",
                   "harvest_method_1" = "HM008")
    names(inputs) <- paste(table_id, names(inputs), sep = "-")
    do.call(session$setInputs, inputs)

    event <- form_data()
    expect_equal(event$harvest_list,
                 list(list(harvest_crop = "ZZ1", harvest_method = "HM008")))
    expect_null(event$harvest_crop)
  })
})

test_that("the image of the edited event is passed on for saving", {
  testServer(mod_form_server, args = form_module_args(), {
    init_signal(TRUE)
    set_values(list(mgmt_operations_event = "observation",
                    observation_type = "observation_type_vegetation",
                    date = "2022-06-01",
                    canopeo_image = "canopeo_image/image.jpg"))
    session$flushReact()
    session$setInputs(mgmt_operations_event = "observation", block = "0",
                      date = as.Date("2022-06-01"))
    set_field(session, field_id("observation_type", "observation"),
              "observation_type_vegetation")

    # the main app moves the file if needed and saves its path
    expect_equal(form_data()$canopeo_image,
                 list(filepath = "canopeo_image/image.jpg", new_file = FALSE))

    # a new event has no image
    reset_values(TRUE)
    session$flushReact()
    expect_null(form_data()$canopeo_image$filepath)
  })
})
