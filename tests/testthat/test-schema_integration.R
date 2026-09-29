# Integration tests against the real management-event schema

test_that("$schema is not a form field", {
  expect_false("$schema" %in% mgmt_schema$common_properties)
})

test_that("an event with a discriminator is registered with subtypes", {
  fert <- mgmt_schema$event_registry[["fertilizer"]]
  expect_true(fert$has_subtypes)
  expect_equal(fert$subtype_discriminator, "fertilizer_type")
  # a select field that isn't a discriminator doesn't create subtypes
  expect_false(mgmt_schema$event_registry[["tillage"]]$has_subtypes)
})

test_that("event type and subtype choices have an empty first option", {
  choices <- build_event_type_choices(mgmt_schema, "en")
  expect_equal(unname(choices[1]), "")
  # planting's English title is "sowing", shown in sentence case
  expect_equal(names(choices)[choices == "planting"], "Sowing")

  sub_choices <- build_subtype_choices(
    mgmt_schema$event_registry[["fertilizer"]], "en")
  expect_equal(unname(sub_choices[1]), "")
  expect_true("fertilizer_type_mineral" %in% sub_choices)
  expect_null(build_subtype_choices(
    mgmt_schema$event_registry[["planting"]], "en"))
})

test_that("harvest totals know which table column they sum", {
  total <- lookup_property(mgmt_schema$property_registry,
                           "harvest_yield_harvest_dw_total", "harvest")
  expect_equal(total$total_of$list_name, "harvest_list")
  expect_equal(total$total_of$property_name, "harvest_yield_harvest_dw")
})

test_that("table columns defined with allOf and $ref get their choices", {
  pl <- lookup_property(mgmt_schema$property_registry, "planting_list",
                        "planting")
  expect_equal(pl$array_columns$planted_crop$type, "selectInput")
  expect_true(length(pl$array_columns$planted_crop$choices) > 0)
})

test_that("a table inside a subtype is registered under the subtype", {
  pr <- mgmt_schema$property_registry
  key <- paste0("soil_layer_list", REGISTRY_KEY_SEP, "observation",
                REGISTRY_KEY_SEP, "observation_type_soil")
  desc <- lookup_property(pr, "soil_layer_list", "observation",
                          "observation_type_soil")
  expect_identical(desc, pr[[key]])
  expect_equal(desc$type, "dataTable")
  expect_equal(schema_table_id(desc), paste0(key, "_table"))
})

test_that("a date field without a value is cleared", {
  # updateDateInput ignores a NULL value, so the widget must be sent null
  sent <- list()
  session <- structure(
    list(sendInputMessage = function(inputId, message) {
      sent[[length(sent) + 1]] <<- message$value
    }),
    class = "ShinySession")
  end_date <- lookup_property(mgmt_schema$property_registry, "end_date",
                              "grazing")

  update_schema_value(session, end_date, "2022-06-10")
  update_schema_value(session, end_date, NULL)
  update_schema_value(session, end_date, "-99.0")

  expect_equal(as.character(sent[[1]]), "2022-06-10")
  # NA is sent to the browser as null
  expect_true(is.na(sent[[2]]))
  expect_true(is.na(sent[[3]]))
})
