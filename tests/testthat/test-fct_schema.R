# Unit tests for schema parsing and helper functions
# Uses inline fixtures for pure function testing

# --- resolve_property ---------------------------------------------------------

test_that("resolve_property merges allOf and carries top-level keys", {
  defs <- list(test_def = list(type = "string", oneOf = list(list(const = "v1"))))
  prop <- list(
    allOf = list(
      list(title = "My Title", title_fi = "Otsikko"),
      list("$ref" = "#/$defs/test_def")
    ),
    "x-ui" = list(foo = TRUE)
  )
  result <- resolve_property(prop, defs)
  expect_equal(result$title, "My Title")
  expect_equal(result$type, "string")
  expect_length(result$oneOf, 1)
  expect_true(result[["x-ui"]]$foo)
})

test_that("resolve_property recursively resolves array items", {
  defs <- list(item_def = list(
    type = "object",
    properties = list(
      name = list(type = "string"),
      weight = list("$ref" = "#/$defs/weight_def")
    )
  ))
  defs$weight_def <- list(type = "number", minimum = 0)
  prop <- list(type = "array", items = list("$ref" = "#/$defs/item_def"))
  result <- resolve_property(prop, defs)
  expect_equal(result$items$type, "object")
  expect_equal(result$items$properties$name$type, "string")
  expect_equal(result$items$properties$weight$type, "number")
  expect_equal(result$items$properties$weight$minimum, 0)
})

# --- determine_widget_type ---------------------------------------------------

test_that("determine_widget_type maps schema types correctly", {
  expect_equal(determine_widget_type(list(type = "string")), "textInput")
  expect_equal(determine_widget_type(list(type = "string", format = "date")),
               "dateInput")
  expect_equal(determine_widget_type(
    list(type = "string", oneOf = list(list(const = "a")))), "selectInput")
  expect_equal(determine_widget_type(
    list(type = "string", "x-ui" = list(discriminator = TRUE))), "selectInput")
  expect_equal(determine_widget_type(list(type = "number")), "numericInput")
  expect_equal(determine_widget_type(list(type = "integer")), "numericInput")
  expect_equal(determine_widget_type(
    list(type = "array", items = list(type = "object"))), "dataTable")
  expect_equal(determine_widget_type(list(const = "planting")), "const")
  expect_equal(determine_widget_type(
    list(type = "string", "x-ui" = list("form-type" = "textAreaInput"))),
    "textAreaInput")
})

# --- extract_oneof_choices ----------------------------------------------------

test_that("extract_oneof_choices extracts const/titles and skips entries without const", {
  input <- list(
    list(const = "A", title = "Label A", title_fi = "Fin A"),
    list(title = "No const here"),
    list(const = "B", title = "Label B")
  )
  result <- extract_oneof_choices(input)
  expect_length(result, 2)
  expect_equal(result[[1]]$value, "A")
  expect_equal(result[[1]]$titles$en, "Label A")
  expect_equal(result[[1]]$titles$fi, "Fin A")
  expect_equal(result[[2]]$value, "B")
})

# --- schema_get_title ---------------------------------------------------------

test_that("schema_get_title fallback chain: language -> en -> fallback", {
  expect_equal(schema_get_title(list(en = "E", fi = "F", sv = "S"), "fi"), "F")
  # empty fi falls back to en
  expect_equal(schema_get_title(list(en = "E", fi = "", sv = ""), "fi"), "E")
  # all empty, use fallback
  expect_equal(schema_get_title(list(en = "", fi = "", sv = ""), "sv", "default"),
               "default")
  # NULL titles
  expect_equal(schema_get_title(NULL, "en", "fallback"), "fallback")
})

# --- schema_get_choices -------------------------------------------------------

test_that("schema_get_choices builds named vector with empty first option", {
  choices <- extract_oneof_choices(list(
    list(const = "A", title = "Aa"),
    list(const = "B", title = "Bb")
  ))
  result <- schema_get_choices(choices, "en")
  expect_equal(length(result), 3)
  expect_equal(unname(result[1]), "")
  expect_equal(unname(result[2]), "A")
  expect_equal(names(result)[2], "Aa")
  expect_equal(unname(result[3]), "B")
  expect_equal(names(result)[3], "Bb")
})

# --- build_property_descriptor ------------------------------------------------

test_that("build_property_descriptor builds array_columns for dataTable", {
  prop <- list(
    type = "array",
    items = list(
      type = "object",
      required = list("col_a"),
      properties = list(
        col_a = list(type = "string", title = "Column A"),
        col_b = list(type = "number", title = "Column B", minimum = 0)
      )
    )
  )
  desc <- build_property_descriptor("my_table", prop,
                                     required = FALSE,
                                     event_type = "planting",
                                     is_array_item = FALSE)
  expect_equal(desc$type, "dataTable")
  expect_length(desc$array_columns, 2)
  expect_equal(desc$array_columns$col_a$type, "textInput")
  expect_true(desc$array_columns$col_a$required)
  expect_equal(desc$array_columns$col_b$type, "numericInput")
  expect_false(desc$array_columns$col_b$required)
  expect_equal(desc$array_columns$col_b$minimum, 0)
})

# --- lookup_property ----------------------------------------------------------

test_that("lookup_property resolves subtype > event > common priority", {
  reg <- list()
  reg[["my_prop"]] <- list(name = "common")
  reg[[paste0("my_prop", REGISTRY_KEY_SEP, "fertilizer")]] <- list(name = "event")
  reg[[paste0("my_prop", REGISTRY_KEY_SEP, "fertilizer", REGISTRY_KEY_SEP,
              "mineral")]] <- list(name = "subtype")

  expect_equal(lookup_property(reg, "my_prop", "fertilizer", "mineral")$name,
               "subtype")
  expect_equal(lookup_property(reg, "my_prop", "fertilizer")$name, "event")
  expect_equal(lookup_property(reg, "my_prop", "fertilizer", "organic")$name,
               "event")
  expect_equal(lookup_property(reg, "my_prop")$name, "common")
  expect_null(lookup_property(reg, "nonexistent"))
})

# --- find_property_by_name ----------------------------------------------------

test_that("find_property_by_name finds common and event properties", {
  expect_equal(find_property_by_name(mgmt_schema, "date")$name, "date")
  expect_equal(find_property_by_name(mgmt_schema, "tillage_practice")$name,
               "tillage_practice")
  expect_null(find_property_by_name(mgmt_schema, "nonexistent"))
})

# --- get_subtype_value --------------------------------------------------------

test_that("get_subtype_value extracts discriminator from event values", {
  er <- mgmt_schema$event_registry

  result <- get_subtype_value(
    list(mgmt_operations_event = "fertilizer",
         fertilizer_type = "fertilizer_type_mineral"), er)
  expect_equal(result, "fertilizer_type_mineral")

  # planting has no subtypes
  expect_null(get_subtype_value(
    list(mgmt_operations_event = "planting"), er))

  # empty values
  expect_null(get_subtype_value(list(), er))
})

# --- field ids -----------------------------------------------------------------

test_that("fields shared by event types have one id per event type", {
  pr <- mgmt_schema$property_registry
  ids <- vapply(pr, function(desc) desc$id, character(1))
  expect_identical(unname(ids), names(pr))

  notes_ids <- vapply(
    Filter(function(desc) desc$name == "mgmt_event_long_notes", pr),
    function(desc) desc$id, character(1))
  expect_true(length(notes_ids) > 1)
})

test_that("an event property redefining a common property relabels it", {
  er <- mgmt_schema$event_registry
  pr <- mgmt_schema$property_registry
  expect_equal(er$grazing$common_overrides$date$titles$en, "Start date")
  expect_null(pr[[paste0("date", REGISTRY_KEY_SEP, "grazing")]])
  expect_false("date" %in% er$grazing$property_names)
})

test_that("x-ui conditions refer to field ids of the same event type", {
  desc <- lookup_property(mgmt_schema$property_registry, "animal_fert_usage",
                          "fertilizer", "fertilizer_type_organic")
  expect_match(desc$condition,
               "input.organic_material___fertilizer___fertilizer_type_organic",
               fixed = TRUE)
  expect_false(grepl("input.organic_material ", desc$condition, fixed = TRUE))
})

test_that("get_relevant_fields returns the fields of the event and subtype", {
  fields <- get_relevant_fields(mgmt_schema, "fertilizer",
                                "fertilizer_type_organic")
  ids <- vapply(fields, function(desc) desc$id, character(1))
  expect_true(paste0("fertilizer_type", REGISTRY_KEY_SEP, "fertilizer") %in% ids)
  expect_true(paste0("organic_material", REGISTRY_KEY_SEP, "fertilizer",
                     REGISTRY_KEY_SEP, "fertilizer_type_organic") %in% ids)
  expect_false(any(mgmt_schema$common_properties %in% ids))
})

# --- UI helpers (fct_schema_ui.R) ---------------------------------------------

test_that("convert_condition_to_js namespaces input references", {
  ns <- shiny::NS("form")
  result <- convert_condition_to_js(
    "input.organic_material == 'RE003'", ns)
  expect_equal(result, "input['form-organic_material'] == 'RE003'")

  result2 <- convert_condition_to_js(
    "input.chemical_type != 'lime'", ns)
  expect_equal(result2, "input['form-chemical_type'] != 'lime'")
})

# --- to_sentence_case ---------------------------------------------------------

test_that("to_sentence_case capitalises lowercase labels only", {
  expect_equal(to_sentence_case("weight of seeds (kg/ha)"),
               "Weight of seeds (kg/ha)")
  expect_equal(to_sentence_case("äestys"), "Äestys")
  expect_equal(to_sentence_case("pH after the application"),
               "pH after the application")
  expect_equal(to_sentence_case(""), "")
})
