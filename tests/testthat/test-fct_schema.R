# Unit tests for schema parsing and helper functions
# Uses inline fixtures for pure function testing

# --- resolve_ref / resolve_property -------------------------------------------

test_that("resolve_ref resolves a $defs pointer", {
  defs <- list(my_type = list(type = "string", oneOf = list(list(const = "a"))))
  obj <- list("$ref" = "#/$defs/my_type")
  result <- resolve_ref(obj, defs)
  expect_equal(result$type, "string")
  expect_length(result$oneOf, 1)
  expect_null(result[["$ref"]])
})

test_that("resolve_ref returns obj unchanged when no $ref", {
  obj <- list(type = "string", title = "x")
  result <- resolve_ref(obj, list())
  expect_identical(result, obj)
})

test_that("resolve_ref returns obj unchanged for unresolvable $ref", {
  obj <- list("$ref" = "#/$defs/nonexistent")
  result <- resolve_ref(obj, list())
  expect_identical(result, obj)
})

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

# --- extract_titles / extract_oneof_choices -----------------------------------

test_that("extract_titles with partial languages defaults to empty string", {
  result <- extract_titles(list(title = "A", title_fi = "B"))
  expect_equal(result, list(en = "A", fi = "B", sv = ""))

  result2 <- extract_titles(list())
  expect_equal(result2, list(en = "", fi = "", sv = ""))
})

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

# --- lang_to_iso --------------------------------------------------------------

test_that("lang_to_iso converts column names and passes through ISO codes", {
  expect_equal(lang_to_iso("disp_name_eng"), "en")
  expect_equal(lang_to_iso("disp_name_fin"), "fi")
  expect_equal(lang_to_iso("disp_name_swe"), "sv")
  expect_equal(lang_to_iso("en"), "en")
  expect_equal(lang_to_iso("fi"), "fi")
  # Unknown language codes fall back to "en"
  expect_equal(lang_to_iso("unknown"), "en")
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

test_that("schema_get_choices returns NULL for NULL/empty input", {
  expect_null(schema_get_choices(NULL, "en"))
  expect_null(schema_get_choices(list(), "en"))
})

# --- build_property_descriptor ------------------------------------------------

test_that("build_property_descriptor populates fields for numeric property", {
  prop <- list(type = "number", minimum = 0, maximum = 100,
               title = "Weight", title_fi = "Paino")
  desc <- build_property_descriptor("test_prop", prop,
                                     required = TRUE,
                                     event_type = "harvest",
                                     is_array_item = FALSE)
  expect_equal(desc$name, "test_prop")
  expect_equal(desc$type, "numericInput")
  expect_true(desc$required)
  expect_equal(desc$minimum, 0)
  expect_equal(desc$maximum, 100)
  expect_equal(desc$titles$en, "Weight")
  expect_equal(desc$titles$fi, "Paino")
  expect_equal(desc$event_type, "harvest")
  expect_false(desc$is_integer)
})

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
  expect_equal(lookup_property(reg, "my_prop")$name, "common")
  expect_null(lookup_property(reg, "nonexistent"))
})

test_that("lookup_property falls through missing subtype to event level", {
  reg <- list()
  reg[[paste0("prop", REGISTRY_KEY_SEP, "harvest")]] <- list(name = "event")

  result <- lookup_property(reg, "prop", "harvest", "some_subtype")
  expect_equal(result$name, "event")
})

# --- normalize_legacy_event / get_legacy_value --------------------------------

test_that("normalize_legacy_event maps old names to new", {
  event <- normalize_legacy_event(list(mgmt_event_notes = "hello"))
  expect_equal(event$mgmt_event_short_notes, "hello")
  expect_null(event$mgmt_event_notes)

  event2 <- normalize_legacy_event(list(planting_notes = "pn"))
  expect_equal(event2$mgmt_event_long_notes, "pn")
  expect_null(event2$planting_notes)

  event3 <- normalize_legacy_event(list(harvest_comments = "hc"))
  expect_equal(event3$mgmt_event_long_notes, "hc")
})

test_that("normalize_legacy_event does not overwrite existing new-name values", {
  event <- normalize_legacy_event(list(
    mgmt_event_notes = "old",
    mgmt_event_short_notes = "new"
  ))
  expect_equal(event$mgmt_event_short_notes, "new")
  # old name is preserved because new name already existed
  expect_equal(event$mgmt_event_notes, "old")
})

test_that("get_legacy_value retrieves value via reverse mapping", {
  expect_equal(get_legacy_value(list(planting_notes = "val"),
                                 "mgmt_event_long_notes"), "val")
  expect_null(get_legacy_value(list(other_field = "x"),
                                "mgmt_event_long_notes"))
})

# --- find_any_property_desc ---------------------------------------------------

test_that("find_any_property_desc uses reverse index for O(1) lookup", {
  reg <- list()
  reg[["date"]] <- list(name = "date", type = "dateInput")
  reg[[paste0("crop_name", REGISTRY_KEY_SEP, "planting")]] <-
    list(name = "crop_name", type = "selectInput")

  rev_idx <- list(crop_name = paste0("crop_name", REGISTRY_KEY_SEP, "planting"))

  expect_equal(find_any_property_desc(reg, "date", rev_idx)$name, "date")
  expect_equal(find_any_property_desc(reg, "crop_name", rev_idx)$name,
               "crop_name")
  expect_null(find_any_property_desc(reg, "nonexistent", rev_idx))
})

test_that("find_any_property_desc falls back to linear scan without index", {
  reg <- list()
  reg[[paste0("crop_name", REGISTRY_KEY_SEP, "planting")]] <-
    list(name = "crop_name")

  result <- find_any_property_desc(reg, "crop_name", NULL)
  expect_equal(result$name, "crop_name")
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

# --- get_schema_table_names ---------------------------------------------------

test_that("get_schema_table_names lists all array table names", {
  table_names <- get_schema_table_names(mgmt_schema)
  expect_true("planting_list_table" %in% table_names)
  expect_true("harvest_list_table" %in% table_names)
  expect_true(all(grepl("_table$", table_names)))
  # should not be empty

  expect_true(length(table_names) >= 3)
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

test_that("make_required_label adds asterisk only for required non-empty labels", {
  result <- make_required_label("Name", TRUE)
  expect_s3_class(result, "shiny.tag.list")
  html <- as.character(result)
  expect_true(grepl("\\*", html))

  expect_equal(make_required_label("Name", FALSE), "Name")
  expect_equal(make_required_label("", TRUE), "")
  expect_null(make_required_label(NULL, TRUE))
})
