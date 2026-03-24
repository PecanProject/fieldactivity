# Integration tests against the real management-event schema
# These test that load_schema() correctly parses the bundled schema and that
# cross-cutting concerns (lookup chains, language, choices) work end-to-end.

# --- load_schema structure ----------------------------------------------------

test_that("load_schema returns all expected components", {
  expect_true(is.list(mgmt_schema))
  expected_names <- c("raw", "event_registry", "property_registry",
                       "property_reverse_index", "common_properties",
                       "event_type_choices")
  for (nm in expected_names) {
    expect_true(nm %in% names(mgmt_schema),
                info = paste("missing component:", nm))
  }
  expect_true(is.list(mgmt_schema$event_registry))
  expect_true(is.list(mgmt_schema$property_registry))
  expect_true(is.character(mgmt_schema$common_properties))
})

test_that("load_schema registers all 15 event types", {
  event_names <- names(mgmt_schema$event_registry)
  expected <- c("planting", "fertilizer", "tillage", "harvest", "chemicals",
                "grazing", "weeding", "irrigation", "mowing", "observation",
                "bed_prep", "inorg_mulch", "Inorg_mul_rem", "measurement",
                "other")
  for (ev in expected) {
    expect_true(ev %in% event_names,
                info = paste("missing event type:", ev))
  }
  expect_equal(length(event_names), 15)
})

test_that("common properties include date and mgmt_operations_event but not $schema", {
  cp <- mgmt_schema$common_properties
  expect_true("date" %in% cp)
  expect_true("mgmt_operations_event" %in% cp)
  expect_true("mgmt_event_short_notes" %in% cp)
  expect_false("$schema" %in% cp)
})

test_that("event_type_choices has titles for all events", {
  etc <- mgmt_schema$event_type_choices
  expect_equal(length(etc), length(mgmt_schema$event_registry))
  for (ev in names(etc)) {
    expect_true(!is.null(etc[[ev]]$en),
                info = paste("missing en title for", ev))
  }
})

# --- Subtype structure --------------------------------------------------------

test_that("fertilizer has 3 subtypes with discriminator fertilizer_type", {
  fert <- mgmt_schema$event_registry[["fertilizer"]]
  expect_true(fert$has_subtypes)
  expect_equal(fert$subtype_discriminator, "fertilizer_type")
  expect_equal(length(fert$subtypes), 3)
  sub_consts <- vapply(fert$subtypes, function(s) s$const, character(1))
  expect_true("fertilizer_type_mineral" %in% sub_consts)
  expect_true("fertilizer_type_soil_amendment" %in% sub_consts)
  expect_true("fertilizer_type_organic" %in% sub_consts)
})

test_that("observation has 8 subtypes with discriminator observation_type", {
  obs <- mgmt_schema$event_registry[["observation"]]
  expect_true(obs$has_subtypes)
  expect_equal(obs$subtype_discriminator, "observation_type")
  expect_equal(length(obs$subtypes), 8)
  sub_consts <- vapply(obs$subtypes, function(s) s$const, character(1))
  expect_true("observation_type_soil" %in% sub_consts)
  expect_true("observation_type_vegetation" %in% sub_consts)
})

test_that("planting has no subtypes", {
  pl <- mgmt_schema$event_registry[["planting"]]
  expect_false(pl$has_subtypes)
  expect_null(pl$subtype_discriminator)
})

# --- Property registry --------------------------------------------------------

test_that("common properties registered with bare keys, correct types", {
  pr <- mgmt_schema$property_registry
  date_desc <- pr[["date"]]
  expect_false(is.null(date_desc))
  expect_equal(date_desc$type, "dateInput")

  event_desc <- pr[["mgmt_operations_event"]]
  expect_false(is.null(event_desc))
  expect_true(event_desc$is_discriminator)
})

test_that("event properties use REGISTRY_KEY_SEP namespacing", {
  pr <- mgmt_schema$property_registry
  key <- paste0("planting_list", REGISTRY_KEY_SEP, "planting")
  expect_false(is.null(pr[[key]]))
  expect_equal(pr[[key]]$type, "dataTable")
})

test_that("$ref properties resolved with choices", {
  desc <- lookup_property(mgmt_schema$property_registry,
                           "fertilizer_applic_method", "fertilizer")
  expect_false(is.null(desc))
  expect_equal(desc$type, "selectInput")
  expect_true(length(desc$choices) > 0)
  choice_values <- vapply(desc$choices, function(ch) ch$value, character(1))
  expect_true("AP001" %in% choice_values)
})

test_that("dataTable properties have resolved array_columns", {
  pl_desc <- lookup_property(mgmt_schema$property_registry,
                              "planting_list", "planting")
  expect_equal(pl_desc$type, "dataTable")
  expect_true("planted_crop" %in% names(pl_desc$array_columns))
  expect_true("planting_material_weight" %in% names(pl_desc$array_columns))

  # planted_crop should be selectInput (resolved from $ref via allOf)
  expect_equal(pl_desc$array_columns$planted_crop$type, "selectInput")

  # planting_material_weight should be numericInput with min >= 0
  pmw <- pl_desc$array_columns$planting_material_weight
  expect_equal(pmw$type, "numericInput")
  expect_true(!is.null(pmw$minimum) && pmw$minimum >= 0)
})

# --- Cross-cutting ------------------------------------------------------------

test_that("get_relevant_properties returns correct sets", {
  # planting event_props should include planting_list
  rel <- get_relevant_properties(mgmt_schema, "planting")
  expect_true("planting_list" %in% rel$event_props)
  expect_equal(rel$common, mgmt_schema$common_properties)
  expect_length(rel$subtype_props, 0)

  # fertilizer with subtype should populate subtype_props
  rel2 <- get_relevant_properties(mgmt_schema, "fertilizer",
                                   "fertilizer_type_mineral")
  expect_true(length(rel2$subtype_props) > 0)
  expect_true("fertilizer_product_name" %in% rel2$subtype_props)

  # all should be the union
  expect_true(all(rel2$common %in% rel2$all))
  expect_true(all(rel2$event_props %in% rel2$all))
  expect_true(all(rel2$subtype_props %in% rel2$all))

  # unknown event type returns empty event/subtype but still has common
  rel3 <- get_relevant_properties(mgmt_schema, "nonexistent")
  expect_length(rel3$event_props, 0)
  expect_length(rel3$subtype_props, 0)
  expect_equal(rel3$common, mgmt_schema$common_properties)
})

test_that("build_event_type_choices and build_subtype_choices produce valid selectInput vectors", {
  choices <- build_event_type_choices(mgmt_schema, "en")
  expect_equal(unname(choices[1]), "")
  expect_true("planting" %in% choices)
  # planting's English title is "sowing"
  planting_idx <- which(choices == "planting")
  expect_equal(names(choices)[planting_idx], "sowing")

  # subtype choices for fertilizer
  fert <- mgmt_schema$event_registry[["fertilizer"]]
  sub_choices <- build_subtype_choices(fert, "en")
  expect_equal(unname(sub_choices[1]), "")
  expect_true("fertilizer_type_mineral" %in% sub_choices)

  # planting has no subtypes
  expect_null(build_subtype_choices(
    mgmt_schema$event_registry[["planting"]], "en"))

  # Finnish subtype choices
  sub_fi <- build_subtype_choices(fert, "fi")
  expect_true(length(sub_fi) > 1)
})

# --- Schema-specific edge cases -----------------------------------------------

test_that("harvest total_of descriptors are parsed correctly", {
  # harvest has 3 properties with total_of_list/total_of_property for auto-sum
  pr <- mgmt_schema$property_registry
  total_desc <- lookup_property(pr, "harvest_yield_harvest_dw_total", "harvest")
  expect_false(is.null(total_desc))
  expect_false(is.null(total_desc$total_of))
  expect_equal(total_desc$total_of$list_name, "harvest_list")
  expect_equal(total_desc$total_of$property_name, "harvest_yield_harvest_dw")

  total_desc2 <- lookup_property(pr, "harv_yield_harv_f_wt_total", "harvest")
  expect_false(is.null(total_desc2$total_of))
  expect_equal(total_desc2$total_of$list_name, "harvest_list")
})

test_that("minItems is captured in array property descriptors", {
  pl <- lookup_property(mgmt_schema$property_registry, "planting_list", "planting")
  expect_equal(pl$min_items, 1)

  hl <- lookup_property(mgmt_schema$property_registry, "harvest_list", "harvest")
  expect_equal(hl$min_items, 1)
})

test_that("x-ui conditions are preserved in property descriptors", {
  # chemicals/chemical_applic_material_list has a condition on chemical_type
  pr <- mgmt_schema$property_registry
  chem_list <- lookup_property(pr, "chemical_applic_material_list", "chemicals")
  expect_false(is.null(chem_list))
  expect_false(is.null(chem_list$xui$condition))
  expect_true(grepl("chemical_type", chem_list$xui$condition))

  # application_ph_start also has a condition
  ph_start <- lookup_property(pr, "application_ph_start", "chemicals")
  expect_false(is.null(ph_start$xui$condition))
})

test_that("allOf in array items resolves $ref choices (planted_crop, harvest_crop)", {
  # planted_crop inside planting_list uses allOf to merge title with $ref to
  # crop_ident_ICASA. The resolved descriptor must have selectInput choices.
  pl <- lookup_property(mgmt_schema$property_registry, "planting_list", "planting")
  planted_crop <- pl$array_columns$planted_crop
  expect_equal(planted_crop$type, "selectInput")
  expect_true(length(planted_crop$choices) > 10)
  # Should contain common crop codes
  crop_values <- vapply(planted_crop$choices, function(ch) ch$value, character(1))
  expect_true("WHT" %in% crop_values)  # wheat

  # Same for harvest_crop
  hl <- lookup_property(mgmt_schema$property_registry, "harvest_list", "harvest")
  harvest_crop <- hl$array_columns$harvest_crop
  expect_equal(harvest_crop$type, "selectInput")
  expect_true(length(harvest_crop$choices) > 10)
})

test_that("chemicals event has no subtypes but has conditional properties", {
  chem <- mgmt_schema$event_registry[["chemicals"]]
  expect_false(chem$has_subtypes)
  expect_null(chem$subtype_discriminator)
  # chemical_type is a regular selectInput, not a discriminator
  ct <- lookup_property(mgmt_schema$property_registry, "chemical_type", "chemicals")
  expect_equal(ct$type, "selectInput")
  expect_false(isTRUE(ct$is_discriminator))
  expect_true(length(ct$choices) >= 6)
})

test_that("minimal events (weeding, other, bed_prep) register correctly", {
  for (ev in c("weeding", "other", "bed_prep")) {
    entry <- mgmt_schema$event_registry[[ev]]
    expect_false(is.null(entry), info = paste(ev, "not registered"))
    expect_false(entry$has_subtypes, info = paste(ev, "should have no subtypes"))
    # Should have at least mgmt_event_long_notes
    expect_true("mgmt_event_long_notes" %in% entry$property_names,
                info = paste(ev, "missing mgmt_event_long_notes"))
  }
})

# --- Nested cases -------------------------------------------------------------

test_that("array table inside subtype: soil_layer_list in observation_type_soil", {
  # soil_layer_list is a dataTable property belonging to the observation_type_soil
  # subtype, not the observation event itself. It must be registered with the
  # triple-key (prop___event___subtype) and have resolved array_columns.
  pr <- mgmt_schema$property_registry
  key <- paste0("soil_layer_list", REGISTRY_KEY_SEP, "observation",
                REGISTRY_KEY_SEP, "observation_type_soil")
  desc <- pr[[key]]
  expect_false(is.null(desc), info = "soil_layer_list not registered under subtype key")
  expect_equal(desc$type, "dataTable")
  expect_true(length(desc$array_columns) >= 5)
  # Should have soil_layer_top_depth as a numericInput column
  expect_false(is.null(desc$array_columns$soil_layer_top_depth))
  expect_equal(desc$array_columns$soil_layer_top_depth$type, "numericInput")

  # Should also be discoverable via lookup_property with subtype context
  desc2 <- lookup_property(pr, "soil_layer_list", "observation",
                            "observation_type_soil")
  expect_identical(desc, desc2)

  # Should appear in get_schema_table_names
  table_names <- get_schema_table_names(mgmt_schema)
  expect_true("soil_layer_list_table" %in% table_names)
})

test_that("subtype property with x-ui condition: animal_fert_usage in organic fertilizer", {
  pr <- mgmt_schema$property_registry
  desc <- lookup_property(pr, "animal_fert_usage", "fertilizer",
                           "fertilizer_type_organic")
  expect_false(is.null(desc))
  expect_false(is.null(desc$xui$condition))
  expect_true(grepl("organic_material", desc$xui$condition))
})

test_that("subtype properties with oneOf choices resolve correctly", {
  pr <- mgmt_schema$property_registry

  # organic_material selectInput inside fertilizer_type_organic subtype
  # has 21 oneOf choices (RE001, RE002, etc.)
  om <- lookup_property(pr, "organic_material", "fertilizer",
                         "fertilizer_type_organic")
  expect_false(is.null(om))
  expect_equal(om$type, "selectInput")
  expect_true(length(om$choices) >= 20)
  om_values <- vapply(om$choices, function(ch) ch$value, character(1))
  expect_true("RE003" %in% om_values)

  # fertilizer_material inside fertilizer_type_soil_amendment has 4 choices
  fm <- lookup_property(pr, "fertilizer_material", "fertilizer",
                         "fertilizer_type_soil_amendment")
  expect_false(is.null(fm))
  expect_equal(fm$type, "selectInput")
  expect_true(length(fm$choices) >= 4)
})

test_that("get_relevant_properties includes subtype array tables", {
  # observation with soil subtype should include soil_layer_list
  rel <- get_relevant_properties(mgmt_schema, "observation",
                                  "observation_type_soil")
  expect_true("soil_layer_list" %in% rel$subtype_props)
  expect_true("soil_layer_list" %in% rel$all)
  # But without subtype, soil_layer_list should NOT appear
  rel2 <- get_relevant_properties(mgmt_schema, "observation")
  expect_false("soil_layer_list" %in% rel2$event_props)
  expect_false("soil_layer_list" %in% rel2$all)
})

# --- Remaining edge cases -----------------------------------------------------

test_that("tillage event has no subtypes despite having a selectInput practice field", {
  till <- mgmt_schema$event_registry[["tillage"]]
  expect_false(till$has_subtypes)
  expect_null(till$subtype_discriminator)
  # tillage_practice is a regular selectInput
  tp <- lookup_property(mgmt_schema$property_registry, "tillage_practice", "tillage")
  expect_equal(tp$type, "selectInput")
  expect_true(length(tp$choices) > 0)
})
