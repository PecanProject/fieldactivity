# Shared test fixtures for events.json files

# Legacy events in the shapes found in real events.json files written before
# the schema was adopted
legacy_events <- list(
  list(
    mgmt_operations_event = "planting",
    date = "2023-05-25",
    mgmt_event_notes = "-99.0",
    planted_crop = c("BAR", "RCL", "ZZ3"),
    planting_notes = "Mixed grass",
    planting_material_weight = c(240, 3, 20),
    planting_depth = c("-99.0", "-99.0", "-99.0"),
    planting_material_source = c("-99.0", "own seeds", "-99.0")
  ),
  list(
    mgmt_operations_event = "harvest",
    date = "2021-06-15",
    mgmt_event_notes = "First cut",
    harvest_area = "-99.0",
    harvest_yield_harvest_dw_total = 4200,
    harvest_crop = "ZZ1",
    harvest_operat_component = "silage",
    harvest_method = "HM008",
    harvest_moisture = "-99.0",
    harvest_comments = "-99.0"
  ),
  list(
    mgmt_operations_event = "organic_material",
    date = "2023-04-27",
    mgmt_event_notes = "-99.0",
    fertilizer_type = "fertilizer_type_organic",
    organic_material = "RE003",
    fertilizer_total_amount = 21000,
    fertilizer_notes = "Separator dry fraction",
    N_in_applied_fertilizer = 12.9,
    fertilizer_K_applied = "-99.0"
  ),
  list(
    mgmt_operations_event = "tillage",
    date = "2023-05-25",
    mgmt_event_notes = "-99.0",
    tillage_practice = "tillage_practice_primary",
    tillage_implement = "TI015",
    tillage_operations_depth = 15,
    tillage_treatment_notes = "Harrowing"
  ),
  list(
    mgmt_operations_event = "other",
    date = "2024-10-09",
    mgmt_event_notes = "Plowing was done 8.10. and 9.10.",
    other_notes = "Plowing."
  ),
  list(
    mgmt_operations_event = "grazing",
    date = "2022-06-01",
    mgmt_event_notes = "-99.0",
    grazing_species = "grazing_species_cattle",
    grazing_period = c("2022-06-01", "2022-06-20"),
    grazing_notes = "-99.0"
  )
)

write_events_file <- function(base_folder, site, block, events) {
  dir.create(file.path(base_folder, site, block), recursive = TRUE)
  jsonlite::write_json(
    list(management = list(rotation = list(), events = events)),
    file.path(base_folder, site, block, "events.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
}
