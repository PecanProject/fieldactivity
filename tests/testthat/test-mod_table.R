# Tests for the schema-driven table module

harvest_list_desc <- function() {
  lookup_property(mgmt_schema$property_registry, "harvest_list", "harvest")
}

table_module_args <- function(override_values) {
  list(desc = harvest_list_desc(), language = reactive("disp_name_eng"),
       override_values = override_values)
}

test_that("table is prefilled with one row per item", {
  override_values <- reactiveVal()
  testServer(mod_table_server, args = table_module_args(override_values), {
    override_values(list(list(harvest_crop = "ZZ1", harvest_method = "HM008"),
                         list(harvest_crop = "BAR")))
    session$flushReact()

    expect_equal(dynamic_rows(), 1:2)
    cells <- table_data()
    expect_match(cells[1, "harvest_crop"], "ZZ1")
    expect_match(cells[1, "harvest_method"], "HM008")
    expect_match(cells[2, "harvest_crop"], "BAR")
  })
})

test_that("removing a row keeps the other rows and validates them by position", {
  override_values <- reactiveVal()
  testServer(mod_table_server, args = table_module_args(override_values), {
    override_values(list(list(), list()))
    session$flushReact()
    session$setInputs(rendered = TRUE, harvest_crop_1 = "ZZ1",
                      harvest_crop_2 = "BAR")
    session$setInputs(remove_row_index = 1L)
    expect_equal(dynamic_rows(), 2L)
    expect_match(table_data()[1, "harvest_crop"], "BAR")

    # the remaining row is now the first displayed row
    session$setInputs(harvest_crop_1 = "")
    expect_false(session$returned$valid())
    session$setInputs(harvest_crop_1 = "BAR")
    expect_true(session$returned$valid())
  })
})
