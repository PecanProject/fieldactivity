# Tests for the image upload module

file_input_module_args <- function(reset_path) {
  list(desc = lookup_property(mgmt_schema$property_registry, "canopeo_image",
                              "observation"),
       language = reactive("disp_name_eng"), set_path = reactiveVal(),
       reset_path = reset_path)
}

test_that("clearing the field forgets that a new file was uploaded", {
  image <- tempfile(fileext = ".png")
  file.create(image)
  on.exit(unlink(image))

  reset_path <- reactiveVal()
  testServer(mod_fileInput_server, args = file_input_module_args(reset_path), {
    session$setInputs(file = list(datapath = image))
    expect_true(session$returned()$new_file)

    # the form clears its fields when it is closed or another event is opened
    reset_path(TRUE)
    session$flushReact()
    expect_null(session$returned()$filepath)
    expect_false(session$returned()$new_file)
  })
})
