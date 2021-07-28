# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

file_path <- if (golem::app_dev()) {
  "dev/dev_events"
} else {
  "/data/fo-event-files"
}

# Run the application
run_app(json_file_path = file_path)
