# A script to create a new shinymanager user database for the app
# This follows instructions from shinymanager's website:
# https://datastorm-open.github.io/shinymanager/
# the site illustrates how the keyring package could be used to store the
# database passphrase

# Define initial credentials (more can be added later through the admin UI)
credentials <- data.frame(
  user = c("user", "admin"),
  password = c("User1", "12345"),
  start = c(NA, NA), # start and expire are optional
  expire = c(NA, NA),
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Create the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "path/to/database.sqlite", # will be created
  passphrase = "set_the_passphrase_here"
)