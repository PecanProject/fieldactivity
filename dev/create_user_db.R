# A script to create a new shinymanager user database for the app
# Most of this is copied from shinymanager's website:
# https://datastorm-open.github.io/shinymanager/
# the site also illustrates how the keyring package could be used to store the
# database passphrase

# define some credentials
credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("azerty", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Init the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "path/to/database.sqlite", # will be created
  passphrase = "set_the_passphrase_here"
)