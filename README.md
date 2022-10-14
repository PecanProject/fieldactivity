
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R Shiny app for management data input

<!-- badges: start -->

[![R-CMD-check](https://github.com/Ottis1/fieldactivity/workflows/R-CMD-check/badge.svg)](https://github.com/PecanProject/fieldactivity/actions)
<!-- badges: end -->

An app for keeping track of field activity in the [Field
Observatory](https://www.fieldobservatory.org) project. Built using
[Shiny](http://shiny.rstudio.com/) and
[Golem](https://thinkr-open.github.io/golem/), the application allows
farmers to enter information about common farming events like tillage,
sowing and harvest. These event data are stored in .json files, which
mostly follow the ICASA standards for agricultural data.

## Installation

<!-- You can install the released version of fieldactivity from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fieldactivity")
``` 
-->

You can install the app from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PecanProject/fieldactivity")
```

## Running the app

To run the app, call `run_app` with the following arguments to define
the json file directory, the user database and the passphrase to the
user database:

``` r
options(golem.app.prod = TRUE) # run in production mode to enable user authentication

fieldactivity::run_app(json_file_path = "~/my_json_file_folder", 
                       user_db_path = "~/my_user_database.sqlite",
                       user_db_passphrase = "password123")
```

Check out the documentation of
[Shinymanager](https://datastorm-open.github.io/shinymanager/) (the user
authentication system used in the app) to find out how to create the
user database. You can also use the supplied R script in
`dev/create_user_db.R` for this purpose.

## Modifying the code

To modify the code, clone the repository and set the working directory
in R to the package folder (or open the RStudio project file
`fieldactivity.Rproj`). You should now be able to run the app by running

``` r
golem::run_dev()
```

Modify `dev/run_dev.R` if necessary, this is the file which
`golem::run_dev()` runs.

## Additional resources

Github pages offer material for the developers, but also for the regular
application users. For the developers, there is the `Get started` tab on
the top of the page, this leads to the vignette of the files that the
package have. By reading this, the developer should have a preliminary
idea/conception of the package logic and which parts of the
functionality the different code sections are responsible for. However,
to get a better picture of the application, we are encouraging to
test/run the package as well while getting familiar to the code base.

For the application users, the instructions can be found via the `Guide`
tab (which is located next to the `Get started`). Here we have
introduced the most meaningful features and functionalities, which the
user should be aware while using the application. The instructions has
been written for someone using the application in product mode,
i.e. signing in to application with a registered site and using those
features that are available for non-developers.

We also encourage users and developers to fill in possible issues or
feature request here:
[Issues](https://github.com/PecanProject/fieldactivity/issues)
