# Next Generation Shiny Apps with {bslib}

Garrick Aden-Buie<br>
**Online workshop at [R/Medicine 2024](https://rconsortium.github.io/RMedicine_website/)** <br>
Tuesday, June 11, 2024<br>
[11:00am to 2:00pm](https://www.timeanddate.com/worldclock/fixedtime.html?msg=Next+Generation+Shiny+Apps+with+%7Bbslib%7D&iso=20240611T11&p1=25&ah=3)


## Links

📘 [Website](https://pkg.garrickadenbuie.com/rmedicine-2024-bslib)

📦 [Repository](https://github.com/gadenbuie/rmedicine-2024-bslib)

## File Organization

There are three main directories in this repository:

* 📁 `exercises/` contains starter code and example solutions for each exercise
  * `exercises/01_app.R` - The starter code for the first exercise
  * `exercises/01_solution_app.R` - An example solution for the first exercise

* 📁 `examples/` contains demo apps used in the workshop

* 📁 `website/` contains the website materials and slides

## Local Setup

If you prefer to use your own computer, you can clone the repository and install the necessary packages.
I used [R 4.4](https://r-project.org) but any recent version of R (>= 4.1) should work.

You can use the [usethis](https://usethis.r-lib.org) package to quickly clone the repository:

```r
usethis::create_from_github(
  "gadenbuie/rmedicine-2024-bslib",
  # Decide where to put the project here:
  destdir = "~/Desktop/rmedicine-2024-bslib"
)
```

This will download the repository and open the project in RStudio.
Inside the project, use the `renv` package to install the necessary packages:

```r
renv::restore()
```

For very speedy installation, I recommend telling renv to use [pak](https://pak.r-lib.org/) to install packages:

```r
Sys.setenv(RENV_CONFIG_PAK_ENABLED = "true")
renv::restore()
```
