if (Sys.getenv("R_CONFIG_ACTIVE", "local") != "rstudio_cloud") {
  source("renv/activate.R")
} else {
  # In Posit Cloud, hook up the renv library path without full renv activation
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  .libPaths(c(renv::paths$library(), .libPaths()))
}