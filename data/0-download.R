# College Scorecard Data -------------------------------------------------------
# Prepared: https://github.com/gadenbuie/scorecard-db/tree/refresh

urls_data <- c(
  "https://github.com/gadenbuie/scorecard-db/raw/refresh/data/tidy/school.rds",
  "https://github.com/gadenbuie/scorecard-db/raw/refresh/data/tidy/scorecard.rds"
)

for (url in urls_data) {
  download.file(url, here::here("data", basename(url)))
}
