library(targets)
library(tarchetypes)

# Packages loaded before each step runs
tar_option_set(packages = c("tidyverse", "pointblank"))

# Load every function in R/
tar_source()

# The month to process: the PIPELINE_MONTH environment variable, or a default
month <- Sys.getenv("PIPELINE_MONTH", unset = "2011-11")
raw_path <- Sys.getenv("PIPELINE_FILE", unset = extract_path(month))

list(
  # A file target: the step reruns if the file's contents change
  tar_target(raw_file, raw_path, format = "file"),
  tar_target(raw, read_extract(raw_file)),
  tar_target(validated, validate_extract(raw)),
  tar_target(sales, clean_sales(validated) ),
  tar_target(kpis, summarise_month(sales, month)),
  tar_target(top_products,top_by(sales, Description)),
  tar_target(top_countries, top_by(sales, Country) ),
  tar_quarto(report, "reports/monthly_report.qmd", execute_params = list(month = month) ),
  tar_target(published, publish_report(report, month), format = "file")
)