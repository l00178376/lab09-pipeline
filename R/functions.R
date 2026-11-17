# ANALIT802 Lab 09: functions for each stage of the monthly sales pipeline.
# Each function takes data (or a file path) and returns data, and changes
# nothing else, except publish_report(), which writes a file.
# Paths are relative to the project root, where run_all.R and tar_make() run.

library(tidyverse)
library(pointblank)

# ---- Ingest ------------------------------------------------------------------

# Path of the extract for a month, e.g. "2011-11" -> data/raw/retail_2011_11.csv
extract_path <- function(month) {
  file.path("data", "raw", paste0("retail_", sub("-", "_", month), ".csv"))
}

# Read one monthly extract with fixed column types, so codes and customer IDs
# stay text and a changed file cannot silently change a type
read_extract <- function(path) {
  read_csv(
    path,
    col_types = cols(
      Invoice = col_character(), StockCode = col_character(),
      Description = col_character(), Quantity = col_double(),
      InvoiceDate = col_character(), Price = col_double(),
      `Customer ID` = col_character(), Country = col_character()
    )
  )
}

# ---- Validate (Part 3: write this function) ---------------------------------

# Check the extract against the assumptions the later stages make.
# Until Part 3 it returns the data unchanged, so the pipeline runs.

validate_extract <- function(raw) {
  
  raw |>
    #Rule 1: Check all eight columns exist
    col_exists(
      columns = c( "Invoice", "StockCode", "Description", "Quantity",
        "InvoiceDate", "Price", "Customer ID", "Country" ),
      actions = stop_on_fail()
    ) |>
    #Rule 2: No negative prices except bad-debt invoices
    col_vals_gte(
      columns = Price,
      value = 0,
      preconditions = function(x) {
        dplyr::filter(x, !startsWith(Invoice, "A"))},
      actions = action_levels(
        warn_at = 1,
        stop_at = 0.01) ) |>
    #Rule 3: Positive quantities except cancellations
    col_vals_gt(
      columns = Quantity,
      value = 0,
      preconditions = function(x) {
        dplyr::filter(x, !startsWith(Invoice, "C"))},
      actions = action_levels(
        warn_at = 0.01,
        stop_at = 0.05 ) ) |>
    # Rule 4: Customer ID must not be missing
    col_vals_not_null(
      columns = `Customer ID`,
      actions = action_levels(
        warn_at = 0.30,
        stop_at = 0.50
      )
    ) |>
    #Rule 5: No repeated rows
    rows_distinct(
      actions = action_levels(
        warn_at = 0.05,
        stop_at = 0.20 ))
}

# ---- Clean and transform -----------------------------------------------------

# Keep completed product sales (as in Lab 03): not cancelled, positive quantity
# and price, and a five-digit product code. Add the line value
clean_sales <- function(validated) {
  validated |> 
    distinct() |>
    filter(
      !startsWith(Invoice, "C"), Quantity > 0, Price > 0,
      str_detect(StockCode, "^[0-9]{5}")
    ) |>
    mutate(InvoiceDate = ymd_hms(InvoiceDate), value = Quantity * Price)
}

# ---- Aggregate ---------------------------------------------------------------

# One row of headline figures for the month
summarise_month <- function(sales, month) {
  sales |>
    summarise(
      month = month,
      revenue = sum(value),
      orders = n_distinct(Invoice),
      customers = n_distinct(`Customer ID`, na.rm = TRUE),
      avg_order_value = revenue / orders
    )
}

# Top n groups by revenue, e.g. top_by(sales, Country)
top_by <- function(sales, group, n = 5) {
  sales |>
    group_by({{ group }}) |>
    summarise(revenue = sum(value), .groups = "drop") |>
    slice_max(revenue, n = n, with_ties = FALSE)
}

# ---- Publish and log ---------------------------------------------------------

# Copy the rendered report into outputs/ under a name that includes the month,
# and return the new path (a file target)
publish_report <- function(report_files, month) {
  html <- report_files[grepl("\\.html$", report_files)]
  dir.create("outputs", showWarnings = FALSE)
  dest <- file.path("outputs", paste0("report_", sub("-", "_", month), ".html"))
  stopifnot(file.copy(html, dest, overwrite = TRUE))
  dest
}
