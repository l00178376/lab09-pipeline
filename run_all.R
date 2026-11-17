# Get the month from the command line
args <- commandArgs(trailingOnly = TRUE)
month <- if (length(args) > 0) args[1] else "2011-11"

source("R/functions.R")

#Get the  extract
sales <- read_extract(extract_path(month))

#validation
checked_sales <- validate_extract(sales)

#Preparing the data
cleaned_sales <- clean_sales(checked_sales)

# figures
monthly_summary <- summarise_month(cleaned_sales, month)
top_products <- top_by(cleaned_sales)


dir.create("outputs", showWarnings = FALSE)
#output file name
output_file <- paste0(
  "outputs/kpis_",
  sub("-", "_", month),
  ".csv"
)
write_csv(monthly_summary, output_file)