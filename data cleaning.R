# data_cleaning.R
# Read and clean up the 'retail_supply_chain_sales_cleaned.csv' file.
# Output: retail_supply_chain_sales_cleaned_v2.csv.

input_file <- "/Users/jindonghanjeff/Desktop/DUKE/Course/Fall_1/Data Science for Business/Team Project/DataSet/retail_supply_chain_sales_cleaned.csv"
output_file <- "retail_supply_chain_sales_cleaned_v2.csv"

cat("Reading:", input_file, "\n")
df <- read.csv(input_file, stringsAsFactors = FALSE)

# 1) Standardize column names to lowercase, separated by underscores.
names(df) <- tolower(names(df))

# Replace all non-alphanumeric and underscore characters with underscores to 
# prevent accidental deletion of letters.
names(df) <- gsub("[^[:alnum:]_]+", "_", names(df))

# Remove leading or trailing underscores
names(df) <- gsub("^_+|_+$", "", names(df))

# 2) Basic type conversions and cleaning


# Date column
if ("order_date" %in% names(df)) {
  df$order_date <- as.Date(df$order_date, format = "%Y-%m-%d")
}
if ("ship_date" %in% names(df)) {
  df$ship_date <- as.Date(df$ship_date, format = "%Y-%m-%d")
}

# Remove Leading and Trailing Spaces from a String
char_cols <- sapply(df, is.character)
df[char_cols] <- lapply(df[char_cols], trimws)

# Required Numeric Column Conversion
num_try <- c("sales", "quantity", "discount", "profit", "postal_code")
for (c in num_try) {
  if (c %in% names(df)) {
    # Some columns may already be numeric; `as.numeric` will return NA for 
    # characters, and additional processing may be needed if there are thousands separators.
    df[[c]] <- as.numeric(as.character(df[[c]]))
  }
}

# 3) Remove exact duplicate rows
before_rows <- nrow(df)
df <- unique(df)
after_rows <- nrow(df)
cat("Removed duplicates:", before_rows - after_rows, "rows\n")

# Missing value statistics (printed to console for debugging purposes)
na_counts <- sapply(df, function(x) sum(is.na(x)))
print(na_counts)

# 5) Handle discount anomalies: If the discount is greater than 1 and 
# appears to be a percentage (<=100), divide it by 100.
if ("discount" %in% names(df)) {
  high_idx <- which(!is.na(df$discount) & df$discount > 1 & df$discount <= 100)
  if (length(high_idx) > 0) {
    cat("Converting", length(high_idx), "discount values that look like percents (>1 and <=100) by dividing by 100\n")
    df$discount[high_idx] <- df$discount[high_idx] / 100
  }
  # Treat missing discounts as 0 (can be changed to NA as needed)
  df$discount[is.na(df$discount)] <- 0
  
  # Limit discounts to [0,1]
  df$discount <- pmin(pmax(df$discount, 0), 1)
}

# 6) Calculate delivery time (days); negative values are treated as NA
if (all(c("ship_date", "order_date") %in% names(df))) {
  df$delivery_time_days <- as.numeric(difftime(df$ship_date, df$order_date, units = "days"))
  df$delivery_time_days[df$delivery_time_days < 0] <- NA
}

# 7) Calculate profit margin (profit_margin = profit / sales), where sales==0 -> NA
# 7) （profit_margin = profit / sales），sales==0 -> NA
if (all(c("profit", "sales") %in% names(df))) {
  df$profit_margin <- ifelse(df$sales == 0, NA, df$profit / df$sales)
}

# 8) Discount Tiering
if ("discount" %in% names(df)) {
  df$discount_level <- cut(df$discount,
                           breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
                           labels = c("Low", "Medium", "High", "Very High"),
                           right = FALSE)
}

# 9) The returned column is normalized to logical values or factors.
if ("returned" %in% names(df)) {
  df$returned <- ifelse(df$returned %in% c(1, "1", "TRUE", "True", "true"), TRUE, FALSE)
}

# 10) Marking anomalies and simple cleaning rules (do not discard directly, only annotate)
df$anomaly <- ""
if ("quantity" %in% names(df)) {
  df$anomaly[df$quantity <= 0 | is.na(df$quantity)] <- paste(df$anomaly[df$quantity <= 0 | is.na(df$quantity)], "non_positive_quantity")
}
if ("sales" %in% names(df)) {
  neg_sales_idx <- which(!is.na(df$sales) & df$sales < 0)
  if (length(neg_sales_idx) > 0) {
    df$anomaly[neg_sales_idx] <- paste(df$anomaly[neg_sales_idx], "negative_sales")
  }
}

# Remove excess leading spaces
df$anomaly <- trimws(df$anomaly)

# 11) Final Output and Basic Report
cat("Final dimensions:", nrow(df), "rows x", ncol(df), "cols\n")

write.csv(df, output_file, row.names = FALSE)

# Also save as RDS for direct loading by R later
saveRDS(df, sub("\\.csv$", ".rds", output_file))

cat("Wrote:", output_file, "and RDS file.\n")

# Brief Statistics/Sample Output
if ("delivery_time_days" %in% names(df)) {
  cat("delivery_time_days summary:\n")
  print(summary(df$delivery_time_days))
}
if ("discount_level" %in% names(df)) {
  cat("discount_level counts:\n")
  print(table(df$discount_level, useNA = "ifany"))
}

cat("Head of cleaned data:\n")
print(utils::head(df))

# End of cleaning

