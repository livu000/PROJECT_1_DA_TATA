
# 1. DATA ANALYTICS - LIBRARY IMPORTS
# Install missing packages once if required (uncomment and run separately):
# install.packages(c(
#   "dplyr", "tidyr", "readr", "matrixStats", "ggplot2", "MASS",
#   "pracma", "caret", "tidymodels", "fs", "writexl"
# ))

# Data manipulation and CSV loading
library(dplyr)        # Filter, select, mutate, group_by, summarise
library(tidyr)        # Pivot, separate, unite, and reshape data
library(readr)        # Read and write CSV and delimited files

# Numerical computing
library(matrixStats)  # Fast row-wise and column-wise matrix statistics

# Data visualization
library(ggplot2)      # Create charts and visualizations

# Scientific computing and statistics
library(MASS)         # Statistical methods, distributions, and regression
library(pracma)       # Numerical methods and mathematical functions

# Machine learning
library(caret)        # Train models, tune parameters, and evaluate performance
library(tidymodels)   # Modeling, preprocessing, and validation workflows
# Prefer tidymodels functions where conflicts exist
tidymodels::tidymodels_prefer()

# Scientific & statistical computing / modeling (Equivalent to scipy/statsmodels)
library(stats)

# File and directory paths (Equivalent to pathlib.Path)
library(fs)

# Excel export (additional package needed for the notebook's .xlsx export)
library(writexl)


# 2. VERIFY INSTALLED LIBRARY VERSIONS
cat("R Base version:  ", R.version.string, "\n")
cat("dplyr version:   ", as.character(packageVersion("dplyr")), "\n")
cat("readr version:   ", as.character(packageVersion("readr")), "\n")
cat("ggplot2 version: ", as.character(packageVersion("ggplot2")), "\n")
cat("stats version:   ", as.character(packageVersion("stats")), "\n")
cat("caret version:   ", as.character(packageVersion("caret")), "\n")
cat("fs version:      ", as.character(packageVersion("fs")), "\n")
cat("tidymodels version:", as.character(packageVersion("tidymodels")), "\n")
cat("writexl version: ", as.character(packageVersion("writexl")), "\n")


# 3. LOAD DATASET INTO A DATA FRAME
# R accepts forward slashes in Windows paths.
# Empty strings and common text representations of missing values become NA.
file_path <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_1/Delinquency_prediction_dataset - Copy.csv"

df <- read.csv(
  file = file_path,
  na.strings = c("", "NA", "N/A", "null", "NULL"),
  stringsAsFactors = FALSE,
  check.names = FALSE # Keeps original column names exactly as in the CSV
)


# 4. CHECK DIMENSIONS (ROWS AND COLUMNS) AND PLOT
# Equivalent to Python: df.shape
total_rows <- nrow(df)
total_columns <- ncol(df)
cat("\nNumber of rows and columns:", total_rows, "rows,", total_columns, "columns\n")

# Bar plot for DataFrame rows and columns
par(mar = c(5, 5, 4, 2))
barplot(
  height = c(total_rows, total_columns),
  names.arg = c("Rows", "Columns"),
  main = "df rows and columns bar plot",
  xlab = "columns and rows of the df",
  ylab = "Count of rows and columns",
  col = c("steelblue", "darkorange")
)


# 5. INSPECT COLUMN NAMES, DATA TYPES, AND NON-NULL COUNTS
# Equivalent to Python: df.columns.tolist()
cat("\nThe column names are:-\n")
print(colnames(df))

# Equivalent to Python: df.info()
cat("\nInfo about the df (Structure & Data Types):\n")
str(df)

# Detailed df.info() equivalent table
# Column_Name, Data_Type, Non_Null_Count and Null_Count
df_info_table <- data.frame(
  Column_Name = colnames(df),
  Data_Type = vapply(df, function(x) class(x)[1], character(1)),
  Non_Null_Count = vapply(df, function(x) sum(!is.na(x)), numeric(1)),
  Null_Count = vapply(df, function(x) sum(is.na(x)), numeric(1)),
  row.names = NULL,
  check.names = FALSE
)
cat("\nDetailed Summary (Equivalent to pandas df.info()):\n")
print(df_info_table)


# 6. FUNCTION: NULL AND NON-NULL VALUE ANALYSIS
# Calculate null/non-null counts and percentages for one vector/column.
null_analysis <- function(series) {
  n_len <- length(series)
  null_count <- sum(is.na(series))
  null_percentage <- if (n_len > 0) (null_count / n_len) * 100 else 0
  non_null_count <- sum(!is.na(series))
  non_null_percentage <- if (n_len > 0) (non_null_count / n_len) * 100 else 0
  
  return(c(
    Null_Count = null_count,
    Null_Percentage = round(null_percentage, 2),
    Non_Null_Count = non_null_count,
    Non_Null_Percentage = round(non_null_percentage, 2)
  ))
}

# Apply null_analysis to every column and transpose so each column is one row.
null_summary <- as.data.frame(t(vapply(
  df,
  null_analysis,
  FUN.VALUE = c(
    Null_Count = 0,
    Null_Percentage = 0,
    Non_Null_Count = 0,
    Non_Null_Percentage = 0
  )
)))

cat("\nNull and Non-Null Analysis Summary:\n")
print(null_summary)
#View(null_summary) # Uncomment to open the data in RStudio's spreadsheet viewer.


# 7. TOTAL NULL VALUES AND OVERALL PERCENTAGE
total_cells <- nrow(df) * ncol(df) # Equivalent to pandas df.size
total_nulls <- sum(is.na(df))
total_nulls_percentage <- if (total_cells > 0) (total_nulls / total_cells) * 100 else 0

cat("\nTotal null values in cells -----> ", total_nulls, "\n")
cat("Total null values in cells percentage -----> ", round(total_nulls_percentage, 2), "%\n")


# 8. PLOT BAR AND PIE CHARTS FOR NULL VS NON-NULL VALUES PER COLUMN
colnames(null_summary) <- c(
  "null_count",
  "null_percentage",
  "non_null_count",
  "non_null_percentage"
)

for (column in rownames(null_summary)) {
  null_count <- null_summary[column, "null_count"]
  null_percentage <- null_summary[column, "null_percentage"]
  non_null_count <- null_summary[column, "non_null_count"]
  non_null_percentage <- null_summary[column, "non_null_percentage"]
  
  # Bar Chart: Null vs Non-Null Count
  par(mar = c(5, 5, 4, 2))
  barplot(
    height = c(null_count, non_null_count),
    names.arg = c("null", "non_null"),
    main = paste("null vs non_null count of", column),
    xlab = "Categories",
    ylab = "Count of Categories",
    col = c("tomato", "steelblue")
  )
  
  # Pie Chart: Percentage of Null vs Non-Null
  pie_values <- c(null_percentage, non_null_percentage)
  pie_labels <- paste0(c("null", "non_null"), " (", pie_values, "%)")
  # A tiny value is used for a 0% slice because base R pie() expects positive values.
  safe_pie_values <- ifelse(pie_values == 0, 0.0001, pie_values)
  pie(
    x = safe_pie_values,
    labels = pie_labels,
    main = paste("percentage of null vs non_null of", column),
    col = c("tomato", "steelblue")
  )
}


# 9. PARTICULAR COLUMN DATA VIEW (TOP 100 AND BOTTOM 100)
# Top 100 values of Employment_Status
cat("\nTop 100 values of Employment_Status:\n")
print(head(df[["Employment_Status"]], 100))

# Bottom 100 values of Income (matches the active Python notebook cell)
cat("\nBottom 100 values of Income:\n")
print(tail(df[["Income"]], 100))


# 10. FUNCTION: VIEW TOP, MIDDLE, AND BOTTOM 100 VALUES
# Inspect a vector/column before correcting data types.
view_100 <- function(series, col_name = NULL) {
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("COLUMN NAME:", col_name, "\n")
  cat(strrep("=", 80), "\n", sep = "")
  
  cat("\n--- TOP 100 VALUES ---\n")
  print(head(series, 100))
  
  n_len <- length(series)
  middle_start <- max(1, floor(n_len / 2) - 49)
  middle_end <- min(n_len, middle_start + 99)
  
  cat("\n--- MIDDLE 100 VALUES ---\n")
  if (n_len > 0) {
    print(series[middle_start:middle_end])
  } else {
    cat("Series is empty.\n")
  }
  
  cat("\n--- BOTTOM 100 VALUES ---\n")
  print(tail(series, 100))
}

# Inspect top, middle and bottom values for every column.
for (column in colnames(df)) {
  view_100(df[[column]], col_name = column)
}

# 11. UNIQUE VALUES AND VALUE COUNTS ANALYSIS
# Number of unique non-null values (Equivalent to Series.nunique()).
unique_value <- function(series) {
  num_unique <- length(unique(series[!is.na(series)]))
  print(num_unique)
}

cat("\nUnique count for Employment_Status:\n")
unique_value(df[["Employment_Status"]])

# Unique values array/vector (Equivalent to df.Employment_Status.unique()).
cat("\nUnique values array for Employment_Status:\n")
print(unique(df$Employment_Status))

# View unique values with counts and the number of unique non-null values.
unique_value_count <- function(series, col_name = NULL) {
  if (!is.null(col_name)) {
    cat("\n--------------------------------------------------\n")
    cat("Column:", col_name, "\n")
  }
  
  val_counts <- sort(table(series, useNA = "no"), decreasing = TRUE)
  cat("The unique_values and its counts ----->\n")
  print(val_counts)
  cat("No count of unique_values -----> ", length(unique(series[!is.na(series)])), "\n")
}

for (Column in colnames(df)) {
  unique_value_count(df[[Column]], col_name = Column)
}

# 12. DUPLICATED COUNT AND PERCENTAGE ANALYSIS
# Works for either a data frame (duplicate rows) or a vector (duplicate values).
duplicated_count <- function(data_obj) {
  dup_sum <- sum(duplicated(data_obj))
  total_len <- NROW(data_obj)
  dup_pct <- if (total_len > 0) (dup_sum / total_len) * 100 else 0
  
  cat("\nThe exact duplicated count:     ", dup_sum, "\n")
  cat("The percentage of duplicated:  ", round(dup_pct, 2), "%\n")
}

# To check duplicates for each individual column, uncomment this loop:
# for (Columns_1 in colnames(df)) {
#   cat("\nChecking column:", Columns_1, "\n")
#   duplicated_count(df[[Columns_1]])
# }

# Check exact duplicate rows across the entire DataFrame.
duplicated_count(df)


# 13. BASIC DESCRIPTIVE STATISTICS ON THE ORIGINAL DATA
# Equivalent to pandas df.describe() for numeric columns: count, mean, std,
# min, 25%, 50%, 75%, and max. pandas describe() excludes text columns by default.
describe_numeric <- function(dataframe) {
  numeric_data <- dataframe[vapply(dataframe, is.numeric, logical(1))]
  
  if (ncol(numeric_data) == 0) {
    cat("No numeric columns are available for describe().\n")
    return(invisible(NULL))
  }
  
  describe_one <- function(x) {
    values <- x[!is.na(x)]
    if (length(values) == 0) {
      return(c(count = 0, mean = NA_real_, std = NA_real_, min = NA_real_,
               `25%` = NA_real_, `50%` = NA_real_, `75%` = NA_real_, max = NA_real_))
    }
    
    q <- as.numeric(quantile(values, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 7))
    c(
      count = length(values),
      mean = mean(values),
      std = if (length(values) > 1) sd(values) else NA_real_,
      min = min(values),
      `25%` = q[1],
      `50%` = q[2],
      `75%` = q[3],
      max = max(values)
    )
  }
  
  result <- as.data.frame(do.call(cbind, lapply(numeric_data, describe_one)))
  colnames(result) <- names(numeric_data)
  rownames(result) <- c("count", "mean", "std", "min", "25%", "50%", "75%", "max")
  return(result)
}

cat("\nBasic statistical description of the original df:\n")
print(describe_numeric(df))


# 14. CREATE df_1 AND TRIM / CONVERT TEXT VALUES TO TITLE CASE
# Make a copy of the original DataFrame for complete-case statistical analysis.
df_1 <- df

# Apply trimws() and title case to character columns, similar to Python str.strip().title().
# Hyphens are temporarily surrounded with spaces so both sides are title-cased,
# e.g. "self-employed" becomes "Self-Employed".
title_case_python_like <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("-", " - ", x, fixed = TRUE)
  x <- tools::toTitleCase(x)
  x <- gsub(" - ", "-", x, fixed = TRUE)
  return(x)
}

trim_proper <- function(dataframe) {
  for (column in names(dataframe)) {
    if (is.character(dataframe[[column]])) {
      values <- trimws(dataframe[[column]])
      non_missing <- !is.na(values)
      values[non_missing] <- vapply(
        values[non_missing],
        title_case_python_like,
        character(1)
      )
      dataframe[[column]] <- values
    }
  }
  return(dataframe)
}

df_1 <- trim_proper(df_1)
cat("\nFirst rows of df_1 after trimming/title case:\n")
print(head(df_1))
cat("\nStructure of df_1 before dropping missing rows:\n")
str(df_1)


# 15. DROP ROWS WITH MISSING VALUES FOR STATISTICAL CALCULATIONS
# The Python notebook calls dropna() for each column. The final effect is to remove
# every row that contains NA in any column (complete-case / listwise deletion).
df_1 <- stats::na.omit(df_1)
cat("\nStructure of df_1 after dropping rows with any NA:\n")
str(df_1)

# Describe df_1 after complete-case deletion (pandas df_1.describe() equivalent).
cat("\nDescriptive summary of df_1 after drop-NA:\n")
print(describe_numeric(df_1))


# 16. DIVIDE NUMERIC AND CATEGORICAL COLUMNS
numeric_columns <- names(df_1)[vapply(df_1, is.numeric, logical(1))]
categorical_columns <- names(df_1)[vapply(
  df_1,
  function(x) is.character(x) || is.factor(x) || is.logical(x),
  logical(1)
)]
datetime_columns <- names(df_1)[vapply(
  df_1,
  function(x) inherits(x, "Date") || inherits(x, "POSIXt"),
  logical(1)
)]
timedelta_columns <- names(df_1)[vapply(df_1, inherits, logical(1), what = "difftime")]
classified_columns <- unique(c(
  numeric_columns, categorical_columns, datetime_columns, timedelta_columns
))
other_columns <- setdiff(names(df_1), classified_columns)

cat("Numeric Columns:", paste(numeric_columns, collapse = ", "), "\n")
cat("\nCategorical Columns:", paste(categorical_columns, collapse = ", "), "\n")
cat("\nDate/Time Columns:", paste(datetime_columns, collapse = ", "), "\n")
cat("\nTime Duration Columns:", paste(timedelta_columns, collapse = ", "), "\n")
cat("\nOther Columns:", paste(other_columns, collapse = ", "), "\n")
cat("\nTotal DataFrame Columns:", length(names(df_1)), "\n")
cat("Total Classified Columns:", length(classified_columns) + length(other_columns), "\n")
cat(
  "All Columns Accounted For:",
  setequal(names(df_1), c(classified_columns, other_columns)),
  "\n"
)

# 17. INSPECT AND STANDARDIZE Employment_Status IN df_1
cat("\nUnique Employment_Status values before standardization:\n")
print(unique(df_1$Employment_Status))

Employment_Status_Standardize <- c(
  "Emp" = "Employed",
  "Self-Employed" = "Self-Employed",
  "Unemployed" = "Unemployed",
  "Employed" = "Employed",
  "Retired" = "Retired"
)

status_values <- as.character(df_1$Employment_Status)
matched_status <- !is.na(status_values) & status_values %in% names(Employment_Status_Standardize)
status_values[matched_status] <- unname(Employment_Status_Standardize[status_values[matched_status]])
df_1$Employment_Status <- status_values

cat("\nEmployment_Status unique values after standardization:\n")
print(unique(df_1$Employment_Status))
cat("\nEmployment_Status value counts after standardization:\n")
print(sort(table(df_1$Employment_Status, useNA = "ifany"), decreasing = TRUE))


# 18. DETAILED DESCRIPTIVE STATISTICS FOR NUMERIC COLUMNS
# The following functions translate the Python notebook's statistics calculations.
# R's quantile(type = 7) uses the same default linear-interpolation convention as
# pandas quantile() for the standard percentile calculations.

# 18.1 Mean, median and mode
mean_median_mode <- function(series) {
  mean_value <- round(mean(series), 5)
  median_value <- round(median(series), 5)
  
  values <- unique(series)
  value_counts <- tabulate(match(series, values), nbins = length(values))
  mode_value <- round(values[value_counts == max(value_counts)], 5)
  
  return(list(
    mean_value = mean_value,
    median_value = median_value,
    mode_value = mode_value
  ))
}

# 18.2 Minimum, maximum, range and coefficient of range
min_max_range <- function(series) {
  min_value <- round(min(series), 5)
  max_value <- round(max(series), 5)
  range_value <- round(max_value - min_value, 5)
  denominator <- max_value + min_value
  coefficient_range <- if (denominator != 0) round(range_value / denominator, 5) else NaN
  
  return(list(
    min_value = min_value,
    max_value = max_value,
    range_value = range_value,
    coefficient_range = coefficient_range
  ))
}

# 18.3 Percentiles
percentiles <- function(series) {
  probs <- c(0.01, 0.05, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50,
             0.60, 0.70, 0.75, 0.80, 0.90, 0.95, 0.99)
  values <- as.numeric(quantile(series, probs = probs, names = FALSE, type = 7))
  names(values) <- c("p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50",
                     "p60", "p70", "p75", "p80", "p90", "p95", "p99")
  return(values)
}

# 18.4 Quartiles, IQR, quartile deviation and coefficient of quartile deviation
quartiles_iqr_qd_cqd <- function(series) {
  quartile_values <- as.numeric(quantile(series, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 7))
  q1 <- quartile_values[1]
  q2 <- quartile_values[2]
  q3 <- quartile_values[3]
  iqr <- round(q3 - q1, 5)
  qd <- round(iqr / 2, 5)
  denominator <- q3 + q1
  cqd <- if (denominator != 0) round(iqr / denominator, 5) else NaN
  
  return(list(q1 = round(q1, 5), q2 = round(q2, 5), q3 = round(q3, 5),
              iqr = iqr, qd = qd, cqd = cqd))
}

# 18.5 Mean absolute deviation, median absolute deviation and scaled MAD
absolute_deviations <- function(series) {
  mean_value <- mean(series)
  median_value <- median(series)
  mean_absolute_deviation <- round(mean(abs(series - mean_value)), 5)
  median_absolute_deviation <- round(median(abs(series - median_value)), 5)
  scaled_mad <- round(median_absolute_deviation * 1.4826, 5)
  
  return(list(
    mean_absolute_deviation = mean_absolute_deviation,
    median_absolute_deviation = median_absolute_deviation,
    scaled_mad = scaled_mad
  ))
}

# 18.6 Sample/population variance, standard deviation and standard error
variance_std_error <- function(series) {
  sample_variance <- if (length(series) > 1) round(var(series), 5) else NaN
  population_variance <- round(mean((series - mean(series))^2), 5)
  sample_std <- if (length(series) > 1) round(sd(series), 5) else NaN
  population_std <- round(sqrt(population_variance), 5)
  standard_error <- if (length(series) > 1) round(sd(series) / sqrt(length(series)), 5) else NaN
  
  return(list(
    sample_variance = sample_variance,
    population_variance = population_variance,
    sample_std = sample_std,
    population_std = population_std,
    standard_error = standard_error
  ))
}

# 18.7 Coefficient of variation (sample SD / mean * 100)
coefficient_variation <- function(series) {
  mean_value <- mean(series)
  std_value <- if (length(series) > 1) sd(series) else NaN
  cv <- if (length(series) > 1 && mean_value != 0) round((std_value / mean_value) * 100, 5) else NaN
  return(cv)
}

# 18.8 Unbiased sample skewness and Fisher excess/Pearson kurtosis.
# These formulas follow the bias-corrected SciPy definitions used in the notebook.
skewness_kurtosis <- function(series) {
  n <- length(series)
  distinct_count <- length(unique(series))
  skewness_value <- NaN
  excess_kurtosis <- NaN
  pearson_kurtosis <- NaN
  
  if (n >= 3 && distinct_count > 1) {
    centered <- series - mean(series)
    m2 <- mean(centered^2)
    m3 <- mean(centered^3)
    if (m2 > 0) {
      g1 <- m3 / (m2^(3 / 2))
      skewness_value <- round((sqrt(n * (n - 1)) / (n - 2)) * g1, 5)
    }
  }
  
  if (n >= 4 && distinct_count > 1) {
    centered <- series - mean(series)
    m2 <- mean(centered^2)
    m4 <- mean(centered^4)
    if (m2 > 0) {
      g2 <- (m4 / (m2^2)) - 3
      excess_kurtosis <- round(((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * g2 + 6), 5)
      pearson_kurtosis <- round(excess_kurtosis + 3, 5)
    }
  }
  
  return(list(
    skewness_value = skewness_value,
    excess_kurtosis = excess_kurtosis,
    pearson_kurtosis = pearson_kurtosis
  ))
}

# 18.9 Root mean square statistics
Root_Mean_Square <- function(series) {
  valid_count <- length(series)
  unique_count <- length(unique(series))
  total_sum <- round(sum(series), 5)
  rms_value <- round(sqrt(mean(series^2)), 5)
  return(list(
    valid_count = valid_count,
    unique_count = unique_count,
    total_sum = total_sum,
    rms_value = rms_value
  ))
}

# 18.10 Outlier statistics using the IQR method
outlier_statistics <- function(series) {
  quartiles <- as.numeric(quantile(series, probs = c(0.25, 0.75), names = FALSE, type = 7))
  q1 <- quartiles[1]
  q3 <- quartiles[2]
  iqr <- q3 - q1
  lower_fence <- round(q1 - (1.5 * iqr), 5)
  upper_fence <- round(q3 + (1.5 * iqr), 5)
  outlier_count <- sum(series < lower_fence | series > upper_fence)
  return(list(
    lower_fence = lower_fence,
    upper_fence = upper_fence,
    outlier_count = as.integer(outlier_count)
  ))
}

# Apply all functions to numeric columns.
for (column in numeric_columns) {
  # Convert to numeric and replace +/- infinity with NA, equivalent to pandas to_numeric.
  series <- suppressWarnings(as.numeric(as.character(df_1[[column]])))
  series[!is.finite(series)] <- NA_real_
  series <- series[!is.na(series)]
  
  if (length(series) == 0) {
    cat("\nColumn Name:", column, "\n")
    cat("No valid numeric values available.\n")
    next
  }
  
  central <- mean_median_mode(series)
  spread <- min_max_range(series)
  pct <- percentiles(series)
  quart <- quartiles_iqr_qd_cqd(series)
  deviations <- absolute_deviations(series)
  variance_results <- variance_std_error(series)
  cv <- coefficient_variation(series)
  shape <- skewness_kurtosis(series)
  rms <- Root_Mean_Square(series)
  outliers <- outlier_statistics(series)
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Column Name:", column, "\n")
  cat(strrep("=", 60), "\n", sep = "")
  
  cat("\n--- DATA SUMMARY ---\n")
  cat("Valid Count:", rms$valid_count, "\n")
  cat("Missing/Invalid Count:", length(df_1[[column]]) - rms$valid_count, "\n")
  cat("Unique Values:", rms$unique_count, "\n")
  cat("Sum:", rms$total_sum, "\n")
  cat("Root Mean Square:", rms$rms_value, "\n")
  
  cat("\n--- CENTRAL TENDENCY ---\n")
  cat("Mean:", central$mean_value, "\n")
  cat("Median:", central$median_value, "\n")
  cat("Mode:", paste(central$mode_value, collapse = ", "), "\n")
  
  cat("\n--- MINIMUM, MAXIMUM AND RANGE ---\n")
  cat("Minimum:", spread$min_value, "\n")
  cat("Maximum:", spread$max_value, "\n")
  cat("Range:", spread$range_value, "\n")
  cat("Coefficient of Range:", spread$coefficient_range, "\n")
  
  cat("\n--- PERCENTILES ---\n")
  cat("P1:", pct["p01"], "\n")
  cat("P5:", pct["p05"], "\n")
  cat("P10:", pct["p10"], "\n")
  cat("P20:", pct["p20"], "\n")
  cat("P25:", pct["p25"], "\n")
  cat("P30:", pct["p30"], "\n")
  cat("P40:", pct["p40"], "\n")
  cat("P50:", pct["p50"], "\n")
  cat("P60:", pct["p60"], "\n")
  cat("P70:", pct["p70"], "\n")
  cat("P75:", pct["p75"], "\n")
  cat("P80:", pct["p80"], "\n")
  cat("P90:", pct["p90"], "\n")
  cat("P95:", pct["p95"], "\n")
  cat("P99:", pct["p99"], "\n")
  
  cat("\n--- QUARTILES AND QUARTILE DISPERSION ---\n")
  cat("Q1:", quart$q1, "\n")
  cat("Q2:", quart$q2, "\n")
  cat("Q3:", quart$q3, "\n")
  cat("IQR:", quart$iqr, "\n")
  cat("Quartile Deviation (QD):", quart$qd, "\n")
  cat("Coefficient of Quartile Deviation (CQD):", quart$cqd, "\n")
  
  cat("\n--- ABSOLUTE DEVIATIONS ---\n")
  cat("Mean Absolute Deviation:", deviations$mean_absolute_deviation, "\n")
  cat("Median Absolute Deviation (MAD):", deviations$median_absolute_deviation, "\n")
  cat("Scaled MAD:", deviations$scaled_mad, "\n")
  
  cat("\n--- VARIANCE, STANDARD DEVIATION AND ERROR ---\n")
  cat("Sample Variance:", variance_results$sample_variance, "\n")
  cat("Population Variance:", variance_results$population_variance, "\n")
  cat("Sample Standard Deviation:", variance_results$sample_std, "\n")
  cat("Population Standard Deviation:", variance_results$population_std, "\n")
  cat("Standard Error:", variance_results$standard_error, "\n")
  
  cat("\n--- RELATIVE DISPERSION ---\n")
  cat("Coefficient of Variation (%):", cv, "\n")
  
  cat("\n--- DISTRIBUTION SHAPE ---\n")
  cat("Skewness:", shape$skewness_value, "\n")
  cat("Excess Kurtosis (Fisher):", shape$excess_kurtosis, "\n")
  cat("Pearson Kurtosis:", shape$pearson_kurtosis, "\n")
  
  cat("\n--- OUTLIER DETECTION (IQR METHOD) ---\n")
  cat("Lower Fence:", outliers$lower_fence, "\n")
  cat("Upper Fence:", outliers$upper_fence, "\n")
  cat("Outlier Count:", outliers$outlier_count, "\n")
}


# 19. RE-DO EDA ON df_1
cat("\nNumber of rows and columns:", nrow(df_1), "rows,", ncol(df_1), "columns\n")
cat("Number of rows:", nrow(df_1), "\n")
cat("Number of columns:", ncol(df_1), "\n")
cat("\nInformation about df_1:\n")
str(df_1)

null_summary_df1 <- as.data.frame(t(vapply(
  df_1,
  null_analysis,
  FUN.VALUE = c(
    Null_Count = 0,
    Null_Percentage = 0,
    Non_Null_Count = 0,
    Non_Null_Percentage = 0
  )
)))
cat("\nNull and Non-Null Summary for df_1:\n")
print(null_summary_df1)

total_nulls_df1 <- sum(is.na(df_1))
total_cells_df1 <- nrow(df_1) * ncol(df_1)
total_nulls_percentage_df1 <- if (total_cells_df1 > 0) total_nulls_df1 / total_cells_df1 * 100 else 0
cat("Total null values in df_1:", total_nulls_df1, "\n")
cat("Total null values percentage in df_1:", round(total_nulls_percentage_df1, 2), "%\n")

# View top, middle, and bottom 100 values from every df_1 column.
for (column in colnames(df_1)) {
  view_100(df_1[[column]], col_name = column)
}

# Unique value counts for every df_1 column.
for (column in colnames(df_1)) {
  unique_value_count(df_1[[column]], col_name = column)
}

# Exact duplicate rows and duplicate percentage.
duplicated_count(df_1)

# Display every record that belongs to a duplicate group.
duplicated_records <- function(dataframe) {
  duplicate_rows <- dataframe[duplicated(dataframe) | duplicated(dataframe, fromLast = TRUE), , drop = FALSE]
  cat("\n--- DUPLICATE RECORDS ---\n")
  if (nrow(duplicate_rows) == 0) {
    cat("No duplicate records found.\n")
  } else {
    print(duplicate_rows)
  }
}
duplicated_records(df_1)

cat("\n--- FINAL df_1 HEAD ---\n")
print(head(df_1))
cat("\n--- FINAL df_1 INFORMATION ---\n")
str(df_1)
cat("\nFinal df_1 Shape:", nrow(df_1), "rows x", ncol(df_1), "columns\n")
cat("\nFinal df_1 Null Counts:\n")
print(colSums(is.na(df_1)))
cat("\nFinal df_1 Duplicate Count:\n")
print(sum(duplicated(df_1)))

# 20. BOX PLOTS FOR NUMERIC COLUMNS IN df_1
box_plot <- function(numeric_columns) {
  for (column in numeric_columns) {
    boxplot(
      df_1[[column]],
      main = paste("Box Plot of", column),
      ylab = column,
      outline = FALSE,
      col = "steelblue"
    )
    grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")
  }
}
box_plot(numeric_columns)


# 21. VIOLIN PLOTS FOR NUMERIC COLUMNS IN df_1
violin_chart <- function(numeric_columns) {
  for (column in numeric_columns) {
    plot_data <- data.frame(value = df_1[[column]])
    p <- ggplot(plot_data, aes(x = "", y = value)) +
      geom_violin(fill = "steelblue", colour = "grey25", na.rm = TRUE) +
      labs(title = paste("Violin Plot of", column), x = NULL, y = column) +
      theme_minimal()
    print(p)
  }
}
violin_chart(numeric_columns)

# 22. CREATE df_2 FROM THE ORIGINAL DATA
# df_2 retains the original rows and missing values; selected columns will later
# have missing numeric values filled using medians calculated from df_1.
df_2 <- df
cat("\nInitial structure of df_2:\n")
str(df_2)

# 23. TRIM/TITLE-CASE TEXT AND STANDARDIZE Employment_Status IN df_2
df_2 <- trim_proper(df_2)
cat("\nFirst rows of df_2 after trimming/title case:\n")
print(head(df_2))
cat("\nStructure of df_2 after trimming/title case:\n")
str(df_2)

df_2_status_values <- as.character(df_2$Employment_Status)
matched_status_df2 <- !is.na(df_2_status_values) & df_2_status_values %in% names(Employment_Status_Standardize)
df_2_status_values[matched_status_df2] <- unname(Employment_Status_Standardize[df_2_status_values[matched_status_df2]])
df_2$Employment_Status <- df_2_status_values

# Verification is performed on df_2 (the DataFrame changed in this section).
cat("\nUnique Employment_Status values in df_2:\n")
print(unique(df_2$Employment_Status))
cat("\nEmployment_Status value counts in df_2, including NA:\n")
print(sort(table(df_2$Employment_Status, useNA = "ifany"), decreasing = TRUE))


# 24. FILL SELECTED MISSING VALUES USING MEDIANS FROM df_1
income_median <- median(df_1[["Income"]], na.rm = TRUE)
credit_score_median <- median(df_1[["Credit_Score"]], na.rm = TRUE)
loan_balance_median <- median(df_1[["Loan_Balance"]], na.rm = TRUE)

# Fill only NA values in df_2. Non-missing values remain unchanged.
df_2$Income[is.na(df_2$Income)] <- income_median
df_2$Credit_Score[is.na(df_2$Credit_Score)] <- credit_score_median
df_2$Loan_Balance[is.na(df_2$Loan_Balance)] <- loan_balance_median

cat("\nRemaining missing values in selected df_2 columns:\n")
print(colSums(is.na(df_2[c("Income", "Credit_Score", "Loan_Balance")])))


# 25. RE-DO EDA ON df_2
cat("\nNumber of rows and columns:", nrow(df_2), "rows,", ncol(df_2), "columns\n")
cat("Number of rows:", nrow(df_2), "\n")
cat("Number of columns:", ncol(df_2), "\n")
cat("\nInformation about df_2:\n")
str(df_2)

null_summary_df2 <- as.data.frame(t(vapply(
  df_2,
  null_analysis,
  FUN.VALUE = c(
    Null_Count = 0,
    Null_Percentage = 0,
    Non_Null_Count = 0,
    Non_Null_Percentage = 0
  )
)))
cat("\nNull and Non-Null Summary for df_2:\n")
print(null_summary_df2)

total_nulls_df2 <- sum(is.na(df_2))
total_cells_df2 <- nrow(df_2) * ncol(df_2)
total_nulls_percentage_df2 <- if (total_cells_df2 > 0) total_nulls_df2 / total_cells_df2 * 100 else 0
cat("Total null values in df_2:", total_nulls_df2, "\n")
cat("Total null values percentage in df_2:", round(total_nulls_percentage_df2, 2), "%\n")

# View top, middle, and bottom 100 values from every df_2 column.
for (column in colnames(df_2)) {
  view_100(df_2[[column]], col_name = column)
}

# Unique value counts for every df_2 column.
for (column in colnames(df_2)) {
  unique_value_count(df_2[[column]], col_name = column)
}

# Exact duplicate rows and duplicate percentage.
duplicated_count(df_2)

# Display all records that belong to a duplicate group.
duplicated_records(df_2)

cat("\n--- FINAL df_2 HEAD ---\n")
print(head(df_2))
cat("\n--- FINAL df_2 INFORMATION ---\n")
str(df_2)
cat("\nFinal df_2 Shape:", nrow(df_2), "rows x", ncol(df_2), "columns\n")
cat("\nFinal df_2 Null Counts:\n")
print(colSums(is.na(df_2)))
cat("\nFinal df_2 Duplicate Count:\n")
print(sum(duplicated(df_2)))


# 26. EXPORT THE CLEANED DATA AS CSV AND EXCEL
# These paths match the destination folder in the Python notebook.
output_dir <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_1/Excel_Files_Folder"
fs::dir_create(output_dir)

csv_output_path <- fs::path(output_dir, "Delinquency_Cleaned_Data_RStudio.csv")
xlsx_output_path <- fs::path(output_dir, "Delinquency_Cleaned_Data_RStudio.xlsx")

# Export to CSV (row.names = FALSE is equivalent to index=False in pandas).
write.csv(df_2, file = csv_output_path, row.names = FALSE, na = "")

# Export to Excel. writexl writes one worksheet by default.
writexl::write_xlsx(df_2, path = xlsx_output_path)

cat("\nExport complete. Files saved to:\n")
cat(csv_output_path, "\n")
cat(xlsx_output_path, "\n")

# Optional: open the final data in RStudio's Data Viewer.
# View(df_2)
