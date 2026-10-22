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

# Scientific & statistical computing / modeling (Equivalent to scipy & statsmodels)
library(stats)
# stats (Built-in): Statistical tests, probability distributions,
# generalized linear models, and scientific calculations.

# File and directory paths (Equivalent to pathlib.Path)
library(fs)
# fs (or base file.path): Manage file paths safely across operating systems.


# ------------------------------------------------------------------------------
# 2. VERIFY INSTALLED LIBRARY VERSIONS
# ------------------------------------------------------------------------------
cat("R Base version:  ", R.version.string, "\n")
cat("dplyr version:   ", as.character(packageVersion("dplyr")), "\n")
cat("readr version:   ", as.character(packageVersion("readr")), "\n")
cat("ggplot2 version: ", as.character(packageVersion("ggplot2")), "\n")
cat("stats version:   ", as.character(packageVersion("stats")), "\n")
cat("caret version:   ", as.character(packageVersion("caret")), "\n")
cat("fs version:      ", as.character(packageVersion("fs")), "\n")


# ------------------------------------------------------------------------------
# 3. LOAD DATASET into a DATA FRAME
# ------------------------------------------------------------------------------
# Note: In R, use forward slashes (/) or raw strings r"(...)" for Windows paths.
# na.strings = c("", "NA", "null", "NULL") ensures empty cells become NA just like pandas.
file_path <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_1/Delinquency_prediction_dataset - Copy.csv"

df <- read.csv(
  file = file_path,
  na.strings = c("", "NA", "N/A", "null", "NULL"),
  stringsAsFactors = FALSE,
  check.names = FALSE # Keeps original column names exactly as in the CSV
)


# ------------------------------------------------------------------------------
# 4. CHECK DIMENSIONS (ROWS AND COLUMNS) & PLOT
# ------------------------------------------------------------------------------
# To check how many rows and columns (Equivalent to df.shape)
total_rows <- nrow(df)
total_columns <- ncol(df)
cat("\nNumber of rows and columns:", total_rows, "rows,", total_columns, "columns\n")

# Bar plot for df rows and columns
# (In RStudio, plots appear automatically in the bottom-right 'Plots' pane)
par(mar = c(5, 5, 4, 2)) # Tight layout margin adjustment
barplot(
  height = c(total_rows, total_columns),
  names.arg = c("Rows", "Columns"),
  main = "df rows and columns bar plot",
  xlab = "columns and rows of the df",
  ylab = "Count of rows and columns",
  col = c("steelblue", "darkorange")
)


# ------------------------------------------------------------------------------
# 5. INSPECT COLUMN NAMES, DATA TYPES, AND NON-NULL COUNTS
# ------------------------------------------------------------------------------
# To print column names (Equivalent to df.columns.tolist())
cat("\nThe column names are:-\n")
print(colnames(df))

# To check DataTypes and structure (Equivalent to df.info())
cat("\nInfo about the df (Structure & Data Types):\n")
str(df)

# Detailed df.info() equivalent table showing Column, Data_Type, and Non_Null_Count
df_info_table <- data.frame(
  Column_Name = colnames(df),
  Data_Type = sapply(df, function(x) class(x)[1]),
  Non_Null_Count = sapply(df, function(x) sum(!is.na(x))),
  Null_Count = sapply(df, function(x) sum(is.na(x))),
  row.names = NULL
)
cat("\nDetailed Summary (Equivalent to pandas df.info()):\n")
print(df_info_table)


# ------------------------------------------------------------------------------
# 6. FUNCTION: NULL AND NON-NULL VALUE ANALYSIS
# ------------------------------------------------------------------------------
# PURPOSE: Calculate null and non-null counts and percentages for one vector/column
null_analysis <- function(series) {
  
  n_len <- length(series)
  
  # Count missing values in the input vector
  null_count <- sum(is.na(series))
  
  # Calculate the percentage of missing values
  null_percentage <- if (n_len > 0) (null_count / n_len) * 100 else 0
  
  # Count non-missing values in the input vector
  non_null_count <- sum(!is.na(series))
  
  # Calculate the percentage of non-missing values
  non_null_percentage <- if (n_len > 0) (non_null_count / n_len) * 100 else 0
  
  # Return all results as a named numeric vector (Equivalent to pd.Series)
  return(c(
    Null_Count = null_count,
    Null_Percentage = round(null_percentage, 2),
    Non_Null_Count = non_null_count,
    Non_Null_Percentage = round(non_null_percentage, 2)
  ))
}

# Apply null_analysis to every column in the DataFrame and transpose (t)
# so each column appears as one row
null_summary <- as.data.frame(t(sapply(df, null_analysis)))

# Display null and non-null statistics for all columns
cat("\nNull and Non-Null Analysis Summary:\n")
print(null_summary)
# View(null_summary) # Uncomment to open interactive spreadsheet tab in RStudio


# ------------------------------------------------------------------------------
# 7. TOTAL NULL VALUES AND OVERALL PERCENTAGE
# ------------------------------------------------------------------------------
total_cells <- nrow(df) * ncol(df) # Equivalent to df.size
total_nulls <- sum(is.na(df))
total_nulls_percentage <- (total_nulls / total_cells) * 100

cat("\ntotal null values in cells -----> ", total_nulls, "\n")
cat("total null values in cells percentage -----> ", total_nulls_percentage, "%\n")


# ------------------------------------------------------------------------------
# 8. PLOT BAR AND PIE CHARTS FOR NULL VS NON-NULL VALUES PER COLUMN
# ------------------------------------------------------------------------------
# Standardize the column names of null_summary
colnames(null_summary) <- c(
  "null_count",
  "null_percentage",
  "non_null_count",
  "non_null_percentage"
)

for (column in rownames(null_summary)) {
  
  # Retrieve null_count and null_percentage for the current column
  null_count <- null_summary[column, "null_count"]
  null_percentage <- null_summary[column, "null_percentage"]
  
  # Retrieve non_null_count and non_null_percentage for the current column
  non_null_count <- null_summary[column, "non_null_count"]
  non_null_percentage <- null_summary[column, "non_null_percentage"]
  
  # 1. Bar Chart: Null vs Non-Null Count
  par(mar = c(5, 5, 4, 2))
  barplot(
    height = c(null_count, non_null_count),
    names.arg = c("null", "non_null"),
    main = paste("null vs non_null count of", column),
    xlab = "Categories",
    ylab = "Count of Categories",
    col = c("tomato", "steelblue")
  )
  
  # 2. Pie Chart: Percentage of Null vs Non-Null
  # Add slice percentages to labels for clarity
  pie_values <- c(null_percentage, non_null_percentage)
  pie_labels <- paste0(c("null", "non_null"), " (", pie_values, "%)")
  
  # Note: Base R pie() requires positive numbers > 0 to draw a slice,
  # so we filter out 0% slices only if needed, or replace 0 with a tiny epsilon
  safe_pie_values <- ifelse(pie_values == 0, 0.0001, pie_values)
  
  pie(
    x = safe_pie_values,
    labels = pie_labels,
    main = paste("percentage of null vs non_null of", column),
    col = c("tomato", "steelblue")
  )
}


# ------------------------------------------------------------------------------
# 9. PARTICULAR COLUMN DATA VIEW (TOP 100 & BOTTOM 100)
# ------------------------------------------------------------------------------
# Top 100 values of Employment_Status
cat("\nTop 100 values of Employment_Status:\n")
print(head(df[["Employment_Status"]], 100))

# Bottom 100 values of Employment_Status
cat("\nBottom 100 values of Employment_Status:\n")
print(tail(df[["Employment_Status"]], 100))


# ------------------------------------------------------------------------------
# 10. FUNCTION: VIEW TOP, MIDDLE, AND BOTTOM 100 VALUES
# ------------------------------------------------------------------------------
# PURPOSE: Inspect values in a column before correcting data types
view_100 <- function(series, col_name = NULL) {
  
  # If col_name is not passed explicitly, capture the expression name
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  # Display the column name header
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("COLUMN NAME:", col_name, "\n")
  cat(strrep("=", 80), "\n", sep = "")
  
  # View the first 100 values
  cat("\n--- TOP 100 VALUES ---\n")
  print(head(series, 100))
  
  # Calculate the starting and ending positions for the middle 100 values
  # (Adjusted for R's 1-based indexing)
  n_len <- length(series)
  middle_start <- max(1, floor(n_len / 2) - 49)
  middle_end <- min(n_len, middle_start + 99)
  
  # View the middle 100 values
  cat("\n--- MIDDLE 100 VALUES ---\n")
  if (n_len > 0) {
    print(series[middle_start:middle_end])
  } else {
    cat("Series is empty.\n")
  }
  
  # View the last 100 values
  cat("\n--- BOTTOM 100 VALUES ---\n")
  print(tail(series, 100))
}

# Loop through every column in the DataFrame and inspect top, middle, and bottom 100
for (column in colnames(df)) {
  view_100(df[[column]], col_name = column)
}


# ------------------------------------------------------------------------------
# 11. UNIQUE VALUES AND VALUE COUNTS ANALYSIS
# ------------------------------------------------------------------------------
# Function to get the count of unique non-null values (Equivalent to series.nunique())
unique_value <- function(series) {
  # na.omit() matches pandas default dropna=True in .nunique()
  num_unique <- length(unique(na.omit(series)))
  print(num_unique)
}

cat("\nUnique count for Employment_Status:\n")
unique_value(df[["Employment_Status"]])

# To get the unique values as an array/vector (Equivalent to df.Employment_Status.unique())
cat("\nUnique values array for Employment_Status:\n")
print(unique(df$Employment_Status))

# Function to view unique values with their counts and total unique count
unique_value_count <- function(series, col_name = NULL) {
  if (!is.null(col_name)) {
    cat("\n--------------------------------------------------\n")
    cat("Column:", col_name, "\n")
  }
  
  # table() counts occurrences; sort(..., decreasing = TRUE) matches pandas .value_counts()
  val_counts <- sort(table(series, useNA = "no"), decreasing = TRUE)
  
  cat("The unique_values and its counts ----->\n")
  print(val_counts)
  
  cat("No count of unique_values -----> ", length(unique(na.omit(series))), "\n")
}

# Loop through all columns to display unique value counts
for (Column in colnames(df)) {
  unique_value_count(df[[Column]], col_name = Column)
}


# ------------------------------------------------------------------------------
# 12. DUPLICATED COUNT AND PERCENTAGE ANALYSIS
# ------------------------------------------------------------------------------
# PURPOSE: Works for both the entire DataFrame (row duplicates) and single columns
duplicated_count <- function(data_obj) {
  
  # duplicated() works on both data.frames (rows) and vectors (elements)
  dup_sum <- sum(duplicated(data_obj))
  
  # NROW() safely returns number of rows for a DataFrame OR length for a 1D vector
  total_len <- NROW(data_obj)
  dup_pct <- if (total_len > 0) (dup_sum / total_len) * 100 else 0
  
  cat("\nThe exact duplicated count:     ", dup_sum, "\n")
  cat("The percentage of duplicated:  ", dup_pct, "%\n")
}

# Uncomment below to check duplicates for each individual column:
# for (Columns_1 in colnames(df)) {
#   cat("\nChecking column:", Columns_1)
#   duplicated_count(df[[Columns_1]])
# }

# Check exact duplicate rows across the entire DataFrame
duplicated_count(df)