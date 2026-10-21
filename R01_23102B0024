# ============================================
# Practical 1
# Air Quality Data Cleaning Using R
# ============================================

# Import Dataset

air_quality <- tryCatch(
  {
    read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv")
  },
  error = function(e)
  {
    cat("Error:", e$message, "\n")
    NULL
  }
)

# Display first six records
head(air_quality)

# Display structure
str(air_quality)

# Display number of rows and columns
cat("Rows:", nrow(air_quality), "\n")
cat("Columns:", ncol(air_quality), "\n")

# Check for missing values
cat("Contains Missing Values:", any(is.na(air_quality)), "\n")


# Count total missing values

cat("Total Missing Values:", sum(is.na(air_quality)), "\n")


# ============================================
# Task 2: Understand NA, NULL, and NaN
# ============================================
# Example of NA
temperature <- c(28, 30, NA, 32)

print(temperature)
cat("Is third value NA? ", is.na(temperature[3]), "\n")
# Example of NULL
missing_object <- NULL

print(missing_object)
cat("Is object NULL? ", is.null(missing_object), "\n")

# Example of NaN
undefined_value <- 0 / 0

print(undefined_value)
cat("Is value NaN? ", is.nan(undefined_value), "\n")

# ============================================
# Task 3 : Missing Value Summary Function
# ============================================

missing_summary <- function(data)
{
  variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
  
  summary_table <- data.frame(
    Variable = character(),
    Total_Records = numeric(),
    Missing_Values = numeric(),
    Missing_Percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(variable in variables)
  {
    total_records <- nrow(data)
    
    missing_values <- sum(is.na(data[[variable]]))
    
    missing_percentage <- (missing_values / total_records) * 100
    
    summary_table <- rbind(
      summary_table,
      data.frame(
        Variable = variable,
        Total_Records = total_records,
        Missing_Values = missing_values,
        Missing_Percentage = round(missing_percentage,2)
      )
    )
    
    if(missing_percentage > 20)
    {
      warning(paste(variable,
                    "contains more than 20% missing values"))
    }
  }
  
  return(summary_table)
}

missing_report <- missing_summary(air_quality)

print(missing_report)

# ============================================
# Task 4 : Identify Invalid Numerical Results
# ============================================

air_quality$pollution_ratio <-
  air_quality$PM2.5 / air_quality$PM10
cat("NA Values:", sum(is.na(air_quality$pollution_ratio)), "\n")

cat("NaN Values:", sum(is.nan(air_quality$pollution_ratio)), "\n")

cat("Infinite Values:",
    sum(is.infinite(air_quality$pollution_ratio)),
    "\n")
air_quality$pollution_ratio[
  is.nan(air_quality$pollution_ratio)
] <- NA

air_quality$pollution_ratio[
  is.infinite(air_quality$pollution_ratio)
] <- NA

cat("After Cleaning\n")

cat("NA Values:",
    sum(is.na(air_quality$pollution_ratio)),
    "\n")

cat("NaN Values:",
    sum(is.nan(air_quality$pollution_ratio)),
    "\n")

cat("Infinite Values:",
    sum(is.infinite(air_quality$pollution_ratio)),
    "\n")


# ============================================
# Task 5 : Handle Missing Numerical Values
# ============================================
numeric_variables <- c(
  "PM2.5",
  "PM10",
  "SO2",
  "NO2",
  "TEMP",
  "WSPM"
)

for(variable in numeric_variables)
{
  
  if(variable %in% names(air_quality))
  {
    
    missing_before <- sum(is.na(air_quality[[variable]]))
    
    median_value <- median(air_quality[[variable]], na.rm = TRUE)
    
    air_quality[[variable]][
      is.na(air_quality[[variable]])
    ] <- median_value
    
    missing_after <- sum(is.na(air_quality[[variable]]))
    
    cat("\n----------------------------\n")
    cat("Variable :", variable, "\n")
    cat("Missing Before :", missing_before, "\n")
    cat("Median Used :", median_value, "\n")
    cat("Missing After :", missing_after, "\n")
  }
  
}

# ============================================
# Task 6 : Handle Missing Categorical Values
# ============================================

calculate_mode <- function(x)
{
  unique_values <- unique(x)
  
  unique_values <- unique_values[!is.na(unique_values)]
  
  counts <- tabulate(match(x, unique_values))
  
  mode_value <- unique_values[which.max(counts)]
  
  return(mode_value)
}

missing_before <- sum(is.na(air_quality$wd))

mode_wd <- calculate_mode(air_quality$wd)

air_quality$wd[
  is.na(air_quality$wd)
] <- mode_wd

missing_after <- sum(is.na(air_quality$wd))

cat("\nMode of wd :", mode_wd, "\n")
cat("Missing Before :", missing_before, "\n")
cat("Missing After :", missing_after, "\n")


# ============================================
# Task 7 : Error Handling using tryCatch()
# ============================================

clean_variable <- function(data, variable_name)
{
  tryCatch({
    
    # Check if variable exists
    if(!(variable_name %in% names(data)))
    {
      stop("Variable does not exist.")
    }
    
    # Check if variable is numeric
    if(!is.numeric(data[[variable_name]]))
    {
      stop("Variable is not numerical.")
    }
    
    # Check if all values are missing
    if(all(is.na(data[[variable_name]])))
    {
      stop("Variable contains only missing values.")
    }
    
    # Calculate median
    median_value <- median(data[[variable_name]], na.rm = TRUE)
    
    # Check if median is valid
    if(is.na(median_value))
    {
      stop("Median could not be calculated.")
    }
    
    # Replace missing values
    data[[variable_name]][is.na(data[[variable_name]])] <- median_value
    
    cat(variable_name, "cleaned successfully.\n")
    
    return(data[[variable_name]])
    
  },
  error = function(e)
  {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

clean_variable(air_quality, "PM2.5")
clean_variable(air_quality, "XYZ")
clean_variable(air_quality, "wd")

# ============================================
# Task 8 : Comparison Table
# ============================================

comparison_table <- data.frame(
  
  Variable = c(
    "PM2.5",
    "PM10",
    "SO2",
    "NO2",
    "TEMP",
    "WSPM",
    "wd"
  ),
  
  Missing_Before = c(
    925,
    718,
    935,
    1023,
    20,
    14,
    81
  ),
  
  Missing_After = c(
    sum(is.na(air_quality$PM2.5)),
    sum(is.na(air_quality$PM10)),
    sum(is.na(air_quality$SO2)),
    sum(is.na(air_quality$NO2)),
    sum(is.na(air_quality$TEMP)),
    sum(is.na(air_quality$WSPM)),
    sum(is.na(air_quality$wd))
  )
  
)

comparison_table$Values_Replaced <-
  comparison_table$Missing_Before -
  comparison_table$Missing_After

print(comparison_table)


# ============================================
# Task 9 : Visualization
# ============================================

before <- comparison_table$Missing_Before
after <- comparison_table$Missing_After

missing_matrix <- rbind(before, after)

barplot(
  missing_matrix,
  
  beside = TRUE,
  
  names.arg = comparison_table$Variable,
  
  col = c("red", "green"),
  
  main = "Missing Values Before and After Cleaning",
  
  xlab = "Variables",
  
  ylab = "Number of Missing Values",
  
  legend.text = c("Before Cleaning", "After Cleaning")
)

# ============================================
# Task 10 : Export Cleaned Dataset
# ============================================

write.csv(
  air_quality,
  "cleaned_air_quality_data.csv",
  row.names = FALSE
)

cat("Cleaned dataset exported successfully.\n")
