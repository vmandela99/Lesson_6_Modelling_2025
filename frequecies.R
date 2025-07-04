# Load necessary library (optional but recommended)
library(readr)

# Import CSV file
data <- read_csv("https://raw.githubusercontent.com/vmandela99/blog_vickman/refs/heads/main/posts/M%26E_01_School_Feeding_Causal_inference_%26_Counterfactuals/school_feeding.csv")  # Use read.csv("your_file.csv") if readr is not installed


## Describe the data
## 5 verbs of programming - Select, Filter, arrange, summarize & Mutate(create new variable)

names(data)

# Load necessary library
library(dplyr)

# Function to clean and summarize data
summarize_school_feeding <- function(data) {
  
  # Select relevant columns and rename them
  cleaned_data <- data %>%
    select(
      poverty_index, educ_hh, hhsize, dirtfloor, bathroom, 
      school_distance, attendance_freq, attendance_rate
    ) %>%
    rename(
      PovertyIndex = poverty_index,
      Education_HH = educ_hh,
      Household_Size = hhsize,
      DirtFloor = dirtfloor,
      BathroomType = bathroom,
      SchoolDistance = school_distance,
      AttendanceFrequency = attendance_freq,
      AttendanceRate = attendance_rate
    )
  
  # Compute frequency tables for categorical variables
  categorical_vars <- c("DirtFloor", "BathroomType")
  freq_tables <- list()
  
  for (var in categorical_vars) {
    freq_table <- as.data.frame(table(cleaned_data[[var]]))
    freq_table$Percentage <- round(prop.table(table(cleaned_data[[var]])) * 100, 2)
    freq_tables[[var]] <- freq_table
  }
  
  # Compute summary statistics for numerical variables
  numerical_vars <- c("PovertyIndex", "Education_HH", "Household_Size", 
                      "SchoolDistance", "AttendanceFrequency", "AttendanceRate")
  
  summary_stats <- cleaned_data %>%
    summarise(across(all_of(numerical_vars), list(
      Mean = ~mean(. , na.rm = TRUE),
      SD = ~sd(. , na.rm = TRUE),
      Count = ~sum(!is.na(.))
    ), .names = "{.col}_{.fn}"))
  
  # Store results in a list
  results <- list(
    FrequencyTables = freq_tables,
    SummaryStatistics = summary_stats
  )
  
  return(results)
}

# Example usage
# Assuming your dataset is named 'school_feeding_data'
output_results <- summarize_school_feeding(data)

# Access saved outputs
output_results$FrequencyTables  # List of frequency tables
output_results$SummaryStatistics  # Data frame of summary stats

# View individual frequency tables
output_results$FrequencyTables$DirtFloor
output_results$FrequencyTables$BathroomType

# View summary statistics
print(output_results$SummaryStatistics)

### Cross Tabulations - chi-square values


