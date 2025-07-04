
# Load necessary library (optional but recommended)
library(readr)
library(dplyr)
library(janitor)

# Import CSV file
data <- read_csv("https://raw.githubusercontent.com/vmandela99/blog_vickman/refs/heads/main/posts/M%26E_01_School_Feeding_Causal_inference_%26_Counterfactuals/school_feeding.csv")  # Use read.csv("your_file.csv") if readr is not installed

# Create age group variable
df <- data %>%
  mutate(age_group = case_when(
    age_hh >= 0 & age_hh <= 14  ~ "0-14 years",
    age_hh >= 15 & age_hh <= 34 ~ "15-34 years",
    age_hh >= 35 & age_hh <= 45 ~ "35-45 years",
    age_hh > 45                 ~ "45+ years"
  )) %>% 
  mutate(gender_of_household_head = ifelse(female_hh == 1, "Female", "Male"))

  

# Function to generate cross-tabulation tables
generate_cross_tabs <- function(data, group_var, cross_vars) {
  result <- list()
  
  for (var in cross_vars) {
    tab <- data %>%
      tabyl(!!sym(group_var), !!sym(var)) %>%
      adorn_totals("both") %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 1) %>%
      adorn_ns()
    
    result[[var]] <- tab
  }
  
  return(result)
}

# Define variables to cross-tabulate
cross_vars <- c("hhsize", "dirtfloor", "bathroom", "poverty_index")

# Generate cross-tabulations
cross_tab_results <- generate_cross_tabs(df, "gender_of_household_head", cross_vars)

# Print results
cross_tab_results
