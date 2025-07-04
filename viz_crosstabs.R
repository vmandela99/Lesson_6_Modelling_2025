# Load necessary libraries
library(ggplot2)
library(dplyr)
library(officer)
library(flextable)
library(tidyverse)

# Sample cross-tabulation data
cross_tab_data <- list(
  dirtfloor = data.frame(
    gender_of_household_head = c("Female", "Male", "Total"),
    No = c(38.8, 39.7, 39.6),
    Yes = c(61.2, 60.3, 60.4)
  ),
  bathroom = data.frame(
    gender_of_household_head = c("Female", "Male", "Total"),
    No = c(45.0, 37.7, 38.4),
    Yes = c(55.0, 62.3, 61.6)
  )
)

# Function to create side-by-side bar charts
plot_cross_tab_dodge <- function(data) {
  melted_data <- pivot_longer(data, cols = -gender_of_household_head, names_to = "Category", values_to = "Percentage")
  
  ggplot(melted_data, aes(x = gender_of_household_head, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
    geom_text(aes(label = paste0(Percentage, "%")), 
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3, color = "black") +
    scale_fill_manual(values = c("No" = "#D2042D", "Yes" = "#000000")) +  # Cherry and black
    theme_minimal() +
    labs(title = "", x = "Gender of Household Head", y = "Percentage", fill = "Condition") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_blank())
}

# Plot and save
p1 <- plot_cross_tab_dodge(cross_tab_data$dirtfloor)
p2 <- plot_cross_tab_dodge(cross_tab_data$bathroom)

# Plot without title, y-axis limit to 100, and smaller size
ggplot(data, aes(x = gender_of_household_head, y = percentage, fill = dirtfloor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("cherry" = "#D2042D", "black" = "black")) +
  theme_minimal(base_size = 10) +  # Reduced font size
  labs(x = "Gender of Household Head", y = "Percentage") +
  theme(legend.title = element_blank(), legend.position = "bottom")

# Save plots
ggsave("dirtfloor_chart.png", p1, width = 7, height = 5)
ggsave("bathroom_chart.png", p2, width = 7, height = 5)
# Save plots
ggsave("dirtfloor_chart.png", p1, width = 7, height = 5)
ggsave("bathroom_chart.png", p2, width = 7, height = 5)