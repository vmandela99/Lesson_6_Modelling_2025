library(tidyverse)


contracteptive <- read_csv("contra_important_data.csv")

important_data <- contracteptive %>% select(1, 15, 10, 7, 18, 14, 17, 15)

library(tidyverse)
library(broom)

# Inspect unique values
unique(important_data$srhknowd1)

# Recode srhknowd1
important_data <- important_data %>%
  mutate(
    srhknowledge = case_when(
      srhknowd1 %in% c("No", "0") ~ "No",
      !is.na(srhknowd1) ~ "Yes",
      TRUE ~ NA_character_
    )
  )


# Keep only complete cases
model_data <- important_data %>%
  select(srhknowledge, age_group, wealthindex,
         site, evsex) %>%
  drop_na()

model_data <- model_data %>%
  mutate(
    srhknowledge = factor(srhknowledge, levels = c("No", "Yes")),
    age_group = factor(age_group),
    site = factor(site),
    evsex = factor(evsex)
  )

# Fit the logistic model
model <- glm(
  srhknowledge ~ age_group  + wealthindex +
    site + evsex ,
  data = model_data,
  family = binomial
)



# View summary
summary(model)

