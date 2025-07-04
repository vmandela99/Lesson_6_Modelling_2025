library(readxl)
library(dplyr)

dairy_data <- read_xlsx("Biweekly1 Final.xlsx")


modeling_data = dairy_data %>% select(43, 915, 931, 963)


model  <- lm(weight ~ Time + Diet, data = ChickWeight)
summary(model)



