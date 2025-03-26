  library(tidyverse)
  library(readr)
  data <- read_csv("Data/Data.csv", locale = locale(encoding = "UTF-8"))
  data <- data %>% rename_with(~c("Timestamp",
    "University", "Gender", "Age", "University_Name", "Specialty",
    "Employment", "Work_Hours", "Study_Hours_Lectures", "Study_Hours_Self",
    "Difficulty", "Email", "Study_Level", "Sleep_Hours"
  ), everything()) %>% mutate(Uni_type = ifelse(University_Name %in% c("KSE", "УКУ"), "Private", "Public")) %>%
    select(c(2:15))


  colnames(data)
  data %>% select(c(University_Name, Uni_type))

