data<- read.csv('C:/Users/Acer/Downloads/Data.csv')
data
# rename columns
data <- data %>% rename_with(~c("Timestamp",
                                "University", "Gender", "Age", "University_Name", "Specialty",
                                "Employment", "Work_Hours", "Study_Hours_Lectures", "Study_Hours_Self",
                                "Difficulty", "Email", "Study_Level", "Sleep_Hours"
), everything()) %>% mutate(Uni_type = ifelse(University_Name %in% c("KSE", "УКУ", 'КУК'), "Private", "Public"))

# alter the data (delete data about non-students, change data types, update values which are not numeric, delete personal information of people
data[data$Study_Hours_Self == "25+"|data$Study_Hours_Lectures == "25+", ] <- "30"
data$Study_Hours_Lectures <- as.numeric(data$Study_Hours_Lectures)
data$Study_Hours_Self <- as.numeric(data$Study_Hours_Self)
data <- data[data$University == 'Так', ]
data <- data |> select(-(Email))

data <- data |> mutate(Study_Hours_Total = Study_Hours_Lectures + Study_Hours_Self)

# обробка годин роботи(
data$Work_Hours |> unique()
data$Work_Hours[data$Work_Hours == "приблизно 15 годин на тиждень, проте може варіюватися в меншу сторону "] <- "15"
data$Work_Hours[data$Work_Hours == "Працюю дистанційно кожного дня по 5-7 годин "| data$Work_Hours== '30+'] <- "30"
data$Work_Hours[data$Work_Hours == "42 години "] <- "42"
data$Work_Hours[data$Work_Hours == "40 + 10 на дорогу"] <- "40"
data$Work_Hours[data$Work_Hours == "3-4 години"] <- "4"
data$Work_Hours[data$Work_Hours == "8-10 годин"] <- "9"
data$Work_Hours[data$Work_Hours == "- "|data$Work_Hours == ""] <- "0"
data$Work_Hours |> unique()


data$Work_Hours <- ifelse(grepl("^[0-9]+-[0-9]+$", data$Work_Hours), 
                          sapply(strsplit(data$Work_Hours, "-"), function(x) round(mean(as.numeric(x)))), 
                          data$Work_Hours)

data$Work_Hours <- as.numeric(data$Work_Hours)
data$Work_Hours

data$Sleep_Hours[is.na(data$Sleep_Hours)] <- median(data$Sleep_Hours, na.rm = TRUE)
data$Sleep_Hours

