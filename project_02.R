library(tidyverse)
library(readr)
data <- read_csv("university.csv")
data <- data %>% rename_with(~c("Timestamp",
                                "University", "Gender", "Age", "University_Name", "Specialty",
                                "Employment", "Work_Hours", "Study_Hours_Lectures", "Study_Hours_Self",
                                "Difficulty", "Email", "Study_Level", "Sleep_Hours"
), everything()) %>% mutate(Uni_type = ifelse(University_Name %in% c("KSE", "УКУ"), "Private", "Public"))

data <- data[data$University == 'Так', ]
data <- data |> select(-(Email))

# обробка годин роботи(
data$Work_Hours |> unique()
data$Work_Hours[data$Work_Hours == "приблизно 15 годин на тиждень, проте може варіюватися в меншу сторону"] <- "15"
data$Work_Hours[data$Work_Hours == "Працюю дистанційно кожного дня по 5-7 годин"| data$Work_Hours== '30+'] <- "30"
data$Work_Hours[data$Work_Hours == "42 години"] <- "42"
data$Work_Hours[data$Work_Hours == "40 + 10 на дорогу"] <- "40"
data$Work_Hours[data$Work_Hours == "3-4 години"] <- "4"
data$Work_Hours[data$Work_Hours == "8-10 годин"] <- "9"
data$Work_Hours[data$Work_Hours == "9-10"] <- "10"
data$Work_Hours[data$Work_Hours == "15-18"] <- "18"
data$Work_Hours[data$Work_Hours == "6-7"] <- "7"
data$Work_Hours[data$Work_Hours == "20-30"] <- "30"
data$Work_Hours[data$Work_Hours == "-"|data$Work_Hours == ""] <- "0"
data$Work_Hours |> unique()

data$Study_Hours_Lectures[data$Study_Hours_Lectures == "25+"] <- "30"
data$Study_Hours_Lectures <- as.numeric(data$Study_Hours_Lectures)
data$Study_Hours_Lectures

data$Study_Hours_Self[data$Study_Hours_Self == "25+"] <- "30"
data$Study_Hours_Self <- as.numeric(data$Study_Hours_Self)
data$Study_Hours_Self

data$Work_Hours <- ifelse(grepl("^[0-9]+-[0-9]+$", data$Work_Hours), 
                          sapply(strsplit(data$Work_Hours, "-"), function(x) round(mean(as.numeric(x)))), 
                          data$Work_Hours)

data$Work_Hours <- as.numeric(data$Work_Hours)
data$Work_Hours

colnames(data)



data %>% 
  group_by(Uni_type) %>% 
  summarize(mean_study_time = mean(Study_Hours_Self, na.rm=TRUE) + mean(Study_Hours_Lectures, na.rm=TRUE))

# H0 - there is no difference between two groups
# H1 - there is difference between two groups
# significant level = 0.05
# I will use t.test, because we do not have much information at all

uni_study_time <- data %>% 
  group_by(Uni_type) %>% 
  summarize(total_study_time = sum(Study_Hours_Self+ Study_Hours_Lectures))

t.test(Study_Hours_Self + Study_Hours_Lectures ~ Uni_type, data = data)
unique(data$Difficulty)

data %>% 
  ggplot(aes(x=Uni_type, y=Study_Hours_Self+Study_Hours_Lectures, fill=Uni_type))+
  geom_boxplot()+
  labs(
    title = "Порівняння часу на навчання між приватними\nта державними університетами",
    x="Тип університету",
    y="Час на навчання (години)"
  )+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white",color="white"),
    axis.line = element_blank(),
    legend.title=element_blank(),
    axis.title.y = element_text(margin = margin(r = 10), color ="#003964"),
    axis.title.x = element_text(margin = margin(t = 15), color ="#003964"),
    text = element_text(color = "#003964", size = 20)
  )+
  scale_fill_manual(values=c("Private" = "#00bbce", "Public" = "#E4E541"))



##### 
chi <- chisq.test(table(data$Difficulty, data$Uni_type), simulate.p.value = TRUE)


chi_table <- table(data$Difficulty, data$Uni_type)
chi_df <- as.data.frame(chi_table)

chi_df %>% 
  ggplot(aes(x=Var2, y=Var1, fill=Freq))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "#003964")+ 
  labs(title="Study difficulty depending on university type",
       x="University type", y="Difficulty", fill="Frequency") +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white",color="white"),
    axis.line = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10), color ="#003964"),
    axis.title.x = element_text(margin = margin(t = 15), color ="#003964"),
    text = element_text(color = "#003964", size = 20)
  )




##### 
chi <- chisq.test(table(data$Difficulty, data$Total_Study_Time), simulate.p.value = TRUE)
chi$observed
chi$expected


data$Study_Time <- factor(case_when(
  data$Study_Hours_Lectures+data$Study_Hours_Self<=15 ~ "Low",   
  data$Study_Hours_Lectures+data$Study_Hours_Self>15 & 
    data$Study_Hours_Lectures+data$Study_Hours_Self< 25 ~ "Medium",
  data$Study_Hours_Lectures+data$Study_Hours_Self>25 ~ "High"
), levels=c("Low", "Medium", "High"))

data$Study_Difficulty_Distribution <- factor(case_when(
  data$Difficulty<=3 ~ "Low",   
  data$Difficulty>3 & 
    data$Difficulty<=7 ~ "Medium",
  data$Difficulty>7 ~ "High"
), levels=c("Low", "Medium", "High"))




table_chi <- table(data$Difficulty, data$Study_Time)
df_plot <- as.data.frame(table_chi)


data$Total_Study_Time <- data$Study_Hours_Self + data$Study_Hours_Lectures

mean_diff <- data %>%
  group_by(Total_Study_Time)%>%
  summarise(mean_difficulty=mean(Difficulty, na.rm=TRUE))

ggplot(mean_diff, aes(x =Total_Study_Time, y = mean_difficulty)) +
  geom_line(color = "#00bbce", size = 1) +  
  geom_point(color = "#A7C539", size = 3) +  
  labs(title = "Difficulty of studying depending on the number of hours\ndevoted to studying",
       x="Study time", y="Difficulty")+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "white",color="white"),
    axis.line = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10), color ="#003964"),
    axis.title.x = element_text(margin = margin(t = 15), color ="#003964"),
    text = element_text(color = "#003964", size = 20))


united_table <- table(data$Study_Difficulty_Distribution, data$Study_Time)
df <- as.data.frame(united_table)

df %>% 
  ggplot(aes(x=Var2, y=Freq, fill=Var1))+
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title="Study difficulty vs study time",
       x="Study time", y="Frequency", fill="Study difficulty")+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "white",color="white"),
    axis.line = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10), color ="#003964"),
    axis.title.x = element_text(margin = margin(t = 15), color ="#003964"),
    text = element_text(color = "#003964", size = 20))+
  scale_x_discrete(labels=c("Low", "Medium", "High"))+
  scale_fill_manual(values=c("Medium"="#00bbce","Low"="#E4E541","High"="#003964"))













