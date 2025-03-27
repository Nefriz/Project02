library(tidyverse)
library(readr)
setwd("C:/Users/38093/Downloads")
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

### Clean data and do some manipulation to make life easier


### Just interesting the mean to calculate by myself
data %>% 
  group_by(Uni_type) %>% 
  summarize(mean_study_time = mean(Study_Hours_Self, na.rm=TRUE) + mean(Study_Hours_Lectures, na.rm=TRUE))

### Set hypothesis for t.test and other metrics

# H0 - there is no difference between different types of universities
# H1 - there is difference between different types of universities
# significant level = 0.05
# I will use t.test, because we do not have much information at all
# and it's also great for calculating the mean


uni_study_time <- data %>% 
  group_by(Uni_type) %>% 
  summarize(total_study_time = sum(Study_Hours_Self+ Study_Hours_Lectures))

t.test(Study_Hours_Self + Study_Hours_Lectures ~ Uni_type, data = data)
unique(data$Difficulty)

#The whole circle above and the graph show us the average number of hours 
#that students at private and public universities devote to studying. 
#Initially, we had to do some manipulations with the sums and so on, 
#but in general we have the following result: our p-value is less than 0.05, 
#so we reject the 0 hypothesis, which tells us that there is no strong 
#difference between the two groups and accept the alternative one. 
#More naturally, I can say that students at private universities study
#more than at public universities and this difference is significant

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

#When I was looking for a medium, I realized that the box raft would fit well 
#here and would be able to show the results of our research in a high-quality 
#way, so I chose it. In conclusion, it shows us the same thing: 
#there is a difference and it is quite noticeable, but I find it funny 
#that an outlier at a state university, I suspect that 
#it was a medical student who estimated the number of hours to study so much


#################################################################
chi <- chisq.test(table(data$Difficulty, data$Uni_type), simulate.p.value = TRUE)

#Here is a simple chi-square to see if there is a difference between 
#difficulty and university type. It turned out that there is not. 
#Our p-value is not less than 0.05, so I had no reason to reject the null
#hypothesis. However, I would like to note that the p-value is not very large, 
#so I will not say that there is no pattern at all, but the graph shows us 
#an interesting thing

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

#In our visualization course, we studied the hit map and it was very good 
#at describing the relationship between complexity and the type of university.
#As we can see from the graph, it is difficult for everyone to study, 
#so we don't have a big dependence, but I would like to note that private
#universities are more difficult


####################################################################
chi <- chisq.test(table(data$Difficulty, data$Total_Study_Time), simulate.p.value = TRUE)
chi$observed
chi$expected
#And since we've already looked at whether the difficulty depends on the type 
#of university and found that it doesn't really depend much, I decided that
#it would be cool to see if there is a relationship between difficulty and
#the number of hours devoted to studying. Spoiler alert: the results 
#are very strange. 
#In general, I used the same chi-square because it fits small samples well. 
#The p-value is again greater than 0.05, so I have no reason to reject 
#the null hypothesis. So this means that there is actually no difference 
#between the number of hours and complexity. The graph will show this 
#very well. 
#And I'll tell you why. I suspect that it's because complexity is a very 
#subjective thing and everyone feels it differently, so the sample can be excluded

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

#To make it easier to visualize, I divided the groups and visualized 
#according to them. 


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

#A few manipulations and we have a linear graph that shows the relationship
#between the number of hours and complexity. And as we can see,
#it is not very revealing, because it seems that if a person spends 
#more time studying, it should not be so difficult, but no, 
#we have a person who spends 60 hours studying and still finds it difficult,
#and vice versa. There are people who spend very little time studying and 
#find it easy.
#As I said earlier, this is because the difficulty is very subjective and 
#it depends not even on the type of university, but on the person and 
#the specialty they are studying



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

#This is an additional chart to the previous x-square. It also shows that
#there is no correlation between the number of hours and the difficulty, 
#it just does it through the bar chart 



#What can be the general conclusion from this part? Students of private
#universities study more than students of state universities, 
#this does not make their life and studies easier, but it will definitely 
#tell us about the amount of knowledge they will have after graduation. 
#I think I can say that according to this, we cannot say that private 
#universities are worse, but it looks like the opposite, although this 
#is also very subjective)








