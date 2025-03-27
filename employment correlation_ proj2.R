library(boot)
set.seed(1313)

# let`s check if there is a significant difference between percentage of employed people and uni_type

data
data1 <- data |> 
  select(Employment, Uni_type, University) |> 
  group_by(Uni_type) |> 
  summarize(Employment_perc = sum(Employment == 'Так')/n()*100) |> 
  pivot_wider(names_from = Uni_type, values_from = Employment_perc)

# here we see the difference, but we need to run a test to check whether there is a significant difference between 
# employment in public and private unis

empl_table <- table(data$Uni_type, data$Employment)
chisq.test(empl_table)

#seems like the difference is not that significant



# let's check the correlation between student's work and study hours

data_working <- data[!(data$Work_Hours == 0), ]
data_working

cor.test(data_working$Study_Hours_Total, data_working$Work_Hours)
# маємо негативну кореляцію, і хоча у нас і достатня вибірка, я пропоную спробувати застосувати бутстрап, оскільки ми сильно урізали початкову вибірку

boot_corr_func <- function(data_working, indices) {
  sample1 <- data_working[indices, ]  
  return(cor(sample1$Study_Hours_Total, sample1$Work_Hours))
}

boot_corr <- boot(data_working, boot_corr_func, R = 10000)

boot.ci(boot_corr, type = "perc")

# негативна кореляція підтверджена знову (чим більше робочих годин - тим менше виділяють на навчання)

# scatter plot for this correlation

data_working |> 
  ggplot(aes(x = Work_Hours, y = Study_Hours_Total, color = Uni_type)) +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = alpha("#153969", 0.7), linewidth = 1) + 
  fabulous_theme +
  labs(
    title = "Student's work and study hours correlation",
    x = "Study hours",
    y = "Work hours",
    color = "Type of uni"
  ) +
  theme(
    axis.title.y = element_text(angle = 90, hjust = 0.5),
    panel.grid.major.x = element_line(color = alpha(palette[3], 0.7), linewidth = 1),
    panel.grid.major.y = element_line(color = alpha(palette[3], 0.7), linewidth = 1)
    )






