self_study_to_lecture_ratio <- data %>% select(self_study_to_lecture_ratio, University_Name, Uni_type)


iqr_rule <- function(data) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  outliers <- data[data < lower_bound | data > upper_bound]
  
  return(outliers)
  }
  
self_study_to_lecture_ratio <- data %>%
  select(self_study_to_lecture_ratio, University_Name, Uni_type, Difficulty, total_time) %>%
  filter(!self_study_to_lecture_ratio %in% iqr_rule(self_study_to_lecture_ratio))

ggplot() +
    geom_histogram(data = self_study_to_lecture_ratio %>% filter(Uni_type == "Private"), 
                   aes(x = self_study_to_lecture_ratio, fill = "Private"), 
                   alpha = 0.5) +
    geom_histogram(data = self_study_to_lecture_ratio %>% filter(Uni_type != "Private"), 
                   aes(x = self_study_to_lecture_ratio, fill = "Public"), 
                   alpha = 0.5) +
    scale_fill_manual(values = c("Private" = palette[3], "Public" = palette[2])) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  ggtitle("Self-study time to lecture time ratio") +
  xlab("ratio") +
  ylab("") +
  labs(fill = "Uni type") +
    fabulous_theme

ks.test(
    self_study_to_lecture_ratio %>% filter(Uni_type == "Private") %>% pull(self_study_to_lecture_ratio),
    self_study_to_lecture_ratio %>% filter(Uni_type != "Private") %>% pull(self_study_to_lecture_ratio)
  ) #p-value = 0.1897 мають одинаковий розподіл
wilcox.test(
  self_study_to_lecture_ratio %>% filter(Uni_type == "Private") %>% pull(self_study_to_lecture_ratio),
  self_study_to_lecture_ratio %>% filter(Uni_type != "Private") %>% pull(self_study_to_lecture_ratio)
  ) # p-value = 0.1882
cor.test(
  self_study_to_lecture_ratio %>% filter(Uni_type == "Private") %>% pull(total_time),
  self_study_to_lecture_ratio %>% filter(Uni_type == "Private") %>% pull(Difficulty)
)
cor.test(
  self_study_to_lecture_ratio %>% filter(Uni_type != "Private") %>% pull(total_time),
  self_study_to_lecture_ratio %>% filter(Uni_type != "Private") %>% pull(Difficulty)
)
sleep_time <- data %>% select(Uni_type, Sleep_Hours, Difficulty)

ggplot() +
  geom_histogram(data = sleep_time %>% filter(Uni_type == "Private"), 
                 aes(x = Sleep_Hours, fill = "Private"), 
                 alpha = 0.5, binwidth = 1) +
  geom_histogram(data = sleep_time %>% filter(Uni_type != "Private"), 
                 aes(x = Sleep_Hours, fill = "Public"), 
                 alpha = 0.5, binwidth = 1) +
  scale_fill_manual(values = c("Private" = palette[3], "Public" = palette[2])) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  ggtitle("Sleep hours") +
  xlab("Sleep Hours") +
  ylab("") +
  labs(fill = "Uni types") +
  fabulous_theme

ggplot(data = self_study_to_lecture_ratio, aes(x = total_time, y = Difficulty, color = Uni_type)) +
  geom_point(size = 3) +
  geom_encircle(data = self_study_to_lecture_ratio %>% filter(Uni_type == "Private"), 
                aes(x = total_time, y = Difficulty), 
                color = palette[1], fill = palette[1], alpha = 0.5) +  
  geom_encircle(data = self_study_to_lecture_ratio %>% filter(Uni_type == "Public"), 
                aes(x = total_time, y = Difficulty), 
                color = palette[3], fill = palette[3], alpha = 0.5) +
  scale_color_manual(values = c("Private" = alpha(palette[1], 0.7), "Public" = alpha(palette[3], 0.7))) +  
  xlab("Time") +
  theme(axis.title.y = element_text(angle = 0)) +
  coord_flip() +
  ggtitle("Total time to Difficulty correlation")+
  fabulous_theme

ks.test(
  sleep_time %>% filter(Uni_type == "Private") %>% pull(Sleep_Hours),
  sleep_time %>% filter(Uni_type != "Private") %>% pull(Sleep_Hours)
) # p-value = 0.08251 мають одинаковий розподіл
wilcox.test(
  sleep_time %>% filter(Uni_type == "Private") %>% pull(Sleep_Hours),
  sleep_time %>% filter(Uni_type != "Private") %>% pull(Sleep_Hours)
) # p-value = 0.05751

library(ggplot2)
library(ggalt)

ggplot(sleep_time, aes(x = Sleep_Hours, y = Difficulty, color = Uni_type)) +
  geom_point(size = 3) +
  geom_encircle(data = sleep_time %>% filter(Uni_type == "Private"), 
                aes(x = Sleep_Hours, y = Difficulty), 
                color = palette[1], fill = palette[1], alpha = 0.5) +  
  geom_encircle(data = sleep_time %>% filter(Uni_type == "Public"), 
                aes(x = Sleep_Hours, y = Difficulty), 
                color = palette[3], fill = palette[3], alpha = 0.5) +
  scale_color_manual(values = c("Private" = alpha(palette[1], 0.7), "Public" = alpha(palette[3], 0.7))) +  
  xlab("Sleep Hours") +
  fabulous_theme

cor.test(sleep_time$Sleep_Hours, sleep_time$Difficulty)

