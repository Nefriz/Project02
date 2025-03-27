install.packages("showtext")
install.packages("extrafont")
library(extrafont)
library(ggplot2)
library(showtext)



setwd("C:\\Users\\admin\\PycharmProjects\\Project02")

font_add_google("Montserrat", "montserrat")
showtext_auto()


example_data <- data.frame(
  Category = rep(c("A", "B", "C", "D"), each = 5),
  Value = c(10, 15, 20, 25, 30, 5, 10, 15, 20, 25, 8, 12, 18, 22, 28, 3, 7, 11, 16, 21),
  Group = rep(c("X", "Y"), times = 10)
)


palette <- c("#153969", "#2a2e3a", "#718bab", "#f4f4f4", "#ffffff")
fabulous_theme <- theme(
  panel.background = element_rect(fill = palette[5]),
  panel.grid.major.y = element_line(color = palette[3], linewidth = 1),
  panel.grid.minor = element_line(color = alpha(palette[3], 0.3)),
  plot.background = element_rect(fill = alpha(palette[4]), 0.5),
  
  axis.title = element_text(color = palette[1]),
  axis.title.y = element_text(color = palette[1], angle = 0, vjust = 0.5),
  axis.text = element_text(color =  palette[1]),
  
  text = element_text(family = "montserrat"),
  
  legend.background = element_rect(fill = palette[4]),
  legend.text = element_text(color = palette[1], family = "montserrat"),
  legend.title = element_text(color = palette[1], family = "montserrat")
  )



ggplot(data = example_data, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(palette[1], palette[3])) +
  fabulous_theme

