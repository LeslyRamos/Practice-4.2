# Load the necessary libraries
library(tidyverse)

# Load the data
url <- "https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt"
rairuoho <- read.table(url, header = TRUE, sep = "\t")

# Reshape the data to long format
rairuoho <- rairuoho %>%
  pivot_longer(cols = day3:day8, names_to = "day", values_to = "length")

# Summarize the data to get the mean length per day and treatment
rairuoho_summary <- rairuoho %>%
  group_by(day, treatment) %>%
  summarize(mean_length = mean(length, na.rm = TRUE))

# Define the plot
plot <- ggplot(rairuoho_summary, aes(x = day, y = mean_length, color = treatment, group = treatment, shape = treatment)) +
  geom_line(size = 0.5) + 
  geom_point(size = 2.1) +
  labs(title = "Grass Growth Over Time by Treatment", x = "Day", y = "Average Length") +
  scale_color_manual(values = c("#018571", "#74a9cf")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# Save the plot as a PDF
ggsave("Grass_Growth_Plot.pdf", plot = plot, width = 8, height = 6)







