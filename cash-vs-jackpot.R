# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(patchwork)

# Read the data
lottery_data <- read.csv("cleaned_lottery_data.csv", stringsAsFactors = FALSE)

# Convert Date column to proper date format
lottery_data$Date <- as.Date(lottery_data$Date)

# Create a long format dataset for easier plotting
lottery_long <- lottery_data %>%
  select(Date, Jackpot, Cash.Value) %>%
  tidyr::pivot_longer(cols = c(Jackpot, Cash.Value),
                      names_to = "Type",
                      values_to = "Value")

# Create color mapping
color_mapping <- c("Jackpot" = "blue", "Cash.Value" = "red")

# Create the combined visualization
p1 <- ggplot(lottery_long, aes(x = Date, y = Value, size = Value, color = Type)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 15)) +
  scale_color_manual(values = color_mapping) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Lottery Jackpot and Cash Value Over Time",
    x = "Date",
    y = "Amount ($)",
    color = "Type",
    size = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create side-by-side faceted plot
p2 <- ggplot(lottery_long, aes(x = Date, y = Value, size = Value, color = Type)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 15)) +
  scale_color_manual(values = color_mapping) +
  scale_y_continuous(labels = scales::dollar_format()) +
  facet_wrap(~Type, ncol = 1) +
  labs(
    title = "Lottery Values (Faceted View)",
    x = "Date",
    y = "Amount ($)",
    size = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

# Time series line plot with points
p3 <- ggplot(lottery_long, aes(x = Date, y = Value, color = Type, group = Type)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(size = Value), alpha = 0.7) +
  scale_size_continuous(range = c(1, 8)) +
  scale_color_manual(values = color_mapping) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Lottery Values Over Time (Line Plot)",
    x = "Date",
    y = "Amount ($)",
    color = "Type",
    size = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Calculate the correlation
correlation <- cor(lottery_data$Jackpot, lottery_data$Cash.Value)

# Create scatter plot
p4 <- ggplot(lottery_data, aes(x = Jackpot, y = Cash.Value)) +
  geom_point(aes(size = Jackpot), color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkgray") +
  scale_size_continuous(range = c(2, 10)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = paste0("Jackpot vs Cash Value (Correlation: ", round(correlation, 3), ")"),
    x = "Jackpot ($)",
    y = "Cash Value ($)",
    size = "Jackpot"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save individual plots
ggsave("lottery_bubble_chart.png", p1, width = 10, height = 6, dpi = 300)
ggsave("lottery_faceted_chart.png", p2, width = 10, height = 8, dpi = 300)
ggsave("lottery_line_chart.png", p3, width = 10, height = 6, dpi = 300)
ggsave("lottery_correlation.png", p4, width = 8, height = 6, dpi = 300)

# Create a combined dashboard
layout <- "
AABB
CCDD
"

combined_plot <- p1 + p2 + p3 + p4 + plot_layout(design = layout)
ggsave("lottery_dashboard.png", combined_plot, width = 16, height = 12, dpi = 300)

# Print summary statistics
summary_stats <- lottery_data %>%
  summarise(
    Min_Jackpot = min(Jackpot, na.rm = TRUE),
    Max_Jackpot = max(Jackpot, na.rm = TRUE),
    Mean_Jackpot = mean(Jackpot, na.rm = TRUE),
    Min_Cash = min(Cash.Value, na.rm = TRUE),
    Max_Cash = max(Cash.Value, na.rm = TRUE),
    Mean_Cash = mean(Cash.Value, na.rm = TRUE),
    Count = n()
  )

print(summary_stats)