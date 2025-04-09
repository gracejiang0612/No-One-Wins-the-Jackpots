
# Read the data
lottery_data <- read.csv("/Users/Grace/Desktop/2025-studio/project3/cleaned_lottery_data.csv", stringsAsFactors = FALSE)

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(tidyr)


# Convert Date column to proper date format
lottery_data$Date <- as.Date(lottery_data$Date, format = "%m/%d/%y")

# Filter to only include rows where both Jackpot and Cash Value are available
lottery_filtered <- lottery_data %>%
  filter(!is.na(Jackpot) & !is.na(Cash.Value) & 
           Jackpot != "" & Cash.Value != "")

# Create a version of the data for the mirrored chart
# For Jackpot, we'll convert values to negative to show below the x-axis
lottery_mirror <- lottery_filtered %>%
  select(Date, Jackpot, Cash.Value) %>%
  mutate(Jackpot = -Jackpot) %>%  # Make jackpot negative for plotting below axis
  pivot_longer(cols = c(Jackpot, Cash.Value),
               names_to = "Type",
               values_to = "Value")

# Create the split axis chart
ggplot(lottery_mirror, aes(x = Date, y = Value, fill = Type, color = Type)) +
  # Add the area fills
  geom_area(alpha = 0.4) +
  # Add lines on top of the areas
  geom_line(linewidth = 1) +
  # Set the colors
  scale_fill_manual(values = c("Jackpot" = "blue", "Cash.Value" = "red")) +
  scale_color_manual(values = c("Jackpot" = "blue", "Cash.Value" = "red")) +
  # Custom y-axis labels that show absolute values
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x)),
    breaks = function(x) {
      # Create breaks on both positive and negative sides
      max_val <- max(abs(x))
      c(seq(-max_val, 0, length.out = 5)[-1], 
        seq(0, max_val, length.out = 5)[-1])
    }
  ) +
  # Add labels and title
  labs(
    title = "Surplus/Deficit Filled Line Chart for Lottery Data",
    subtitle = "Cash Value (top, red) vs Jackpot (bottom, blue)",
    x = "Date",
    y = "Amount ($)",
    fill = "Type",
    color = "Type"
  ) +
  # Add annotation explaining the chart
  annotate("text", x = min(lottery_filtered$Date) + 100, 
           y = max(lottery_filtered$Cash.Value) * 0.7, 
           label = "The shaded area of\nthese charts allows a\nbalance to be shown -\neither against a\nbaseline or between\ntwo series.", 
           hjust = 0, size = 3.5) +
  # Add a horizontal line at y=0 (the x-axis/baseline)
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.7) +
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position = "right"
  )

# Save the plot
ggsave("lottery_split_axis.png", width = 12, height = 8, dpi = 300)

# For a cleaner version with filled areas only
ggplot(lottery_mirror, aes(x = Date, y = Value, fill = Type)) +
  # Add the area fills
  geom_area(alpha = 0.6) +
  # Set the colors
  scale_fill_manual(values = c("Jackpot" = "blue", "Cash.Value" = "red")) +
  # Custom y-axis labels that show absolute values
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x)),
    breaks = function(x) {
      # Create breaks on both positive and negative sides
      max_val <- max(abs(x))
      c(seq(-max_val, 0, length.out = 5)[-1], 
        seq(0, max_val, length.out = 5)[-1])
    }
  ) +
  # Add labels and title
  labs(
    title = "Lottery Values: Mirror Chart",
    subtitle = "Cash Value (top, red) vs Jackpot (bottom, blue)",
    x = "Date",
    y = "Amount ($)",
    fill = "Type"
  ) +
  # Add a horizontal line at y=0 (the x-axis/baseline)
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.7) +
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position = "right"
  )

# Save the second plot
ggsave("/Users/Grace/Desktop/2025-studio/project3/lottery_mirror_chart.svg", width = 12, height = 8, dpi = 300)