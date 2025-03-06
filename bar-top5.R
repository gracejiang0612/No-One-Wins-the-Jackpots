# Install required packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(svglite)) install.packages("svglite")

# Load the packages
library(ggplot2)
library(svglite)

# Create data frame with state frequencies (already sorted)
state_data <- data.frame(
  state = c("NY", "CA", "NJ", "OH", "MI", "TX", "GA", "IL", "MD", "FL"),
  frequency = c(39, 37, 24, 18, 15, 14, 13, 12, 8, 6),
  state_full = c("New York", "California", "New Jersey", "Ohio", "Michigan", 
                 "Texas", "Georgia", "Illinois", "Maryland", "Florida")
)

# Filter to only the top 5 states
top5_states <- state_data[1:5, ]

# Reorder factors for proper sorting in the plot
top5_states$state_full <- factor(top5_states$state_full, 
                                 levels = rev(top5_states$state_full))

# Create a red color palette with 5 shades
red_colors <- c("#FF6666", "#FF3333", "#CC0000", "#990000", "#660000")

# Create the horizontal bar chart
top5_chart <- ggplot(top5_states, aes(x = state_full, y = frequency, fill = frequency)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradientn(colors = red_colors) +
  coord_flip() +  # Makes the bar chart horizontal
  labs(
    title = "Top 5 States with Most Lottery Jackpot Wins",
    x = NULL,  # Remove x axis label
    y = "Number of Wins",
    caption = "By: Xinlin Jiang | Data source: https://www.megamillions.com"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # Remove the legend
  ) +
  # Add the count labels at the end of each bar
  geom_text(aes(label = frequency), hjust = -0.2, color = "black", size = 4)

# Show the plot
print(top5_chart)

# Save the chart as SVG for Illustrator
ggsave("top5_lottery_states.svg", plot = top5_chart, width = 8, height = 5, dpi = 300)

# Also save as PNG for quick reference
ggsave("top5_lottery_states.png", plot = top5_chart, width = 8, height = 5, dpi = 300)

# Confirmation message
print("Bar chart files have been saved successfully. You can now import the SVG into Illustrator.")