# Install required packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(maps)) install.packages("maps")
# Install svglite for SVG export
if (!require(svglite)) install.packages("svglite")
# Load the packages (this is critical!)
library(ggplot2)
library(maps)

# Create data frame with state frequencies
state_data <- data.frame(
  state = c("NY", "CA", "NJ", "OH", "MI", "TX", "GA", "IL", "MD", "FL", "MA", "VA", "WA", 
            "AZ", "PA", "IN", "TN", "ME", "MN", "WI", "NH", "SC", "AR", "MO", "KS", "NC", "AL"),
  frequency = c(39, 37, 24, 18, 15, 14, 13, 12, 8, 6, 4, 4, 4, 3, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Get US states map data
us_states <- map_data("state")

# Create a mapping of state abbreviations to full names
state_abbr_to_name <- c(
  AL = "alabama", AK = "alaska", AZ = "arizona", AR = "arkansas", CA = "california",
  CO = "colorado", CT = "connecticut", DE = "delaware", FL = "florida", GA = "georgia",
  HI = "hawaii", ID = "idaho", IL = "illinois", IN = "indiana", IA = "iowa",
  KS = "kansas", KY = "kentucky", LA = "louisiana", ME = "maine", MD = "maryland",
  MA = "massachusetts", MI = "michigan", MN = "minnesota", MS = "mississippi", MO = "missouri",
  MT = "montana", NE = "nebraska", NV = "nevada", NH = "new hampshire", NJ = "new jersey",
  NM = "new mexico", NY = "new york", NC = "north carolina", ND = "north dakota", OH = "ohio",
  OK = "oklahoma", OR = "oregon", PA = "pennsylvania", RI = "rhode island", SC = "south carolina",
  SD = "south dakota", TN = "tennessee", TX = "texas", UT = "utah", VT = "vermont",
  VA = "virginia", WA = "washington", WV = "west virginia", WI = "wisconsin", WY = "wyoming",
  DC = "district of columbia"
)

# Convert abbreviations to full state names
state_data$state_name <- unlist(state_abbr_to_name[state_data$state])

# Merge map data with frequency data
# We need dplyr for left_join, so let's check if it's available, otherwise use merge
if (require(dplyr)) {
  choropleth_data <- left_join(us_states, state_data, by = c("region" = "state_name"))
} else {
  # If dplyr is not available, use base R merge
  choropleth_data <- merge(us_states, state_data, 
                           by.x = "region", by.y = "state_name", 
                           all.x = TRUE)
}

# Create a custom red color palette
red_colors <- c("#FFEEEE", "#FFCCCC", "#FF9999", "#FF6666", "#FF3333", "#CC0000", "#990000", "#660000")

# Create the map
ggplot(data = choropleth_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = frequency), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(
    name = "Winning Times", 
    colors = red_colors,
    na.value = "grey90"
  ) +
  labs(
    title = "Lottery Jackpot Wins by State",
    subtitle = "Red color intensity represents frequency of wins from fewer (light) to more (dark)",
    caption = "By: Xinlin Jiang | Data source: https://www.megamillions.com"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Save the plot
ggsave("lottery_wins_map.svg", width = 10, height = 6, dpi = 30)
# Also save as PNG for quick reference
ggsave("lottery_wins_map.png", width = 8, height = 5, dpi = 300)