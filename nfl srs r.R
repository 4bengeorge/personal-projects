############### Today, we are going to look at SRS Data in the NFL
# SRS is a simple rating system,  is a power rating that measures a team's 
# quality based on its average point differential and the strength of its 
# schedule, where a rating of 0.0 is considered exactly average.

# I like to start a script by clearing the environment
rm(list = ls())

### So, let's load our csv
# library readxl to open excel files
library(readxl)

# Load the data from the CSV file
# we'll call this week 11 data (time of writing this in season)
Week_11_data <- read_excel("NFL SRS DATA.xlsx")

# Now, ideally you want a high SRS score
# Let's make a graph that shows the top 5 teams with high SRS scores

# Load some more necessary libraries. install them first if you haven't: install.packages(c("dplyr", "ggplot2")))
library(dplyr)
library(ggplot2)

# Data Cleaning and Preparation
# The team names (Tm) often contain an asterisk (*) or plus (+) which can be removed for cleaner labels. This is what we see in Pro Football Reference (where the data is from)

Week_11_data_cleaned <- Week_11_data %>%
  mutate(
    Tm = gsub("[*+]", "", Tm), # Remove * and + characters from Team name
    SRS = as.numeric(SRS)      # Ensure SRS is treated as a number
  ) %>%

  # Filter out any potential rows where SRS is NA after conversion
  filter(!is.na(SRS))

  # let's also remove our old environment to avoid clutter
rm(Week_11_data)

# Top 5 Teams by SRS Score
top_5_teams_srs <- Week_11_data_cleaned %>%
  arrange(desc(SRS)) %>% # Sort in descending order by SRS
  head(5)                # Select the top 5 rows

# Before we begin our ggplot & geom_bar, we need to identify our colors for each team:

# Define the colors for the top 5 teams.
# The names MUST exactly match the team names in the 'Tm' column (e.g., "Indianapolis Colts").
team_colors_top5 <- c(
  "Los Angeles Rams" = "gold",
  "Seattle Seahawks" = "plum1",   
  "Indianapolis Colts" = "darkslategray1",  
  "Houston Texans" = "springgreen",      
  "Kansas City Chiefs" = "indianred"   
)

# Generate the Bar Chart using ggplot
print("Generating Top 5 SRS Score Bar Chart...")

top_5_teams_srs %>%
  ggplot(aes(x = reorder(Tm, SRS), y = SRS, fill = Tm)) + 
  geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(SRS, 2)),
            vjust = 1.5, color = "black", size = 6) +
  scale_fill_manual(values = team_colors_top5) +
  labs(
    title = "Top 5 NFL Teams by SRS Score",
    subtitle = "Simple Rating System (SRS) scores for the highest-ranked teams",
    x = "Team",
    y = "SRS Score (Higher is Better)",
    caption = "Data sourced from Pro Football Reference"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.position = "none"
  )

# Optional: display the data used for the plot as well in the console
print("Data for Top 5 Teams:")
print(top_5_teams_srs)

##########################################################
## Now, let's do the bottom 5 teams SRS data

# Bottom 5 Teams by SRS Score (Sorting ascending and taking the top 5)
bottom_5_teams_srs <- Week_11_data_cleaned %>%
  arrange(SRS) %>% # Sort in ASCENDING order by SRS (lowest scores first)
  head(5)          # Select the top 5 (which are the bottom 5 scores)

# Define the colors for the bottom 5 teams.
team_colors_bottom5 <- c(
  "Cincinnati Bengals" = "sienna1", 
  "Las Vegas Raiders" = "grey89",   
  "Cleveland Browns" = "pink",     
  "Tennessee Titans" = "skyblue",  
  "New York Jets" = "limegreen"     
)

# Generate the Bar Chart using ggplot
print("Generating Bottom 5 SRS Score Bar Chart...")

bottom_5_teams_srs %>%
  # Reorder puts the lowest SRS scores first, which is desired for the bottom 5 visualization
  ggplot(aes(x = reorder(Tm, SRS), y = SRS, fill = Tm)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(SRS, 2)),
            vjust = -0.5, color = "black", size = 6) + # Text placed above the bar
  scale_fill_manual(values = team_colors_bottom5) +
  labs(
    title = "Bottom 5 NFL Teams by SRS Score", # ***UPDATED TITLE***
    subtitle = "Simple Rating System (SRS) scores for the lowest-ranked teams", # ***UPDATED SUBTITLE***
    x = "Team",
    y = "SRS Score (Lower is Worse)", # ***UPDATED Y-AXIS LABEL***
    caption = "Data sourced from Pro Football Reference"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 20),
    axis.text.x = element_text(face = "bold", size = 14, color = "black", hjust = 0.5),
    axis.text.y = element_text(size = 15, color = "black"),
    legend.position = "none"
  )

# Optional: display the data used for the plot as well in the console
print("Data for Bottom 5 Teams:")
print(bottom_5_teams_srs)

#### You can download these by clicking "export" in the Plots viewer

