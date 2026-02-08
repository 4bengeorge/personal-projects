############### Today, we are going to look at SOS Data in the NFL over the past 15 seasons
# SOS in the NFL stands for Strength of Schedule which is a metric representing the combined winning percentage of a team's opponents. It is used to measure the difficulty of a team's schedule, with a higher percentage indicating a tougher schedule. It also serves as a tie-breaker for NFL draft positioning and playoff seeding. 

# I like to start a script by clearing the environment
rm(list = ls())

### So, let's load our xl and libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# set working directory
setwd("C:/Users/4beng.DESKTOP-0B0APHG/OneDrive/Desktop/Personal Projects/NFL SRS DATA")

# Load the data from the xlsx file
SOS_DATA <- read_excel("C:/Users/4beng.DESKTOP-0B0APHG/OneDrive/Desktop/Personal Projects/SOS NFL SB.xlsx")

## Filter for Super Bowl teams (I added SB Winner/Loser/? manually on xl)
SB_TEAMS <- SOS_DATA %>% #pipe %%
  filter(grepl("SB WINNER|SB LOSER|SB ?", SOS_DATA[[2]], ignore.case = FALSE))

# So we have all of our data and SB teams.. for now we're gonna make their names look pretty for graphing and eliminate words such as "SB WINNER" since we have saved in another view

SB_TEAMS_CLEAN <- SB_TEAMS %>%
  mutate(
    Team_Clean = gsub("\\*|\\+|SB WINNER|SB LOSER|SB \\?", "", SB_TEAMS[[2]]) %>% trimws(),
    Year = SB_TEAMS[[1]],
    WL = case_when(
      grepl("SB WINNER", SB_TEAMS[[2]]) ~ "(W)",
      grepl("SB LOSER", SB_TEAMS[[2]]) ~ "(L)",
      grepl("SB \\?", SB_TEAMS[[2]]) ~ "(?)",
      TRUE ~ ""
    ),
    Team_Year = paste(Team_Clean, Year, WL)
  ) %>%
  arrange(SoS) %>%
  head(5)

# Now we have our select teams, let's set them up with their team colors in preparation for graphing
team_colors <- c(
  "Denver Broncos" = "#FB4F14",
  "Baltimore Ravens" = "#241773",
  "New England Patriots" = "#0C2340",
  "Pittsburgh Steelers" = "#FFB612",
  "Seattle Seahawks" = "#002244",
  "San Francisco 49ers" = "#AA0000",
  "New York Giants" = "#0B2265",
  "Green Bay Packers" = "darkgreen",
  "Cincinnati Bengals" = "#FB4F14",
  "Los Angeles Rams" = "#003594",
  "Kansas City Chiefs" = "#E31837",
  "Tampa Bay Buccaneers" = "#D50A0A",
  "Philadelphia Eagles" = "#004C54",
  "Atlanta Falcons" = "#A71930",
  "Carolina Panthers" = "#0085CA"
)

# Match these colors to teams
SB_TEAMS_CLEAN$Color <- team_colors[SB_TEAMS_CLEAN$Team_Clean]

# Now let's plot our graph looking at top 5 easiest strength of schedule
ggplot(SB_TEAMS_CLEAN, aes(x = reorder(Team_Year, SoS), y = -SoS, fill = Color)) + # gg plot to plot graph
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(labels = function(x) -x, limits = c(0, 5)) + # fixing axis here
  labs(
       x = "Teams Year (W/L Super Bowl)",
       y = "Strength of Schedule (SoS)") +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, color = "black", size = 20, family = "Arial"), 
        axis.text.y = element_text(size = 13, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 20, family = "Arial", color = "black", margin = margin(t = 20)), 
        axis.title.y = element_text(size = 20, family = "Arial", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 18, color = "black", family = "Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## save image for instagram post 
ggsave("top4easystrengthschedule.png", width = 8, height = 8, dpi = 150)

# Now, let's do the opposite and find the hardest
SB_TEAMS_HARD <- SB_TEAMS %>%
  mutate(
    Team_Clean = gsub("\\*|\\+|SB WINNER|SB LOSER|SB \\?", "", SB_TEAMS[[2]]) %>% trimws(),
    Year = as.numeric(SB_TEAMS[[1]]),
    SoS = as.numeric(SoS), 
    WL = case_when(
      grepl("SB WINNER", SB_TEAMS[[2]]) ~ "(W)",
      grepl("SB LOSER", SB_TEAMS[[2]]) ~ "(L)",
      grepl("SB \\?", SB_TEAMS[[2]]) ~ "(?)",
      TRUE ~ ""
    ),
    Team_Year = paste(Team_Clean, Year, WL)
  ) %>%
  arrange(desc(SoS)) %>% 
  head(5)

# Let's color map it again to ensure for 2nd graph is colored
SB_TEAMS_HARD$Color <- team_colors[SB_TEAMS_HARD$Team_Clean]
SB_TEAMS_HARD$Color[is.na(SB_TEAMS_HARD$Color)] <- "black" 

# Plot the graph here
ggplot(SB_TEAMS_HARD, aes(x = reorder(Team_Year, -SoS), y = SoS, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  labs(
       x = "Teams Year (W/L Super Bowl)",
       y = "Strength of Schedule (SoS)") +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, color = "black", size = 18),
        axis.text.y = element_text(size = 13, color ="black"),
        axis.title.x = element_text(size = 20, color = "black", margin = margin(t = 20)),
        axis.title.y = element_text(size = 20, color = "black"),
        plot.title = element_text(hjust = 0.5, size = 18, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## save image for instagram post 
ggsave("top4worststrengthschedule.png", width = 8, height = 8, dpi = 150)

### Further, 
# Let's simplify our data by the two teams and create a bell curve regarding where they stand amongst previous SB finalists regarding SoS
SB_TEAMS_ALL <- SB_TEAMS %>%
  mutate(
    Team_Clean = gsub("\\*|\\+|SB WINNER|SB LOSER|SB \\?|SB\\?", "", .[[2]]) %>% trimws(),
    Year = as.numeric(.[[1]]),
    SoS = as.numeric(SoS)
  )

# Plot Bellcurve
ggplot(SB_TEAMS_ALL, aes(x = SoS)) +
  geom_density(fill = "violetred1", color = "violetred1", alpha = 0.6) +
  geom_vline(xintercept = -4.5, color = "#0C2340", size = 1, linetype = "solid") +
  annotate("label", x = -4.5, y = 0.15, label = "2025 PATRIOTS\n(-4.5 SoS)", 
           fill = "#0C2340", color = "white", fontface = "bold", family = "Arial", size = 5.5) +
  geom_vline(xintercept = 1.6, color = "#002244", size = 1, linetype = "dashed") +
  annotate("label", x = 1.6, y = 0.10, label = "2025 SEAHAWKS\n(1.6 SoS)", 
           fill = "#69BE28", color = "white", fontface = "bold", family = "Arial", size = 5.5) +
  coord_cartesian(xlim = c(-5, 8)) + 
  scale_x_continuous(breaks = seq(-4, 8, by = 2)) +
  
  
  # Aesthetics, this way of plotting aesthetics code is simpler than above slightly
  labs(
       x = "Strength of Schedule (Harder Path —>)",
       y = "Frequency of Teams") +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 25), 
    axis.title.y = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

## save image for instagram post 
ggsave("bellcurve.png", width = 12, height = 10, dpi = 150)


### now let's make a box plot comparing Super Bowl Winners vs. Losers
SB_BOX_DATA <- SB_TEAMS_ALL %>%
  mutate(Outcome = case_when(
    grepl("SB WINNER", .[[2]]) ~ "SB Winner",
    grepl("SB LOSER", .[[2]])  ~ "SB Loser",
    TRUE ~ "TBD"
  )) %>%
  filter(Outcome != "TBD") 

# gg plot our box plot here
ggplot(SB_BOX_DATA, aes(x = Outcome, y = SoS, fill = Outcome)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 3, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  
  # --- Patriots Reference ---
  geom_hline(yintercept = -4.5, color = "darkgoldenrod1", linetype = "dashed", linewidth = 1.3) +
  annotate("text", x = 1.5, y = -5.8, label = "2025 Patriots (-4.5)", 
           color = "darkgoldenrod1", fontface = "bold", size = 8, family = "Arial") +
  
  # --- Seahawks Reference ---
  geom_hline(yintercept = 1.6, color = "#69BE28", linetype = "dashed", linewidth = 1.3) +
  annotate("text", x = 1.5, y = 12.25, label = "2025 Seahawks (1.6)", 
           color = "#69BE28", fontface = "bold", size = 8, family = "Arial") +
  
  # Aesthetics
  scale_fill_manual(values = c("SB Winner" = "#228B22", "SB Loser" = "#DC143C")) +
  labs(title = NULL, subtitle = NULL, x = "", y = "Strength of Schedule (SoS)") +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 25, face = "bold", color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # Expand bottom margin to fit the labels
    plot.margin = margin(10, 10, 40, 10) 
  )

# Save for Instagram
ggsave("sb_winner_loser_boxplot.png", width = 10, height = 10, dpi = 150)


### Now let's make a scatter plot to compare the Patriots & Seahawks SRS vs SoS comparison among SB teams
SB_SRS <- SB_TEAMS %>%
  mutate(
    Team_Clean = gsub("\\*|\\+|SB WINNER|SB LOSER|SB \\?|SB\\?", "", .[[2]]) %>% trimws(),
    Year = as.numeric(.[[1]]),
    SRS = as.numeric(SRS),
    WL = case_when(
      grepl("SB WINNER", .[[2]]) ~ "Winner",
      grepl("SB LOSER", .[[2]]) ~ "Loser",
      TRUE ~ "TBD"
    )
  ) %>%
  filter(!is.na(SRS))

# Handpick 10 diverse teams across SRS/SoS spectrum (handpicked so no team name overlay)
notable_teams <- SB_SRS %>%
  filter(
      (Team_Clean == "Carolina Panthers" & Year == 2015) |  
      (Team_Clean == "Baltimore Ravens" & Year == 2012) |    
      (Team_Clean == "Atlanta Falcons" & Year == 2016) |     
      (Team_Clean == "Pittsburgh Steelers" & Year == 2010) | 
      (Team_Clean == "Cincinnati Bengals" & Year == 2021) |  
      (Team_Clean == "Kansas City Chiefs" & Year == 2023) |  
      (Team_Clean == "Tampa Bay Buccaneers" & Year == 2020) | 
      (Team_Clean == "Philadelphia Eagles" & Year == 2017) | 
      (Team_Clean == "San Francisco 49ers" & Year == 2019)  
  ) %>%
  mutate(
    Team_Abbrev = case_when(
      Team_Clean == "Pittsburgh Steelers" ~ "PIT",
      Team_Clean == "Kansas City Chiefs" ~ "KC",
      Team_Clean == "San Francisco 49ers" ~ "SF",
      Team_Clean == "Philadelphia Eagles" ~ "PHI",
      Team_Clean == "Carolina Panthers" ~ "CAR",
      Team_Clean == "Cincinnati Bengals" ~ "CIN",
      Team_Clean == "Baltimore Ravens" ~ "BAL",
      Team_Clean == "Atlanta Falcons" ~ "ATL",
      Team_Clean == "Tampa Bay Buccaneers" ~ "TB",
      TRUE ~ substr(Team_Clean, 1, 3)
    ),
    Label = paste(Team_Abbrev, Year, sep = " "),
    vjust_adj = case_when(
      SRS > 10 ~ -1.3,
      SRS < 3 ~ 1.5,
      TRUE ~ -1.2
    ),
    hjust_adj = case_when(
      SoS < -1 ~ -0.15,
      SoS > 5 ~ 1.15,
      TRUE ~ 0.5
    )
  )
ggplot(SB_SRS, aes(x = SoS, y = SRS, color = WL)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_point(data = filter(SB_SRS, Team_Clean == "New England Patriots" & Year == 2025),
             size = 4, shape = 18, color = "#0C2340") +
  geom_point(data = filter(SB_SRS, Team_Clean == "Seattle Seahawks" & Year == 2025),
             size = 4, shape = 18, color = "#FFD700") +
  geom_text(data = filter(SB_SRS, Team_Clean == "New England Patriots" & Year == 2025),
            aes(label = "2025 PATS"), vjust = -1.5, color = "#0C2340", fontface = "bold", size = 3.5, family = "Arial") +
  geom_text(data = filter(SB_SRS, Team_Clean == "Seattle Seahawks" & Year == 2025),
            aes(label = "2025 SEA"), vjust = -1.5, color = "#FFD700", fontface = "bold", size = 3.5, family = "Arial") +
  geom_text(data = notable_teams, aes(label = Label, vjust = vjust_adj, hjust = hjust_adj), 
            size = 3.5, fontface = "bold", family = "Arial") +
  geom_hline(yintercept = median(filter(SB_SRS, WL == "Winner")$SRS, na.rm = TRUE), 
             linetype = "dashed", color = "#228B22", size = 1) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  scale_color_manual(values = c("Winner" = "#228B22", "Loser" = "#DC143C", "TBD" = "#FFD700")) +
  scale_x_continuous(limits = c(-6, NA)) +
  labs(
    x = "Strength of Schedule (SoS) →",
    y = "Simple Rating System (SRS) →",
    color = "Outcome") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "top",
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.line = element_line(color = "black")
  )

# Save for Instagram (1080x1080 square post)
ggsave(
  "easy_schedule_playoff_dooooooom.png", 
  width = 180 / 300, 
  height = 1640 / 300, 
  dpi = 300
)

### Now look at all playoff teams with easy schedules - how far did they get?
# Patriots are the ONLY easy-schedule team to reach Super Bowl

# Get all playoff teams (marked with * or +)
PLAYOFF_TEAMS <- SOS_DATA %>%
  filter(grepl("\\*|\\+", SOS_DATA[[2]])) %>%
  mutate(
    Team_Clean = gsub("\\*|\\+|SB WINNER|SB LOSER|SB \\?|SB\\?", "", .[[2]]) %>% trimws(),
    Year = as.numeric(.[[1]]),
    SoS = as.numeric(SoS),
    Record = paste0(.[[3]], "-", .[[4]]),
    # Determine how far they made it
    Playoff_Round = case_when(
      grepl("SB WINNER", .[[2]]) ~ 5,
      grepl("SB LOSER|SB \\?", .[[2]]) ~ 4,
      TRUE ~ NA_real_  # We'll assign these manually for the easy schedule teams
    )
  ) %>%
  filter(!is.na(SoS))

# Filter for easy schedule teams (SoS < -2.0) and manually code their playoff exits
EASY_SCHEDULE_PLAYOFF <- PLAYOFF_TEAMS %>%
  filter(SoS < -2.0) %>%
  filter(!(Team_Clean == "Cincinnati Bengals" & Year == 2013)) %>% ## just removing these 3 teams who were above 2016 pats.. to clear space
  filter(!(Team_Clean == "Buffalo Bills" & Year == 2025)) %>%
  filter(!(Team_Clean == "Green Bay Packers" & Year == 2013)) %>%
  mutate(
    # Manually code playoff round for key teams based on historical data
    Playoff_Round = case_when(
      Team_Clean == "New England Patriots" & Year == 2025 ~ 4,  # Super Bowl (TBD)
      Team_Clean == "Seattle Seahawks" & Year == 2010 ~ 2,      # Won Wild Card, lost Divisional
      Team_Clean == "Denver Broncos" & Year == 2011 ~ 2,        # Won Wild Card, lost Divisional  
      Team_Clean == "Buffalo Bills" & Year == 2017 ~ 1,         # Lost Wild Card
      Team_Clean == "Tennessee Titans" & Year == 2017 ~ 2,      # Lost Divisional
      Team_Clean == "Indianapolis Colts" & Year == 2020 ~ 1,    # Lost Wild Card
      Team_Clean == "Indianapolis Colts" & Year == 2012 ~ 1,    # Lost Wild Card
      Team_Clean == "Baltimore Ravens" & Year == 2014 ~ 2,      # Lost Divisional
      TRUE ~ Playoff_Round
    ),
    Round_Label = case_when(
      Playoff_Round == 1 ~ "Lost Wild Card",
      Playoff_Round == 2 ~ "Lost Divisional",
      Playoff_Round == 3 ~ "Lost Conference",
      Playoff_Round == 4 ~ "Super Bowl",
      Playoff_Round == 5 ~ "Won Super Bowl",
      TRUE ~ "Unknown"
    ),
    Team_Year = paste(Team_Clean, Year, sep = "\n"),
    Is_Patriots = ifelse(Team_Clean == "New England Patriots" & Year == 2025, "Patriots", "Other")
  ) %>%
  arrange(SoS) %>%
  head(8)  

ggplot(EASY_SCHEDULE_PLAYOFF, aes(x = reorder(Team_Year, SoS), y = Playoff_Round, fill = Is_Patriots)) +
  geom_bar(stat = "identity", color = "black", size = 0.8) +
  geom_text(aes(label = Round_Label), hjust = -0.1, size = 5, fontface = "bold", family = "Arial") +
  geom_text(aes(label = paste0("SoS: ", SoS)), y = 0.3, hjust = 0, size = 4.5, color = "white", fontface = "bold", family = "Arial") +
  scale_fill_manual(values = c("Patriots" = "#0C2340", "Other" = "#DC143C")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                     labels = c("Wild Card", "Divisional", "Conference", "Super Bowl", "SB Winner"),
                     limits = c(0, 5.5)) +
  coord_flip() +
  labs(
       x = "Team & Year",
       y = "Furthest Playoff Round Reached") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save for Instagram (1080x1080 square post)
ggsave(
  "sbteamss.png", 
  width = 3380 / 300, 
  height = 1440 / 300, 
  dpi = 300
)
## Conclusion, The 2025 Patriots had the 3rd easiest schedule of ANY playoff team in 15 years (-4.5 SoS), and they're the ONLY team with a schedule this easy to reach a Super Bowl - all others got knocked out early.
# The data shows a clear pattern - SB winners typically dominate tough competition (high SRS + positive SoS), meanwhile, the Patriots are in a grey area after playing an easy schedule and displaying average performance metrics.