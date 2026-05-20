library(tidyverse)
library(StatsBombR)
library(ggsoccer)
library(scales)
library(ggplot2)
library(shiny)
library(shinylive)
library(plotly)


comps <- FreeCompetitions()

# Filter to Bundesliga 15/16
bundesliga <- comps[comps$competition_name == "1. Bundesliga" & comps$season_name == "2015/2016", ]
# Get all matches for that competition
bundesliga_matches <- FreeMatches(Competitions = bundesliga)


# Bayern matches
bayern_matches <- bundesliga_matches[
  bundesliga_matches$home_team.home_team_name == "Bayern Munich" |
    bundesliga_matches$away_team.away_team_name == "Bayern Munich",
]

bayern_events <- free_allevents(MatchesDF = bayern_matches)
bayern_events <- allclean(bayern_events)

bayern_passes <- bayern_events[
  bayern_events$type.name == "Pass" &
    bayern_events$team.name == "Bayern Munich",
]

bayern_clean <- bayern_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(bayern_clean)

bayern_clean$team <- "Bayern Munich"

# Bayern Pass origins

ggplot(bayern_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Bayern Munich pass origins",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Bayern Pass destinations

ggplot(bayern_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Bayern Munich pass destinations",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Bayern Progressive Pass Filter

bayern_progressive_passes <- bayern_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Bayern Progressive pass origins

ggplot(bayern_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Bayern Munich progressive pass origins",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )




bayern_clean <- bayern_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



bayern_lane_counts <- bayern_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

bayern_lane_counts



# Bayern Munich pass distribution by vertical lane

ggplot(bayern_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#4A0015") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Bayern Munich pass distribution by vertical lane",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


bayern_progressive_passes <- bayern_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



bayern_progressive_lane_counts <- bayern_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

bayern_progressive_lane_counts



# Bayern Munich progressive pass distribution by vertical lane

ggplot(bayern_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#4A0015") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Bayern Munich progressive pass distribution",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


# Gladbach Matches


gladbach_matches <- bundesliga_matches[
  bundesliga_matches$home_team.home_team_name == "Borussia Mönchengladbach" |
    bundesliga_matches$away_team.away_team_name == "Borussia Mönchengladbach",
]

gladbach_events <- free_allevents(MatchesDF = gladbach_matches)
gladbach_events <- allclean(gladbach_events)

gladbach_passes <- gladbach_events[
  gladbach_events$type.name == "Pass" &
    gladbach_events$team.name == "Borussia Mönchengladbach",
]

gladbach_clean <- gladbach_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(gladbach_clean)

gladbach_clean$team <- "Borussia Mönchengladbach"



# Gladbach Pass origins

ggplot(gladbach_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Borussia Mönchengladbach pass origins",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Gladbach Pass destinations

ggplot(gladbach_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Borussia Mönchengladbach pass destinations",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Gladbach Progressive Pass Filter

gladbach_progressive_passes <- gladbach_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Gladbach Progressive pass origins

ggplot(gladbach_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Borussia Mönchengladbach progressive pass origins",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )




gladbach_clean <- gladbach_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



gladbach_lane_counts <- gladbach_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

gladbach_lane_counts




# Borussia Mönchengladbach pass distribution by vertical lane

ggplot(gladbach_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#2E5E3B") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Borussia Mönchengladbach pass distribution by vertical lane",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



gladbach_progressive_passes <- gladbach_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



gladbach_progressive_lane_counts <- gladbach_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

gladbach_progressive_lane_counts



# Borussia Mönchengladbach progressive pass distribution by vertical lane

ggplot(gladbach_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#2E5E3B") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Borussia Mönchengladbach progressive pass distribution",
    subtitle = "2015/16 Bundesliga",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Filter to Premier League 15/16
prem <- comps[comps$competition_name == "Premier League" & comps$season_name == "2015/2016", ]
# Get all matches for that competition
prem_matches <- FreeMatches(Competitions = prem)



# Arsenal matches
arsenal_matches <- prem_matches[
  prem_matches$home_team.home_team_name == "Arsenal" |
    prem_matches$away_team.away_team_name == "Arsenal",
]

arsenal_events <- free_allevents(MatchesDF = arsenal_matches)
arsenal_events <- allclean(arsenal_events)

arsenal_passes <- arsenal_events[
  arsenal_events$type.name == "Pass" &
    arsenal_events$team.name == "Arsenal",
]

arsenal_clean <- arsenal_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(arsenal_clean)

arsenal_clean$team <- "Arsenal"



# Arsenal Pass origins

ggplot(arsenal_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Arsenal pass origins",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Arsenal Pass destinations

ggplot(arsenal_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Arsenal pass destinations",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Arsenal Progressive Pass Filter

arsenal_progressive_passes <- arsenal_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Arsenal Progressive pass origins

ggplot(arsenal_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Arsenal progressive pass origins",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )




arsenal_clean <- arsenal_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



arsenal_lane_counts <- arsenal_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

arsenal_lane_counts


# Arsenal pass distribution by vertical lane

ggplot(arsenal_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#EF0107") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Arsenal pass distribution by vertical lane",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



arsenal_progressive_passes <- arsenal_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



arsenal_progressive_lane_counts <- arsenal_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

arsenal_progressive_lane_counts


# Arsenal progressive pass distribution by vertical lane

ggplot(arsenal_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#EF0107") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Arsenal progressive pass distribution",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Man City matches
mancity_matches <- prem_matches[
  prem_matches$home_team.home_team_name == "Manchester City" |
    prem_matches$away_team.away_team_name == "Manchester City",
]

mancity_events <- free_allevents(MatchesDF = mancity_matches)
mancity_events <- allclean(mancity_events)

mancity_passes <- mancity_events[
  mancity_events$type.name == "Pass" &
    mancity_events$team.name == "Manchester City",
]

mancity_clean <- mancity_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(mancity_clean)

mancity_clean$team <- "Manchester City"



# Man City Pass origins

ggplot(mancity_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Manchester City pass origins",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Man City Pass destinations

ggplot(mancity_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Manchester City pass destinations",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Man City Progressive Pass Filter

mancity_progressive_passes <- mancity_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Man City Progressive pass origins

ggplot(mancity_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Manchester City progressive pass origins",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )




mancity_clean <- mancity_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



mancity_lane_counts <- mancity_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

mancity_lane_counts


# Manchester City pass distribution by vertical lane

ggplot(mancity_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#6CABDD") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Manchester City pass distribution by vertical lane",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



mancity_progressive_passes <- mancity_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



mancity_progressive_lane_counts <- mancity_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

mancity_progressive_lane_counts


ggplot(mancity_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#6CABDD") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Manchester City progressive pass distribution",
    subtitle = "2015/16 Premier League",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Filter to Serie A 15/16
seriea <- comps[comps$competition_name == "Serie A" & comps$season_name == "2015/2016", ]
# Get all matches for that competition
seriea_matches <- FreeMatches(Competitions = seriea)


# Juventus matches
juve_matches <- seriea_matches[
  seriea_matches$home_team.home_team_name == "Juventus" |
    seriea_matches$away_team.away_team_name == "Juventus",
]

juve_events <- free_allevents(MatchesDF = juve_matches)
juve_events <- allclean(juve_events)

juve_passes <- juve_events[
  juve_events$type.name == "Pass" &
    juve_events$team.name == "Juventus",
]

juve_clean <- juve_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(juve_clean)

juve_clean$team <- "Juventus"


# Juventus Pass origins

ggplot(juve_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Juventus pass origins",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Juventus Pass destinations

ggplot(juve_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Juventus pass destinations",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Juventus Progressive Pass Filter

juve_progressive_passes <- juve_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Juventus Progressive pass origins

ggplot(juve_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Juventus progressive pass origins",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )




juve_clean <- juve_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



juve_lane_counts <- juve_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

juve_lane_counts



# Juventus pass distribution by vertical lane

ggplot(juve_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#000000") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Juventus pass distribution by vertical lane",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


juve_progressive_passes <- juve_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



juve_progressive_lane_counts <- juve_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

juve_progressive_lane_counts


# Juventus progressive pass distribution by vertical lane

ggplot(juve_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#000000") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Juventus progressive pass distribution",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


# Roma matches
roma_matches <- seriea_matches[
  seriea_matches$home_team.home_team_name == "AS Roma" |
    seriea_matches$away_team.away_team_name == "AS Roma",
]

roma_events <- free_allevents(MatchesDF = roma_matches)
roma_events <- allclean(roma_events)

roma_passes <- roma_events[
  roma_events$type.name == "Pass" &
    roma_events$team.name == "AS Roma",
]

roma_clean <- roma_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(roma_clean)

roma_clean$team <- "AS Roma"


# Roma Pass origins

ggplot(roma_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Roma pass origins",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Roma Pass destinations

ggplot(roma_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Roma pass destinations",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Roma Progressive Pass Filter

roma_progressive_passes <- roma_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Roma Progressive pass origins

ggplot(roma_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Roma progressive pass origins",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



roma_clean <- roma_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



roma_lane_counts <- roma_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

roma_lane_counts



# Roma pass distribution by vertical lane

ggplot(roma_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#6B0F1A") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Roma pass distribution by vertical lane",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


roma_progressive_passes <- roma_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



roma_progressive_lane_counts <- roma_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

roma_progressive_lane_counts


# Roma progressive pass distribution by vertical lane

ggplot(roma_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#6B0F1A") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Roma progressive pass distribution",
    subtitle = "2015/16 Serie A",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Filter to Ligue 1 15/16
ligue1 <- comps[comps$competition_name == "Ligue 1" & comps$season_name == "2015/2016", ]
# Get all matches for that competition
ligue1_matches <- FreeMatches(Competitions = ligue1)


# PSG matches
psg_matches <- ligue1_matches[
  ligue1_matches$home_team.home_team_name == "Paris Saint-Germain" |
    ligue1_matches$away_team.away_team_name == "Paris Saint-Germain",
]

psg_events <- free_allevents(MatchesDF = psg_matches)
psg_events <- allclean(psg_events)

psg_passes <- psg_events[
  psg_events$type.name == "Pass" &
    psg_events$team.name == "Paris Saint-Germain",
]

psg_clean <- psg_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(psg_clean)

psg_clean$team <- "Paris Saint-Germain"



# PSG Pass origins

ggplot(psg_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "PSG pass origins",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# PSG Pass destinations

ggplot(psg_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "PSG pass destinations",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# PSG Progressive Pass Filter

psg_progressive_passes <- psg_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# PSG Progressive pass origins

ggplot(psg_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "PSG progressive pass origins",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



psg_clean <- psg_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



psg_lane_counts <- psg_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

psg_lane_counts



ggplot(psg_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#001F3F") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Paris Saint-Germain pass distribution by vertical lane",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )




psg_progressive_passes <- psg_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



psg_progressive_lane_counts <- psg_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

psg_progressive_lane_counts


# Paris Saint-Germain progressive pass distribution by vertical lane

ggplot(psg_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#001F3F") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Paris Saint-Germain progressive pass distribution",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Lyon matches
lyon_matches <- ligue1_matches[
  ligue1_matches$home_team.home_team_name == "Lyon" |
    ligue1_matches$away_team.away_team_name == "Lyon",
]

lyon_events <- free_allevents(MatchesDF = lyon_matches)
lyon_events <- allclean(lyon_events)

lyon_passes <- lyon_events[
  lyon_events$type.name == "Pass" &
    lyon_events$team.name == "Lyon",
]

lyon_clean <- lyon_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(lyon_clean)

lyon_clean$team <- "Lyon"



# Lyon Pass origins

ggplot(lyon_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Lyon pass origins",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Lyon Pass destinations

ggplot(lyon_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Lyon pass destinations",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Lyon Progressive Pass Filter

lyon_progressive_passes <- lyon_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Lyon Progressive pass origins

ggplot(lyon_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Lyon progressive pass origins",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



lyon_clean <- lyon_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



lyon_lane_counts <- lyon_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

lyon_lane_counts


# Olympique Lyonnais pass distribution by vertical lane

ggplot(lyon_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#007FFF") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Olympique Lyonnais pass distribution by vertical lane",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )


lyon_progressive_passes <- lyon_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



lyon_progressive_lane_counts <- lyon_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

lyon_progressive_lane_counts



# Olympique Lyonnais progressive pass distribution by vertical lane

ggplot(lyon_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#007FFF") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Olympique Lyonnais progressive pass distribution",
    subtitle = "2015/16 Ligue 1",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Filter to La Liga 15/16
laliga <- comps[comps$competition_name == "La Liga" & comps$season_name == "2015/2016", ]
# Get all matches for that competition
laliga_matches <- FreeMatches(Competitions = laliga)



# Barca matches
barca_matches <- laliga_matches[
  laliga_matches$home_team.home_team_name == "Barcelona" |
    laliga_matches$away_team.away_team_name == "Barcelona",
]

barca_events <- free_allevents(MatchesDF = barca_matches)
barca_events <- allclean(barca_events)

barca_passes <- barca_events[
  barca_events$type.name == "Pass" &
    barca_events$team.name == "Barcelona",
]

barca_clean <- barca_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(barca_clean)

barca_clean$team <- "Barcelona"



# Barca Pass origins

ggplot(barca_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Barcelona pass origins",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Barca Pass destinations

ggplot(barca_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Barcelona pass destinations",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Barca Progressive Pass Filter

barca_progressive_passes <- barca_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Barca Progressive pass origins

ggplot(barca_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Barcelona progressive pass origins",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



barca_clean <- barca_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



barca_lane_counts <- barca_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

barca_lane_counts



ggplot(barca_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#004D98") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Barcelona pass distribution by vertical lane",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



barca_progressive_passes <- barca_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



barca_progressive_lane_counts <- barca_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

barca_progressive_lane_counts



# Barcelona progressive pass distribution by vertical lane

ggplot(barca_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#004D98") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Barcelona progressive pass distribution",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Real Madrid matches
real_matches <- laliga_matches[
  laliga_matches$home_team.home_team_name == "Real Madrid" |
    laliga_matches$away_team.away_team_name == "Real Madrid",
]

real_events <- free_allevents(MatchesDF = real_matches)
real_events <- allclean(real_events)

real_passes <- real_events[
  real_events$type.name == "Pass" &
    real_events$team.name == "Real Madrid",
]

real_clean <- real_passes %>%
  select(
    match_id,
    minute,
    second,
    player.name,
    pass.recipient.name,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.length,
    pass.angle,
    under_pressure,
    possession,
    play_pattern.name,
    pass.switch,
    pass.cross,
    pass.through_ball,
    pass.cut_back,
    pass.outcome.name
  )

glimpse(real_clean)

real_clean$team <- "Real Madrid"


# Real Pass origins

ggplot(real_clean, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Real Madrid pass origins",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Real Pass destinations

ggplot(real_clean, aes(x = pass.end_location.x, y = pass.end_location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  scale_fill_viridis_c(name = "Passes") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Real Madrid pass destinations",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# Real Progressive Pass Filter

real_progressive_passes <- real_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



# Real Progressive pass origins

ggplot(real_progressive_passes, aes(x = location.x, y = location.y)) +
  
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "grey40"
  ) +
  
  geom_bin2d(bins = 25, alpha = 0.85) +
  
  scale_fill_viridis_c(name = "Progressive\npasses") +
  
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 80),
    expand = FALSE
  ) +
  
  labs(
    title = "Real Madrid progressive pass origins",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = NULL
  ) +
  
  theme_pitch() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



real_clean <- real_clean %>%
  mutate(
    vertical_lane = case_when(
      location.y < 16 ~ "Left Wing",
      location.y < 32 ~ "Left Half-Space",
      location.y < 48 ~ "Centre",
      location.y < 64 ~ "Right Half-Space",
      TRUE ~ "Right Wing"
    )
  )



real_lane_counts <- real_clean %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

real_lane_counts



# Real Madrid pass distribution by vertical lane

ggplot(real_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#C9A227") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Real Madrid pass distribution by vertical lane",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = "Share of passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



real_progressive_passes <- real_clean %>%
  filter(
    pass.end_location.x > location.x + 15
  )



real_progressive_lane_counts <- real_progressive_passes %>%
  count(vertical_lane) %>%
  mutate(
    pct = n / sum(n)
  )

real_progressive_lane_counts


# Real Madrid progressive pass distribution by vertical lane

ggplot(real_progressive_lane_counts,
       aes(x = factor(
         vertical_lane,
         levels = c(
           "Left Wing",
           "Left Half-Space",
           "Centre",
           "Right Half-Space",
           "Right Wing"
         )
       ),
       y = pct)) +
  
  geom_col(width = 0.7, fill = "#C9A227") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Real Madrid progressive pass distribution",
    subtitle = "2015/16 La Liga",
    x = NULL,
    y = "Share of progressive passes"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    legend.position = "none"
  )



# Pass distribution datasets

bayern_lane_counts$team <- "Bayern Munich"
bayern_lane_counts$league <- "1. Bundesliga"

gladbach_lane_counts$team <- "Borussia Mönchengladbach"
gladbach_lane_counts$league <- "1. Bundesliga"

arsenal_lane_counts$team <- "Arsenal"
arsenal_lane_counts$league <- "Premier League"

mancity_lane_counts$team <- "Manchester City"
mancity_lane_counts$league <- "Premier League"

juve_lane_counts$team <- "Juventus"
juve_lane_counts$league <- "Serie A"

roma_lane_counts$team <- "AS Roma"
roma_lane_counts$league <- "Serie A"

psg_lane_counts$team <- "Paris Saint-Germain"
psg_lane_counts$league <- "Ligue 1"

lyon_lane_counts$team <- "Lyon"
lyon_lane_counts$league <- "Ligue 1"

barca_lane_counts$team <- "Barcelona"
barca_lane_counts$league <- "La Liga"

real_lane_counts$team <- "Real Madrid"
real_lane_counts$league <- "La Liga"





# Pass distribution datasets

bayern_progressive_lane_counts$team <- "Bayern Munich"
bayern_progressive_lane_counts$league <- "1. Bundesliga"

gladbach_progressive_lane_counts$team <- "Borussia Mönchengladbach"
gladbach_progressive_lane_counts$league <- "1. Bundesliga"

arsenal_progressive_lane_counts$team <- "Arsenal"
arsenal_progressive_lane_counts$league <- "Premier League"

mancity_progressive_lane_counts$team <- "Manchester City"
mancity_progressive_lane_counts$league <- "Premier League"

juve_progressive_lane_counts$team <- "Juventus"
juve_progressive_lane_counts$league <- "Serie A"

roma_progressive_lane_counts$team <- "AS Roma"
roma_progressive_lane_counts$league <- "Serie A"

psg_progressive_lane_counts$team <- "Paris Saint-Germain"
psg_progressive_lane_counts$league <- "Ligue 1"

lyon_progressive_lane_counts$team <- "Lyon"
lyon_progressive_lane_counts$league <- "Ligue 1"

barca_progressive_lane_counts$team <- "Barcelona"
barca_progressive_lane_counts$league <- "La Liga"

real_progressive_lane_counts$team <- "Real Madrid"
real_progressive_lane_counts$league <- "La Liga"




combined_lane_counts <- bind_rows(
  bayern_lane_counts,
  gladbach_lane_counts,
  arsenal_lane_counts,
  mancity_lane_counts,
  juve_lane_counts,
  roma_lane_counts,
  psg_lane_counts,
  lyon_lane_counts,
  barca_lane_counts,
  real_lane_counts
)



combined_progressive_lane_counts <- bind_rows(
  bayern_progressive_lane_counts,
  gladbach_progressive_lane_counts,
  arsenal_progressive_lane_counts,
  mancity_progressive_lane_counts,
  juve_progressive_lane_counts,
  roma_progressive_lane_counts,
  psg_progressive_lane_counts,
  lyon_progressive_lane_counts,
  barca_progressive_lane_counts,
  real_progressive_lane_counts
)



glimpse(combined_lane_counts)

glimpse(combined_progressive_lane_counts)


unique(combined_progressive_lane_counts$team)



# Save cleaned datasets


saveRDS(
  combined_lane_counts,
  "data/cleaned/combined_lane_counts.rds"
)


saveRDS(
  combined_progressive_lane_counts,
  "data/cleaned/combined_progressive_lane_counts.rds"
)





# Combined heatmap datasets
# =========================

combined_pass_origins <- bind_rows(
  bayern_clean,
  gladbach_clean,
  arsenal_clean,
  mancity_clean,
  juve_clean,
  roma_clean,
  psg_clean,
  lyon_clean,
  barca_clean,
  real_clean
)

combined_pass_destinations <- combined_pass_origins




combined_progressive_origins <- bind_rows(
  bayern_progressive_passes,
  gladbach_progressive_passes,
  arsenal_progressive_passes,
  mancity_progressive_passes,
  juve_progressive_passes,
  roma_progressive_passes,
  psg_progressive_passes,
  lyon_progressive_passes,
  barca_progressive_passes,
  real_progressive_passes
)



saveRDS(
  combined_pass_origins,
  "data/cleaned/combined_pass_origins.rds"
)

saveRDS(
  combined_pass_destinations,
  "data/cleaned/combined_pass_destinations.rds"
)

saveRDS(
  combined_progressive_origins,
  "data/cleaned/combined_progressive_origins.rds"
)


pass_origins %>%
  filter(team == "Bayern Munich") %>%
  ggplot(aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey40") +
  geom_bin2d(bins = 25, alpha = 0.85) +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  scale_fill_viridis_c(name = "Passes") +
  theme_pitch()