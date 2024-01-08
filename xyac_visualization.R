# load relevant libraries
source('helpers.R')
source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(transformr)
library(ggnewscale)
library(ggridges)
library(patchwork)
library(ggplot2)
library(magick)
library(gt)
library(gtExtras)
library(RColorBrewer)

players <- read_csv("data/players.csv") %>% 
  select(nflId, position)

df_tracking <- data.frame()

#read in xyac data
for (w in seq(1,9)) {

  temp_df <- arrow::read_parquet(paste0("data/tracking_with_xyac_week_",w,".parquet"))
  
  #storing temporary dataframe in full season dataframe
  df_tracking <-
    bind_rows(temp_df, df_tracking)
  
}

rm(temp_df)

#we only need offensive team/defensive team/yac/xyac/yacoe/epa
plays <- df_tracking %>% 
  mutate(defensiveTeam = if_else(homeTeamAbbr != possessionTeam, homeTeamAbbr, visitorTeamAbbr),
         yacoe = yards_after_catch - xyac) %>% 
  select(gameId, playId, possessionTeam, defensiveTeam, yards_after_catch, xyac) %>% 
  unique()

#join participation and ftn data
participation <- nflreadr::load_participation(seasons = 2022, include_pbp = TRUE)

#filter to weeks 1-9//renaming
part <- participation %>% filter(week < 10) %>% 
  #only want complete passes
  filter(complete_pass == 1) %>% 
  #select cols
  select(old_game_id, play_id, complete_pass, route, offense_personnel, defense_man_zone_type,
         defense_coverage_type, xyac_mean_yardage, air_yards, pass_location) %>% 
  mutate(old_game_id = as.numeric(old_game_id))

plays <- left_join(plays, part, by = c("gameId" = "old_game_id", "playId" = "play_id"))


receivers <- df_tracking %>% 
  filter(isBallcarrier == TRUE) %>% 
  mutate(defensiveTeam = if_else(homeTeamAbbr != possessionTeam, homeTeamAbbr, visitorTeamAbbr),
         yacoe = yards_after_catch - xyac) %>% 
  left_join(players) %>% 
  select(gameId, playId, possessionTeam, defensiveTeam, displayName, position, yards_after_catch, xyac)  %>% 
  group_by(possessionTeam, displayName, position) %>% 
  summarise(catches = n(),xyac = mean(xyac), yac = mean(yards_after_catch), yacoe = yac - xyac) %>%
  filter(catches >= 10) %>% 
  arrange(desc(yacoe)) %>% 
  ungroup()

league_max_xyac <- max(receivers$xyac)
league_min_xyac <- min(receivers$xyac)

league_max_yacoe <- max(receivers$yacoe)
league_min_yacoe <- min(receivers$yacoe)

chart_team <- "MIA"

yac_table <- receivers %>% 
  filter(possessionTeam == chart_team) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**",chart_team,"'s Receiver Performance Through Yards After The Catch (YAC)**")),
             subtitle = paste0("Weeks 1-9, 2022 | Minimum 10 Catches")) %>% 
  cols_label(possessionTeam = "Team",
             displayName = "Player",
             position = "Pos.",
             catches = "Receptions",
             yac = html("Actual YAC<br>per Catch"),
             xyac = html("Expected YAC<br>per Catch"),
             yacoe = html("YAC Over<br>Expected")) %>%
  cols_align(align = "center",
             columns = c(position, catches, xyac, yac, yacoe)) %>% 
  cols_hide(c(possessionTeam)) %>% 
  cols_width(c(position) ~ px(50)) %>% 
  cols_width(c(catches, xyac, yac, yacoe) ~ px(110)) %>% 
  fmt_number(columns = c(xyac, yac, yacoe)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(catches)
      )
    )
  ) %>%
  data_color(
    columns = c(xyac),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_xyac, league_max_xyac)
    ),
    alpha = 0.7
  ) %>%
  data_color(
    columns = c(yacoe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_yacoe, league_max_yacoe)
    ),
    alpha = 0.7
  )

gtExtras::gtsave_extra(yac_table, filename = paste0("Charts/",chart_team," YAC.png"),
                       vwidth = 650)

def_coverage_yac <- plays %>% 
  group_by(defensiveTeam, defense_man_zone_type, defense_coverage_type) %>%
  summarise(plays = n(), xyac = mean(xyac), yac = mean(yards_after_catch), yacoe = yac - xyac) %>% 
  mutate(defense_coverage_type = stringr::str_to_title(sub("_", " ", defense_coverage_type)),
         defense_man_zone_type = stringr::str_to_title(sub("_", " ", defense_man_zone_type))) %>% 
  filter(plays >= 5)


league_max_xyac <- max(def_coverage_yac$xyac)
league_min_xyac <- min(def_coverage_yac$xyac)

league_max_yacoe <- max(def_coverage_yac$yacoe)
league_min_yacoe <- min(def_coverage_yac$yacoe)


chart_team <- "CLE"
def_cov_table <- def_coverage_yac %>% 
  filter(defensiveTeam == chart_team) %>% 
  gt(groupname_col = "defense_man_zone_type") %>% 
  tab_header(title = md(paste0("**",chart_team," Defensive Coverage Performance**")),
             subtitle = paste0("Weeks 1-9, 2022 | Minimum 5 plays")) %>% 
  cols_label(defense_coverage_type = "Coverage",
             plays = "# Plays",
             yac = html("Actual YAC<br>per Catch"),
             xyac = html("Expected YAC<br>per Catch"),
             yacoe = html("YAC Over<br>Expected")) %>%
  cols_align(align = "center",
             columns = c(plays, xyac, yac, yacoe)) %>% 
  # gt() %>% 
  cols_hide(c(defensiveTeam)) %>% 
  cols_width(c(plays, xyac, yac, yacoe) ~ px(110)) %>% 
  fmt_number(columns = c(xyac, yac, yacoe)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(plays)
      )
    )
  ) %>%
  data_color(
    columns = c(xyac),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_xyac, league_max_xyac)
    ),
    alpha = 0.8
  ) %>%
  data_color(
    columns = c(yacoe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_yacoe, league_max_yacoe)
    ),
    alpha = 0.8
  )

gtExtras::gtsave_extra(def_cov_table, filename = paste0("Charts/",chart_team," defensive coverage YAC.png"),
                       vwidth = 510)


def_route_yac <- plays %>% 
  group_by(defensiveTeam, route) %>%
  summarise(plays = n(), xyac = mean(xyac), yac = mean(yards_after_catch), yacoe = yac - xyac) %>% 
  ungroup() %>% 
  mutate(route = stringr::str_to_title(route)) %>% 
  filter(plays >= 5, route != "NA")

league_max_xyac <- max(def_route_yac$xyac)
league_min_xyac <- min(def_route_yac$xyac)

league_max_yacoe <- max(def_route_yac$yacoe)
league_min_yacoe <- min(def_route_yac$yacoe)


chart_team <- "CLE"

def_routes_table <- def_route_yac %>% 
  filter(defensiveTeam == chart_team) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**",chart_team," Defensive Routes Performance**")),
             subtitle = paste0("Weeks 1-9, 2022 | Minimum 5 plays")) %>% 
  cols_label(route = "Route",
             plays = "# Plays",
             yac = html("Actual YAC<br>per Catch"),
             xyac = html("Expected YAC<br>per Catch"),
             yacoe = html("YAC Over<br>Expected")) %>%
  cols_align(align = "center",
             columns = c(plays, xyac, yac, yacoe)) %>% 
  cols_hide(c(defensiveTeam)) %>%
  cols_width(c(plays, xyac, yac, yacoe) ~ px(110)) %>% 
  fmt_number(columns = c(xyac, yac, yacoe)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(plays)
      )
    )
  ) %>%
  data_color(
    columns = c(xyac),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_xyac, league_max_xyac)
    ),
    alpha = 0.8
  ) %>%
  data_color(
    columns = c(yacoe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = c(league_min_yacoe, league_max_yacoe)
    ),
    alpha = 0.8
  )

gtExtras::gtsave_extra(def_routes_table, filename = paste0("Charts/",chart_team," defensive routes YAC.png"),
                       vwidth = 510)

def_passing <- plays %>% 
  mutate(pass_depth = case_when(air_yards <0 ~ "Screen (<0 air yards)",
                                air_yards <= 9 ~ "Short (<10 air yards)",
                                air_yards >=10 & air_yards <=19 ~ "Intermediate (10-19 air yards)",
                                air_yards >= 20 ~ "Deep (20+ air yards)")) %>% 
  group_by(defensiveTeam, pass_location, pass_depth) %>% 
  summarize(def_plays = n(), xyac = median(xyac), yac = median(yards_after_catch), yacoe = yac - xyac) %>% 
  ungroup() %>% 
  filter(!is.na(pass_location)) %>% 
  # group_by(pass_location, pass_depth) %>% 
  mutate(def_rel_sr = xyac/max(xyac[def_plays >= 5]),
         pass_location = stringr::str_to_title(pass_location)) %>% 
  ungroup()

def_passing$pass_depth <- factor(def_passing$pass_depth, levels = c("Screen (<0 air yards)", "Short (<10 air yards)", "Intermediate (10-19 air yards)", "Deep (20+ air yards)"))

chart_team <- "CLE"

def_chart <- ggplot(data = def_passing %>% filter(defensiveTeam == chart_team)) +
  geom_tile(aes(x = pass_location, y = pass_depth, fill = def_rel_sr), alpha = 0.8) +
  geom_text(aes(x = pass_location, y = pass_depth, label = paste0("Passes: ", def_plays)),
            nudge_y = 0.125,
            size = 3,
            fontface = "bold") +
  geom_text(aes(x = pass_location, y = pass_depth, label = paste0("xYAC: ", round(xyac, 2))),
            size = 3,
            fontface = "bold") +
  geom_text(aes(x = pass_location, y = pass_depth, label = paste0("YACOE: ", round(yacoe, 2))),
            nudge_y = -0.125,
            size = 3,
            fontface = "bold") +
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  scale_y_discrete(expand = expansion(mult = c(0,0)))+
  scale_fill_gradient2(low = brewer.pal(n=5, "PRGn")[1],
                       mid = brewer.pal(n=5, "PRGn")[3],
                       high = brewer.pal(n=5, "PRGn")[5],
                       midpoint = 0.5) +
  labs(title = paste0(chart_team, " defensive YAC success"),
       subtitle = "Green indicates worse defensive xYAC performance relative to the rest of the league",
       x = NULL,
       y = NULL) +
  theme_FE+
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 6),
        legend.position = "none",
        panel.grid.major = element_blank())


brand_nfl_plot(def_chart,
               asp = 9/9,
               logo = F,
               save_name = paste0("Charts/",chart_team," DEF xyac Locations.png"))

# team-level charts
df <- read_parquet("data/xyac_epa_.parquet")

plays <- read_csv("data/plays.csv") %>%
  mutate(uniquePlayId = paste(gameId, playId, sep = "_"))

ngs <- nflreadr::load_participation(2022, include_pbp = T) %>% 
  mutate(gameId = as.numeric(old_game_id)) %>% 
  select(gameId, playId = play_id, offense_formation, desc, 
         offense_personnel, defenders_in_box, defense_personnel,
         number_of_pass_rushers, ngs_air_yards, time_to_throw,
         was_pressure, route, defense_man_zone_type, defense_coverage_type)

xyac_catch_point <- df %>% 
  left_join(ngs)


xyac_mixed_df <- xyac_catch_point %>%
  mutate(offTeam = posteam,
         defTeam = defteam) %>%
  select(uniquePlayId, offTeam, defTeam, xyac_epa, true_epa)

off_df <- xyac_mixed_df %>%
  group_by(offTeam) %>%
  summarise(xyac_epa = mean(xyac_epa),
            true_epa = mean(true_epa))

def_df <- xyac_mixed_df %>%
  group_by(defTeam) %>%
  summarise(xyac_epa = mean(xyac_epa),
            true_epa = mean(true_epa))


## DEFENSES
x_midpoint <- (max(def_df$xyac_epa) + min(def_df$xyac_epa)) / 2
x_high <- (max(def_df$xyac_epa) + x_midpoint)*0.51
x_low <- (min(def_df$xyac_epa) + x_midpoint)*0.49

y_midpoint <- (max(def_df$true_epa) + min(def_df$true_epa)) / 2
y_high <- (max(def_df$true_epa) + y_midpoint)*0.51
y_low <- (min(def_df$true_epa) + y_midpoint)*0.49

def_plot <- ggplot(def_df, aes(x = xyac_epa, y = true_epa)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_nfl_logos(aes(team_abbr = defTeam), width = 0.04, alpha = 0.7) +
  scale_y_reverse()+
  scale_x_reverse()+
  labs(x = "EPA per Play based on Expected Yards after Catch",
       y = "Actual EPA per Play",
       title = "Defenses: xYAC-Based EPA and Actual EPA",
       subtitle = "Weeks 1-9 of 2022 season | Axes reversed to show best teams at top-right",
       caption = "Data: Big Data Bowl 2024") +
  annotate("text", x = x_high, y = y_low, label = "Good tacklers force underperformance", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_high, label = "Poor tacklers allow overperformance", fontface = 2, alpha = 0.8, size = 2.5)+
  theme_FE

brand_nfl_plot(def_plot,
               asp = 16/9,
               logo = F,
               save_name = paste0("Charts/DEF EPA performance.png"))


## DEFENSES
x_midpoint <- (max(off_df$xyac_epa) + min(off_df$xyac_epa)) / 2
x_high <- (max(off_df$xyac_epa) + x_midpoint)*0.51
x_low <- (min(off_df$xyac_epa) + x_midpoint)*0.49

y_midpoint <- (max(off_df$true_epa) + min(off_df$true_epa)) / 2
y_high <- (max(off_df$true_epa) + y_midpoint)*0.51
y_low <- (min(off_df$true_epa) + y_midpoint)*0.49

off_plot <- ggplot(off_df, aes(x = xyac_epa, y = true_epa)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_nfl_logos(aes(team_abbr = offTeam), width = 0.04, alpha = 0.7) +
  labs(x = "EPA per Play based on Expected Yards after Catch",
       y = "Actual EPA per Play",
       title = "Offenses: xYAC-Based EPA and Actual EPA",
       subtitle = "Weeks 1-9 of 2022 season",
       caption = "Data: Big Data Bowl 2024") +
  annotate("text", x = x_high, y = y_low, label = "Poor playmakers underperform expectation", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_high, label = "Good playmakers overperform expectation", fontface = 2, alpha = 0.8, size = 2.5)+
  theme_FE

brand_nfl_plot(off_plot,
               asp = 16/9,
               logo = F,
               save_name = paste0("Charts/OFF EPA performance.png"))


df <- data.frame()

for (w in seq(1,9)) {
  temp_df <- arrow::read_parquet(paste0("data/tracking_with_xyac_week_",w,".parquet"))
  
  df <-
    bind_rows(temp_df, df)
  
}


plays <- read_csv("data/plays.csv") %>%
  mutate(uniquePlayId = paste(gameId, playId, sep = "_"))

ngs <- nflreadr::load_participation(2022, include_pbp = T) %>% 
  mutate(gameId = as.numeric(old_game_id)) %>% 
  select(gameId, playId = play_id, offense_formation, desc, 
         offense_personnel, defenders_in_box, defense_personnel,
         number_of_pass_rushers, ngs_air_yards, time_to_throw,
         was_pressure, route, defense_man_zone_type, defense_coverage_type)

xyac_catch_point <- df %>% 
  filter(event == "pass_outcome_caught") %>%
  left_join(ngs)


xyac_mixed_df <- xyac_catch_point %>%
  mutate(offTeam = possessionTeam,
         defTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
  select(uniquePlayId, offTeam, defTeam, route, xyac = xyac, yards_after_catch)

off_df <- xyac_mixed_df %>%
  group_by(offTeam) %>%
  summarise(xyac = mean(xyac),
            yac = mean(yards_after_catch),
            yacoe = yac - xyac)

def_df <- xyac_mixed_df %>%
  group_by(defTeam) %>%
  summarise(xyac = mean(xyac),
            yac = mean(yards_after_catch),
            yacoe = yac - xyac)

## DEFENSES
x_midpoint <- (max(def_df$xyac) + min(def_df$xyac)) / 2
x_high <- (max(def_df$xyac) + x_midpoint)*0.525
x_low <- (min(def_df$xyac) + x_midpoint)*0.475

y_midpoint <- (max(def_df$yacoe) + min(def_df$yacoe)) / 2
y_high <- -0.1#(max(def_df$yacoe) + y_midpoint)*0.525
y_low <- (min(def_df$yacoe) + y_midpoint)*0.475

def_plot <- ggplot(def_df, aes(x = xyac, y = yacoe)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept = x_midpoint, linetype = "dashed") +
  geom_hline(yintercept = y_midpoint, linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = defTeam), width = 0.04, alpha = 0.7) +
  labs(x = "Average Expected YAC (xYAC)",
       y = "Average YAC Allowed over Expected",
       subtitle = "Weeks 1-9 of 2022 season | Axes reversed to show best teams at top-right",
       title = "Defenses: xYAC Allowed & YAC Over Expected") +
  annotate("text", x = x_high, y = y_high, label = "Bad spacing increases xYAC\nBad tackling allows\nYAC over expected", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_high, y = y_low, label = "Bad spacing increases xYAC\nGood tackling prevents\nYAC over expected", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_high, label = "Good spacing limits xYAC\nBad tackling allows\nYAC over expected", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_low, label = "Good spacing limits xYAC\nGood tackling prevents\nYAC over expected", fontface = 2, alpha = 0.8, size = 2.5)+
  theme_FE

brand_nfl_plot(def_plot,
               asp = 16/9,
               logo = F,
               save_name = paste0("Charts/DEF xyac performance.png"))


## OFFENSES
x_midpoint <- (max(off_df$xyac) + min(off_df$xyac)) / 2
x_high <- (max(off_df$xyac) + x_midpoint)*0.525
x_low <- (min(off_df$xyac) + x_midpoint)*0.475

y_midpoint <- (max(off_df$yacoe) + min(off_df$yacoe)) / 2
y_high <- 0.5#(max(off_df$yacoe) + y_midpoint)*0.55
y_low <- -1#(min(off_df$yacoe) + y_midpoint)*0.45

off_plot <- ggplot(off_df, aes(x = xyac, y = yacoe)) +
  geom_vline(xintercept = x_midpoint, linetype = "dashed") +
  geom_hline(yintercept = y_midpoint, linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = offTeam), width = 0.04, alpha = 0.7) +
  labs(x = "Average Expected YAC (xYAC)",
       y = "Average YAC over Expected",
       subtitle = "Weeks 1-9 of 2022 season",
       title = "Offenses: xYAC Created & YAC Over Expected") +
  annotate("text", x = x_high, y = y_high, label = "Good spacing improves xYAC\nGood playmakers create\nadditional YAC", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_high, y = y_low, label = "Good spacing improves xYAC\nBall carriers underperform\nYAC expectations", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_high, label = "Bad spacing reduces xYAC\nGood playmakers create\nadditional YAC", fontface = 2, alpha = 0.8, size = 2.5)+
  annotate("text", x = x_low, y = y_low, label = "Bad spacing reduces xYAC\nBall carriers underperform\nYAC expectations", fontface = 2, alpha = 0.8, size = 2.5)+
  theme_FE

brand_nfl_plot(off_plot,
               asp = 16/9,
               logo = F,
               save_name = paste0("Charts/OFF xyac performance.png"))


xyac_epa <- read_parquet("data/xyac_epa_.parquet")

plays <- read_csv("data/plays.csv") %>%
  mutate(uniquePlayId = paste(gameId, playId, sep = "_"))
tackles <- read_csv("data/tackles.csv") %>%
  mutate(uniquePlayId = paste(gameId, playId, sep = "_"))

missed_tackles <- tackles %>%
  group_by(uniquePlayId) %>%
  summarise(missed_tackles = sum(pff_missedTackle)) %>%
  mutate(has_missed_tackle = ifelse(missed_tackles > 0, "Plays With Missed Tackle", "Plays Without Missed Tackle")) %>%
  filter(!is.na(missed_tackles))

xyac_mixed_df <- xyac_epa %>%
  mutate(offTeam = posteam,
         defTeam = defteam,
         yacoe_epa = true_epa-xyac_epa) %>%
  select(uniquePlayId, offTeam, defTeam, xyac_epa, true_epa, yacoe_epa)

xyac_missed_tackles <- inner_join(xyac_mixed_df, missed_tackles, by = "uniquePlayId")

xyac_missed_tackles$has_missed_tackle <- factor(xyac_missed_tackles$has_missed_tackle, levels = c("Plays Without Missed Tackle", "Plays With Missed Tackle"))

missed_tackle_epa <- mean(xyac_missed_tackles$yacoe_epa[xyac_missed_tackles$has_missed_tackle == "Plays With Missed Tackle"])
no_missed_tackle_epa <- mean(xyac_missed_tackles$yacoe_epa[xyac_missed_tackles$has_missed_tackle == "Plays Without Missed Tackle"])

fill_colors <- c("Plays With Missed Tackle" = "lightgreen", 
                 "Plays Without Missed Tackle" = "#CBC3E3")


tackle_chart <- ggplot(xyac_missed_tackles, aes(x = yacoe_epa)) +
  geom_density(aes(fill = has_missed_tackle), alpha = 0.8) +
  theme(legend.position = "top") +
  theme_FE+
  labs(x = "EPA gained on YAC over expected",
       fill = "",
       title = "Missed tackles increase the median EPA gained on YAC by 0.36 EPA/play",
       y = "Density") +
  scale_fill_manual(values = fill_colors) +
  geom_text(x=2.8, y=0.8, label="YACOE EPA With Missed Tackles") +
  geom_text(x=2.8, y=0.75, label="Mean: +0.31 | Median: +0.22") +
  geom_text(x=2.8, y=0.65, label="YACOE EPA Without Missed Tackles") +
  geom_text(x=2.8, y=0.6, label="Mean: -0.15 | Median: -0.14") +
  annotate("rect", xmin = 0.51, xmax = 5, ymin = 0.55, ymax = 0.85,
           alpha = .4) +
  xlim(-5.25, 5.25)

brand_nfl_plot(tackle_chart,
               asp = 16/9,
               logo = F,
               save_name = paste0("Charts/YAC EPA missed tackles.png"))

