library(tidyverse)
library(arrow)
library(nflverse)

# Import Data -------------------------------------------------------------

pbp <- nflfastR::load_pbp(seasons = 2022)

import_xyac_tracking = function() {
  datafiles <- list.files("data")
  is_xyac <- ifelse(!is.na(str_extract(datafiles, "xyac")), TRUE, FALSE)
  
  xyac_files <- datafiles[is_xyac]
  
  full_xyac <- map_dfr(.x = paste0("data/",xyac_files), .f = ~ read_parquet(.x))
  
  return(full_xyac)
  
}

df <- import_xyac_tracking()

plays <- read_csv("data/plays.csv")


# Create EPA from xYAC ----------------------------------------------------

xyac_frame <- df %>%
  filter(isBallcarrier == TRUE) %>%
  select(playId, gameId, frameId, uniqueFrameId, pred_yac, event, nflId, displayName) %>%
  left_join(pbp %>% mutate(old_game_id = as.numeric(old_game_id)), 
            by = c("playId" = "play_id", "gameId" = "old_game_id")) %>%
  mutate(xyards = air_yards + pred_yac)

xyac_for_epa <- xyac_frame %>%
  mutate(failed_fourth_down = ifelse(down == 4 & xyards < ydstogo, 1, 0),
         down = ifelse(down < 4 & xyards < ydstogo, down + 1, 1),
         posteam = ifelse(failed_fourth_down, posteam, defteam),
         ydstogo = ifelse(xyards < ydstogo & down < 4, ydstogo - floor(xyards), 10),
         yardline_100 = ifelse(yardline_100 - xyards > 0, yardline_100 - xyards, 0)) %>%
  select(game_id, playId, season, home_team, posteam, roof, half_seconds_remaining, yardline_100, down, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining, failed_fourth_down) %>% 
  nflfastR::calculate_expected_points() %>%
  mutate(xyac_ep=ifelse(failed_fourth_down,-1*ep, ep)) %>%
  mutate(xyac_ep = ifelse(yardline_100 == 0, 7, xyac_ep)) 

xyac_epa_final <- xyac_frame %>%
  mutate(uniquePlayId = paste(gameId, playId, sep = "_")) %>%
  select(uniquePlayId, playId, gameId, frameId, posteam, defteam, ballcarrierId=nflId, ballcarrierName = displayName, true_ep = ep, true_epa = epa, down_pre = down, ydstogo_pre = ydstogo, yard_pre = yardline_100, xyards, yards_gained, event) %>%
  cbind(xyac_for_epa %>% select(xyac_ep, yardline_100, down_post = down, ydstogo_post = ydstogo, yard_post = yardline_100)) %>%
  mutate(xyac_epa = xyac_ep - true_ep) %>% 
  select(uniquePlayId, playId, gameId, frameId, down_pre, ydstogo_pre, yard_pre, down_post, ydstogo_post, yard_post, xyards, true_yards = yards_gained, xyac_ep, true_ep, xyac_epa, true_epa, event, posteam, defteam, ballcarrierId, ballcarrierName)

random_play <- function(x) {
  r <- sample(unique(xyac_epa_final$uniquePlayId),1)
  r_gameId <- gsub("_.*","",r) %>% as.numeric()
  r_playId <- gsub(".*_","",r) %>% as.numeric()
  
  return(list(gameId = r_gameId, playId = r_playId))
}

write_parquet(xyac_epa_final, "data/xyac_epa.parquet")
