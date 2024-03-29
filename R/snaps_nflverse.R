library(tidyverse)
library(nflverse)

## Once NFL season has started, change 2022 to 2023
nflreadr::clear_cache()

participation <- load_participation(2023) %>%
  mutate(possession_team = case_when(possession_team == "OAK" ~ "LV",
                        TRUE ~ possession_team))

df2 <- participation %>%
  separate_rows(offense_players, sep = ";") ## Oakland missing offense player IDs - have logged github issue
## Once NFL season has started, change 2022 to 2023
rosters <- load_rosters(2023) %>%
  select(position, full_name, gsis_id, team, season, years_exp) %>%
  filter(position == "WR" | position == "TE" | position == "RB")

rosters2 <- rosters %>%
  select(position, gsis_id, team, season) %>%
  distinct(gsis_id, team, season, .keep_all = TRUE)
## Once NFL season has started, change 2022 to 2023
pbp <- load_pbp(2023)

df3 <- pbp %>%
  left_join(df2, by = c('old_game_id' = 'old_game_id', 'play_id' = 'play_id'), na_matches = "never") %>%
  filter(play_type == "pass",sack == 0, week <= 18) %>%
  select(season, posteam, desc, air_yards, yards_after_catch, epa, receiver_player_id, receiver_player_name, offense_players) %>%
  left_join(rosters, by = c('offense_players' = 'gsis_id', 'season' = 'season'), na_matches = "never")
## Once NFL season has started, change 2022 to 2023
stats <- load_player_stats(2023)%>%
  filter(week <= 18) %>%
  group_by(player_id, season, recent_team) %>%
  summarise(rec_yards_total = sum(receiving_yards),
            targets_total = sum(targets),
            receiving_epa_total = sum(receiving_epa, na.rm = TRUE)) %>%
  ungroup()

routes <- df3 %>%
  group_by(season, posteam, full_name, offense_players, years_exp) %>%
  summarise(pass_snaps = n()) %>%
  drop_na(full_name) %>%
  ungroup() %>%
  filter(pass_snaps >5) %>%
  left_join(stats, by = c('offense_players' = 'player_id', 'season' = 'season', 'posteam' = 'recent_team'), na_matches = "never") %>%
  mutate(yards_per_pass_snap = rec_yards_total/pass_snaps,
         targets_per_pass_snap = targets_total/pass_snaps,
         rec_epa_per_pass_snap = receiving_epa_total/pass_snaps) %>%
  mutate(years_exp = years_exp+1) %>%
  mutate_if(is.numeric, round, 2)%>%
  mutate(full_name = case_when(offense_players == "00-0035640" ~ "DK Metcalf",
                               offense_players == "00-0036196" ~ "Gabriel Davis",
                               TRUE ~ full_name)) %>%
  mutate(name_season = paste(full_name, season)) %>%
  left_join(rosters2, by = c('offense_players' = 'gsis_id', 'season' = 'season'), na_matches = "never") 

## Once 2023 NFL season has started, change this to 2016_2022
routes_2016_2022 <- read.csv("read_data/routes_2016_2022.csv")
## change routes_2016_2021 to 2016_2022
routes_all <- bind_rows(routes, routes_2016_2022)

write.csv(routes_all, file = "rec_per_snap_data.csv", row.names = F)
