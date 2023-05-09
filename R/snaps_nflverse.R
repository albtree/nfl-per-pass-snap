library(stringr)
library(nflverse)
library(googledrive)
library(ggrepel)
library(tidyverse)


participation <- load_participation(2016:2022) %>%
  mutate(possession_team = case_when(possession_team == "OAK" ~ "LV",
                        TRUE ~ possession_team))

df2 <- participation %>%
  separate_rows(offense_players, sep = ";") ## Oakland missing offense player IDs - have logged github issue

rosters <- load_rosters(2016:2022) %>%
  select(position, full_name, gsis_id, team, season, years_exp) %>%
  filter(position == "WR" | position == "TE" | position == "RB")

rosters2 <- rosters %>%
  select(position, gsis_id, team, season) %>%
  distinct(gsis_id, team, season, .keep_all = TRUE)

pbp <- load_pbp(2016:2022)
## Breakdown is here, but why?
df3 <- pbp %>%
  left_join(df2, by = c('old_game_id' = 'old_game_id', 'play_id' = 'play_id'), na_matches = "never") %>%
  filter(play_type == "pass",sack == 0, week <= 18) %>%
  select(season, posteam, desc, air_yards, yards_after_catch, epa, receiver_player_id, receiver_player_name, offense_players) %>%
  left_join(rosters, by = c('offense_players' = 'gsis_id', 'season' = 'season'), na_matches = "never")

stats <- load_player_stats(2016:2022)%>%
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

write.csv(routes, "C://Users/talbi/Documents/R/routes_shiny/routes.csv", row.names = F)
drive_upload("routes.csv", name = "routes.csv", type = "spreadsheet", overwrite = TRUE)
1

routes <- read.csv("routes.csv")


#routes %>%
  filter(targets_total >= 90) %>%
  ggplot(aes(x = targets_per_pass_snap, y = yards_per_pass_snap)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.035, alpha = 0.7) +
  geom_label_repel(aes(label = full_name), size = 3, max.overlaps = 15) +
  labs(title = "Targets & Yards per Pass Snap - Mike Evans, yikes",
       subtitle = "Minimum 90 targets",
       x = "Targets per Pass Snap",
       y = "Receiving Yards per Pass Snap",
       caption = "Data = nflverse. Author = @TAlbTree")+
  theme_ipsum_rc()+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
ggsave("tprr_vs_yprr.png", bg = "#ffffff")

routes %>%
  filter(years_exp == 0, season > 2019, pass_snaps >= 150) %>%
  ggplot(aes(x = targets_per_pass_snap, y = yards_per_pass_snap)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.035, alpha = 0.7) +
  geom_label_repel(aes(label = full_name), size = 3, max.overlaps = 15) +
  labs(title = "Targets & Yards per Pass Snap - 2020 and 2021 Rookies",
       subtitle = "Minimum 150 pass snaps",
       x = "Targets per Pass Snap",
       y = "Receiving Yards per Pass Snap",
       caption = "Data = nflverse. Author = @TAlbTree")+
  theme_ipsum_rc()+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 8))
ggsave("tprr_vs_yprr_2021rookies.png", bg = "#ffffff")

unique(LV18$old_game_id)
unique(LV18_part$old_game_id)

unique(LV18_rosters$gsis_id)
unique(LV18_part$offense_players)

