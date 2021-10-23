

## update pbpbs
library(tidyverse)
library(mysportsfeedsR)
library(tidynbadata)
library(lubridate)
library(glue)


authenticate_v2_x(apikey = keyring::key_get('msf_api_key'))
sched <- get_team_schedule(team = 'Knicks')
kgs <- sched %>% filter(status == 'complete') %>% pull(msf_game_id)
all_pbps <- map(kgs, load_pbp, team = 83)



all_pbps[[1]]
pbp <- all_pbps[[1]]

pbp %>% mutate(pof_count = map_int(gs_this_pof_vec, length)) %>% count(pof_count)
pbp %>% mutate(pof_count = map_int(gs_opp_pof_vec, length)) %>% count(pof_count)

pt <- compute_game_playing_time(all_pbps[[2]], 83)

sum(pt$minutes_played)


pd <- readRDS('~/tidynbadata_archive/player_data_archive/player_data.rds')
knicks <- pd$api_json$players %>% filter(teamAsOfDate.id == 83)
knicks <- knicks %>% select(player.id, player.firstName, player.lastName, player.jerseyNumber, player.currentRosterStatus, player.primaryPosition) %>% filter(player.currentRosterStatus == 'ROSTER')
knicks

all_games <- mysportsfeedsR::msf_get_results(league = 'nba',
                                             season = getOption('tidynbadata.current_season'),
                                             feed = 'seasonal_games',
                                             version = getOption('tidynbadata.msf_version_id'),
                                             params = list())

games <- all_games$api_json$games


audit_pof_vec(all_pbps[[2]], 83, pt, games)

pbp <- pbp %>% mutate(pof_this_count = map_int(gs_this_pof_vec, length),
               pof_opp_count = map_int(gs_opp_pof_vec, length),
               bad_row = pof_this_count != 5,
               row_number = row_number())


pbp %>% summarize(min(row_number[bad_row]))

pbp %>% filter(bad_row) %>% pull(row_number)

pbp %>% filter(row_number > 395) %>% view
# step through process_msf_pbp to figure out how it works

debugonce(process_msf_pbp)

raw_66689 <- readRDS('/Users/kevin/tidynbadata_archive/pbp_archive/66689.rds')
pbp2 <- process_msf_pbp(raw_66689)

pbp <- pbp2$`83`

pbp2$`83` %>% mutate(pof_count = map_int(gs_this_pof_vec, length)) %>% count(pof_count)
pbp2$`83` %>% mutate(pof_count = map_int(gs_opp_pof_vec, length)) %>% count(pof_count)

pbp <- pbp %>% mutate(pof_this_count = map_int(gs_this_pof_vec, length),
                      pof_opp_count = map_int(gs_opp_pof_vec, length),
                      bad_row = pof_this_count != 5,
                      row_number = row_number())
pbp %>% summarize(min(row_number[bad_row]))

pbp %>% filter(bad_row) %>% pull(row_number)

pbp %>% select(gs_description, gs_quarter, gs_quarter_seconds_elapsed, ends_with('vec'), contains('sub')) %>% filter(gs_quarter >= 4) %>% view

# all_pbps[[2]] %>% select(gs_description, gs_quarter, gs_quarter_seconds_elapsed, ends_with('vec'),contains('sub')) %>%
#   mutate(elapse_time_min = floor(gs_quarter_seconds_elapsed / 60),
#          elapse_time_sec = gs_quarter_seconds_elapsed - (60 * elapse_time_min)) %>% view

pbp %>% select(gs_description, gs_quarter, gs_quarter_seconds_elapsed, ends_with('vec'),contains('sub')) %>%
  mutate(elapse_time_min = floor(gs_quarter_seconds_elapsed / 60),
         elapse_time_sec = gs_quarter_seconds_elapsed - (60 * elapse_time_min)) %>% view

#debugonce(get_lineup_last_names)
# debugonce(summarize_lineup_performance)
summarize_lineup_performance(pbp, 1, 4, pd$api_json$players)

compute_team_eight_factors(83)
tidynbadata::compute_team_stats(pbp)

