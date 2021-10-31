


## this script is used to debug a single pbp file


## update pbpbs
library(tidyverse)
#detach('package:mysportsfeedsR', unload = TRUE)
library(mysportsfeedsR)
library(tidynbadata)
library(lubridate)
library(glue)
library(tesseract)


authenticate_v2_x(apikey = keyring::key_get('msf_api_key'))


team <- interpret_team('Knicks')
sched <- get_team_schedule(team = team$name)
gs <- sched %>% filter(status == 'complete') %>% pull(msf_game_id)

gs
all_pbps <- map(gs, load_pbp, team = team$id)

games <- mysportsfeedsR::msf_get_results(league = 'nba',
                                         season = getOption('tidynbadata.current_season'),
                                         feed = 'seasonal_games',
                                         version = getOption('tidynbadata.msf_version_id'),
                                         params = list())[['api_json']][['games']]
audit_pof_vec(all_pbps[[length(gs)]], team = 83, pt = compute_game_playing_time(all_pbps[length(gs)], team = 83), games = games)




game_id <- 66751






pd <- get_player_data()
player_data <- pd$api_json$players %>% filter(teamAsOfDate.id == team$id)
pd <- player_data %>%
  select(player.id, player.firstName, player.lastName, player.jerseyNumber, player.currentRosterStatus, player.primaryPosition,
         player.currentTeam.id, player.currentTeam.abbreviation) %>%
  filter(player.currentRosterStatus == 'ROSTER')
# player_data



# load_raw ----
raw <- readRDS(glue('/Users/kevin/tidynbadata_archive/pbp_archive/{game_id}.rds'))
pbp_raw <- process_msf_pbp(raw)
pbp <- pbp_raw[[as.character(team$id)]]


pt <- compute_game_playing_time(pbp, team$name)

pt %>% arrange(last)

# if these two are not equal, problem
sum(pt$minutes_played)
(48 * 5) + ((max(pbp$gs_quarter) - 4) * (5 * 5)) # this compute the expected number of player minutes (accounting for possible OTs)


audit_pof_vec(pbp,team$id, pt, games)


pbp %>% mutate(pof_count = map_int(gs_this_pof_vec, length)) %>% count(pof_count)



debugonce(read_boxscore_screen_shot)
esp <- read_boxscore_screen_shot()


pt %>% left_join(esp, by = c('last' = 'last_name')) %>% filter(round(minutes_played, 0) != min) %>%
  select(last, first, min)


pbp %>% filter(gs_event_type == 'sub', sub_player_in == 31054 | sub_player_out == 31054)
pbp %>% filter(str_detect(gs_description, 'Grimes'))


# view_pbp ----
substitutions <- pbp %>% select(gs_description, gs_quarter, gs_quarter_seconds_elapsed, gs_this_pof_vec, contains('sub'), gs_event_type, gs_event_team) %>%
  mutate(lui = map_chr(gs_this_pof_vec, get_lineup_initials, pd = player_data))
view(substitutions)

# row_from_desc ----
sen <- "Derrick Rose enters the game for Miles McBride"
quarter <- 4
elp <- 0
get_player_ids_from_desc(sen, player_data)
row <- generate_substitution_row_from_desc(sen, player_data = player_data, quarter = quarter, elapsed_time_in_quarter = elp)
dput(row)



d <- summarize_lineup_performance(pbp, player_data = player_data, minimum_minutes = 0, use_player_initials = T)
d
sum(d$min)
