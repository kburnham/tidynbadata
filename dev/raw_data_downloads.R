
# play_by_play ----
raw <- msf_get_results(version = '2.0', league = 'nba', season = "2018-2019-regular",
                         feed = 'game_playbyplay',
                         param = list(game = 47724))


# schedule ----
all_games <- msf_get_results(league = 'nba',
                             season = "2018-2019-regular",
                             feed = 'seasonal_games',
                             version = '2.0',
                             params = list(team = '83'))

all_games$api_json$games %>% filter(schedule.id %in%  c(47576, 47587, 47611, 47724)) %>% select(schedule.startTime)


# game player info ----
raw <- msf_get_results(league = 'nba',
                       version = '2.0',
                       feed = 'game_lineup',
                       season = "2018-2019-regular",
                       params = list(game = 47820))

