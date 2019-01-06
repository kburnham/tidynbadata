library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)
library(tidynbadata)
authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')

sched <- get_team_schedule(team = 'Knicks')

kgs <- sched %>% filter(status == 'complete') %>% pull(msf_game_id)

# OT win vs Bucks
game_id <- 47899

pbp <- load_pbp(47915, 'Knicks')

all_pbps <- map(kgs, load_pbp, team = 'Knicks')



get_raw_pbp(game_id = game_id)$api_json$plays %>% select(contains('ump')) %>% names()


venues_raw <- msf_get_results(version = '2.0', league = 'nba',
                              season = getOption('tidynbadata.current_season'),
                              feed = 'seasonal_venues')

ven <- venues_raw$api_json$venues
