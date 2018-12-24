library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)
library(tidynbadata)
authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')

sched <- get_team_schedule(team = 'Knicks')

# OT win vs Bucks
game_id <- 47899

pbp <- load_pbp(game_id, 'Knicks')

nrow(pbp)
