

## make sample pbp data
## use Knicks/Bucks game data

# I need to make sure that this stays up-to-date with the changes to the pbp processing

library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)
library(tidynbadata)
authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')
sched <- get_team_schedule(team = 'Knicks')

get_lineup(53045, team = 'NYK')

game_id <- sched %>% filter(result == 'win' & opponent == 'MIL') %>% pull(msf_game_id)

pbp <- load_pbp(game_id, team = 'Knicks')
names(pbp)

tidynbadata_pbp <- pbp

devtools::use_data(tidynbadata_pbp, overwrite = TRUE)
