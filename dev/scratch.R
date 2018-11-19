


library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)

authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')


walk(list.files('R', pattern = '.R$', full.names = TRUE), function(x) {
  message(glue('loading {basename(x)} . . .'))
  source(x)
})


sched <- get_team_schedule()
sched %>% filter(status == 'complete') %>% pull(msf_game_id)



pbp <-  process_raw_pbp(47576)

pbp <- load_pbp_data(47576, 'Knicks')

sched %>% filter(msf_game_id == 47576)



pbp <- pbp %>% ungroup()
pbp$fieldGoalAttempt.shotType


#pbp$home_lineup_id <- map_chr(pbp$home_team_pof, make_lineup_id)
pbp <- pbp %>% mutate(home_lineup_id = map_chr(home_team_pof, make_lineup_id))


poss <- pbp %>% filter(!is.na(home_lineup_id)) %>% group_by(home_lineup_id) %>%
  do(possesions = compute_possesions(.))


poss %>% names()

tibble(x = poss$home_lineup_id, y = flatten_int(poss$possesions)) %>% summarize(sum(y))


pbp %>% filter(home_lineup_id == '9087-10102-13735-15206-15282') %>% View



## problem
# the pbp data indicates the home_players on the floor and the away players on the floor
# however, when we combine data from two games for a single team, say the Knicks, there is no gaurantee
# that the Knicks will be the home team for both games

# one possibility is to make two archives of every play-by-play
# the names of the columns then become team_players on the floor and opponent players on the floor
#


