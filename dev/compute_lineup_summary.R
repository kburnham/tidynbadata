
library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)

authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')


walk(list.files('R', pattern = '.R$', full.names = TRUE), function(x) {
  message(glue('loading {basename(x)} . . .'))
  source(x)
})


#lu <- get_lineup(47820, 83, use_archive = FALSE)


#rpbp <- get_raw_pbp(47820, use_archive = F)



sched <- get_team_schedule()
knick_games <- sched %>% filter(status == 'complete') %>% pull(msf_game_id)
kg <- knick_games %>% map(load_pbp_data, team = 'Knicks')

# debugonce(load_pbp_data)
# g47724 <- load_pbp_data(47824, 83)

#kg[[1]] %>% count(event_type)


player_data <- msf_get_results(version = '2.0',
                               league = 'nba',
                               feed = 'players',
                               season = tidynbadata$CURRENT_SEASON)
pd <- player_data$api_json$players
#
pd <- pd %>% mutate(player_inits = glue('{str_sub(player.firstName, 1,1)}{str_sub(player.lastName, 1, 1)}{player.jerseyNumber}'))

  all <- do.call('bind_rows', kg)
#all$lineup_inits <- map_chr(unlist(all$this_team_pof_vec), get_lineup_initials, pd)







# all %>% group_by(this_team_pof_id) %>%
#   summarize(possesions = compute_possesion_basic(fga = sum(event_type == 'fga' & event_team == 'this_team'),
#                                                  to = sum(event_type == 'to' & event_team == 'this_team'),
#                                                  fta = sum(event_type == 'fta' & event_team == 'this_team'),
#                                                  or = sum(event_type == 'oreb' & event_team == 'this_team')))
#
#
# all$event_type
# all %>% count(event_type)





all %>% group_by(game_id) %>%
  summarize(possessions = estimate_team_possessions_custom(.data)) %>%
  pull(possessions) %>% mean

kg[[1]] %>% group_by(game_id) %>%
  summarize(possessions = estimate_team_possessions_custom(.data))

all %>% summarize(poss = estimate_team_possessions_custom(.data))

ppp <- all %>% group_by(game_id) %>%
  do(n = nrow(.),
     possessions = estimate_team_possessions_custom(.)) %>%
  unnest() %>% head()

names(ppp)
ppp$possessions




all %>% #group_by(game_id) %>%
  summarize(n = n(),
            knicks_score  = sum(score_points_this_team),
            opp_score = sum(score_points_opponent_team),
            possessions = estimate_team_possessions_custom(.data),
            games = length(unique(game_id)),
            eff_fgp = compute_effective_field_goal_percentage(.data),
            to_rate = compute_turnover_rate(.data),
            off_reb_rate = compute_offensive_rebound_rate(.data),
            free_throw_rate = compute_free_throw_rate(.data, use_attempted = FALSE))


all %>% group_by(game_id) %>%
  summarize(eff_fgp = compute_effective_field_goal_percentage(.data),
                  opp_eff_fgp = compute_effective_field_goal_percentage(.data, team = 'opp'),
                  to_rate = compute_turnover_rate(.data),
                  opp_to_rate = compute_turnover_rate(.data, team = 'opp'),
                  off_reb_rate = compute_offensive_rebound_rate(.data),
                  def_reb_rate_allowed = 1 - compute_offensive_rebound_rate(.data, team = 'opp'),
                  ft_rate = compute_free_throw_rate(.data, use_attempted = F),
                  opp_ft_rate = compute_free_throw_rate(.data, use_attempted = F, team = 'opp'))
