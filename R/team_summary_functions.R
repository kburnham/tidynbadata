#' Compute the 8 factors for all games for a given team
#'
#' @param team a team name, city, abrreviation or id
#' @export
#'
#' @return a tibble with team_id, game_id, game_number and each of the 8 factors
#'

compute_team_eight_factors <- function(team) {
  team_id <- interpret_team(team)$id
  # get team schedule
  games <- get_team_schedule(team = team_id) %>%
    filter(status == 'complete') %>% pull(msf_game_id)

  # get team pbps
  pbps <- map(games, load_pbp, team = team_id)

  # compute 8 factors
  factors <- map_df(pbps, compute_eight_factors)

  # add team_id, game_id and game_number_cols
  factors <- factors %>%
    mutate(
      team_id = team_id,
      game_id = games,
      game_num = seq(nrow(.)))

  return(factors)
}
