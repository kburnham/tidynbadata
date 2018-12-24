#' Load the raw game box score data and return a tibble of data for all players
#' for a given team in that game
#'
#' @param game_id msf game id
#' @param team the msf team id, name, city or abbr for the desired team's data
#'
#' @return raw box score data for all players on the requested team for the requested game

get_player_game_data <- function(game_id, team) {
  bs_raw <- get_raw_box_score(game_id)
  team_id <- interpret_team(team)$id

  if (bs_raw$api_json$game$homeTeam$id == team_id) {
    loc <- 'home'
  } else if (bs_raw$api_json$game$awayTeam$id == team_id) {
    loc <- 'away'
  } else stop('The provided team: ', team,
              ' was not found in the box score for the game_id: ',
              game_id)

  players <- bs_raw$api_json$stats[[loc]]$players

  attach_player_info <- function(pl, location) {
    pl$playerStats %>%
      bind_rows() %>%
      bind_cols(pl %>% select(-playerStats)) %>%
      mutate(game_id = bs_raw$api_json$game$id,
             team = bs_raw$api_json$game[[glue('{location}Team')]]$abbreviation,
             team_id = bs_raw$api_json$game[[glue('{location}Team')]]$id,
             location = location)


  }

  final <- players %>% attach_player_info(location = loc)

  return(final)

}

#' Load the raw team box score data and return a tibble of data for team data
#'
#' @param game_id msf game id
#' @param team the msf team id, name, city or abbr for the desired team's data
#'
#' @return raw box score data for the requested team for the requested game

get_team_game_data <- function(game_id) {
  bs_raw <- get_raw_box_score(game_id)
  home_team <- bs_raw$api_json$game$homeTeam$id
  away_team <- bs_raw$api_json$game$awayTeam$id

  home_data <- bs_raw$api_json$stats$home$teamStats
  away_data <- bs_raw$api_json$stats$away$teamStats
  ts <- list('home'=  bind_cols(interpret_team(home_team), tibble(game_id = game_id), home_data),
             'away' = bind_cols(interpret_team(away_team), tibble(game_id = game_id), away_data)
             )

  return(ts)


}


#' Transform raw box score data to a tidy format with new names
#'
#' @param box_score_data output from the function get_raw_box_score(game_id)
#'
#' @return a tibble with box score data for the game data provided
#'
summarize_raw_box_scores <- function(raw_box_score_data) {
  box_summary <- raw_box_score_data %>%
    group_by(player.id) %>%
    summarize(gm_played = n(),
              attempted_twos = sum(fieldGoals.fg2PtAtt),
              made_twos = sum(fieldGoals.fg2PtMade),
              accuracy_twos = round(made_twos / attempted_twos, 3),
              attempted_threes = sum(fieldGoals.fg3PtAtt),
              made_threes = sum(fieldGoals.fg3PtMade),
              accuracy_threes = round(made_threes / attempted_threes, 3),
              accuracy_fgs = round((made_twos + made_threes) / (attempted_twos + attempted_threes), 3),
              attempted_ones = sum(freeThrows.ftAtt),
              made_ones = sum(freeThrows.ftMade),
              accuracy_ones = round(attempted_ones / made_ones, 3),
              min_played = sum(miscellaneous.minSeconds / 60) %>% round(3),
              min_played_per_game = round(min_played / gm_played, 3),
              pts = sum(offense.pts),
              pts_per_game = round(pts / gm_played, 1),
              pts_per_min = round(pts / min_played, 3),
              pts_max = max(offense.pts),
              pts_min = min(offense.pts),
              plus_minus = sum(miscellaneous.plusMinus),
              plus_minus_per_game = round(plus_minus / gm_played, 1),
              plus_minus_per_min = round(plus_minus / min_played, 3),
              plus_minus_max = max(miscellaneous.plusMinus),
              plus_minus_min = min(miscellaneous.plusMinus),
              assists = sum(offense.ast),
              assists_per_game = round(assists / gm_played, 1),
              assists_per_min = round(assists / min_played, 3),
              assists_max = max(offense.ast),
              rebounds = sum(rebounds.reb),
              rebounds_per_game = round(rebounds /gm_played, 1),
              rebounds_per_min = round(rebounds / min_played, 3),
              rebounds_max = max(rebounds.reb),
              rebounds_def = sum(rebounds.defReb),
              rebounds_def_per_game = round(rebounds_def / gm_played, 1),
              rebounds_def_per_min = round(rebounds_def / min_played, 3),
              rebounds_def_max = max(rebounds.defReb),
              rebounds_off = sum(rebounds.offReb),
              rebounds_off_per_game = round(rebounds_off / gm_played, 1),
              rebounds_off_per_min = round(rebounds_off / min_played, 3),
              rebounds_off_max = max(rebounds.offReb),
              steals = sum(defense.stl),
              steals_per_game = round(steals / gm_played, 1),
              steals_per_min = round(steals / min_played, 3),
              steals_max = max(defense.stl),
              turnovers = sum(defense.tov),
              turnovers_per_game = round(turnovers / gm_played, 1),
              turnovers_per_min = round(turnovers / min_played, 3),
              turnovers_max = max(defense.tov),
              blocks = sum(defense.blk),
              blocks_per_game =round(blocks / gm_played, 1),
              blocks_per_min = round(blocks / gm_played, 3),
              blocks_max = max(defense.blk),
              blocked = sum(defense.blkAgainst),
              blocked_per_game = round(blocked / gm_played, 1),
              blocked_per_min = round(blocked / min_played, 3),
              blocked_max = max(defense.blkAgainst),
              ejections = sum(miscellaneous.ejections)
    )


  player_info <- raw_box_score_data %>% filter(team == get('team')) %>%
    select(starts_with('player')) %>%
    group_by(player.id) %>%
    summarize(first_name = last(player.firstName),
              last_name = last(player.lastName),
              position = last(player.position),
              jersey_number = last(player.jerseyNumber))


  final <- box_summary %>% left_join(player_info, by = 'player.id')
  return(final)

}



