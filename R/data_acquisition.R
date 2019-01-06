
#' Download tibble of the schedule (with results for completed games) for a given NBA team (use the 3 letter id)
#'
#' @param team the 3 letter id for an NBA team
#' @param season the season you want the schedule for
#' @param version the msf api version
#' @param ... additional paramaters passed to `msf_get_results()`
#' @export
#' @importFrom dplyr mutate select if_else
#' @family data_acquisition
#' @return a tibble

get_team_schedule <- function(team,
                              season = getOption('tidynbadata.current_season'),
                              version = getOption('tidynbadata.msf_version_id')
                              ) {

  team <- interpret_team(team)$abbr
  all_games <- mysportsfeedsR::msf_get_results(league = 'nba',
                               season = season,
                               feed = 'seasonal_games',
                               version = version,
                               params = list(team = tolower(team)))

  team_games <- all_games$api_json$games

  ## make a team specific data.frame from the returned data
    team_games <- team_games %>%
    mutate(msf_game_id = schedule.id,
           team = team,
           opponent = if_else(schedule.homeTeam.abbreviation == team,
                             schedule.awayTeam.abbreviation,
                             schedule.homeTeam.abbreviation),
           location = if_else(schedule.homeTeam.abbreviation == team,
                              'home',
                              'away'),
           start_time_UTC = schedule.startTime %>% lubridate::ymd_hms(),
           date = start_time_UTC %>% - hours(5)  %>% as.Date(),
           status = if_else(schedule.playedStatus == 'COMPLETED',
                            'complete',
                            NA_character_),
           result = case_when(is.na(status) ~ NA_character_,
                              location == 'home' &
                                score.homeScoreTotal > score.awayScoreTotal ~ 'win',
                              location == 'away' &
                                score.awayScoreTotal > score.homeScoreTotal ~ 'win',
                              TRUE ~ 'loss'),
           wins  = cumsum(result == 'win'),
           losses = cumsum(result == 'loss'),
           venue = schedule.venue.name
    ) %>%
    select(msf_game_id, team, opponent, date, start_time_UTC,
           location, status, result, wins, losses, venue)
  return(team_games)

}


#' Get a list of starters and bench players for a given game_id
#'
#' @param game_id the MSF game id for the desired game
#' @param team strng to indicate team. Is passed to interpret team so can be any of name, city, id or abbreviation
#' @export
#'
get_lineup <- function(game_id, team) {
  team_id <- interpret_team(team)$id
  check_archive_dir()
  lu_archive <- file.path(getOption('tidynbadata.archive_path'), 'line_up_archive')
  if (!dir.exists(lu_archive)) dir.create(lu_archive)
  lu_file_path <- file.path(lu_archive, glue('{game_id}_{team_id}.rds'))
  if (file.exists(lu_file_path)) {
    message(glue::glue('Archived line up for game: {game_id} team: {team_id}
                       found and is being returned. To force a new API call,
                       `set use_archive = FALSE`'))
    return(readRDS(lu_file_path))
  } else {
    message(glue::glue('No archive for game: {game_id} team: {team_id}
                       found, making a new API call . . .'))
  }


  raw <- mysportsfeedsR::msf_get_results(league = 'nba',
                           version = getOption('tidynbadata.msf_version_id'),
                           feed = 'game_lineup',
                           season = getOption('tidynbadata.current_season'),
                           params = list(game = game_id))


    lu <- raw$api_json$teamLineups$actual.lineupPositions[[which(raw$api_json$teamLineups$team.id == team_id)]] %>%
      dplyr::mutate(position = stringr::str_replace(position, '[0-9]', '')) %>% dplyr::arrange(dplyr::desc(position))
    saveRDS(lu, lu_file_path)
    return(lu)

}

#' Get a raw copy of a game's play-by-play data from the local archive when possible
#'
#' @param game_id the msf game id for the game
#'
#' @return a raw play-by-play object from mysportsfeeds, this is also archived

get_raw_pbp <- function(game_id) {
  check_archive_dir()
  pbp_archive <- file.path(getOption('tidynbadata.archive_path'), 'pbp_archive')
  if (!dir.exists(pbp_archive)) dir.create(pbp_archive)
  pbp_file_path <- file.path(pbp_archive, glue('{game_id}.rds'))
  if (file.exists(pbp_file_path)) {
    message(glue::glue('Archived play-by-play for game {game_id} found and is being returned.
                 To force a new API call delete the file at {pbp_file_path}'))
    return(readRDS(pbp_file_path))
  } else {
    message(glue('No archives play_by_play for game {game_id} found, making a new API call . . .'))
  }


  raw <- msf_get_results(league = 'nba',
                         version = getOption('tidynbadata.msf_version_id'),
                         feed = 'game_playbyplay',
                         season = getOption('tidynbadata.current_season'),
                         params = list(game = game_id))

  saveRDS(raw, pbp_file_path)
  return(raw)
}

#' downloads a data.frame of NBA player data from mysportsfeeds
#'
#' @param force_reload when TRUE will ignore the archive and download new data
#'
#' @return a data.frame of player data

get_player_data <- function(force_reload = FALSE) {
  check_archive_dir()
  player_data_archive <- file.path(getOption('tidynbadata.archive_path'), 'player_data_archive')
  if (!dir.exists(player_data_archive)) dir.create(player_data_archive)
  pd_file_path <- file.path(player_data_archive, 'player_data.rds')
  if (file.exists(pd_file_path) & !force_reload) {
    message(glue::glue('Archived player data found and is being returned.'))
    return(readRDS(pd_file_path))
  } else {
    message(glue::glue('No archived player data
                  found (or force_reload is TRUE),
                 making a new API call . . .'))

    player_data <- mysportsfeedsR::msf_get_results(version = getOption('tidynbadata.msf_version_id'),
                                   league = 'nba',
                                   feed = 'players',
                                   season = getOption('tidynbadata.current_season'))
  }

  saveRDS(player_data$api_json$players, pd_file_path)
  return(player_data$api_json$players)

}




