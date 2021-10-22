
#' Download tibble of the schedule (with results for completed games) for a given NBA team (use the 3 letter id)
#'
#' @param team the 3 letter id for an NBA team
#' @param season the season you want the schedule for
#' @param version the msf api version
#' @param ... additional paramaters passed to `msf_get_results()`
#' @export
#' @importFrom dplyr mutate select if_else
#' @family data_acquisition
#' @return a tibble showing game data for the given team

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
#' @param season string name of desired season. Defaults to \code{getOption('tidynbadata.current_season')}
#' @export
#' @family data_acquisition
#' @return tibble of lineup data for given game and team
#'
get_lineup <- function(game_id, team, season = getOption('tidynbadata.current_season')) {
  team_id <- interpret_team(team)$id
  check_archive_dir()
  lu_archive <- file.path(getOption('tidynbadata.archive_path'), 'line_up_archive')
  if (!dir.exists(lu_archive)) dir.create(lu_archive)
  lu_file_path <- file.path(lu_archive, glue('{game_id}_{team_id}.rds'))
  if (file.exists(lu_file_path)) {
    message(glue::glue('Archived line up for game: {game_id} team: {team_id}
                       found and is being returned. To force a new API call,
                       `set use_archive = FALSE`'))
   lu <- readRDS(lu_file_path)
  } else {
    message(glue::glue('No archive for game: {game_id} team: {team_id}
                       found, making a new API call . . .'))
    raw <- mysportsfeedsR::msf_get_results(league = 'nba',
                                           version = getOption('tidynbadata.msf_version_id'),
                                           feed = 'game_lineup',
                                           season = season,
                                           params = list(game = game_id))


    lu <- raw$api_json$teamLineups$actual.lineupPositions[[which(raw$api_json$teamLineups$team.id == team_id)]] %>%
      dplyr::mutate(position = stringr::str_replace(position, '[0-9]', '')) %>% dplyr::arrange(dplyr::desc(position))
    saveRDS(lu, lu_file_path)
  }

  lu <- fix_lineup(lu, game_id, team_id)



  return(lu)

}

#' Get a raw copy of a game's play-by-play data from the local archive when possible
#'
#' @param game_id the msf game id for the game
#' @export
#' @family data_acquisition
#' @return a raw play-by-play object from mysportsfeeds, which is also archived

get_raw_pbp <- function(game_id) {
  check_archive_dir()
  pbp_archive <- file.path(getOption('tidynbadata.archive_path'), 'pbp_archive')
  if (!dir.exists(pbp_archive)) dir.create(pbp_archive)
  pbp_file_path <- file.path(pbp_archive, glue::glue('{game_id}.rds'))
  if (file.exists(pbp_file_path)) {
    message(glue::glue('Archived play-by-play for game {game_id} found and is being returned.
                 To force a new API call delete the file at {pbp_file_path}'))
    return(readRDS(pbp_file_path))
  } else {
    message(glue::glue('No archived play_by_play for game {game_id} found, making a new API call . . .'))
  }


  raw <- msf_get_results(league = 'nba',
                         version = getOption('tidynbadata.msf_version_id'),
                         feed = 'game_playbyplay',
                         season = getOption('tidynbadata.current_season'),
                         params = list(game = game_id))

  if (raw$response$status_code == 204) stop('the following game_id was not found: ', game_id)
  saveRDS(raw, pbp_file_path)
  return(raw)
}

#' downloads a data.frame of NBA player data from mysportsfeeds
#'
#' @param season season for desired player data, defaults to \code{getOption('tidynbadata.current_season')}
#' @export
#' @family data_acquisition
#' @return a data.frame of player data

get_player_data <- function(season = getOption('tidynbadata.current_season')) {
  check_archive_dir()
  player_data_archive <- file.path(getOption('tidynbadata.archive_path'), 'player_data_archive')
  if (!dir.exists(player_data_archive)) dir.create(player_data_archive)
  pd_file_path <- file.path(player_data_archive, 'player_data.rds')
  if (file.exists(pd_file_path)) {
    message(glue::glue('Archived player data found and is being returned. To force a new download delete the
                       fle at {pd_file_path}'))
    return(readRDS(pd_file_path))
  } else {
    message(glue::glue('No archived player data
                  found (or force_reload is TRUE),
                 making a new API call . . .'))

    player_data <- mysportsfeedsR::msf_get_results(version = getOption('tidynbadata.msf_version_id'),
                                   league = 'nba',
                                   feed = 'players',
                                   season = season)
    saveRDS(player_data, pd_file_path)
  }


  return(player_data)

}


#' Retrieve a raw box score pull from mysportsfeeds based on a game_id
#'
#' @param game_id id of requested game
#' @export
#' @family data_acquisition
#' @return a raw boxscore object from mysportsfeeds, which is also archived

get_raw_msf_box_score <- function(game_id) {
  check_archive_dir()
  msf_boxscore_archive <- file.path(getOption('tidynbadata.archive_path'), 'msf_boxscore_archive')
  if (!dir.exists(msf_boxscore_archive)) dir.create(msf_boxscore_archive)
  msf_bs_file_path <- file.path(msf_boxscore_archive, glue::glue('{game_id}.rds'))
  if (file.exists(msf_bs_file_path)) {
    message(glue::glue('Archived msf boxscore for game {game_id} found and is being returned.
                 To force a new API call delete the file at {msf_bs_file_path}'))
    return(readRDS(msf_bs_file_path))
  } else {
    message(glue::glue('No archived msf boxscore for game {game_id} found, making a new API call . . .'))
  }

  raw <- msf_get_results(league = 'nba',
                         version = getOption('tidynbadata.msf_version_id'),
                         feed = 'game_boxscore',
                         season = getOption('tidynbadata.current_season'),
                         params = list(game = game_id))

  saveRDS(raw, msf_bs_file_path)
  return(raw)

}

