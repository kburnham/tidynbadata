
#' Download tibble of the schedule (with results for completed games) for a given NBA team (use the 3 letter id)
#'
#' @param team the 3 letter id for an NBA team, use 'ALL-TEAMS' for a data.frame with the schedule for all teams
#' @param season the season you want the schedule for
#' @param version the msf api version
#' @param ... additional paramaters passed to `msf_get_results()`
#' @export
#' @family data_acquisition
#' @return a tibble

get_team_schedule <- function(team,
                              season = "2018-2019-regular",
                              version = "2.0"
                              ) {

  team <- interpret_team(team)$abbr
  all_games <- msf_get_results(league = 'nba',
                               season = season,
                               feed = 'seasonal_games',
                               version = version,
                               params = list(team = tolower(team)))

  team_games <- all_games$api_json$games


  ## make a team specific data.frame from the returned data
    team_games <- team_games %>%
    mutate(msf_game_id = schedule.id,
           team = team,
           opponent = ifelse(schedule.homeTeam.abbreviation == team,
                             schedule.awayTeam.abbreviation,
                             schedule.homeTeam.abbreviation),
           location = ifelse(schedule.homeTeam.abbreviation == team, 'home', 'away'),
           date = as.Date(schedule.startTime),
           time = schedule.startTime %>% ymd_hms() %>% strftime(format = "%H:%M CT"),
           status = ifelse(schedule.playedStatus == 'COMPLETED', 'complete', NA),
           result = case_when(is.na(status) ~ NA_character_,
                              location == 'home' & score.homeScoreTotal > score.awayScoreTotal ~ 'win',
                              location == 'away' & score.awayScoreTotal > score.homeScoreTotal ~ 'win',
                              TRUE ~ 'loss'),
           wins  = cumsum(result == 'win'),
           losses = cumsum(result == 'loss'),
           venue = schedule.venue.name
    ) %>%
    select(msf_game_id, team, opponent, date, time, location, status, result, wins, losses, venue)
  return(team_games)

}











#' Get a list of starters and bench players for a given game_id
#'
#' @param game_id the MSF game id for the desired game
#' @param team strng to indicate team. Is passed to interpret team so can be any of name, city, id or abbreviation
#' @param use_archive when TRUE (the default) makes a local archive of the requested lineup and checks the local archive before downloading new data
#'
#'
get_lineup <- function(game_id, team, use_archive = TRUE) {
  team_id <- interpret_team(team)$id

  if (use_archive) {
    check_archive_dir()
    lu_archive <- file.path(tidynbadata$ARCHIVE_DIR, 'line_up_archive')
    if (!dir.exists(lu_archive)) dir.create(lu_archive)
    lu_file_path <- file.path(lu_archive, glue('{game_id}_{team_id}.rds'))
    if (file.exists(lu_file_path)) {
      message(glue('Archived line up for game: {game_id} team: {team_id} found and is being returned. To force a new API call, `set use_archive = FALSE`'))
      return(readRDS(lu_file_path))
    } else {
      message(glue('No archive for game: {game_id} team: {team_id} found, making a new API call . . .'))
    }
  }

  raw <- msf_get_results(league = 'nba',
                           version = tidynbadata$MSF_VERSION,
                           feed = 'game_lineup',
                           season = tidynbadata$CURRENT_SEASON,
                           params = list(game = game_id))


    lu <- raw$api_json$teamLineups$actual.lineupPositions[[which(raw$api_json$teamLineups$team.id == team_id)]] %>%
      mutate(position = str_replace(position, '[0-9]', '')) %>% arrange(desc(position))
    if (use_archive) saveRDS(lu, lu_file_path)
    return(lu)

}

#' Get a raw copy of a game's play-by-play data from the local archive when possible
#'
#' @param game_id the msf game id for the game
#' @param use_archive when TRUE (the default) will first check locally for a copy and if not make a msf api call
#'

get_raw_pbp <- function(game_id, use_archive = TRUE) {
  if (use_archive) {
    check_archive_dir()
    pbp_archive <- file.path(tidynbadata$ARCHIVE_DIR, 'pbp_archive')
    if (!dir.exists(pbp_archive)) dir.create(pbp_archive)
    pbp_file_path <- file.path(pbp_archive, glue('{game_id}.rds'))
    if (file.exists(pbp_file_path)) {
      message(glue('Archived play-by-play for game {game_id} found and is being returned. To force a new API call, `set use_archive = FALSE`'))
      return(readRDS(pbp_file_path))
    } else {
      message(glue('No archives play_by_play for game {game_id} found, making a new API call . . .'))
    }
  }


  raw <- msf_get_results(league = 'nba',
                         version = tidynbadata$MSF_VERSION,
                         feed = 'game_playbyplay',
                         season = tidynbadata$CURRENT_SEASON,
                         params = list(game = game_id))




  if (use_archive) saveRDS(raw, pbp_file_path)
  return(raw)


}


get_player_data <- function(force_reload = FALSE) {
  check_archive_dir()
  player_data_archive <- file.path(tidynbadata$ARCHIVE_DIR, 'player_data_archive')
  if (!dir.exists(player_data_archive)) dir.create(player_data_archive)
  pd_file_path <- file.path(player_data_archive, 'player_data.rds')
  if (file.exists(pd_file_path) & !force_reload) {
    message(glue('Archived player data found and is being returned.'))
    return(readRDS(pd_file_path))
  } else {
    message(glue('No archived player data
                  found (or force_reload is TRUE),
                 making a new API call . . .'))

    player_data <- msf_get_results(version = tidynbadata$MSF_VERSION,
                                   league = 'nba',
                                   feed = 'players',
                                   season = tidynbadata$CURRENT_SEASON)
  }

  saveRDS(player_data$api_json$players, pd_file_path)
  return(player_data$api_json$players)

}




