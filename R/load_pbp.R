#' Load an archived play-by-play file if it exists, otherwise,
#' process one and return it.
#'
#' @param game_id unique id for game
#' @param team any of team name, city, id or abbreviation. If NA returns a
#' list with a pbp for each team
#' @export
#' @return when team is NA a list with both team's pbp data is returned, when a
#' team_id is provided a data.frame of that team's pbp data is returned
#'
load_pbp <- function(game_id, team = NA) {
  # if the archive does not exist, load the raw and archive it
  check_archive_dir()
  pbp_archive <- file.path(getOption('tidynbadata.archive_path'), 'processed_pbp_data')
  if (!dir.exists(pbp_archive)) dir.create(pbp_archive)
  pbp_file_name <- glue::glue('{game_id}.rds')
  pbp_file_path <- file.path(pbp_archive, pbp_file_name)
  if (file.exists(pbp_file_path)) {
    message(glue::glue('Archived pbp data for game {game_id} found and loaded.'))
    proc_pbp <- readRDS(pbp_file_path)
  } else {
    message(glue::glue('No archived data for game {game_id} found. Making one . . .'))
    raw_pbp <- get_raw_pbp(game_id = game_id)
    proc_pbp <- process_msf_pbp(raw_pbp)
    saveRDS(proc_pbp, pbp_file_path)
  }
    if (is.na(team)) {
      message(glue::glue('No team provided so a list with both data sets is returned.'))
      return(proc_pbp)
    } else {
      team_id <- interpret_team(team)$id
      pbp_team <- proc_pbp[[as.character(team_id)]]
      message(glue::glue('Returning pbp data for game {game_id} and team {team_id}'))
      return(pbp_team)
    }
  }


#' This function checks the archive to see which pbps have been processed
#' and the schedule to see which games have been played and then downloads,
#' processes and archives the play-by-play data, returning nothing.
#' @return NULL
#'
#' @export
#'
update_pbp_summaries <- function() {
  # get the list of game_ids we have processed already
  check_archive_dir()
  proc_pbp_path <- file.path(getOption("tidynbadata.archive_path"), "processed_pbp_data")
  already_have <- list.files(proc_pbp_path) %>% str_replace(".rds", "")

  # get the list of completed games
  all_games <- mysportsfeedsR::msf_get_results(league = 'nba',
                                               season = getOption('tidynbadata.current_season'),
                                               feed = 'seasonal_games',
                                               version = '2.0')
  need_to_get <- get_all_completed_game_ids() %>%
    setdiff(already_have)

  walk(need_to_get, load_pbp)

  message('All pbps are now up to date.')

  return(NULL)

}


#' Load a vector of all completed game_ids
#'
#' @export
#' @return a vector of all completed game_ids

get_all_completed_game_ids <- function() {

  all_games <- mysportsfeedsR::msf_get_results(league = 'nba',
                                               season = getOption('tidynbadata.current_season'),
                                               feed = 'seasonal_games',
                                               version = '2.0')

  completed_games <- all_games$api_json$games %>%
    filter(schedule.playedStatus == "COMPLETED") %>%
    pull(schedule.id) %>%
    as.character()

  return(completed_games)



}
