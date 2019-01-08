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
      pbp_team <- proc_pbp[[glue('`{team_id}`')]]
      message(glue::glue('Returning pbp data for game {game_id} and team {team_id}'))
      return(pbp_team)
    }
  }
