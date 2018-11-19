#' Check that the archive directory exists
#' @keywords internal
#'
check_archive_dir <- function() {
  if (!dir.exists(tidynbadata$ARCHIVE_DIR)) stop(glue('The archive dir ({tidynbadata$ARCHIVE_DIR}) could not be found. You must create it
                                                      with the command `dir.create(tidynbadata$ARCHIVE_DIR)` or set use_archive to FALSE.'))
  return(NULL)
}


#' Given a team input of unknown type (id, name, abbreviation) return all 3
interpret_team <- function(team_input) {
  if (team_input %in% tidynbadata$TEAM_DATA$id) return(tidynbadata$TEAM_DATA %>% filter(id == team_input))
  if (team_input %in% tidynbadata$TEAM_DATA$abbr) return(tidynbadata$TEAM_DATA %>% filter(abbr == team_input))
  if (team_input %in% tidynbadata$TEAM_DATA$name) return(tidynbadata$TEAM_DATA %>% filter(name == team_input))
  if (team_input %in% tidynbadata$TEAM_DATA$city) return(tidynbadata$TEAM_DATA %>% filter(city == team_input))
  stop(glue('The provided value - {team_input} - could not be found in the team data as an id, abbreviation, name or city.'))
}
