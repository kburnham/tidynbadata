#' Check that the archive directory exists
#'
#'
#'
#'
#'
#' @return NULL
#'
check_archive_dir <- function() {
  archive_dir <- getOption('tidynbadata.archive_path')
  if (!dir.exists(archive_dir)) stop(glue::glue('The archive dir ({archive_dir}) could not be found. You must create it
                                                      with the command `dir.create(getOption("tidynbadata.archive_path"))` or set use_archive to FALSE.'))
  return(NULL)
}


#' Given a team input of unknown type (id, name, city, abbreviation) return all 4
#' @param team_input a team id, team name, team city or team abbreviation to be interpreted
#' @export
#' @return a data.frame giving the id, abbr, name and city of the input team
#' @importFrom dplyr filter
#'
interpret_team <- function(team_input) {
  if (team_input %in% tidynbadata_team_info$id) return(tidynbadata_team_info %>% filter(id == team_input))
  if (team_input %in% tidynbadata_team_info$abbr) return(tidynbadata_team_info %>% filter(abbr == team_input))
  if (team_input %in% tidynbadata_team_info$name) return(tidynbadata_team_info %>% filter(name == team_input))
  if (team_input %in% tidynbadata_team_info$city) return(tidynbadata_team_info %>% filter(city == team_input))
  stop(glue::glue('The provided value - {team_input} - could not be found in the team data as an id, abbreviation, name or city.'))
}
