
#' a function for searching lineups that do or do not include certain players

filter_lineup <- function(lineup, inlcudes, excludes)
  if (all(includes %in% lineup) & !any(excludes %in% lineup)) return(TRUE) else return(FALSE)

#' convert a vector of player ids into a single dash separated string, sorted
#' low to high
#' @param lineup a vector of player ids
#' @return a single string of each provided id separated by dashed or NA if
#' length of provided vector is not 5
#'
make_lineup_id <- function(lineup) {
  if (length(lineup) != 5) return(NA)
  return(lineup %>% sort() %>% paste(collapse = '-'))
}

#' break a dash separated id string into its component parts and return as a vector
#'
#' @param lineup_id a string of dash separated player ids
#'
#' @return a vector of idividual ids

break_lineup_id <- function(lineup_id)
  return(lineup_id %>% stringr::str_split(pattern = '-') %>% unlist() %>% as.numeric() %>% sort())

#' Convert a lineup to a string of player initials + player jersey number
#'
#' @param lineup a lineup vector
#' @param pd player data
#'
#' @return a concateneated string of initial-number combinations

get_lineup_initials <- function(lineup, pd) {
  if (length(lineup) != 5) return(NA)
  get_inits <- function(player_id, pd) {
    inits <- pd %>% filter(player.id == player_id) %>%
      mutate(inits = glue::glue('{str_sub(player.firstName, 1,1)}{str_sub(player.lastName, 1, 1)}{player.jerseyNumber}')) %>%
      dplyr::pull(inits)
  }
  inits <- purrr::map_chr(lineup, get_inits, pd = pd)
  return(paste(inits, collapse = '-'))

}




