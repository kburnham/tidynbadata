
#' a function for searching lineups that do or do not include certain players
#'
#' This function returns TRUE for any lineup that includes all of includes and none of excludes.
#'
#' @param lineup a length five vector indicating players on the floor
#' @param includes a vector of ids to check for inclusion in lineup
#' @param excludes a vector of ids to check for exclusion fromt he lineup
#' @export
#' @return a boolean indicating if the lineup includes all of includes and excludes all of excludes

filter_lineup <- function(lineup, includes, excludes) {
  #it has to have all of includes
  if (!all(includes %in% lineup)) return(FALSE)

  #and it can't have any excludes
  if (any(excludes %in% lineup)) return(FALSE)

  # otherwise, it's ok
  return(TRUE)
}


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
#' @export
#' @family lineup_helpers
#' @return a concateneated string of initial-number combinations

get_lineup_initials <- function(lineup, pd) {
  if (length(lineup) != 5) return(NA)
  get_inits <- function(player_id, pd) {
    inits <- pd %>% filter(player.id == player_id) %>%
      mutate(inits = glue::glue('{str_sub(player.firstName, 1,1)}{str_sub(player.lastName, 1, 1)}{player.jerseyNumber}')) %>%
      dplyr::pull(inits)
    if (length(inits) == 0) {
      message(glue("{player_id} not found in player data"))
      inits <- as.character(player_id)
    }
    return(inits)
  }
  inits <- purrr::map_chr(lineup, get_inits, pd = pd)
  return(paste(inits, collapse = '-'))

}


#' Convert a lineup to a string of player last name + player jersey number
#'
#' @param lineup a lineup vector
#' @param pd player data
#'
#' @export
#' @family lineup_helpers
#' @return a concateneated string of last name-number combinations

get_lineup_last_names <- function(lineup, pd) {
  if (length(lineup) != 5) return(NA)
  get_lastname <- function(player_id, pd) {
    lastname <- pd %>% filter(player.id == player_id) %>%
      mutate(lastname = glue::glue('{player.lastName}{player.jerseyNumber}')) %>%
      dplyr::pull(lastname)
    if (length(lastname) == 0) {
      message(glue("{player_id} not found in player data"))
      inits <- as.character(player_id)
    }
    return(lastname)
  }
  lastname <- purrr::map_chr(lineup, get_lastname, pd = pd)
  return(paste(lastname, collapse = '-'))

}





#' subset a pbp datset according to players that must be, or must not be, in the current lineup
#'
#' @param pbp a processed play-by-play dataset
#' @param includes a vector of player ids that must be in  \code{gs_this_pof_vec}
#' @param excludes a vector of player ids that myst not be in \code{gs_this_pof_vec}
#' @export
#' @family lineup_helpers
#' @return a pbp dataset subset according to includes and excludes
#'

subset_pbp_by_player_ids <- function(pbp, includes, excludes) {
  return(pbp %>% dplyr::filter(filter_lineup(gs_this_pof_vec, includes, excludes)))
}


#' tag a pbp datset according to players that must be, or must not be, in the current lineup
#'
#' @param pbp a processed play-by-play dataset
#' @param includes a vector of player ids that must be in  \code{gs_this_pof_vec}
#' @param excludes a vector of player ids that must not be in \code{gs_this_pof_vec}
#' @export
#' @family lineup_helpers
#' @return the same pbp dataset with a new Boolean column indicating which rows match the includes and excludes criteria
#'

tag_pbp_by_player_ids <- function(pbp, includes, excludes) {
  return(pbp %>% dplyr::mutate(include_lineup = filter_lineup(gs_this_pof_vec, includes, excludes)))
}






