
#' a function for searching lineups that do or do not include certain players

filter_lineup <- function(lineup, inlcudes, excludes)
  if (all(includes %in% lineup) & !any(excludes %in% lineup)) return(TRUE) else return(FALSE)


make_lineup_id <- function(lineup) {
  if (length(lineup) != 5) return(NA)
  return(lineup %>% sort() %>% paste(collapse = '-'))
}


break_lineup_id <- function(lineup_id)
  return(lineup_id %>% str_split(sep = '-')) %>% sort()
#
# convert_lineup_id <- function(to = )


get_lineup_initials <- function(lineup, pd) {
  if (length(lineup) != 5) return(NA)
  get_inits <- function(player_id, pd) {
    inits <- pd %>% filter(player.id == player_id) %>%
      mutate(inits = glue('{str_sub(player.firstName, 1,1)}{str_sub(player.lastName, 1, 1)}{player.jerseyNumber}')) %>%
      pull(inits)
  }
  inits <- map_chr(lineup, get_inits, pd = pd)
  return(glue_collapse(inits, sep = '-'))

}

get_lineup_initials <- function(lineup, pd) {
  get_inits <- function(player_id, pd) {
    return(pd %>% filter(player.id == player_id) %>% pull(player_inits))
  }
  inits <- map_chr(lineup, get_inits, pd = pd)
  return(inits)
}

#g1$this_team_pof_vec[1] %>% unlist() %>% get_lineup_initials()


