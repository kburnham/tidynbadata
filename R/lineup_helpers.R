
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


