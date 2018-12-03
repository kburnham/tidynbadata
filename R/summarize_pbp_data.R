
#' Estimate the number of possessions for a given chunk of play-by-play data using the basic method
#' see https://www.nbastuffer.com/analytics101/possession/

#' @param fga number of field goal attempts in the period
#' @param to turnovers
#' @param fta free throw attempts
#' @param or offensive rebounds
#' @return the estimated number of possessions for the period
#'
estimate_possessions_basic <- function(fga, to, fta, or) {
  possessions <- as.integer(round(.96 * ((fga + to + (.475 * fta) - or)), 0))
  return(possessions)
}


#' estimate the number of possessions for a given chunk of play-by-play data using the advanced method
#' see https://www.nbastuffer.com/analytics101/possession/

#' @param fga number of field goal attempts in the period
#' @param fta free throw attempts
#' @param or offensive rebounds
#' @param fg field goals
#' @param to turnovers
#' @param dr defensize rebounds
#' @param opp_fga opponent field goal attempts
#' @param opp_fta opponent free throw attempts
#' @param opp_or opponent offensize rebounds
#' @param opp_fg opponent field goals
#' @param opp_to opponent turnovers
#' @param opp_dr opponent defensive rebounds
#'
#'
#'
#' @return the estimated number of possessions for the period
#'


estimate_possessions_advanced <- function(fga, fta, or, fg, to, dr,
                                        opp_fga, opp_fta, opp_or,
                                        opp_fg, opp_to, opp_dr) {
  off_reb_rate <- or / (or + opp_dr)
  def_reb_rate <- opp_or / (opp_or + dr)
  missed_fg <- fga - fg
  opp_missed_fg <- opp_fga - opp_fg
  this_team <- fga + (.4 * fta) - (1.07 * (off_reb_rate * missed_fg)) + to
  opponent_team <- opp_fga + (.4 * opp_fta) - (1.07 * (def_reb_rate * opp_missed_fg)) + opp_to

  possessions <- .5 * (this_team + opponent_team)

  return(possessions)

}


estimate_possessions_basic <- function(dat) {

  dat %>% summarize()
  possessions <- as.integer(round(.96 * ((fga + to + (.475 * fta) - or)), 0))
  return(possessions)
}

estimate_team_possessions_custom <- function(dat) {

  opp_fga <- sum(dat$event_type_detail == 'fga_opp')
  opp_oreb <- sum(dat$event_type_detail == 'oreb_opp')
  opp_to <- sum(dat$event_type_detail == 'to_opp')
  opp_final_ft_made = sum(dat$event_type_detail == 'fta_opp' &
                            dat$freeThrowAttempt.result == 'SCORED' &
                            dat$freeThrowAttempt.attemptNum == dat$freeThrowAttempt.totalAttempts
                            )

  possessions <- (opp_fga - opp_oreb) + opp_to + opp_final_ft_made
  return(possessions)
}


#' Compute effective field goal percentage
#'
#' @param dat a tibble with play-by-play data
#' @return a dbl of a team's effective field goal percentage for the period in question

compute_effective_field_goal_percentage <- function(dat, team = 'this') {
  #https://www.nbastuffer.com/analytics101/four-factors/
  if (!team %in% c('this', 'opp')) stop('team should be "this" or "opp" not ', team)
  fgm <- sum(dat$event_type_detail == glue('fga_{team}') & dat$fieldGoalAttempt.result == 'SCORED')
  fgm3 <- sum(dat$event_type_detail == glue('fga_{team}') & dat$fieldGoalAttempt.points == 3 & dat$fieldGoalAttempt.result == 'SCORED')
  fga <- sum(dat$event_type_detail == glue('fga_{team}'))

  eff_fg_per <- (fgm + (.5 * fgm3)) / fga
  return(eff_fg_per)

}


#' Compute the turnover rate for a given period of pbp time
#'
#' @param dat pbp data
#' @return a dbl of team's turnover rate

compute_turnover_rate <- function(dat, team = 'this') {
  #https://www.nbastuffer.com/analytics101/four-factors/
  if (!team %in% c('this', 'opp')) stop('team should be "this" or "opp" not ', team)
  to <- sum(dat$event_type_detail == glue('to_{team}'))
  fga <- sum(dat$event_type_detail == glue('fga_{team}'))
  fta <- sum(dat$event_type_detail == glue('fta_{team}'))

  turn_rate <- to / (fga + (.44 * fta) + to)
  return(turn_rate)
}


compute_offensive_rebound_rate <- function(dat, team = 'this') {
  #https://www.nbastuffer.com/analytics101/four-factors/
  if (!team %in% c('this', 'opp')) stop('team should be "this" or "opp" not ', team)
  or <- sum(dat$event_type_detail == glue('oreb_{team}') & !is.na(dat$rebound.retrievingPlayer.id))
  opp_dr <- sum(dat$event_type_detail == glue('dreb_{if_else(team == "this", "opp", "this")}') & !is.na(dat$rebound.retrievingPlayer.id))
  reb_rate <- or / (or + opp_dr)
  return(reb_rate)
}


compute_free_throw_rate <- function(dat, use_attempted = TRUE, team = 'this') {
  if (!team %in% c('this', 'opp')) stop('team should be "this" or "opp" not ', team)
  ft <- if_else(use_attempted, sum(dat$event_type_detail == glue('fta_{team}')), sum(dat$event_type_detail == glue('fta_{team}') & dat$freeThrowAttempt.result == 'SCORED'))
  fga <- sum(dat$event_type_detail == glue('fga_{team}'))
  ftr <- ft / fga
  return(ftr)

}
