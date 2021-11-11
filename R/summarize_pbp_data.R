#' Estimate the number of possessions for a team for a given chink of pbp data
#'
#' This function computes possesions as the sum of:
#' 1. opponent field goals - opponent offensive rebounds
#' 2. opponent turnovers
#' 3. opponent final free throws made (e.g the 2nd of 2 or 3rd of 3)
#'
#'
#' @param dat a chunk of pbp data to be analyzed
#' @export
#' @family summarize_pbp_data
#' @return estimated number of possesions in the pbp chunk


estimate_team_possessions_custom <- function(dat) {

  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
  opp_oreb <- sum(dat$gs_event_type_detail == 'oreb_opp')
  opp_to <- sum(dat$gs_event_type_detail == 'to_opp')
  opp_final_ft_made = sum(dat$gs_event_type_detail == 'fta_opp' &
                            dat$fta_result == 'make' &
                            dat$fta_num == dat$fta_total_attempts
                            )

  possessions <- (opp_fga - opp_oreb) + opp_to + opp_final_ft_made
  return(possessions)
}

#' A basic function for estimating the number of possesions for a team
#' in a given chunk of play-by-play data
#'
#' @param dat a chunk of pbp data
#' @export
#' @family summarize_pbp_data
#' @return a double estimating the number of plays in the provided pbp data
#'

estimate_team_possessions_basic <- function(dat) {
  fga <- sum(dat$gs_event_type_detail == 'fga_this')
  to <- sum(dat$gs_event_type_detail == 'to_this')
  fta <- sum(dat$gs_event_type_detail == 'fta_this')
  oreb <- sum(dat$gs_event_type_detail == 'oreb_this')

  possessions <- .96 * ((fga + to) + .44 * (fta - oreb))
}




#' Compute effective field goal percentage
#'
#' This function returns both the offensive and defensive effective field goal
#' percentage for the reference team in a given chunk of pbp data
#'
#' @param dat a tibble with play-by-play data
#' @export
#' @family summarize_pbp_data
#' @return the effective field goal percentage for a given chunk of pbp data for the reference team
#'

compute_effective_fgp <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

    fga <- sum(dat$gs_event_type_detail == 'fga_this')
    fg <- sum(dat$gs_this_points_row > 1)
    fg3 <- sum(dat$gs_this_points_row == 3)
    eff_fg_per <- (fg + (.5 * fg3)) / fga
    return(eff_fg_per)
}

#' Compute defensive effective field goal percentage
#'
#' This function returns both the offensive and defensive effective field goal
#' percentage for the reference team in a given chunk of pbp data
#'
#' @param dat a tibble with play-by-play data
#' @export
#' @family summarize_pbp_data
#' @return the defensive effective field goal percentage for a given chunk of pbp data for the reference team
#'

compute_effective_defensive_fgp <- function(dat) {
  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
  opp_fg <- sum(dat$gs_opp_points_row > 1)
  opp_fg3 <- sum(dat$gs_opp_points_row == 3)
  opp_eff_fg_per <- (opp_fg + (.5 * opp_fg3)) / opp_fga
  return(opp_eff_fg_per)
}


#' Compute the turnover rate for a given period of pbp time
#'
#' @param dat pbp data
#' @export
#' @family summarize_pbp_data
#' @return turnover rate for the given chunk of pbp data

compute_turnover_rate <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

  fga <- sum(dat$gs_event_type_detail == 'fga_this')
  to <- sum(dat$gs_event_type_detail == 'to_this')
  fta <- sum(dat$gs_event_type_detail == 'fta_this')
  turn_rate <- to / (fga + (.44 * fta) + to)
  return(turn_rate)
}


#' Compute the defensive turnover rate for a given period of pbp time
#'
#' @param dat pbp data
#' @export
#' @family summarize_pbp_data
#' @return defensive turnover rate for the given chunk of pbp data

compute_defensive_turnover_rate <- function(dat) {

  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
  opp_to <- sum(dat$gs_event_type_detail == 'to_opp')
  opp_fta <- sum(dat$gs_event_type_detail == 'fta_opp')

  def_turn_rate <- opp_to / (opp_fga + (.44 * opp_fta) + opp_to)
  return(def_turn_rate)
}


#' Compute the offensive  rebound rate for the reference team for a given
#' chunk of pbp data
#'
#' @param dat a tibble with pbp data
#' @export
#' @family summarize_pbp_data
#' @return offensive rebounding percentage for the reference team in the given chunk of data
#'

compute_rebound_rate <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

  oreb <- sum(dat$gs_event_type_detail == 'oreb_this')
  opp_dreb <- sum(dat$gs_event_type_detail == 'dreb_opp')
  reb_rate <- oreb / (oreb + opp_dreb)
  return(reb_rate)
}

#' Compute the defensive rebound rate for the reference team for a given
#' chunk of pbp data
#'
#' @param dat a tibble with pbp data
#' @export
#' @family summarize_pbp_data
#' @return defensive rebounding percentage for the reference team in the given chunk of data
#'

compute_defensive_rebound_rate <- function(dat) {

  dreb <- sum(dat$gs_event_type_detail == 'dreb_this')
  opp_oreb <- sum(dat$gs_event_type_detail == 'oreb_opp')
  def_reb_rate <- 1 - (opp_oreb / (opp_oreb + dreb))
  return(def_reb_rate)

}

#' Compute the free throw rate for a chunk of pbp data
#'
#' @param dat a tibble of pbp data
#' @export
#' @family summarize_pbp_data
#' @return the free throw rate for the reference team for the given chunk of pbp data
#'

compute_free_throw_rate <- function(dat, use_attempted = TRUE) {
  ft <- if_else(use_attempted, sum(dat$gs_event_type_detail == 'fta_this'), sum(dat$gs_this_points_row == 1))
  fga <- sum(dat$gs_event_type_detail == 'fga_this')
  ftr <- ft / fga
  return(ftr)
}

#' Compute the defensive free throw rate for a chunk of pbp data
#'
#' @param dat a tibble of pbp data
#' @export
#' @family summarize_pbp_data
#' @return the defensivefree throw rate for the reference team for the given chunk of pbp data
#'

compute_defensive_free_throw_rate <- function(dat, use_attempted = TRUE) {
  opp_ft <- if_else(use_attempted, sum(dat$gs_event_type_detail == glue('fta_opp')), sum(dat$gs_opp_points_row == 1))
  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
  def_ftr <- opp_ft / opp_fga
  return(def_ftr)
}

#' Computes the plus/minus for the reference team for a given chunk of pbp data
#' @param dat a chunk of pbp data
#' @export
#' @family summarize_pbp_data
#' @return the plus/minus for the reference team for the given chunk of data

compute_plus_minus <- function(dat) {
  pts_for <-  sum(dat$gs_this_points_row, na.rm = TRUE)
  pts_against <- sum(dat$gs_opp_points_row, na.rm = TRUE)
  return(pts_for - pts_against)
}


#' Compute the average plus minus for the reference team for a given chunk of pbp data
#' @param dat pbp data
#' @param units must be one of 'second', 'minute' (the default), 'game'
#' @export
#' @family summarize_pbp_data
#' @return the average plus minus for the reference team for the provided data


compute_average_plus_minus <- function(dat, units = 'minute') {
  if (!units %in% c('minute', 'second', 'game')) stop('units must be one of "minute", "second" or "game"')
  pm <- compute_plus_minus(dat)
  total_seconds_playing_time = sum(dat$gs_seconds_until_next_event)
  if (units == 'second') avg_pm <- pm/total_seconds_playing_time
  else if (units == 'minute') avg_pm <- pm/(total_seconds_playing_time / 60)
  else avg_pm <- pm / (total_seconds_playing_time / 2880)
  return(avg_pm)
}


#' Compute 8 advanced stats factors for a team given a chunk of pbp dat
#'
#' @param dat a tibble of pbp data
#'
#' @export
#' @return a single row data.frame with 8 factor stats for a team
#'

compute_eight_factors <- function(dat, ...) {
  if (length(unique(dat$team_id)) > 1) stop('input data should all be for the same team\'s games')

  efp <- compute_effective_fgp(dat)
  defp <- compute_effective_defensive_fgp(dat)
  tr <- compute_turnover_rate(dat)
  dtr <- compute_defensive_turnover_rate(dat)
  rr <- compute_rebound_rate(dat)
  drr <- compute_defensive_rebound_rate(dat)
  ftr <- compute_free_throw_rate(dat)
  dftr <- compute_defensive_free_throw_rate(dat)

  all <- data.frame(efp = efp,
                    defp = defp,
                    tr = tr,
                    dtr = dtr,
                    rr = rr,
                    drr = drr,
                    ftr = ftr,
                    dftr = dftr)

  return(all)
}
