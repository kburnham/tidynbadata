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
#' @return a list with efgp (effective field goal percentage) and defgp (defensive effecgive field goal percentage)
#'

compute_effective_field_goal_percentage <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

    fga <- sum(dat$gs_event_type_detail == 'fga_this')
    fg <- sum(dat$gs_this_points_row > 1)
    fg3 <- sum(dat$gs_this_points_row == 3)

    opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
    opp_fg <- sum(dat$gs_opp_points_row > 1)
    opp_fg3 <- sum(dat$gs_opp_points_row == 3)


  eff_fg_per <- (fg + (.5 * fg3)) / fga
  opp_eff_fg_per <- (opp_fg + (.5 * opp_fg3)) / opp_fga

  return(list(efgp = eff_fg_per,
              defgp = opp_eff_fg_per))

}


#' Compute the turnover rate for a given period of pbp time
#'
#' @param dat pbp data
#' @export
#' @family summarize_pbp_data
#' @return a list with reference team's turnover rate and defensive turnover rate

compute_turnover_rate <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

  fga <- sum(dat$gs_event_type_detail == 'fga_this')
  to <- sum(dat$gs_event_type_detail == 'to_this')
  fta <- sum(dat$gs_event_type_detail == 'fta_this')

  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')
  opp_to <- sum(dat$gs_event_type_detail == 'to_opp')
  opp_fta <- sum(dat$gs_event_type_detail == 'fta_opp')

  turn_rate <- to / (fga + (.44 * fta) + to)
  def_turn_rate <- opp_to / (opp_fga + (.44 * opp_fta) + opp_to)
  return(list(tor  = turn_rate,
              dtor = def_turn_rate))
}


#' Compute the offensive and defensive rebound rate for the reference team for a given
#' chunk of pbp data
#'
#' @param dat a tibble with pbp data
#' @export
#' @family summarize_pbp_data
#' @return a list with orp (offensive rebounding percentage) and drp (defensive rebounding percentage)
#'

compute_rebound_rate <- function(dat) {
  #https://www.nbastuffer.com/analytics101/four-factors/

  oreb <- sum(dat$gs_event_type_detail == 'oreb_this')
  dreb <- sum(dat$gs_event_type_detail == 'dreb_this')

  opp_dreb <- sum(dat$gs_event_type_detail == 'dreb_opp')
  opp_oreb <- sum(dat$gs_event_type_detail == 'oreb_opp')

  reb_rate <- oreb / (oreb + opp_dreb)
  def_reb_rate <- 1 - (opp_oreb / (opp_oreb + dreb))
  return(list(orp = reb_rate,
              drp = def_reb_rate))
}

#' Compute offensive and defensive rebound rate for a chunck of pbp data
#'
#' @param dat a tibble of pbp data
#' @export
#' @family summarize_pbp_data
#' @return a list with ftr (free throw rate) and dftr (defensive free throw rate)
#'

compute_free_throw_rate <- function(dat, use_attempted = TRUE) {
  ft <- if_else(use_attempted, sum(dat$gs_event_type_detail == 'fta_this'), sum(dat$gs_this_points_row == 1))
  opp_ft <- if_else(use_attempted, sum(dat$gs_event_type_detail == glue('fta_opp')), sum(dat$gs_opp_points_row == 1))

  fga <- sum(dat$gs_event_type_detail == 'fga_this')
  opp_fga <- sum(dat$gs_event_type_detail == 'fga_opp')

  ftr <- ft / fga
  opp_ftr <- opp_ft / opp_fga

  return(list(ftr = ftr,
              dftr = opp_ftr))

}


#' This function wraps the eight factors functions and returns a tibble with a single
#' row of all eight factors
#'
#' @param dat a chunk of pbp data
#' @export
#' @family summarize_pbp_data
#' @return a one_row tibble with 8 factors data for the given input
#' chunk of pbp data

compute_eight_factors <- function(dat, ...) {
  efp <- compute_effective_field_goal_percentage(dat)
  tr <- compute_turnover_rate(dat)
  rr <- compute_rebound_rate(dat)
  ftr <- compute_free_throw_rate(dat)

  all <- list(efp, tr, rr, ftr) %>% flatten() %>% as.tibble()

  return(all)

}
