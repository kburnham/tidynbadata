#' A function that converts the names of a data.frame to those provided by a name_map.
#' The name_map is a named vector in which the names are the column names of the raw data
#' and the values are the column names expected by tidynbadata functions
#'
#' @param dat a tibble (or data.frame)
#' @param name_map a named vector the names of which should match the names of \code{dat}
#'
#' @return dat, with new names as provided by the values of the name_map

set_tidynba_names <- function(dat, name_map) {
  # this needs to be more thoughtful.
  # newly added columns to the input data should just be ignored?
  # only alert when REQUIRED columns are missing
  not_in_dat <- setdiff(names(name_map), names(dat))
  not_in_map <- setdiff(names(dat), names(name_map))
  if (!setequal(names(dat), names(name_map))) {
    message('These names are in the play-by-play data.frame, but not in the name map:')
    not_in_map %>% walk(~message(.))

    message('These names are in the name map, but not in the play-by-play data.frame:')
    not_in_dat %>% walk(~message(.))

    stop('The names of the provided data do not match the names of the name map.
         This means that the names of the raw data have changed, and/or that columns have been
         added or subtracted. Inspect the raw data.frame to find out.')
  }

  new_names <- unname(name_map[names(dat)])
  names(dat) <- new_names
  return(dat)

}

#' Generates a name_map that converts the names of a raw play-by-play data.frame
#' to the names expected by tidynbadata

load_pbp_name_map <- function() {
  name_map <- c(
    'description' = 'description',
    'jumpBall.wonBy' = 'jumpBall.wonBy',
    'jumpBall.tippedToPlayer.id' = 'jumpBall.tippedToPlayer.id',
    'jumpBall.awayPlayer.id' = 'jumpBall.awayPlayer.id',
    'jumpBall.homePlayer.id' = 'jumpBall.homePlayer.id',
    'playStatus.quarter' = 'playStatus.quarter',
    'playStatus.secondsElapsed' = 'playStatus.secondsElapsed',
    'playStatus.awayPlayersOnCourt' = 'playStatus.awayPlayersOnCourt',
    'playStatus.homePlayersOnCourt' = 'playStatus.homePlayersOnCourt',
    'foul.type' = 'foul.type',
    'foul.isPersonal' = 'foul.isPersonal',
    'foul.isTechnical' = 'foul.isTechnical',
    'foul.isFlagrant1' = 'foul.isFlagrant1',
    'foul.isFlagrant2' = 'foul.isFlagrant2',
    'foul.team.id' = 'foul.team.id',
    'foul.team.abbreviation' = 'foul.team.abbreviation',
    'foul.penalizedPlayer.id' = 'foul.penalizedPlayer.id',
    'foul.drawnByPlayer.id' = 'foul.drawnByPlayer.id',
    'foul.location.x' = 'foul.location.x',
    'foul.location.y' = 'foul.location.y',
    'fieldGoalAttempt.shotType' = 'fieldGoalAttempt.shotType',
    'fieldGoalAttempt.distanceFeet' = 'fieldGoalAttempt.distanceFeet',
    'fieldGoalAttempt.points' = 'fieldGoalAttempt.points',
    'fieldGoalAttempt.result' = 'fieldGoalAttempt.result',
    'fieldGoalAttempt.team.id' = 'fieldGoalAttempt.team.id',
    'fieldGoalAttempt.team.abbreviation' = 'fieldGoalAttempt.team.abbreviation',
    'fieldGoalAttempt.shootingPlayer.id' = 'fieldGoalAttempt.shootingPlayer.id',
    'fieldGoalAttempt.assistingPlayer.id' = 'fieldGoalAttempt.assistingPlayer.id',
    'fieldGoalAttempt.blockingPlayer.id' = 'fieldGoalAttempt.blockingPlayer.id',
    'fieldGoalAttempt.location.x' = 'fieldGoalAttempt.location.x',
    'fieldGoalAttempt.location.y' = 'fieldGoalAttempt.location.y',
    'rebound.type' = 'rebound.type',
    'rebound.team.id' = 'rebound.team.id',
    'rebound.team.abbreviation' = 'rebound.team.abbreviation',
    'rebound.retrievingPlayer.id' = 'rebound.retrievingPlayer.id',
    'freeThrowAttempt.attemptNum' = 'freeThrowAttempt.attemptNum',
    'freeThrowAttempt.totalAttempts' = 'freeThrowAttempt.totalAttempts',
    'freeThrowAttempt.location' = 'freeThrowAttempt.location',
    'freeThrowAttempt.result' = 'freeThrowAttempt.result',
    'freeThrowAttempt.team.id' = 'freeThrowAttempt.team.id',
    'freeThrowAttempt.team.abbreviation' = 'freeThrowAttempt.team.abbreviation',
    'freeThrowAttempt.shootingPlayer.id' = 'freeThrowAttempt.shootingPlayer.id',
    'turnover.type' = 'turnover.type',
    'turnover.isStolen' = 'turnover.isStolen',
    'turnover.location' = 'turnover.location',
    'turnover.team.id' = 'turnover.team.id',
    'turnover.team.abbreviation' = 'turnover.team.abbreviation',
    'turnover.lostByPlayer.id' = 'turnover.lostByPlayer.id',
    'turnover.stolenByPlayer.id' = 'turnover.stolenByPlayer.id',
    'substitution.team.id' = 'substitution.team.id',
    'substitution.team.abbreviation' = 'substitution.team.abbreviation',
    'substitution.incomingPlayer.id' = 'substitution.incomingPlayer.id',
    'substitution.outgoingPlayer.id' = 'substitution.outgoingPlayer.id',
    'violation.type' = 'violation.type',
    'violation.teamOrPersonal' = 'violation.teamOrPersonal',
    'violation.team.id' = 'violation.team.id',
    'violation.team.abbreviation' = 'violation.team.abbreviation',
    'violation.player.id' = 'violation.player.id',
    'original_row' = 'original_row',
    'total_elapsed_seconds' = 'total_elapsed_seconds',
    'home_pof_vec' = 'home_pof_vec',
    'away_pof_vec' = 'away_pof_vec',
    'home_pof_id' = 'home_pof_id',
    'away_pof_id' = 'away_pof_id',
    'home_segment_number' = 'home_segment_number',
    'away_segment_number' = 'away_segment_number',
    'game_id' = 'game_id',
    'seconds_since_last_event' = 'seconds_since_last_event'
  )
  return(name_map)
}

