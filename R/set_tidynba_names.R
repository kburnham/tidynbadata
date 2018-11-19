#' A function that converts the names of a data.frame to those provided by a name_map.
#' The name_map is a named vector in which the names are the column names of the raw data
#' and the values are the column names expected by tidynbadata functions
#'
#' @param dat a tibble (or data.frame)
#' @param name_map a named vector the names of which should match the names of \code{dat}
#'
#' @return dat, with new names as provided by the values of the name_map

set_tidynba_names <- function(dat, name_map) {
  if (!setequal(names(dat), names(name_map))) stop('The names of the provided data do not match the names of the name map.
         This means that the names of the raw data have changed, and/or that columns have been
         added or subtracted. Inspect the raw data.frame to find out.')

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
    'jumpBall.tippedToPlayer' = 'jumpBall.tippedToPlayer',
    'jumpBall.awayPlayer.id' = 'jumpBall.awayPlayer.id',
    'jumpBall.awayPlayer.firstName' = 'jumpBall.awayPlayer.firstName',
    'jumpBall.awayPlayer.lastName' = 'jumpBall.awayPlayer.lastName',
    'jumpBall.awayPlayer.position' = 'jumpBall.awayPlayer.position',
    'jumpBall.awayPlayer.jerseyNumber' = 'jumpBall.awayPlayer.jerseyNumber',
    'jumpBall.homePlayer.id' = 'jumpBall.homePlayer.id',
    'jumpBall.homePlayer.firstName' = 'jumpBall.homePlayer.firstName',
    'jumpBall.homePlayer.lastName' = 'jumpBall.homePlayer.lastName',
    'jumpBall.homePlayer.position' = 'jumpBall.homePlayer.position',
    'jumpBall.homePlayer.jerseyNumber' = 'jumpBall.homePlayer.jerseyNumber',
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
    'foul.penalizedPlayer.firstName' = 'foul.penalizedPlayer.firstName',
    'foul.penalizedPlayer.lastName' = 'foul.penalizedPlayer.lastName',
    'foul.penalizedPlayer.position' = 'foul.penalizedPlayer.position',
    'foul.penalizedPlayer.jerseyNumber' = 'foul.penalizedPlayer.jerseyNumber',
    'foul.drawnByPlayer.id' = 'foul.drawnByPlayer.id',
    'foul.drawnByPlayer.firstName' = 'foul.drawnByPlayer.firstName',
    'foul.drawnByPlayer.lastName' = 'foul.drawnByPlayer.lastName',
    'foul.drawnByPlayer.position' = 'foul.drawnByPlayer.position',
    'foul.drawnByPlayer.jerseyNumber' = 'foul.drawnByPlayer.jerseyNumber',
    'foul.location.x' = 'foul.location.x',
    'foul.location.y' = 'foul.location.y',
    'fieldGoalAttempt.shotType' = 'fieldGoalAttempt.shotType',
    'fieldGoalAttempt.distanceFeet' = 'fieldGoalAttempt.distanceFeet',
    'fieldGoalAttempt.points' = 'fieldGoalAttempt.points',
    'fieldGoalAttempt.result' = 'fieldGoalAttempt.result',
    'fieldGoalAttempt.team.id' = 'fieldGoalAttempt.team.id',
    'fieldGoalAttempt.team.abbreviation' = 'fieldGoalAttempt.team.abbreviation',
    'fieldGoalAttempt.shootingPlayer.id' = 'fieldGoalAttempt.shootingPlayer.id',
    'fieldGoalAttempt.shootingPlayer.firstName' = 'fieldGoalAttempt.shootingPlayer.firstName',
    'fieldGoalAttempt.shootingPlayer.lastName' = 'fieldGoalAttempt.shootingPlayer.lastName',
    'fieldGoalAttempt.shootingPlayer.position' = 'fieldGoalAttempt.shootingPlayer.position',
    'fieldGoalAttempt.shootingPlayer.jerseyNumber' = 'fieldGoalAttempt.shootingPlayer.jerseyNumber',
    'fieldGoalAttempt.assistingPlayer.id' = 'fieldGoalAttempt.assistingPlayer.id',
    'fieldGoalAttempt.assistingPlayer.firstName' = 'fieldGoalAttempt.assistingPlayer.firstName',
    'fieldGoalAttempt.assistingPlayer.lastName' = 'fieldGoalAttempt.assistingPlayer.lastName',
    'fieldGoalAttempt.assistingPlayer.position' = 'fieldGoalAttempt.assistingPlayer.position',
    'fieldGoalAttempt.assistingPlayer.jerseyNumber' = 'fieldGoalAttempt.assistingPlayer.jerseyNumber',
    'fieldGoalAttempt.blockingPlayer.id' = 'fieldGoalAttempt.blockingPlayer.id',
    'fieldGoalAttempt.blockingPlayer.firstName' = 'fieldGoalAttempt.blockingPlayer.firstName',
    'fieldGoalAttempt.blockingPlayer.lastName' = 'fieldGoalAttempt.blockingPlayer.lastName',
    'fieldGoalAttempt.blockingPlayer.position' = 'fieldGoalAttempt.blockingPlayer.position',
    'fieldGoalAttempt.blockingPlayer.jerseyNumber' = 'fieldGoalAttempt.blockingPlayer.jerseyNumber',
    'fieldGoalAttempt.location.x' = 'fieldGoalAttempt.location.x',
    'fieldGoalAttempt.location.y' = 'fieldGoalAttempt.location.y',
    'rebound.type' = 'rebound.type',
    'rebound.team.id' = 'rebound.team.id',
    'rebound.team.abbreviation' = 'rebound.team.abbreviation',
    'rebound.retrievingPlayer.id' = 'rebound.retrievingPlayer.id',
    'rebound.retrievingPlayer.firstName' = 'rebound.retrievingPlayer.firstName',
    'rebound.retrievingPlayer.lastName' = 'rebound.retrievingPlayer.lastName',
    'rebound.retrievingPlayer.position' = 'rebound.retrievingPlayer.position',
    'rebound.retrievingPlayer.jerseyNumber' = 'rebound.retrievingPlayer.jerseyNumber',
    'freeThrowAttempt.attemptNum' = 'freeThrowAttempt.attemptNum',
    'freeThrowAttempt.totalAttempts' = 'freeThrowAttempt.totalAttempts',
    'freeThrowAttempt.location' = 'freeThrowAttempt.location',
    'freeThrowAttempt.result' = 'freeThrowAttempt.result',
    'freeThrowAttempt.team.id' = 'freeThrowAttempt.team.id',
    'freeThrowAttempt.team.abbreviation' = 'freeThrowAttempt.team.abbreviation',
    'freeThrowAttempt.shootingPlayer.id' = 'freeThrowAttempt.shootingPlayer.id',
    'freeThrowAttempt.shootingPlayer.firstName' = 'freeThrowAttempt.shootingPlayer.firstName',
    'freeThrowAttempt.shootingPlayer.lastName' = 'freeThrowAttempt.shootingPlayer.lastName',
    'freeThrowAttempt.shootingPlayer.position' = 'freeThrowAttempt.shootingPlayer.position',
    'freeThrowAttempt.shootingPlayer.jerseyNumber' = 'freeThrowAttempt.shootingPlayer.jerseyNumber',
    'turnover.type' = 'turnover.type',
    'turnover.isStolen' = 'turnover.isStolen',
    'turnover.location' = 'turnover.location',
    'turnover.team.id' = 'turnover.team.id',
    'turnover.team.abbreviation' = 'turnover.team.abbreviation',
    'turnover.lostByPlayer.id' = 'turnover.lostByPlayer.id',
    'turnover.lostByPlayer.firstName' = 'turnover.lostByPlayer.firstName',
    'turnover.lostByPlayer.lastName' = 'turnover.lostByPlayer.lastName',
    'turnover.lostByPlayer.position' = 'turnover.lostByPlayer.position',
    'turnover.lostByPlayer.jerseyNumber' = 'turnover.lostByPlayer.jerseyNumber',
    'turnover.stolenByPlayer.id' = 'turnover.stolenByPlayer.id',
    'turnover.stolenByPlayer.firstName' = 'turnover.stolenByPlayer.firstName',
    'turnover.stolenByPlayer.lastName' = 'turnover.stolenByPlayer.lastName',
    'turnover.stolenByPlayer.position' = 'turnover.stolenByPlayer.position',
    'turnover.stolenByPlayer.jerseyNumber' = 'turnover.stolenByPlayer.jerseyNumber',
    'substitution.team.id' = 'substitution.team.id',
    'substitution.team.abbreviation' = 'substitution.team.abbreviation',
    'substitution.incomingPlayer.id' = 'substitution.incomingPlayer.id',
    'substitution.incomingPlayer.firstName' = 'substitution.incomingPlayer.firstName',
    'substitution.incomingPlayer.lastName' = 'substitution.incomingPlayer.lastName',
    'substitution.incomingPlayer.position' = 'substitution.incomingPlayer.position',
    'substitution.incomingPlayer.jerseyNumber' = 'substitution.incomingPlayer.jerseyNumber',
    'substitution.outgoingPlayer.id' = 'substitution.outgoingPlayer.id',
    'substitution.outgoingPlayer.firstName' = 'substitution.outgoingPlayer.firstName',
    'substitution.outgoingPlayer.lastName' = 'substitution.outgoingPlayer.lastName',
    'substitution.outgoingPlayer.position' = 'substitution.outgoingPlayer.position',
    'substitution.outgoingPlayer.jerseyNumber' = 'substitution.outgoingPlayer.jerseyNumber',
    'violation.type' = 'violation.type',
    'violation.teamOrPersonal' = 'violation.teamOrPersonal',
    'violation.team.id' = 'violation.team.id',
    'violation.team.abbreviation' = 'violation.team.abbreviation',
    'violation.player.id' = 'violation.player.id',
    'violation.player.firstName' = 'violation.player.firstName',
    'violation.player.lastName' = 'violation.player.lastName',
    'violation.player.position' = 'violation.player.position',
    'violation.player.jerseyNumber' = 'violation.player.jerseyNumber'

  )
  return(name_map)
}

