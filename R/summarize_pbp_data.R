#' Given a chunk of play-by-play data, determine the number of possesions for each team
#'
#' @param pbp_data play-by-play data as returned by \code{add_lineups_to_pbp()}
#' @team either 'home' or 'away'
#' @type specified how to compute possesion. Currently supported types are: 'basic'
#' @export
#' @return

compute_possesions  <- function(team = tidynbadata$DEFAULT_TEAM, type = 'basic') {
  supported_types = c('basic')
  team_id <- interpret_team(team)$id
  if (!type %in% supported_types) stop(type, ' is not supported. Supported types are ', supported_types)
  #Basic Possession Formula=0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]
  if (type == 'basic') {
    #Basic Possession Formula=0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]
    # see https://www.nbastuffer.com/analytics101/possession/
    # we just need field goal attempts, turnovers, free throw attempts and offensive rebounds
    pd <- pbp_data %>% summarize(fga = sum(fieldGoalAttempt.team.id == team_id, na.rm = TRUE),
                                 to = sum(turnover.team.id == team_id, na.rm = TRUE),
                                 fta = sum(freeThrowAttempt.team.id == team_id, na.rm = TRUE),
                                 or = sum(rebound.type == 'OFFENSIVE' & rebound.team.id == team_id, na.rm = TRUE)
    )

    possesions <- as.integer(round(.96 * ((pd$fga + pd$to + (.44 * pd$fta) - pd$or)), 0))

  } else if (type == 'advanced') {
    #More Specific Possession Formula=0.5*((Field Goal Attempts + 0.4*Free Throw Attempts – 1.07*(Offensive Rebounds/(Offensive Rebounds + Opponent Defensive Rebounds))*(Field Goal Attempts – FG) + Turnovers) + (Opponent Field Goal Attempts + 0.4*(Opponent Free Throw Attempts) – 1.07*(Opponent Offensive Rebounds)/(Opponent Offensive Rebounds + Defensive Rebounds))*(Opponent Field Goal Attempts – Opponent FG) + Opponent Turnovers))

  }


  return(possesions)

}


comput_possesion_basic <- function(fga, to, fta, or) {

}
