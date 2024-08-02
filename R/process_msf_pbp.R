#' High level function for converting raw msf pbp to a tidynbadata pbp tibble
#'
#' Typically it is not necessary to call this function directly. It is better to
#' use \code{load_pbp(game_id, team)} which will handle acquisition of the
#' raw data and archiving of the results.
#' @param raw_msf_pbp a raw api pull of a play-by-play from mysportsfeeds
#' (as returned by \code{get_raw_pbp(game_id)})
#' @importFrom dplyr left_join mutate lead
#' @importFrom tidyr fill
#' @export
#'
#' @return a length 2 list of pbp tibbles, one for each team involved.
#' This list is also archived.
#'
process_msf_pbp <- function(raw_msf_pbp) {
  raw_msf_pbp <- fix_pbp(raw_msf_pbp)
  plays <- get_msf_raw_columns(raw_msf_pbp)
  game <- raw_msf_pbp[['api_json']][['game']]
  game_id <- game[['id']]
  home_team <- game[['homeTeam']][['id']]
  away_team <- game[['awayTeam']][['id']]
  # add play_id (= row number) and game_id columns
  plays <- plays %>%
    mutate(play_id = seq(nrow(.)),
           game_id = game_id,
           gs_total_elapsed_seconds = purrr::map2_dbl(playStatus.quarter,
                                               playStatus.secondsElapsed,
                                               compute_tes)
    )

  home_subs <- plays %>% filter(substitution.team.id == home_team)
  away_subs <- plays %>% filter(substitution.team.id == away_team)

  # process lineups for each team
  proc_home <- process_lineups(home_team, home_subs, game_id = game_id)
  proc_away <- process_lineups(away_team, away_subs, game_id = game_id)

  # join the original play-by-play to each teams lineup record
  final <- plays %>%
    left_join(proc_home %>%
                select(-gs_total_elapsed_seconds,
                       home_pof_vec = pof,
                       home_segment_number = segment_number,
                       home_pof_id = pof_id),
                             by = 'play_id',
                             suffix = c('', '.proc')) %>%
    left_join(proc_away %>% select(-gs_total_elapsed_seconds,
                                   away_pof_vec = pof,
                                   away_segment_number = segment_number,
                                   away_pof_id = pof_id),
              by = 'play_id',
              suffix = c('', '.proc')) %>%
    fill(home_pof_vec, away_pof_vec, home_segment_number, away_segment_number) %>%
    mutate(home_pof_id = purrr::map_chr(home_pof_vec, make_lineup_id),
           away_pof_id = purrr::map_chr(away_pof_vec, make_lineup_id),
           gs_seconds_until_next_event = lead(gs_total_elapsed_seconds,
                                              default = max(gs_total_elapsed_seconds)) - gs_total_elapsed_seconds
    )



  # we create two versions of the pbp data, one from each team's perspective
  # this makes it easy to aggregate and summarize
  home_pbp <- customize_msf_pbp(final, team_id = home_team, opponent_id = away_team, loc = 'home')
  away_pbp <- customize_msf_pbp(final, team_id = away_team, opponent_id = home_team, loc = 'away')

  final <- structure(list(home_pbp, away_pbp),
                     .Names = c(as.character(home_team),
                                as.character(away_team)))

  return(final)


}

#' Extract the raw columns needed from a provided mysportsfeeds nba play-by-play
#'and drop the columns we don't need
#'
#'  This function is used by \code{process_msf_pbp()}
#'
#'  @param raw_msf_pbp the list returned by \code{get_raw_pbp(game_id)}
#'  @importFrom dplyr setdiff select
#'  @return the play by by data from the raw_msf_pbp with unnecessary columns
#'  removed
get_msf_raw_columns <- function(raw_msf_pbp) {
  # violation columns may not exist, need to create with all NAs
  plays <- raw_msf_pbp$api_json$plays

  # only retain the columns that we need
  ps_cols <- c("description", "playStatus.quarter", "playStatus.secondsElapsed")
  jb_cols <- c("jumpBall.wonBy", "jumpBall.awayPlayer.id", "jumpBall.homePlayer.id",
               "jumpBall.tippedToPlayer.id")
  fga_cols <- c("fieldGoalAttempt.shotType", "fieldGoalAttempt.distanceFeet",
                "fieldGoalAttempt.points", "fieldGoalAttempt.result",
                "fieldGoalAttempt.team.id", "fieldGoalAttempt.shootingPlayer.id",
                "fieldGoalAttempt.assistingPlayer.id", "fieldGoalAttempt.blockingPlayer.id",
                "fieldGoalAttempt.location.x", "fieldGoalAttempt.location.y")

  reb_cols <- c("rebound.type", "rebound.team.id", "rebound.retrievingPlayer.id")
  foul_cols <- c("foul.type", "foul.isPersonal", "foul.isTechnical",
                 "foul.isFlagrant1", "foul.isFlagrant2", "foul.team.id",
                 "foul.penalizedPlayer.id", "foul.drawnByPlayer.id",
                 "foul.location.x", "foul.location.y")
  fta_cols <- c("freeThrowAttempt.attemptNum", "freeThrowAttempt.totalAttempts",
                "freeThrowAttempt.location", "freeThrowAttempt.result", "freeThrowAttempt.team.id",
                "freeThrowAttempt.shootingPlayer.id")
  to_cols <- c("turnover.type", "turnover.isStolen", "turnover.location",
               "turnover.team.id", "turnover.lostByPlayer.id",
               "turnover.stolenByPlayer.id")
  sub_cols <- c("substitution.team.id", "substitution.incomingPlayer.id",
                "substitution.outgoingPlayer.id")

  vio_cols <- c("violation.type", "violation.teamOrPersonal", "violation.team.id",
                "violation.player.id")

  missing_vio_cols <- setdiff(vio_cols, names(plays))
  for (mvc in missing_vio_cols) plays[[mvc]] <- NA

  missing_jb_cols <- setdiff(jb_cols, names(plays))
  for (jb in missing_jb_cols) plays[[jb]] <- NA_integer_


  ## test for existence of location data, where missing data is necessary, use -1n
  if (!"foul.location.x" %in% names(plays)) {
    plays$foul.location.x <- NA
    plays$foul.location.x[!is.na(plays$foul.type)] <- -1
  }

  if (!"foul.location.y" %in% names(plays)) {
    plays$foul.location.y <- NA
    plays$foul.location.y[!is.na(plays$foul.type)] <- -1
  }

  if (!"fieldGoalAttempt.location.x" %in% names(plays)) {
    plays$fieldGoalAttempt.location.x <- NA
    plays$fieldGoalAttempt.location.x[!is.na(plays$fieldGoalAttempt.result)] <- -1
  }

  if (!"fieldGoalAttempt.location.y" %in% names(plays)) {
    plays$fieldGoalAttempt.location.y <- NA
    plays$fieldGoalAttempt.location.y[!is.na(plays$fieldGoalAttempt.result)] <- -1
  }


  needed_cols <- c(ps_cols, jb_cols, fga_cols,
                   reb_cols, foul_cols, vio_cols,
                   fta_cols, to_cols, sub_cols)


  new_pbp <- plays %>%
    select(one_of(needed_cols))

  return(new_pbp)

}





#' compute the total time elapsed in the game given the quarter and time elapsed
#' @param x the quarter number (5+ for overtime periods)
#' @param y seconds elapsed since the beginning of the quarter
#' @return an integer indicating the total number of gametime seconds since the
#' start of the game
#' @export
#' @importFrom dplyr if_else
#'
compute_tes <- function(x, y) {
  # this handles infinite overtime periods
  tes <- if_else(x < 6,
                 (x - 1) * 720 + y,
                 2880 + ((x - 5) * 300) + y)
}


#' Transform raw pbp data so that it is specific to one of the teams
#'
#' Used internally by process_msf_pbp()
#'
#' @param plays a raw data.frame from msf play-by-play data
#' @param team_id id of the reference team
#' @param opponent_id id of the opponent team
#' @param loc either 'home' or 'away' indicating the location from the perspective of the reference team
#' @importFrom dplyr transmute case_when if_else
#' @return a data.frame with customized pbp data
#'
customize_msf_pbp# <- function(plays, team_id, opponent_id, loc) {

  loc <- toupper(loc)
  if (!loc %in% c('HOME', 'AWAY')) stop('loc must be "home" or "away"')
  custom <- plays %>%
    transmute(gs_description = description,
              jb_wonby_team = case_when(is.na(jumpBall.wonBy) ~ NA_character_,
                                        jumpBall.wonBy == loc ~ 'this',
                                        jumpBall.wonBy == 'NEITHER' ~ 'neither',
                                        TRUE ~ 'opp'),
              jb_tippedtoplayer = jumpBall.tippedToPlayer.id,
              jb_this_player = case_when(loc == 'HOME' ~ jumpBall.homePlayer.id,
                                         loc == 'AWAY' ~ jumpBall.awayPlayer.id,
                                         TRUE ~ NA_integer_),
              jb_opp_player = case_when(loc == 'HOME' ~ jumpBall.awayPlayer.id,
                                        loc == 'AWAY' ~ jumpBall.homePlayer.id,
                                        TRUE ~ NA_integer_),
              gs_quarter = playStatus.quarter,
              gs_quarter_seconds_elapsed = playStatus.secondsElapsed,
              gs_total_elapsed_seconds = gs_total_elapsed_seconds,
              gs_this_pof_vec = case_when(loc == 'HOME' ~ home_pof_vec,
                                         loc == 'AWAY' ~ away_pof_vec),
              gs_opp_pof_vec = case_when(loc == 'AWAY' ~ home_pof_vec,
                                          loc == 'HOME' ~ away_pof_vec),
              gs_this_pof_id = case_when(loc == 'HOME' ~ home_pof_id,
                                         loc == 'AWAY' ~ away_pof_id),
              gs_opp_pof_id = case_when(loc == 'AWAY' ~ home_pof_id,
                                         loc == 'HOME' ~ away_pof_id),
              gs_seconds_until_next_event = gs_seconds_until_next_event,
              gs_event_type = case_when(!is.na(fieldGoalAttempt.team.id) ~ 'fga',
                                     !is.na(freeThrowAttempt.team.id) ~ 'fta',
                                     !is.na(rebound.team.id) & rebound.type == 'OFFENSIVE' ~ 'oreb',
                                     !is.na(rebound.team.id) ~ 'dreb',
                                     !is.na(turnover.team.id) ~ 'to',
                                     !is.na(jumpBall.wonBy) ~ 'jb',
                                     !is.na(foul.team.id) ~ 'foul',
                                     !is.na(substitution.team.id) ~ 'sub',
                                     !is.na(violation.team.id) ~ 'vio'
              ),
              gs_event_team = case_when(gs_event_type == 'fga' ~ if_else(fieldGoalAttempt.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'fta' ~ if_else(freeThrowAttempt.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'oreb' ~ if_else(rebound.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'dreb' ~ if_else(rebound.team.id  == team_id, 'this', 'opp'),
                                     gs_event_type == 'to' ~ if_else(turnover.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'jb' ~ if_else(jumpBall.wonBy == team_id, 'this', 'opp'),
                                     gs_event_type == 'foul' ~ if_else(foul.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'sub' ~ if_else(substitution.team.id == team_id, 'this', 'opp'),
                                     gs_event_type == 'vio' ~ if_else(violation.team.id == team_id, 'this', 'opp'),
                                     TRUE ~ NA_character_),
              gs_event_type_detail = glue::glue('{gs_event_type}_{if_else(gs_event_team == "this", "this", "opp")}'),
              event_player = case_when(gs_event_type == 'fga' ~ fieldGoalAttempt.shootingPlayer.id,
                                       gs_event_type == 'fta'  ~ freeThrowAttempt.shootingPlayer.id,
                                       gs_event_type %in% c('oreb', 'dreb') ~ rebound.retrievingPlayer.id,
                                       gs_event_type == 'to' ~ turnover.lostByPlayer.id,
                                       gs_event_type == 'jb' ~ ifelse(loc == 'HOME', jumpBall.homePlayer.id, jumpBall.awayPlayer.id),
                                       gs_event_type == 'foul' ~ foul.penalizedPlayer.id,
                                       gs_event_type == 'sub' ~ substitution.incomingPlayer.id,
                                       gs_event_type == 'vio' ~ violation.player.id),
              team_id = team_id,
              opponent_id = opponent_id,
              game_id = game_id,
              fga_team = case_when(is.na(fieldGoalAttempt.team.id) ~ NA_character_,
                                   fieldGoalAttempt.team.id == team_id ~ 'this',
                                   fieldGoalAttempt.team.id == opponent_id ~ 'opp'),
              fga_result = case_when(is.na(fieldGoalAttempt.result) ~ NA_character_,
                                     fieldGoalAttempt.result == 'BLOCKED' ~ 'block',
                                     fieldGoalAttempt.result == 'MISSED' ~ 'miss',
                                     fieldGoalAttempt.result == 'SCORED' ~ 'make'
                                     ),
              fga_points = fieldGoalAttempt.points,
              fga_location_x = fieldGoalAttempt.location.x,
              fga_location_y = fieldGoalAttempt.location.y,
              fga_distance_feet = fieldGoalAttempt.distanceFeet,
              fga_assist_player = fieldGoalAttempt.assistingPlayer.id,
              fga_block_player = fieldGoalAttempt.blockingPlayer.id,
              reb_type = tolower(str_sub(rebound.type, 1, 3)),
              reb_team = case_when(is.na(rebound.team.id) ~ NA_character_,
                                   rebound.team.id == team_id ~ 'this',
                                   rebound.team.id == opponent_id ~ 'opp'),
              reb_player = rebound.retrievingPlayer.id,
              fta_num = freeThrowAttempt.attemptNum,
              fta_total_attempts = freeThrowAttempt.totalAttempts,
              fta_result = case_when(is.na(freeThrowAttempt.result) ~ NA_character_,
                                     freeThrowAttempt.result == 'MISSED' ~ 'miss',
                                     freeThrowAttempt.result == 'SCORED' ~ 'make'),
              fta_team = case_when(is.na(freeThrowAttempt.team.id) ~ NA_character_,
                                   freeThrowAttempt.team.id == team_id ~ 'this',
                                   freeThrowAttempt.team.id == opponent_id ~ 'opp'),
              to_type = turnover.type,
              to_isstolen = turnover.isStolen,
              to_team = case_when(is.na(turnover.team.id) ~ NA_character_,
                                  turnover.team.id == team_id ~ 'this',
                                  turnover.team.id == opponent_id ~ 'opp'),
              to_player= turnover.lostByPlayer.id,
              to_stolenby = turnover.stolenByPlayer.id,
              foul_type = foul.type,
              foul_ispersonal = foul.isPersonal,
              foul_istechnical = foul.isTechnical,
              foul_isflagrant1 = foul.isFlagrant1,
              foul_isflagrant2 = foul.isFlagrant2,
              foul_team = case_when(is.na(foul.team.id) ~ NA_character_,
                                    foul.team.id == team_id ~ 'this',
                                    foul.team.id == opponent_id ~ 'opp'),
              foul_player = foul.penalizedPlayer.id,
              foul_fouled_player = foul.drawnByPlayer.id,
              foul_location_x = foul.location.x,
              foul_location_y = foul.location.y,
              vio_type = violation.type,
              vio_team = case_when(is.na(violation.team.id) ~ NA_character_,
                                   violation.team.id == team_id ~ 'this',
                                   violation.team.id == opponent_id ~ 'opp'),
              sub_team = case_when(is.na(substitution.team.id) ~ NA_character_,
                                   substitution.team.id == team_id ~ 'this',
                                   substitution.team.id == opponent_id ~ 'opp'),
              sub_player_in = substitution.incomingPlayer.id,
              sub_player_out = substitution.outgoingPlayer.id,
              this_segment_number = case_when(loc == 'HOME' ~ home_segment_number,
                                              loc == 'AWAY' ~ away_segment_number),
              opp_segment_number = case_when(loc == 'HOME' ~ away_segment_number,
                                             loc == 'AWAY' ~ home_segment_number),
              tnd_data_source = 'msf',
              unique_id = glue::glue('{game_id}-{team_id}-{play_id}')
    )

  custom <- custom %>% add_score_data()

  return(custom)


}


#' Add score data to custom pbp data
#'
#' @param custom a data.frame of custom pbp data
#'
#' @return the same data.frame as the input with multiple added columns
#' @importFrom dplyr mutate case_when
add_score_data <- function(custom) {
  new <- custom %>% mutate(gs_this_points_row = case_when(fga_result == 'make' &
                                                            fga_team == 'this' ~
                                                            fga_points,
                                                          fta_result == 'make' &
                                                            fta_team == 'this' ~ 1L,
                                                          TRUE ~ 0L),
                           gs_opp_points_row = case_when(fga_result == 'make' &
                                                           fga_team == 'opp' ~
                                                           fga_points,
                                                         fta_result == 'make' &
                                                           fta_team == 'opp' ~ 1L,
                                                         TRUE ~ 0L),
                           gs_this_points_current_total = cumsum(gs_this_points_row),
                           gs_opp_points_current_total = cumsum(gs_opp_points_row),
                           gs_score_game_status =
                             case_when(gs_this_points_current_total >
                                         gs_opp_points_current_total ~ 'winning',
                                       gs_opp_points_current_total >
                                         gs_this_points_current_total ~ 'losing',
                                       TRUE ~ 'tied'),
                           gs_score_differential = gs_this_points_current_total -
                             gs_opp_points_current_total
                           )

  return(new)
}



#' a helper function for \code{process_msf_pbp()} that adds list columns indicating which players
#' are on the floor for any moment of the game
#'
#' @param team team id, name, city or abbreviation
#' @param subs data.frame of substitution information
#' @param game_id the msf game_id for the requested game
#' @importFrom dplyr filter pull bind_rows mutate select
#' @return play-by-play data with players_on_the_floor columns attached
#'

process_lineups <- function(team, subs, game_id) {
  # get starters
  pof <- get_lineup(game_id, team) %>%
    filter(position == 'Starter') %>%
    pull(player.id)
  first_row <- tibble(pof = list(pof), gs_total_elapsed_seconds = 0, play_id = 1)
  players_in <- subs$substitution.incomingPlayer.id
  players_out <- subs$substitution.outgoingPlayer.id

  players_on_the_floor <- list()
  for (i in seq(players_in)) {
    pof <- process_sub(pof, players_in[i], players_out[i])
    players_on_the_floor[[i]] <- pof
  }

  pof_df <- tibble::tibble(pof = players_on_the_floor,
                   gs_total_elapsed_seconds = subs$gs_total_elapsed_seconds,
                   play_id = subs$play_id
  )
  pof_df <- bind_rows(first_row, pof_df)
  # here we are adding the segment data
  # a segment is an uninterrupted period of time during which the same five players are on the court
  # a segment should increment the first time a sub comes in at a given time period, but not again for
  # subs that come in at the same time
  pof_df <- pof_df %>%
    # create segement numbers by filtering to unique time_elapsed
    filter(!duplicated(gs_total_elapsed_seconds)) %>%
    mutate(segment_number = seq(nrow(.))) %>%
    select(play_id, segment_number) %>%
    # then join back to the original data and fill
    right_join(pof_df, by = 'play_id') %>%
    tidyr::fill(segment_number)  %>%
    # in addition to the pof vector, we want a character id that can be used for grouping
    mutate(pof_id = make_lineup_id(unlist(pof)))

  return(pof_df)

}



#' a helper function for inserting and removing players from a lineup vector
#' @param current the vector of player ids currently in the game
#' @param player_in the id of the player coming into the game
#' @param player_out the id of the player leaving the game
#'
#' @return a low to high sorted vector of the player ids on the court after the
#' substitution is complete
#'
process_sub <- function(current, player_in, player_out) {
  if (is.na(player_out)) return(c(current, player_in))
  if (is.na(player_in)) return(c(current[current != player_out]))
  c(current[current != player_out], player_in) %>% sort

}
