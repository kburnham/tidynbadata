#' Process raw pbp data
#'
#'
#' @param game_id msf game id
#' @export
#' @return processed play by play data
#'

process_raw_pbp <- function(game_id) {

  check_archive_dir()
  if (!dir.exists(file.path(tidynbadata$ARCHIVE_DIR, 'processed_pbp_data'))) {
    message('Creating processed_pbp_data archive dir . . .')
    dir.create(file.path(tidynbadata$ARCHIVE_DIR, 'processed_pbp_data'))
  }

  # check for a preexisting processed pbp
  pbp_file_path <- file.path(tidynbadata$ARCHIVE_DIR, 'processed_pbp_data', glue('{game_id}.rds'))
  if (file.exists(pbp_file_path)) {
    message(glue('Archived pbp data found and is being returned . . . '))
    pbp <- readRDS(pbp_file_path)
    return(pbp)
  }
  message('No archive found, getting raw data and processing . . .')
  raw_pbp <- get_raw_pbp(game_id)
  pbp <- raw_pbp[['api_json']][['plays']]
  game <- raw_pbp[['api_json']][['game']]


  pbp <- fix_pbp(pbp = pbp, game_id = game_id)

  pbp[['original_row']] <- seq(1:nrow(pbp))
  # this adds elapsed seconds since start of game, accounting for possible overtimes
  pbp[['total_elapsed_seconds']] <- map2_dbl(pbp[['playStatus.quarter']],
                                             pbp[['playStatus.secondsElapsed']],
                                             function(x, y) {
                                               tes <- if_else(x < 6,
                                                             (x - 1) * 720 + y,
                                                             2880 + ((x - 5) * 300) + y)
                                             })

  pbp[['game_id']] <- game_id

  # process each team separately first, join back later
  home_team <- game$homeTeam$id
  away_team <- game$awayTeam$id

  home_subs <- pbp %>% filter(substitution.team.id == home_team)
  away_subs <- pbp %>% filter(substitution.team.id == away_team)

  # process lineups for each team
  proc_home <- process_lineups(home_team, home_subs, game_id = game_id)

  proc_away <- process_lineups(away_team, away_subs, game_id = game_id)

  # join the original play-by-play to each teams lineup record
  final <- pbp %>% left_join(proc_home %>% select(-total_elapsed_seconds,
                                                  home_pof_vec = pof,
                                                  home_segment_number = segment_number,
                                                  home_pof_id = pof_id),
                             by = 'original_row',
                             suffix = c('', '.proc')) %>%
    left_join(proc_away %>% select(-total_elapsed_seconds,
                                   away_pof_vec = pof,
                                   away_segment_number = segment_number,
                                   away_pof_id = pof_id),
              by = 'original_row',
              suffix = c('', '.proc')) %>%
    fill(home_pof_vec, away_pof_vec, home_segment_number, away_segment_number) %>%
    mutate(home_pof_id = map_chr(home_pof_vec, make_lineup_id),
           away_pof_id = map_chr(away_pof_vec, make_lineup_id),
           seconds_until_next_event = lead(total_elapsed_seconds, default = max(total_elapsed_seconds)) - total_elapsed_seconds
           )


  home_pbp <- customize_pbp(final, team_id = home_team, opponent_id = away_team, loc = 'home')
  away_pbp <- customize_pbp(final, team_id = away_team, opponent_id = home_team, loc = 'away')




  home_pbp <- home_pbp %>% add_score_data()
  away_pbp <- away_pbp %>% add_score_data()


  final <- structure(list(home_pbp, away_pbp), .Names = c(as.name(home_team), as.name(away_team)))
  saveRDS(final, pbp_file_path)
  return(final)


}


#' a helper function for process_raw_pbp that converts a raw play-by-play data.frame to one customized to
#' the home or away team
#'
#' @param pbp raw pbp data
#' @param team_id  msf id of the team you wish to customize the report for
#' @param opponent_id msf id of the opponent
#' @param loc location of game with reference to custom team - either 'home' or 'away'
#' @export
#'
#' @return a play-by-play customized to one team with references to 'home' and 'away' changed to 'team' and 'opponent'
#' accordingly

customize_pbp <- function(pbp, team_id, opponent_id, loc) {

  ## the jumpBall.tippedToPlayer column changed at one point
  ## so if the old name exists, rename it
  if ('jumpBall.tippedToPlayer' %in% names(pbp)) names(pbp)[names(pbp) == 'jumpBall.tippedToPlayer'] <- 'jumpBall.tippedToPlayer.id'
  if ('violation.player' %in% names(pbp) & !'violation.player.id' %in% names(pbp)) {
    names(pbp)[names(pbp) == 'violation.player'] <- 'violation.player.id'
    pbp$violation.player <- NULL
  }



  ## there are numerous extraneous player data columns. We only need to retain columns indication the player id
  custom <- pbp %>%
    select(-ends_with('firstName'),
           -ends_with('lastName'),
           -ends_with('position'),
           -ends_with('jerseyNumber'))

  # make sure we have all the columns we need
  custom <- set_tidynba_names(custom, load_pbp_name_map())


  custom <- custom %>%
    mutate(jumpBall.wonBy = case_when(is.na(jumpBall.wonBy) ~ NA_character_,
                                      jumpBall.wonBy == toupper(loc) ~ 'this_team',
                                      TRUE ~ 'opponent_team'),

           rebound.team = case_when(is.na(rebound.team.id) ~ NA_character_,
                                    rebound.team.id == team_id ~ "this_team",
                                    rebound.team.id == opponent_id ~ 'opponent_team'
           ),
           foul.team = case_when(is.na(foul.team.id) ~ NA_character_,
                                 foul.team.id == team_id ~ "this_team",
                                 foul.team.id == opponent_id ~ 'opponent_team'
           ),

           fieldGoalAttempt.team = case_when(is.na(fieldGoalAttempt.team.id) ~ NA_character_,
                                             fieldGoalAttempt.team.id == team_id ~ "this_team",
                                             fieldGoalAttempt.team.id == opponent_id ~ "opponent_team"
           ),
           freeThrowAttempt.team = case_when(is.na(freeThrowAttempt.team.id) ~ NA_character_,
                                             freeThrowAttempt.team.id == team_id ~ 'this_team',
                                             freeThrowAttempt.team.id == opponent_id ~ 'opponent_team'
           ),
           turnover.team = case_when(is.na(turnover.team.id) ~ NA_character_,
                                     turnover.team.id == team_id ~ 'this_team',
                                     turnover.team.id == opponent_id ~ 'opponent_team'
           ),
           substitution.team = case_when(is.na(substitution.team.id) ~ NA_character_,
                                         substitution.team.id == team_id ~ 'this_team',
                                         substitution.team.id == opponent_id ~ 'opponent_team'
           ),
           violation.team = case_when(is.na(violation.team.id) ~ NA_character_,
                                      violation.team.id == team_id ~ 'this_team',
                                      violation.team.id == opponent_id ~ 'opponent_team'
           ),
           event_type = case_when(!is.na(fieldGoalAttempt.team) ~ 'fga',
                                  !is.na(freeThrowAttempt.team) ~ 'fta',
                                  !is.na(rebound.team) & rebound.type == 'OFFENSIVE' ~ 'oreb',
                                  !is.na(rebound.team) ~ 'dreb',
                                  !is.na(turnover.team) ~ 'to',
                                  !is.na(jumpBall.wonBy) ~ 'jb',
                                  !is.na(foul.team) ~ 'foul',
                                  !is.na(substitution.team) ~ 'sub',
                                  !is.na(violation.team) ~ 'vio'
                                  ),
           event_team = case_when(event_type == 'fga' ~ fieldGoalAttempt.team,
                                  event_type == 'fta' ~ freeThrowAttempt.team,
                                  event_type == 'oreb' ~ rebound.team,
                                  event_type == 'dreb' ~ rebound.team,
                                  event_type == 'to' ~ turnover.team,
                                  event_type == 'jb' ~ jumpBall.wonBy,
                                  event_type == 'foul' ~ foul.team,
                                  event_type == 'sub' ~ substitution.team,
                                  event_type == 'vio' ~ violation.team,
                                  TRUE ~ NA_character_),
           event_type_detail = glue('{event_type}_{if_else(event_team == "this_team", "this", "opp")}'),
           event_player = case_when(event_type == 'fga' ~ fieldGoalAttempt.shootingPlayer.id,
                                    event_type == 'fta'  ~ freeThrowAttempt.shootingPlayer.id,
                                    event_type %in% c('oreb', 'dreb') ~ rebound.retrievingPlayer.id,
                                    event_type == 'to' ~ turnover.lostByPlayer.id,
                                    event_type == 'jb' ~ ifelse(loc == 'home', jumpBall.homePlayer.id, jumpBall.awayPlayer.id),
                                    event_type == 'foul' ~ foul.penalizedPlayer.id,
                                    event_type == 'sub' ~ substitution.incomingPlayer.id,
                                    event_type == 'vio' ~ violation.player.id),
           team_id = team_id,
           opponent_id = opponent_id,
           possession_change = case_when(fieldGoalAttempt.result == 'SCORED' ~ TRUE,
                                         event_type == 'to' ~ TRUE,
                                         event_type == 'dreb' ~ TRUE,
                                         )
           ) %>%
    set_names(names(.) %>%
                str_replace('home', if_else(loc == 'home', 'this_team', 'opponent_team')) %>%
                str_replace('away', if_else(loc == 'away', 'this_team', 'opponent_team')))

  ## sometimes rebound types are not recorded, we default to DEFENSIVE, but we want a warning
  wh <- which(custom$event_type %in% c('oreb', 'dreb') & is.na(custom$rebound.type))
  msg <- glue('There were {length(wh)} rebounds of unknown type that have been labelled as "DEFENSIVE"')
  if (length(wh) > 0) message(msg)


  ## check columns that should have no NAs
  no_na_cols <- c('event_type', 'event_team', 'description')
  walk(no_na_cols, function(x) {
    nas <- which(is.na(custom[[x]]))
    if (length(nas) > 10) {
      stop('There were ', length(nas), ' NAs in the ', x, ' column.')
    } else if (length(nas) > 0) {
      print(nas)
      stop('The ', x , ' column has NAs. See above for indexes of rows with NAs')
    }
  })



  return(custom)
}

#' a helper function for \code{process_raw_pbp()} that adds list columns indicating which players
#' are on the floor for any moment of the game
#'
#' @param team team id, name, city or abbreviation
#' @param subs data.frame of substitution information
#' @export
#'
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

  pof_df <- tibble(pof = players_on_the_floor,
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
    fill(segment_number)  %>%
    # in addition to the pof vector, we want a character id that can be used for grouping
    mutate(pof_id = make_lineup_id(unlist(pof)))

  return(pof_df)

}



# helper function for inserting and removing players from a lineup vector
process_sub <- function(current, player_in, player_out) {
  if (is.na(player_out)) return(c(current, player_in))
  if (is.na(player_in)) return(c(current[current != player_out]))
  c(current[current != player_out], player_in) %>% sort

}


#' Add current score information to a customized play-by-play tibble
#'
#' @param custom_pbp
#'
#' @returm play-by-play data with the following columns appended (Each with the \code{score} prefix:
#'  score_points_this_team - number of points scored by reference team in that row
#'  score_points_opponent_team - number of point scored by opponent team in that row
#'  score_this_team_current - current score of the reference team in that row
#'  score_opponent_team_current - current score of the oponent team in that row
#'  score_game_status - one of 'winning', 'losing', 'tied' from perspective of reference team
#'  score_differential - current reference team score minus current oponent team score
#'
add_score_data <- function(custom_pbp) {
  new <- custom_pbp %>% mutate(score_points_this_team = case_when(fieldGoalAttempt.result == 'SCORED' & fieldGoalAttempt.team == 'this_team' ~ fieldGoalAttempt.points,
                                                                  freeThrowAttempt.result == 'SCORED' & freeThrowAttempt.team == 'this_team' ~ 1L,
                                                                  TRUE ~ 0L),
                               score_points_opponent_team = case_when(fieldGoalAttempt.result == 'SCORED' & fieldGoalAttempt.team == 'opponent_team' ~ fieldGoalAttempt.points,
                                                                      freeThrowAttempt.result == 'SCORED' & freeThrowAttempt.team == 'opponent_team' ~ 1L,
                                                                      TRUE ~ 0L),
                               score_this_team_current = cumsum(score_points_this_team),
                               score_opponent_team_current = cumsum(score_points_opponent_team),
                               score_game_status = case_when(score_this_team_current > score_opponent_team_current ~ 'winning',
                                                             score_opponent_team_current > score_this_team_current ~ 'losing',
                                                             TRUE ~ 'tied'),
                               score_differential = score_this_team_current - score_opponent_team_current
                               )

  return(new)
}
#'
#'
#' Load customized play-by-play data for a given game and team
#'
#' @param game_id the msf_game_id for the desired game
#' @param team the id, name, city or abbr of the desired team
#' @export
#'
#' @return a pbp data.frame
#'
load_pbp_data <- function(game_id, team) {
  # check the archive, if there is one, load and return, if not, load raw, process, archive and return
  pbp <- process_raw_pbp(game_id)
  team_id <- interpret_team(team)$id
  return(pbp[[glue('`{team_id}`')]])
}



