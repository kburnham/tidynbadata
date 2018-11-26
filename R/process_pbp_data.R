#' Functions for processing and customizing play by play data
#'
#' This function replaces add_lineups_to_pbp. It uses the raw results of the
#' msf api call, extracts the play-by-play archive data.frame and:
#' 1) adds players on the floor vectors for each team
#' 2) adds columns - OriginalRow this just numbers each row in the original data.frame for later rejoins
#'                 - total_elapsed_seconds - this indicates the number of seconds of play time elapsed since
#'                 the beginning of the game
#' 3) personalizes the data.frame for each of the two teams involved, this means:
#'    a) rename the columns from "home"-"away" to "team"-"opponent"
#'    b) does the same with the players on the floor list columns
#' 4) archives both versions of the pbp in the tidynbadata$ARCHIVE_DIR/proc_pbp_archive
#' 5) Add other columns (i.e. current team score, current oppononet score, game_status (winning, losing, tied))

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
           seconds_since_last_event = total_elapsed_seconds - lag(total_elapsed_seconds, default = total_elapsed_seconds[1])
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
#'
#' @return a play-by-play customized to one team with references to 'home' and 'away' changed to 'team' and 'opponent'
#' accordingly

customize_pbp <- function(pbp, team_id, opponent_id, loc) {

  ## the jumpBall.tippedToPlayer column changed at one point
  ## so if the old name exists, rename it
  if ('jumpBall.tippedToPlayer' %in% names(pbp)) names(pbp)[names(pbp) == 'jumpBall.tippedToPlayer'] <- 'jumpBall.tippedToPlayer.id'
  if ('violation.player' %in% names(pbp)) names(pbp)[names(pbp) == 'violation.player'] <- 'violation.player.id'

  ## if no violations occur, those columns are not part of the export, but we want to add them
  violation_columns <- c("violation.type", "violation.teamOrPersonal", "violation.team.id",
                         "violation.team.abbreviation", "violation.player.id")

  for (vc in violation_columns) if (!vc %in% names(pbp)) pbp[[vc]] <- NA

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
                                  !is.na(rebound.team) ~ 'reb',
                                  !is.na(turnover.team) ~ 'to',
                                  !is.na(jumpBall.wonBy) ~ 'jb',
                                  !is.na(foul.team) ~ 'foul',
                                  !is.na(substitution.team) ~ 'sub',
                                  !is.na(violation.team) ~ 'vio'
                                  ),
           event_team = case_when(event_type == 'fga' ~ fieldGoalAttempt.team,
                                  event_type == 'fta' ~ freeThrowAttempt.team,
                                  event_type == 'reb' ~ rebound.team,
                                  event_type == 'to' ~ turnover.team,
                                  event_type == 'jb' ~ jumpBall.wonBy,
                                  event_type == 'foul' ~ foul.team,
                                  event_type == 'sub' ~ substitution.team,
                                  event_type == 'vio' ~ violation.team,
                                  TRUE ~ NA_character_),
           team_id = team_id,
           opponent_id = opponent_id
           ) %>%
    set_names(names(.) %>%
                str_replace('home', if_else(loc == 'home', 'this_team', 'opponent_team')) %>%
                str_replace('away', if_else(loc == 'away', 'this_team', 'opponent_team')))
  ## add segment data
  # a segment is a period of time during the game where all five players on the floor remain the same
  # a segment changes when a subsitution is made
  # but sometimes several subs come in at once, each getting a different row in the data, we don't want to make a different segment when two or more
  # players enter the game at the same time.





  return(custom)
}

#' a helper function for \code{process_raw_pbp()} that adds list columns indicating which players
#' are on the floor for any moment of the game
#'
#' @param team team id, name, city or abbreviation
#' @param subs data.frame of substitution information
#'
#' @return play-by-play data with players_on_the_floor columns attached
#'

process_lineups <- function(team, subs, game_id) {
  # get starters
  pof <- get_lineup(game_id, team) %>%
    filter(position == 'Starter') %>%
    pull(player.id)
  first_row <- tibble(pof = list(pof), total_elapsed_seconds = 0, original_row = 1)
  players_in <- subs$substitution.incomingPlayer.id
  players_out <- subs$substitution.outgoingPlayer.id

  players_on_the_floor <- list()
  for (i in seq(players_in)) {
    pof <- process_sub(pof, players_in[i], players_out[i])
    players_on_the_floor[[i]] <- pof
  }

  pof_df <- tibble(pof = players_on_the_floor,
                   total_elapsed_seconds = subs$total_elapsed_seconds,
                   original_row = subs$original_row
  )
  pof_df <- bind_rows(first_row, pof_df)
  # here we are adding the segment data
  # a segment is an uninterrupted period of time during which the same five players are on the court
  # a segment should increment the first time a sub comes in at a given time period, but not again for
  # subs that come in at the same time
  pof_df <- pof_df %>%
    filter(!duplicated(total_elapsed_seconds)) %>%
    mutate(segment_number = seq(nrow(.))) %>%
    select(original_row, segment_number) %>%
    right_join(pof_df, by = 'original_row') %>%
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
#'  - score_points_this_team - number of points scored by reference team in that row
#'  - score_points_opponent_team - number of point scored by opponent team in that row
#'  - score_this_team_current - current score of the reference team in that row
#'  - score_opponent_team_current - current score of the oponent team in that row
#'  - score_game_status - one of 'winning', 'losing', 'tied' from perspective of reference team
#'  - score_differential - current reference team score minus current oponent team score
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
#'
#'
load_pbp_data <- function(game_id, team) {
  ## check the archive, if there is one, load and return, if not, load raw, process, archive and return
  pbp <- process_raw_pbp(game_id)
  team_id <- interpret_team(team)$id
  return(pbp[[glue('`{team_id}`')]])
}

#' Add segment information to custom play-by-play. This creates a segement id for every row
#' A segment is a contiguous period of time with the same 5 players on the court.
add_segment_data <- function(pof, tes) {
  #TODO
  # not sure how to handle this yet


}

