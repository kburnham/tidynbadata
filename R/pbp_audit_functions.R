#' Compute playing time for all players for a given game and team. This is used primarily to trouble shoot
#' players-on-the-floor issues caused by missing or extra substitutions
#'
#' @param pbp play by play data for exactly one full game
#' @param team the city, nickname, abbreviation or id of the desired team
#' @family pbp_audit
#' @export
#' @return a tibble indicating player id, last and first name, jersey number and minutes played

compute_game_playing_time <- function(pbp, team) {

  team_id <- interpret_team(team)$id
  if (length(pbp) == 2) pbp <- pbp[[as.character(team_id)]]
  game_id <- pbp$game_id[1]
  lineup <- get_lineup(game_id, team_id)
  player_ids <- lineup$player.id[!is.na(lineup$player.id)]

  # for every player, create a column in the data which is TRUE when they are on the floor, FALSE otherwise
  for (p in player_ids) pbp[[as.character(p)]] <- map_lgl(pbp$gs_this_pof_vec, ~p %in% .)

  # for each player, compute seconds played by
  minutes_played <- map_dbl(as.character(player_ids), ~sum(pbp$gs_seconds_until_next_event[pbp[[.]]])) / 60
  names(minutes_played) <- as.character(player_ids)
  pt <- enframe(minutes_played) %>% set_names(c('player_id', 'minutes_played'))
  pt <- pt %>% mutate(game_id = game_id, team_id = team_id, minutes_played = round(minutes_played, 1))

  players <- get_player_data()$api_json$players

  pt <- pt %>% left_join(players %>% mutate(player_id = as.character(player.id)),
                         by = 'player_id') %>%
    select(last = player.lastName,
           first = player.firstName,
           minutes_played,
           num = player.jerseyNumber,
           player_id, game_id, team_id)

  return(pt)

}


#' Audit the gs_this_pof_vec column for pbp data printing out informative messages to the console
#'
#' @param pbp a tibble of play-by-play data
#' @param team desired team's name, nickname, abbreviation or id
#' @param pt a data.frame of playing time from the game in question, as returned by the function \code{compute_game_playing_time()}
#' @param games a complete (raw) schedule of nba games
#' @family pbp_audit
#' @export
#' @return NULL


audit_pof_vec <- function(pbp, team, pt, games) {
  team_id <- interpret_team(team)$id
  if (length(pbp) == 2) pbp <- pbp[[as.character(team_id)]]
  game_id <- pbp$game_id[1]

  if (str_detect(pbp$gs_description[2], 'added for start')) {
  message('A player was inserted in row 2 of the data and
           probably should not have been')
    message('You may wish to use the following to create an entry in the fix_pbp ')
    message(glue('if (plays$description[2] == {pbp$plays_description[2]} {{
      message("The pbp data improperly inserts a starter in row 2")
      plays <- plays %>% slice(-2)
                 }}'))

    return(NULL)

  }

  pbp <- pbp %>% mutate(pof_length = map_int(gs_this_pof_vec, length),
                        bad_pof = pof_length != 5 & gs_seconds_until_next_event > 0)

  bad_summary <- pbp %>% count(bad_pof, gs_quarter) %>% filter(bad_pof)

  if (nrow(bad_summary) == 0) {
    message(glue('No bad rows found for game {game_id} team {team_id}'))
    return(NULL)
  }

  message(glue_data(bad_summary, 'There are {n} rows of bad pof data in quarter {gs_quarter}'))

  first_bad_pof <- pbp %>% filter(bad_pof) %>% slice(1) %>%
    unnest(gs_this_pof_vec) %>%
    mutate(gs_this_pof_vec = as.character(gs_this_pof_vec)) %>%
    select(gs_this_pof_vec, gs_quarter, gs_quarter_seconds_elapsed) %>%
    left_join(pt, by = c('gs_this_pof_vec' = 'player_id'))



  first_bad_pof %>% slice(1) %>%
    glue_data('The first row of bad data in the pbp occurs in quarter {gs_quarter}
              at {gs_quarter_seconds_elapsed} seconds')


    # message('The pbp data contains rows with 6 men on the floor at the same time. The players are:')
were_on_the_floor <- pbp %>% filter(gs_quarter == first_bad_pof$gs_quarter[1] &
                                      gs_quarter_seconds_elapsed == first_bad_pof$gs_quarter_seconds_elapsed[1] &
                                      sub_team == 'this' & str_detect(gs_description, 'added')) %>%
  separate(unique_id, into = c(NA, NA, 'row_number'), sep = '-') %>%
  left_join(first_bad_pof %>% transmute(pof_player_id = as.integer(gs_this_pof_vec),
                                        last, first, minutes_played), by = c('sub_player_in' = 'pof_player_id')) %>%
  select(player_id = sub_player_in, row_number, gs_description, last, first, minutes_played)

if (nrow(first_bad_pof) > 5) {
  message("There were too many players on the floor. One of these players should not have been inserted into the game")
  print(were_on_the_floor)

} else if (nrow(first_bad_pof < 5)) {
  message("There were too few players on the floor.")
  first_bad_ix <- min(which(pbp$bad_pof))
  message(glue("One of these players should be added after row {first_bad_ix - 1}"))
  print(pt %>% filter(!player_id %in% were_on_the_floor$player_id))



} else if (nrow(first_bad_pof) == 5) {
  message("no bad rows detected")
  return(NULL)
}



  games <- games %>% filter(schedule.id == game_id) %>%
    mutate(date = as.Date(ymd_hms(schedule.startTime), tz = 'EST'),
           home = schedule.homeTeam.abbreviation,
           away = schedule.awayTeam.abbreviation)


  message('you can search the following to find the box score:')
  message(glue_data(games, 'ESPN NBA boxscore {date} {interpret_team(home)$name} {interpret_team(away)$name}'))



}


#' convert a screen shot of ESPN playing time data to a data.frame
#'
#' @param infile the path to the screen shot, by default will use the most recent screen shot on the Desktop
#' @export
#' @family audit_functions
#' @return data.frame of playing time info


read_boxscore_screen_shot <- function(infile = 'most_recent_screen_shot') {

  if (infile == 'most_recent_screen_shot') {
    infile <- file.info(list.files("~/Desktop", full.names = T)) %>%
      mutate(path = row.names(.)) %>%
      filter(str_detect(path, 'Screen Shot')) %>%
      arrange(desc(mtime)) %>%
      head(1) %>%
      pull(path)
  }


  eng <- tesseract("eng")
  text <- tesseract::ocr(infile, engine = eng)

  f <- text %>% str_split('\n') %>% unlist() %>%
    str_split(' ') %>%
    keep(. != "") %>% transpose() %>%
    as.data.frame() %>%
    set_names(c('first_init', 'last_name',  'pos', 'min')) %>%
    filter(first_init != 'BENCH')
  #f
  return(f)

}




#' generate a row of substitution data for a pbp data.frame. This can be dput into fix pbp to add a missing row
#'
#' @param player_in_id the id of the player entering the game
#' @param player_out_id the id of the player leaving the game
#' @param player_data a data.frame of player data, as returned by \code{get_player_data()$api_json$players}
#' @param quarter the quarter in which the substitution should occur
#' @param elapsed_time_in_quarter number of seconds since the start of the quarter when the sub should be inserted
#' @export
#' @family audit_functions
#' @return a one row data.frame with substitution data. This can be inserted into a pbp data.frame


generate_substitution_row <- function(player_in_id, player_out_id, player_data, quarter, elapsed_time_in_quarter) {

  player_in <- player_data %>% filter(player.id == player_in_id) %>%
    select(first_name = player.firstName,
           last_name = player.lastName,
           position = player.primaryPosition,
           id = player.id,
           jersey_number = player.jerseyNumber,
           team_id = player.currentTeam.id,
           team_abbr = player.currentTeam.abbreviation)

  player_out <- player_data %>% filter(player.id == player_out_id) %>%
    select(first_name = player.firstName,
           last_name = player.lastName,
           position = player.primaryPosition,
           id = player.id,
           jersey_number = player.jerseyNumber,
           team_id = player.currentTeam.id,
           team_abbr = player.currentTeam.abbreviation)

  new_description <- as.character(glue("{player_in$first_name} {player_in$last_name} added for {player_out$first_name} {player_out$last_name}"))
  # we want ot make a single row of data subbing in the one player for the other
  new_row <- tibble(description = new_description,
                    substitution.incomingPlayer.id = as.integer(player_in$id),
                    substitution.outgoingPlayer.id = as.integer(player_out$id),
                    total_elapsed_seconds = compute_tes(quarter, elapsed_time_in_quarter),
                    substitution.team.abbreviation = player_in$team_abbr,
                    substitution.team.id = as.integer(player_in$team_id),
                    substitution.incomingPlayer.lastName = player_in$last_name,
                    substitution.incomingPlayer.firstName = player_in$first_name,
                    substitution.incomingPlayer.position = player_in$position,
                    substitution.incomingPlayer.jerseyNumber = as.integer(player_in$jersey_number),
                    substitution.outgoingPlayer.lastName = player_out$last_name,
                    substitution.outgoingPlayer.firstName = player_out$first_name,
                    substitution.outgoingPlayer.position = player_out$position,
                    substitution.outgoingPlayer.jerseyNumber = as.integer(player_out$jersey_number),
                    playStatus.quarter = as.integer(quarter),
                    playStatus.secondsElapsed = as.integer(elapsed_time_in_quarter))

  return(new_row)
}





#' Given a sentence of data from ESPN play by play about a substituon, return the player ids of the entering and exiting player. Note that this function only works when the players have a single first and last name (e.g. Julius Randle, but not J.R. Barrett)
#'
#' @param sen the text indicating the substition (e.g."Mitchell Robinson enters the game for Julius Randle")
#' @param player_data a data.frame of player data, as returned by \code{get_player_data()$api_json$players}
#' @family audit_functions
#' @export
#' @return a length two list with names "pi_id" and "po_id" giving the msf player ids of the two players involved in the substitution

get_player_ids_from_desc <- function(sen, player_data) {
  s <- sen %>% str_remove(' enters the game for')
  v <- s %>% str_split(' ') %>% unlist()
  if (length(v) != 4) stop ('length should be 4 - 2 last names and 2 first names')
  pi_id <- player_data %>% filter(player.firstName == v[1], player.lastName == v[2]) %>% pull(player.id)
  po_id <- player_data %>% filter(player.firstName == v[3], player.lastName == v[4]) %>% pull(player.id)

  return(list(player_in = pi_id, player_out = po_id))

}

#' Given a sentence of substitution data, this function returns a row of substitution data to be insterted into a pbp. This function combines the two functions \code{get_player_ids_from_desc()} and
#' \code{generate_substitution_row()} to generate a single row of substitution data from a short description of the substition.
#'
#' @param desc a seneence that describes the substition, be in the format "x enters the game for y"
#' @param player_data a data.frame of player data, as returned by \code{get_player_data()$api_json$players}
#' @export
#' @family audit_functions
#' @return a 1 row data.frame of substitution data


generate_substitution_row_from_desc <- function(desc, player_data, quarter, elapsed_time_in_quarter) {
  ids <- get_player_ids_from_desc(desc, player_data)
  row <- generate_substitution_row(player_in_id = ids$player_in, player_out_id = ids$player_out, player_data = player_data,
                                   quarter = quarter, elapsed_time_in_quarter = elapsed_time_in_quarter)

  return(row)

}


#' Delete an archived processed play-by-play data set. Typically this is done when we know that it contains errors and want to force a reprocessing
#'
#' @param game_id
#' @export
#' @family audit_functions
#' @return a Boolean indicting if the remove operation was successful
#'
#'
remove_pbp <- function(game_id) {
  file.remove(file.path(getOption('tidynbadata.archive_path'), 'processed_pbp_data', glue::glue('{game_id}.rds')))
}

