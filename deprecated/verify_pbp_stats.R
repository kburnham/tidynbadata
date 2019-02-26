#' Compare the play-by-play computed team stats with the team stats provided by
#' mysportsfeeds as a quality check on the play-by-play data and its processing
#'
#' @param game_id an msf game id
#'
#' @return NULL this function is used for its side effects
#'
verify_pbp_team_stats <- function(game_id) {
  ## compute the tidynba team stats from the game
  msf_raw <- get_raw_msf_box_score(game_id)
  home_team_id = msf_raw$api_json$game$homeTeam$id
  away_team_id = msf_raw$api_json$game$awayTeam$id


  pbp_data <- load_pbp(game_id, team = home_team_id)
  tnd_teamstats <- compute_team_stats(pbp_data)
  msf_home_teamstats <- msf_raw$api_json$stats$home$teamStats
  msf_away_teamstats <- msf_raw$api_json$stats$away$teamStats

  ## make a vector mapping values expected to be equal in the two dfs
  expected_equal_home <- c(fga_this = "fieldGoals.fgAtt",
                           fga_made_this ="fieldGoals.fgMade",
                           three_att_this = "fieldGoals.fg3PtAtt",
                           three_made_this = "fieldGoals.fg3PtMade",
                           total_reb_this = "rebounds.reb",
                           def_reb_this = "rebounds.defReb",
                           off_reb_this = "rebounds.offReb",
                           fta_this = "freeThrows.ftAtt",
                           fta_made_this = "freeThrows.ftMade",
                           assists_this = "offense.ast",
                           steals_this = "defense.stl",
                           blocks_this = "defense.blk",
                           foul_this = "miscellaneous.fouls",
                           foul_tech_this = "miscellaneous.foulTech",
                           foul_flag1_this = "miscellaneous.foulFlag1",
                           foul_flag2_this ="miscellaneous.foulFlag2"
                           )
  # this makes the same map for the away team stats by replacing 'this' with 'opp' in the names
  expected_equal_away <- expected_equal_home %>%
    set_names(names(expected_equal_home) %>%
                str_replace('this', 'opp'))


  # create a names logical vector where the name is the tidynbadata team stat
  # and the value indicates whether or not it matches the value in the msf box score
  home_check = map2_lgl(expected_equal_home,
       names(expected_equal_home),
       ~(msf_home_teamstats[[.x]] == tnd_teamstats[[.y]]))

  away_check = map2_lgl(expected_equal_away,
                        names(expected_equal_away),
                        ~(msf_away_teamstats[[.x]] == tnd_teamstats[[.y]]))

  if (sum(c(home_check, away_check)) == 0) {
    message('All values were as expected')
    return(NULL)
  }
  message('There were differences between the team stats as computed by tidybnadata on the
          play-by-play data and the mysportsfeeds team stats object')

  tnd_values <- tnd_teamstats %>%
    select(one_of(names(home_check[!home_check])), one_of(names(away_check[!away_check])))



  walk(expected_equal_home, names(expected_equal_home), function(msf, tnd) {
    if (tnd %in% names(tnd_values)) {

      stat <- tnd
      tnd_value <- tnd_values[[stat]]
      msf_value <- msf_home_teamstats[[msf]]
      message(glue::glue("{tnd_value} {msf_value"))
    }
  })











  }







  report <- list(
    fga_this = tnd_teamstats$fga_this == msf_home_teamstats$fieldGoals.fgAtt,
    fga_opp = tnd_teamstats$fga_opp == msf_away_teamstats$fieldGoals.fgAtt

  )




  ## get the msf team stats for the game

  ## compare the two at the critical points and make are report comparing
  # and contrasting the two data sets


