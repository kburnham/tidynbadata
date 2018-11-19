
pbp$total_seconds <- ((pbp$playStatus.quarter - 1) * 720) + pbp$playStatus.secondsElapsed
pbp %>% filter(substitution.team.id == 83) %>% select(description, starts_with('substi')) %>% View



lu <- get_lineup(game_id, 'Knicks')
lu
starters <- lu %>% filter(position == 'Starter') %>% pull(player.id) %>% sort()
starters
pbp %>% select(starts_with('play')) %>% names





nyk_subs <- pbp %>% filter(substitution.team.id == 83 & !is.na(substitution.team.id))

#nyk_subs$substitution.outgoingPlayer.id[is.na(nyk_subs$substitution.outgoingPlayer.id)] <- ""


players_in <- nyk_subs$substitution.incomingPlayer.id
players_out <- nyk_subs$substitution.outgoingPlayer.id

pof <- starters
players_on_the_floor <- list()



for (i in seq(players_in)) {
  print(pof)
  pof <- process_sub(pof, players_in[i], players_out[i])
  print(i)
  players_on_the_floor[[i]] <- pof
}

nrow(nyk_subs)
length(players_on_the_floor)
View(tibble(pof = players_on_the_floor, time = nyk_subs$total_seconds))

length(players_in)


#'
#'


add_lineups_to_pbp <- function(game_id) {

  # check for an archived version
  check_archive()

  raw_pbp <- get_raw_pbp(game_id)
  pbp <- raw_pbp[['api_json']][['plays']]
  game <- raw_pbp[['api_json']][['game']]
  pbp[['original_row']] <- seq(1:nrow(pbp))
  pbp[['total_elapsed_seconds']] <- ((pbp$playStatus.quarter - 1) * 720) + pbp$playStatus.secondsElapsed


  # process each team separately first, join back later
  home_team <- game$homeTeam$id
  away_team <- game$awayTeam$id

  home_subs <- pbp %>% filter(substitution.team.id == home_team)
  away_subs <- pbp %>% filter(substitution.team.id == away_team)

  # helper function for inserting and removing players from a lineup vector
  process_sub <- function(current, player_in, player_out) {
    if (is.na(player_out)) return(c(current, player_in))
    if (is.na(player_in)) return(c(current[current != player_out]))
    c(current[current != player_out], player_in) %>% sort

  }

  # a function that figures out which players are on the floor for a given team for each second of the game
  process_lineups <- function(team, subs) {
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
    return(pof_df)

  }

  # process lineups for each team
  proc_home <- process_lineups(home_team, home_subs)
  proc_away <- process_lineups(away_team, away_subs)

  # join the original play-by-play to each teams lineup record
  final <- pbp %>% left_join(proc_home %>% select(-total_elapsed_seconds, home_team_pof = pof),
                             by = 'original_row',
                             suffix = c('', '.proc')) %>%
    left_join(proc_away %>% select(-total_elapsed_seconds, away_team_pof = pof),
              by = 'original_row',
              suffix = c('', '.proc')) %>%
    fill(home_team_pof, away_team_pof)


  return(final)


}


