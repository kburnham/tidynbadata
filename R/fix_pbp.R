## pbp fix functions - for pbps with known errors, we fix those errors with the fix_pbp function


fix_pbp <- function(raw_msf_pbp) {
  game_id <- raw_msf_pbp[['api_json']][['game']][['id']]
  # detach the pbp data, but don't forget to reattach
  plays <- raw_msf_pbp[['api_json']][['plays']]
  ## sometimes there are 'player.id' columns that are tagged as just 'player'
  # find and fix those here
  old_names <- names(plays)
  names(plays) <- str_replace(names(plays), regex('Player$'), 'Player.id')
  if (!identical(names(plays), old_names)) {
    walk(setdiff(names(plays), old_names),
         ~message(glue('{.} renamed from {str_replace(., ".id", "")}
                       for game {game_id}')))
  }

  vio_cols <- c("violation.type", "violation.teamOrPersonal", "violation.team.id",
                "violation.player.id")

  for (vc in vio_cols) if (!vc %in% names(plays)) plays[[vc]] <- NA
  ## make sure that all id columns are treated as integers
  id_cols <- plays %>% select(ends_with('.id')) %>% names()
  for (col in id_cols) plays[[col]] <- as.integer(plays[[col]])


  ## this conditional is used to fix data with known flaws
  if (game_id == 66675) {
    # start of 4th Q subs are missing
    # msf has c(9142, 9282, 9399, 17181, 27591)
    # espn shows these subs at start of 4th
    # Alec Burks enters the game for Evan Fournier
    # Mitchell Robinson enters the game for Julius Randle

    new_row = tibble(description = "Mitchell Robinson added for Julius Randle",
                     substitution.incomingPlayer.id = 15282L,
                     substitution.outgoingPlayer.id = 9282L,
                     total_elapsed_seconds = 2160L,
                     substitution.team.abbreviation = 'NYK',
                     substitution.team.id = 83L,
                     substitution.incomingPlayer.lastName = 'Robinson',
                     substitution.incomingPlayer.firstName = 'Mitchell',
                     substitution.incomingPlayer.position = 'C',
                     substitution.incomingPlayer.jerseyNumber = 23L,
                     substitution.outgoingPlayer.lastName = 'Randle',
                     substitution.outgoingPlayer.firstName = 'Julius',
                     substitution.outgoingPlayer.position = 'C',
                     substitution.outgoingPlayer.jerseyNumber = 30L,
                     playStatus.quarter = 4L,
                     playStatus.secondsElapsed = 0L)

    new_row2 = tibble(description = "Alec Burks added for Evan Fournier",
                      substitution.incomingPlayer.id = 9507L,
                      substitution.outgoingPlayer.id = 9399L,
                      total_elapsed_seconds = 2160L,
                      substitution.team.abbreviation = 'NYK',
                      substitution.team.id = 83L,
                      substitution.incomingPlayer.lastName = 'Alec',
                      substitution.incomingPlayer.firstName = 'Burks',
                      substitution.incomingPlayer.position = 'SG',
                      substitution.incomingPlayer.jerseyNumber = 18L,
                      substitution.outgoingPlayer.lastName = 'Evan',
                      substitution.outgoingPlayer.firstName = 'Fournier',
                      substitution.outgoingPlayer.position = 'SG',
                      substitution.outgoingPlayer.jerseyNumber = 13L,
                      playStatus.quarter = 4L,
                      playStatus.secondsElapsed = 0L)


    new_plays <- bind_rows(plays %>% slice(1:351),
                           new_row,
                           new_row2,
                           plays %>% slice(352:nrow(plays))
    )
    plays <- new_plays

    new_row = tibble(description = "Obi Toppin enters the game for Alec Burks",
                     substitution.incomingPlayer.id = 27591L,
                     substitution.outgoingPlayer.id = 9507L,
                     total_elapsed_seconds = 3180L,
                     substitution.team.abbreviation = 'NYK',
                     substitution.team.id = 83L,
                     substitution.incomingPlayer.lastName = 'Obi',
                     substitution.incomingPlayer.position = 'PF',
                     substitution.incomingPlayer.jerseyNumber = 1L,
                     substitution.outgoingPlayer.lastName = 'Burks',
                     substitution.outgoingPlayer.firstName = 'Alec',
                     substitution.outgoingPlayer.position = 'SG',
                     substitution.outgoingPlayer.jerseyNumber = 18L,
                     playStatus.quarter = 5L,
                     playStatus.secondsElapsed = 0L)



    new_plays <- bind_rows(plays %>% slice(1:467),
                           new_row,
                           plays %>% slice(468:nrow(plays))
    )

    # line 474 appears to be bogus

    plays <- bind_rows(plays %>% slice(1:473),
                      plays %>% slice(475:nrow(plays)))




  }


  # need to reattach the fixed play data here
  raw_msf_pbp[['api_json']][['plays']] <- plays
  return(raw_msf_pbp)
}
