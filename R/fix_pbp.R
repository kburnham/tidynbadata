## pbp fix functions - for pbps with known errors, we fix those errors with the fix_pbp function


# I want to change this to input and output a full raw_msf_pbp list rather
# than just the pbp data


fix_pbp <- function(raw_msf_pbp) {
  game_id <- raw_msf_pbp[['api_json']][['game']][['id']]
  # detach the pbp data, but don't forget to reattach
  plays <- raw_msf_pbp[['api_json']][['plays']]
  ## sometimes there are 'player.id' columns that are tagged as just 'player'
  # find and fix those here
  old_names <- names(plays)
  names(plays) <- str_replace(names(plays), regex('player$'), 'player.id')
  if (!identical(names(plays), old_names(plays))) {
    walk(setdiff(names(plays), old_names),
         ~message(glue('{.} renamed from {str_replace(., ".id", "")}
                       for game {game_id}')))
  }



  vio_cols <- c("violation.type", "violation.teamOrPersonal", "violation.team.id",
                "violation.player.id")

  for (vc in vio_cols) if (!vc %in% names(plays)) plays[[vc]] <- NA
  ## make sure that all id columns are treated as integers
  id_cols <- pbp %>% select(ends_with('.id')) %>% names()
  for (col in id_cols) pbp[[col]] <- as.integer(pbp[[col]])

  if (game_id == 47712)  {
    ix <- which(pbp$description == "Emmanuel Mudiay added for start of quarter" & pbp$playStatus.quarter == 6)
    new_row = tibble(description = "Noah Vonleh added for start of quarter",
                     substitution.incomingPlayer.id = 9451L,
                     total_elapsed_seconds = 3180L,
                     substitution.team.abbreviation = 'NYK',
                     substitution.team.id = 83L,
                     substitution.incomingPlayer.lastName = 'Vonleh',
                     substitution.incomingPlayer.firstName = 'Noah',
                     substitution.incomingPlayer.position = 'PF',
                     substitution.incomingPlayer.jerseyNumber = 21L,
                     playStatus.quarter = 6L,
                     playStatus.secondsElapsed = 0L
    )
    pbp <- bind_rows(pbp %>% slice(1:ix),
                     new_row,
                     pbp %>% slice((ix + 1): nrow(pbp))
    )


  } else if (game_id == 47870) {
    # there is a rebound in this game that is called defensive,
    # but should be offensive
    ix <- 63
    pbp$rebound.type[ix] <- 'OFFENSIVE'
  } else {
    #
  }

  return(raw_msf_pbp)
}
