## pbp fix functions - for pbps with known errors, we fix those errors with the fix_pbp function



fix_pbp <- function(pbp, game_id) {

  ## if no violations occur, those columns are not part of the export, but we want to add them
  if ('violation.player' %in% names(pbp) & !'violation.player.id' %in% names(pbp)) {
    pbp$violation.player.id <- pbp$violation.player
    pbp$violation.player <- NULL
  }
  if (!'violation.type' %in% names(pbp)) pbp$violation.type <- NA_character_
  if (!'violation.player.id' %in% names(pbp)) pbp$violation.player.id <- NA_integer_
  if (!'violation.team.id' %in% names(pbp)) pbp$violation.team.id <- NA_integer_
  if (!'violation.teamOrPersonal' %in% names(pbp)) pbp$violation.teamOrPersonal <- NA_character_
  if (!'violation.team.abbreviation' %in% names(pbp)) pbp$violation.team.abbreviation <- NA_character_
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
    # these is a rebound in this game that is called defensive, but should be offensive
    ix <- 63
    pbp$rebound.type[ix] <- 'OFFENSIVE'
  } else {
    #
  }

  return(pbp)
}
