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
  if (game_id == 47712)  {
    # 47712 ----
    if (plays$description[592] == "Emmanuel Mudiay added for start of quarter") {
      message('Noah Vonleh was not added for the start of the 2nd OT')
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
                       playStatus.secondsElapsed = 0L)

      new_row2 = tibble(description = "Justin Holiday added for start of quarter",
                       substitution.incomingPlayer.id = 9155L,
                       total_elapsed_seconds = 3180L,
                       substitution.team.abbreviation = 'CHI',
                       substitution.team.id = 89L,
                       substitution.incomingPlayer.lastName = 'Holiday',
                       substitution.incomingPlayer.firstName = 'Justin',
                       substitution.incomingPlayer.position = 'SG',
                       substitution.incomingPlayer.jerseyNumber = 7L,
                       playStatus.quarter = 6L,
                       playStatus.secondsElapsed = 0L)




      new_plays <- bind_rows(plays %>% slice(1:592),
                             new_row,
                             new_row2,
                             plays %>% slice(593:nrow(plays))
                             )


      plays <- new_plays


    }



  } else if (game_id == 47870) {
    # 47870 ----
    # there is a rebound in this game that is called defensive,
    # but should be offensive
    ix <- 63
    plays$rebound.type[ix] <- 'OFFENSIVE'
  } else if (game_id == 47608) {
    # 47608 ----
    # the pbp data inserts PJ Tucker as a sub at 0 seconds, even though
    # he is already a starter
    if (plays$description[2] == "PJ Tucker added for start of quarter (SUB)") {
      plays <- plays %>% slice(-2)
    }
  } else if (game_id == 47615) {
    # 47615 ----

    if (plays$description[437] == "Markieff Morris added for start of quarter") {
      message('For game 47615 - Markieef Morris is erroneously added as a 4th quarter sub,
              but he does not enter the game until later. Removing row 437.')
      plays <- plays %>% slice(-437)
    }
  } else if (game_id == 47617) {
    # 47617 ----
    if (plays$description[413] == "JaVale McGee added for start of quarter") {
      message("For game 47617 - Javale McGee is added as a 4th quarter starter,
            but should not have been ")
      plays <- plays %>% slice(-413)
    }

  } else if (game_id == 47618) {
    # 47618 ----
    if (str_detect(plays$description[2], "Landry Shamet added for start of quarter")) {
      message('for game 47618 Landry Shamet was erroneously added to start
              the 1st quarter even though he was already a starter')
      plays <- plays %>% slice(-2)
    }
  } else if (game_id == 47655) {
    # 47655 ----
    if (plays$description[397] == "Boban Marjanovic added for start of quarter") {
      message("For game 47655 - Boban Marjanovic is added as a 4th quarter starter,
            but should not have been ")
      plays <- plays %>% slice(-397)
    }
  } else if (game_id == 47673) {
    # 47673----
    if (!any(plays$substitution.incomingPlayer.id[plays$playStatus.quarter == 5] == 9425, na.rm = T)) {
      message('Game 47673: Ish Smith was a 1st OT starter, but is not included in the pbp data')
      new_row = tibble(description = "Ish Smith added for start of quarter",
                       substitution.incomingPlayer.id = 9425L,
                       total_elapsed_seconds = 2880L,
                       substitution.team.abbreviation = 'DET',
                       substitution.team.id = 88L,
                       substitution.incomingPlayer.lastName = 'Smith',
                       substitution.incomingPlayer.firstName = 'Ish',
                       substitution.incomingPlayer.position = 'PG',
                       substitution.incomingPlayer.jerseyNumber = 14L,
                       playStatus.quarter = 5L,
                       playStatus.secondsElapsed = 0L
      )

      ix <- which(plays$description == "Andre Drummond added for start of quarter" &
                    plays$playStatus.quarter == 5)

      plays <- bind_rows(plays %>% slice(1:ix),
                             new_row,
                             plays %>% slice((ix + 1):nrow(plays))
                             )

      }
    } else if (game_id == 47691) {
      # 47691 ----
      if (plays$description[2] == "Kyle Anderson added for start of quarter (SUB)") {
        message('the pbp data inserts Kyle Anderson as a sub at 0 seconds, even though
                 he is already a starter')
        plays <- plays %>% slice(-2)
      }

    } else if (game_id == 47697) {
      if (plays$description[261] == "Vince Carter added for start of quarter") {
        message('Vince Carter is erroneously added as a 3rd quarter starter')
        plays <- plays %>% slice(-261)
      }
    } else if (game_id == 47709) {
      if (plays$description[451] == "Bam Adebayo added for start of quarter") {
        message("Game 47709 Bam Adebayo erroneously added as a 4th quarter starter")
        plays <- plays %>% slice(-451)
      }
    } else if (game_id == 47725) {
      if (plays$description[2] == "Dante Cunningham added for start of quarter (SUB)") {
        message('the pbp data inserts Dante Cunningham as a sub at 0 seconds, even though
                 he is already a starter')
        plays <- plays %>% slice(-2)
      }
    } else if (game_id == 47729) {
      if (plays$description[126] == "Salah Mejri added for start of quarter") {
        message("Salah Mejri erroneously added as 2nd quarter starter")
        plays <- plays %>% slice(-126)
      }
    } else if (game_id == 47733) {
      if (plays$description[394] == "Al Horford added for start of quarter") {
        message("Al Horford erroneously added as 4th quarter starter")
        plays <- plays %>% slice(-394)
        }
    } else if (game_id  == 47741) {
      if (plays$description[2] == "Derrick Favors added for start of quarter (SUB)") {
        message('the pbp data inserts Derrick Favors as a sub at 0 seconds, even though
                 he is already a starter')
        plays <- plays %>% slice(-2)
      }
    } else if (game_id == 47779) {
      if (plays$description[126] == "DeAndre Jordan added for start of quarter") {
        message("DeAndre Jordan erroneously added as 2nd quarter starter")
        plays <- plays %>% slice(-126)
      }
    } else if (game_id == 47785) {
      if (plays$description[406] == "Kyrie Irving added for start of quarter") {
        message("Kyrie Irving erroneously added as 2nd quarter starter")
        plays <- plays %>% slice(-406)
      }
     message("Marcus  Morris was not added at the start of the first OT, but should have been")
      new_row = tibble(description = "Marcus Morris added for start of quarter",
                       substitution.incomingPlayer.id = 9207L,
                       total_elapsed_seconds = 2880L,
                       substitution.team.abbreviation = 'BOS',
                       substitution.team.id = 82L,
                       substitution.incomingPlayer.lastName = 'Morris',
                       substitution.incomingPlayer.firstName = 'Marcus',
                       substitution.incomingPlayer.position = 'PF',
                       substitution.incomingPlayer.jerseyNumber = 13L,
                       playStatus.quarter = 5L,
                       playStatus.secondsElapsed = 0L
      )

      plays <- bind_rows(plays %>% slice(1:528),
                         new_row,
                         plays %>% slice((529):nrow(plays))
      )


    } else if (game_id == 47809) {
      if (plays$description[254] == "Zaza Pachulia added for start of quarter") {
        message("Zaza Pachulia was erroneously added as a 3rd quarter starter.")
        plays <- plays %>% slice(-254)
      }
      if (plays$description[383] == "Reggie Jackson added for start of quarter") {
        message("Reggie Jackson was erroneously added as a 4th quarter starter.")
        plays <- plays %>% slice(-383)
      }

      if (plays$description[253] == "Ante Zizic added for start of quarter") {
        message("Ante Zizic was erroneously added as a 3rd quarter starter.")
        plays <- plays %>% slice(-253)
      }
    } else if (game_id == 47812) {
      if (plays$description[2] == "Omari Spellman added for start of quarter (SUB)") {
        message("Omari Spellman added at start of game, but was aleady a starter")
        plays <- plays %>% slice(-2)
      }
    }  else if (game_id == 47818) {
      if (plays$description[123] == "Markieff Morris removed for start of quarter") {
        message("Markieef Morris was correctly added for the start of Q2, but was then dropped as well")
        plays <- plays %>% slice(-123)
      }
      if (plays$description[413] == "Markieff Morris removed for start of quarter") {
        message("Markieef Morris was correctly added for the start of Q4, but was then dropped as well")
        plays <- plays %>% slice(-413)
      }
      if (plays$description[250] == "Mark Morris added for start of quarter") {
        message('Markieef Morris incorrectly added as a 3rd quarter starter')
        plays <- plays %>% slice(-250)
      }


    }



  # if (str_detect(plays$description[2], 'added for start of quarter')) {
  #   message(glue('For game {game_id} there is a sub added for row 2, that was probably already a starter'))
  #   plays <- plays %>% slice(-2)
  # }
  # need to reattach the fixed play data here
  raw_msf_pbp[['api_json']][['plays']] <- plays


  return(raw_msf_pbp)
}
