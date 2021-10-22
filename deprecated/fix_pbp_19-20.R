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


    } else if (game_id == 47779) {
      if (plays$description[126] == "DeAndre Jordan added for start of quarter") {
        message("DeAndre Jordan incorrectly added as a 2nd quarter starter")
        plays <- plays %>% slice(-126)
      }
    } else if (game_id == 47785) {
      if (plays$description == "Kyrie Irving added for start of quarter") {
        plays <- plays %>% slice(-406)
      }
    } else if (game_id == 47812) {
      if (str_detect(plays$description[2], 'added for start of quarter')) {
        message('a player that was already a starter was inserted into the game in row 2')
        plays <- plays %>% slice(-2)
      }
    } else if (game_id == 47827) {
      if (plays$description[137] == "PJ Tucker added for start of quarter") {
        message("PJ Tucker incorrectly added for the start of the 2nd quarter")
        plays <- plays %>% slice(-137)
      }
    } else if (game_id == 47834) {
      if (str_detect(plays$description[2], 'added for start of quarter')) {
        message('a player that was already a starter was inserted into the game in row 2')
        plays <- plays %>% slice(-2)
      }
    } else if (game_id == 47838) {
      if (plays$description[418] == "Cheick Diallo added for start of quarter") {
        message("Cheick Diallo incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-418)
      }
    } else if (game_id == 47845) {
      if (nrow(plays %>% filter(playStatus.quarter == 3 &
                         playStatus.secondsElapsed == 0 &
                         str_detect(description, 'Juancho Hernangomez added'))) == 0) {
        message('Juancho Hernangomez was not added as a 3rd period starter')
        new_row <- tibble(description = "Juancho Hernangomez added for start of quarter",
                          substitution.incomingPlayer.id = 10135L,
                          total_elapsed_seconds = 1440L,
                          substitution.team.abbreviation = 'DEN',
                          substitution.team.id = 99L,
                          substitution.incomingPlayer.lastName = 'Hernangomez',
                          substitution.incomingPlayer.firstName = 'Juancho',
                          substitution.incomingPlayer.position = 'SF',
                          substitution.incomingPlayer.jerseyNumber = 41L,
                          playStatus.quarter = 3L,
                          playStatus.secondsElapsed = 0L
        )

        plays <- bind_rows(plays %>% slice(1:247),
                           new_row,
                           plays %>% slice((248):nrow(plays))
                           )

        ##also need to remove him at the start of 4th quarter
        new_row <- tibble(description = "Juancho Hernangomez removed for start of quarter",
                          substitution.outgoingPlayer.id = 10135L,
                          total_elapsed_seconds = 2160L,
                          substitution.team.abbreviation = 'DEN',
                          substitution.team.id = 99L,
                          substitution.outgoingPlayer.lastName = 'Hernangomez',
                          substitution.outgoingPlayer.firstName = 'Juancho',
                          substitution.outgoingPlayer.position = 'SF',
                          substitution.outgoingPlayer.jerseyNumber = 41L,
                          playStatus.quarter = 4L,
                          playStatus.secondsElapsed = 0L
        )

        plays <- bind_rows(plays %>% slice(1:352),
                           new_row,
                           plays %>% slice((353):nrow(plays))
        )
      }
    } else if (game_id == 47824) {
      if (plays$description[137] == "Marcus Morris removed for start of quarter") {
        message("Marcus Morris is removed twice at the start of quarter 2")
        plays <- plays %>% slice(-137)

      }

      if (plays$description[285] == "Marcus Morris removed for start of quarter") {
        message("Marcus Morris is removed twice at the start of quarter 3")
        plays <- plays %>% slice(-285)
      }


      if (plays$description[390] == "Marc added for start of quarter") {
        message("Marcus Morris incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-390)
      }
     } else if (game_id == 47812) {
      if (str_detect(plays$description[2], 'added for start of quarter')) {
        message('a player that was already a starter was inserted into the game in row 2')
        plays <- plays %>% slice(-2)
      }
    } else if (game_id == 47894) {
      if (plays$description[275] == "Vince Carter added for start of quarter") {
        message("Vince Carter did not play in game 47894, but was added and removed from the game")
        plays <- plays %>% slice(c(-275, -393))
      }
    } else if (game_id == 47898) {
      if (plays$description[382] == "Paul Millsap added for start of quarter") {
        message("Paul Milsap incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-382)
      }
    } else if (game_id == 47913) {
      if (plays$description[386] == "Stephen Curry added for start of quarter") {
        message("Steph Curry incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-386)
      }
    } else if (game_id == 47917) {
      if (plays$description[131] == "Karl-Anthony Towns added for start of quarter") {
        message("Karl-Anthony Towns incorrectly added as 2nd quarter starter")
        plays <- plays %>% slice(-131)
      }

    } else if (game_id  %in%  c(47938, 47952, 48024L, 48039L, 48048L, 48093L,
                                48151L, 48280L, 48353L, 48541L, 48579L)) {
      if (str_detect(plays$description[2], 'added for start of quarter')) {
        message('a player that was already a starter was inserted into the game in row 2')
        plays <- plays %>% slice(-2)
      }

    } else if (game_id == 47940) {
      if (plays$description[404] == "Jarrett Allen added for start of quarter") {
        message("Jarrett Allen incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-404)
      }
      message("Joe Harris should be added as 1OT starter")
      if (nrow(plays %>% filter(playStatus.quarter == 5 &
                                playStatus.secondsElapsed == 0 &
                                str_detect(description, 'Joe Harris added'))) == 0) {
        new_row <- tibble(description = "Joe Harris added for start of quarter",
                          substitution.incomingPlayer.id = 9540L,
                          total_elapsed_seconds = 2880L,
                          substitution.team.abbreviation = 'BRO',
                          substitution.team.id = 84L,
                          substitution.incomingPlayer.lastName = 'Harris',
                          substitution.incomingPlayer.firstName = 'Joe',
                          substitution.incomingPlayer.position = 'SF',
                          substitution.incomingPlayer.jerseyNumber = 12L,
                          playStatus.quarter = 5L,
                          playStatus.secondsElapsed = 0L
        )


        plays <- bind_rows(plays %>% slice(1:531),
                           new_row,
                           plays %>% slice((532):nrow(plays))
        )
      }

        if (plays$description[403] == "Jonas Valanciunas added for start of quarter") {
          message("Jonas Valanciunas incorrectly added as 4th quarter starter")
          plays <- plays %>% slice(-403)
        }
      } else if (game_id == 48081) {
        if (plays$description[115] == "Chris Paul added for start of quarter") {
          message("Chris Paul incorrectly added as 2nd quarter starter")
          plays <- plays %>% slice(-115)
        }
      } else if (game_id == 48092) {
        if (plays$description[420] == "Solomon Hill added for start of quarter") {
          message("Solomon Hill incorrectly added as 4th quarter starter")
          plays <- plays %>% slice(-420)
        }
      } else if (game_id == 48134) {
        if (nrow(plays %>% filter(playStatus.quarter == 5 &
                                  playStatus.secondsElapsed == 0 &
                                  str_detect(description, 'Tucker added'))) == 0) {
          new_row <- tibble(description = "P.J. Tucker added for start of quarter",
                            substitution.incomingPlayer.id = 9430L,
                            total_elapsed_seconds = 2880L,
                            substitution.team.abbreviation = 'HOU',
                            substitution.team.id = 109L,
                            substitution.incomingPlayer.lastName = 'Tucker',
                            substitution.incomingPlayer.firstName = 'P.J.',
                            substitution.incomingPlayer.position = 'SF',
                            substitution.incomingPlayer.jerseyNumber = 4L,
                            playStatus.quarter = 5L,
                            playStatus.secondsElapsed = 0L
          )

          plays <- bind_rows(plays %>% slice(1:498),
                             new_row,
                             plays %>% slice((499):nrow(plays))
          )
        }
      } else if (game_id == 48138) {
        if (nrow(plays %>% filter(playStatus.quarter == 5 &
                                  playStatus.secondsElapsed == 0 &
                                  str_detect(description, 'Wendell Carter Jr. added'))) == 0) {
          message("Wendell Carter Jr. was not added as a 5th quarter starter")
          new_row <- tibble(description = "Wendell Carter Jr. added for start of quarter",
                            substitution.incomingPlayer.id = 15204L,
                            total_elapsed_seconds = 2880L,
                            substitution.team.abbreviation = 'CHI',
                            substitution.team.id = 89L,
                            substitution.incomingPlayer.lastName = 'Carter Jr.',
                            substitution.incomingPlayer.firstName = 'Wendell',
                            substitution.incomingPlayer.position = 'C',
                            substitution.incomingPlayer.jerseyNumber = 34L,
                            playStatus.quarter = 5L,
                            playStatus.secondsElapsed = 0L
          )

          plays <- bind_rows(plays %>% slice(1:483),
                             new_row,
                             plays %>% slice((484):nrow(plays))
          )
        }

      } else if (game_id == 48169) {
        if (plays$description[359] == "Mike Muscala added for start of quarter") {
          message("Mike Muscala incorrectly added as 4th quarter starter")
          plays <- plays %>% slice(-359)
        }
      }
  else if (game_id == 48188) {
        if (nrow(plays %>% filter(playStatus.quarter == 5 &
                                  playStatus.secondsElapsed == 0 &
                                  str_detect(description, 'Ferguson added'))) == 0) {
          message("Terrance Ferguson was not added as a 5th quarter starter")
          new_row <- tibble(description = "Terrance Ferguson added for start of quarter",
                            substitution.incomingPlayer.id = 13748L,
                            total_elapsed_seconds = 2880L,
                            substitution.team.abbreviation = 'OKL',
                            substitution.team.id = 96L,
                            substitution.incomingPlayer.lastName = 'Ferguson',
                            substitution.incomingPlayer.firstName = 'Terrance',
                            substitution.incomingPlayer.position = 'SG',
                            substitution.incomingPlayer.jerseyNumber = 23L,
                            playStatus.quarter = 5L,
                            playStatus.secondsElapsed = 0L
          )

          plays <- bind_rows(plays %>% slice(1:495),
                             new_row,
                             plays %>% slice((496):nrow(plays))
          )
        }

    if (nrow(plays %>% filter(playStatus.quarter == 6 &
                              playStatus.secondsElapsed == 0 &
                              str_detect(description, 'Ferguson removed'))) == 0) {
      message("Terrance Ferguson was not removed at start of 2nd OT")
      new_row <- tibble(description = "Terrance Ferguson removed for start of quarter",
                        substitution.outgoingPlayer.id = 13748L,
                        total_elapsed_seconds = 3480L,
                        substitution.team.abbreviation = 'OKL',
                        substitution.team.id = 96L,
                        substitution.outgoingPlayer.lastName = 'Ferguson',
                        substitution.outgoingPlayer.firstName = 'Terrance',
                        substitution.outgoingPlayer.position = 'SG',
                        substitution.outgoingPlayer.jerseyNumber = 23L,
                        playStatus.quarter = 6L,
                        playStatus.secondsElapsed = 0L
      )

      plays <- bind_rows(plays %>% slice(1:532),
                         new_row,
                         plays %>% slice((533):nrow(plays))
      )
    }


  } else if (game_id == 48190) {
    if (plays$description[270] == "Furkan Korkmaz added for start of quarter") {
      message("Furkan Korkmaz incorrectly added as 3rd quarter starter")
      plays <- plays %>% slice(-270)
    }
  } else if (game_id == 48208) {
    if (plays$description[399] == "Thomas Bryant added for start of quarter") {
      message("Thomas Bryant incorrectly added for start of 4th quarter")
      plays <- plays %>% slice(-399)
    }
    message("Thomas Bryant not added at start of 2nd OT")
    new_row <- tibble(description = "Thomas Bryant added for start of quarter",
                      substitution.incomingPlayer.id = 13856L,
                      total_elapsed_seconds = 3480L,
                      substitution.team.abbreviation = 'WAS',
                      substitution.team.id = 94L,
                      substitution.incomingPlayer.lastName = 'Bryant',
                      substitution.incomingPlayer.firstName = 'Thomas',
                      substitution.incomingPlayer.position = 'C',
                      substitution.incomingPlayer.jerseyNumber = 31L,
                      playStatus.quarter = 6L,
                      playStatus.secondsElapsed = 0L
    )

    plays <- bind_rows(plays %>% slice(1:599),
                       new_row,
                       plays %>% slice((600):nrow(plays))
    )


    message("Serge Ibaka incorrectly added as 4th quarter starter")
    plays <- plays %>% slice(-398)


  } else if (game_id == 48239) {
    if (plays$description[551] == "Raymond Felton added for start of quarter") {
      message("Raymond Felton incorrectly added as OT starter")
      plays <- plays %>% slice(-551)
    }
  } else if (game_id == 48255) {
    if (plays$description[111] == "Nene added for start of quarter") {
      message("Nene added for start of quarter correctly, but then removed")
      new_row <- plays %>% slice(111)
      plays <- plays %>% slice(-111)
      plays <- bind_rows(plays %>% slice(1:117),
                         new_row,
                         plays %>% slice((118):nrow(plays))
      )

    }
  } else if (game_id == 48263) {
    if (plays$description[267] == "Dwight Powell added for start of quarter") {
      message("Dwight Powell incorrectly added as 3rd quarter starter")
      plays <- plays %>% slice(-267)
    }
  } else if (game_id == 48274) {
    if (plays$description[123] == "Devin Booker added for start of quarter") {
      message("Devin Booker incorrectly added as 2nd quarter starter")
      plays <- plays %>% slice(-123)
    }
  } else if (game_id == 48281) {
    if(plays$description[270] == "Robin Lopez added for start of quarter") {
      message("Robin Lopez incorrectly added as 3rd quarter starter")
      plays <- plays %>% slice(-270)
    }
  } else if (game_id == 48317) {

    if (str_detect(plays$description[2], 'added for start of quarter')) {
      message('a player that was already a starter was inserted into the game in row 2')
      plays <- plays %>% slice(-2)
    }

    if (plays$description[274] == "Lou Williams added for start of quarter") {
      message("Lou Williams incorrectly added as 3rd quarter starter")
      plays <- plays %>% slice(-274)
    }
  } else if (game_id == 48338) {
    if (plays$description[404] == "Boban Marjanovic added for start of quarter") {
      message("Boban Marjanovic incorrectly added as 4th quarter starter")
      plays <- plays %>% slice(-404)
    }
    if (plays$description[403] == "Tyson Chandler added for start of quarter") {
      message("Tyson Chandler incorrectly added as 4th quarter starter")
      plays <- plays %>% slice(-403)
    }
  } else if (game_id == 51287) {
    if(plays$description[362] == "Nene added for start of quarter (SUB)") {
      message("Nene was added and then removed at the start of the 4th quarter")
      new_row <- plays %>% slice(362)
      plays <- plays %>% slice(-362)
      plays <- bind_rows(plays %>% slice(1:364),
                         new_row,
                         plays %>% slice((365):nrow(plays))
      )
    }
  } else if (game_id == 48375) {
    if (plays$description[264] == "Ersan Ilyasova added for start of quarter") {
      message("Ersan Ilyasova incorrectly added as 3rd quarter starter")
      plays <- plays %>% slice(-264)
    }
  } else if (game_id == 48391) {
    if (plays$description[437] == "Draymond Green added for start of quarter") {
      message("Draymond Green incorrectly added as 4th quarter starter")
      plays <- plays %>% slice(-437)
    }
  } else if (game_id == 48422) {
    if (plays$description[593] == "Ante Zizic added for start of quarter") {
      message("Ante Zizic incorrectly added at 2nd OT starter")
      plays <- plays %>% slice(-593)
    }
  } else if (game_id == 48450) {
    if (nrow(plays %>% filter(playStatus.quarter == 5 &
                              playStatus.secondsElapsed == 0 &
                              str_detect(description, 'Ingles added'))) == 0) {

      message("Joe Ingles was not added at the start of the first OT")
      new_row <- tibble(description = "Joe Ingles added for start of quarter",
                        substitution.incomingPlayer.id = 9513L,
                        total_elapsed_seconds = 2880L,
                        substitution.team.abbreviation = 'UTA',
                        substitution.team.id = 98L,
                        substitution.incomingPlayer.lastName = 'Ingles',
                        substitution.incomingPlayer.firstName = 'Joe',
                        substitution.incomingPlayer.position = 'SF',
                        substitution.incomingPlayer.jerseyNumber = 2L,
                        playStatus.quarter = 5L,
                        playStatus.secondsElapsed = 0L
      )

      plays <- bind_rows(plays %>% slice(1:538),
                         new_row,
                         plays %>% slice((539):nrow(plays))
      )

      message("Joe Ingles should have been removed at start of 2OT")

      new_row <- tibble(description = "Joe Ingles removed for start of quarter",
                        substitution.outgoingPlayer.id = 9513L,
                        total_elapsed_seconds = 3180L,
                        substitution.team.abbreviation = 'UTA',
                        substitution.team.id = 98L,
                        substitution.outgoingPlayer.lastName = 'Ingles',
                        substitution.outgoingPlayer.firstName = 'Joe',
                        substitution.outgoingPlayer.position = 'SF',
                        substitution.outgoingPlayer.jerseyNumber = 2L,
                        playStatus.quarter = 6L,
                        playStatus.secondsElapsed = 0L
      )
      plays <- bind_rows(plays %>% slice(1:606),
                         new_row,
                         plays %>% slice((607):nrow(plays))
      )
      }
    } else if (game_id == 48461) {
      if (plays$description[118] == "Nene added for start of quarter") {

        message("Hilario Nene was added at the start of the 2nd quarter but then removed")
        plays <- plays %>% slice(-118)
        new_row <- tibble(description = "Nene added for start of quarter",
                          substitution.incomingPlayer.id = 9521L,
                          total_elapsed_seconds = 720L,
                          substitution.team.abbreviation = 'HOU',
                          substitution.team.id = 109L,
                          substitution.incomingPlayer.lastName = 'Nene',
                          substitution.incomingPlayer.firstName = '',
                          substitution.incomingPlayer.position = 'C',
                          substitution.incomingPlayer.jerseyNumber = 42L,
                          playStatus.quarter = 2L,
                          playStatus.secondsElapsed = 0L
        )
        plays <- bind_rows(plays %>% slice(1:121),
                           new_row,
                           plays %>% slice((122):nrow(plays))
        )
      }
    } else if (game_id == 48465) {
      if (plays$description[132] == "LaMarcus Aldridge added for start of quarter") {
        message("LaMarcus Aldridge incorrectly added as 2nd quarter starter")
        plays <- plays %>% slice(-132)
      }
    } else if (game_id == 48481) {
      if (plays$description[438] == "Dario Saric added for start of quarter") {
        message("Dario Saric incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-438)
      }
    } else if (game_id == 48489) {
      if (plays$description[449] == "Nikola Mirotic added for start of quarter") {
        message("Nikola Mirotic incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-449)
      }
    } else if (game_id == 48493) {
      if (plays$description[129] == "Kevin Knox added for start of quarter") {
        message('Kevin Knox incorrectly added as 2nd quarter starter')
        plays <- plays %>% slice(-129)
      }
      if (plays$description[128] == "Dennis Smith Jr. added for start of quarter") {
        message("Dennis Smith Jr. incorrectly added as 2nd quarter starter")
        plays <- plays %>% slice(-128)
      }
    } else if (game_id == 48494) {
      if (plays$description[114] == "Clint Capela added for start of quarter") {
        message("Clint Capela incorrectly added as 2nd quarter starter")
        plays <- plays %>% slice(-114)
      }
      if (plays$description[116] == "Justise Winslow added for start of quarter") {
        message('Justice Winslow incorrectly added as 2nd quarter starter')
        plays <- plays %>% slice(-116)
      }
    } else if (game_id == 48518) {
      if (plays$description[395] == "Reggie Jackson added for start of quarter") {
        message("Reggie Jackson incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-395)
      }

    } else if (game_id == 48603) {
      if (plays$description[414] == "Trae Young added for start of quarter") {
        message("Trae Young in correctly addes as 4th quarter starter")
        plays <- plays %>% slice(-414)
      }
    } else if (game_id == 48638) {
      if (plays$description[371] == 'Bobby Portis added for start of quarter') {
        message("Bobby Portis incorrectly added as 4th quarter starter")
        plays <- plays %>% slice(-371)
      }
    } else if (game_id == 48639) {
      if (nrow(plays %>% filter(playStatus.quarter == 5 &
                                playStatus.secondsElapsed == 0 &
                                str_detect(description, 'Caboclo added'))) == 0) {
        message("Bruno Caboclo was not added at the start of the first OT period")
        new_row <- tibble(description = "Bruno Caboclo added for start of quarter",
                          substitution.incomingPlayer.id = 9494L,
                          total_elapsed_seconds = 2880L,
                          substitution.team.abbreviation = 'MEM',
                          substitution.team.id = 107L,
                          substitution.incomingPlayer.lastName = 'Caboclo',
                          substitution.incomingPlayer.firstName = 'Bruno',
                          substitution.incomingPlayer.position = 'SF',
                          substitution.incomingPlayer.jerseyNumber = 20L,
                          playStatus.quarter = 5L,
                          playStatus.secondsElapsed = 0L
        )
        plays <- bind_rows(plays %>% slice(1:522),
                           new_row,
                           plays %>% slice((523):nrow(plays))
        )

      }
    } else if (game_id == 48653) {
      if (plays$description[105] == "Nene added for start of quarter") {

        message("Hilario Nene was added at the start of the 2nd quarter but then removed")
        plays <- plays %>% slice(-105)
        new_row <- tibble(description = "Nene added for start of quarter",
                          substitution.incomingPlayer.id = 9521L,
                          total_elapsed_seconds = 720L,
                          substitution.team.abbreviation = 'HOU',
                          substitution.team.id = 109L,
                          substitution.incomingPlayer.lastName = 'Nene',
                          substitution.incomingPlayer.firstName = '',
                          substitution.incomingPlayer.position = 'C',
                          substitution.incomingPlayer.jerseyNumber = 42L,
                          playStatus.quarter = 2L,
                          playStatus.secondsElapsed = 0L
        )
        plays <- bind_rows(plays %>% slice(1:112),
                           new_row,
                           plays %>% slice((113):nrow(plays))
        )
      }
    } else if (game_id == 48663) {
      if (plays$description[274] == "Yogi Ferrell added for start of quarter (SUB)") {
        message("Yogi Ferrell incorrectly added as 3rd quarter starter")
        plays <- plays %>% slice(-274)
      }
      if (plays$description[412] == "SUB: Hield FOR Ferrell") {
        message("Buddy Hield is already on the floor, but is subbed in for Ferrell (who is not on the floor)")
        plays <- plays %>% slice(-412)
      }
    }


  # need to reattach the fixed play data here
  raw_msf_pbp[['api_json']][['plays']] <- plays
  return(raw_msf_pbp)
}
