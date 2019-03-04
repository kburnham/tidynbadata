


#' This funtion is used to fix erroneous lineups on load
#'
#'
fix_lineup <- function(lu, game_id, team_id) {

  if (game_id == 47744 & team_id == 102) {
    message('Avery Bradley and Boban Marjanovic are both listed as starters in this game, but neither played
            Instead Shai Gilgeous-Alexander and Marcin Gortat are starters')

    lu$position[3] <- 'Bench'
    lu$position[9] <- 'Starter'
    lu$position[2] <- 'Bench'
    new_row <- data.frame(position = 'Starter',
                          player.id = 9519,
                          player.firstName = 'Marcin',
                          player.lastName = 'Gortat',
                          player.position = 'C',
                          player.jerseyNumber = 13)
    lu <- lu %>% bind_rows(new_row) %>% arrange(desc(position))



  }



  return(lu)

}
