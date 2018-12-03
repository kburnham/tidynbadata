


## a plot score function that plots Knicks score and opponent score from start to finish

#' Plot the score differential of a game based on play-by-play data
#' @param pbp a data.frame of play-by-play data from a single game
#'
#' @return a ggplot object


plot_game_score <- function(pbp) {
  if (length(unique(pbp$team_id)) != 1)
    stop('The provided play-by-play data includes data from
         more than 1 game and is not suitable for use in this function.')

  team_name <- interpret_team(pbp$team_id[1])$name
  p <- pbp %>% ggplot(aes(x = total_elapsed_seconds / 60, y = score_differential)) +
    geom_line(color = '#006BB6') +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = c(12, 24, 36, 48, 53, 58, 63), size = .2) +
    coord_cartesian(xlim = c(2, max((pbp$total_elapsed_seconds / 60) - 3)),
                    ylim = c(min(-20, min(pbp$score_differential)), max(20, max(pbp$score_differential)))) +
    scale_x_continuous(name = '', breaks = c(6, 18, 30, 42, 51, 56, 61, 66, 71, 76, 81),
                       labels = c('Q1', 'Q2', 'Q3', 'Q4', 'OT1', 'OT2', 'OT3', 'OT4', 'OT5',
                                  'OT6', 'OT7')) +
    scale_y_continuous(name = '') +
    ggtitle(glue('{team_name} Lead'))
  return(p)
}



