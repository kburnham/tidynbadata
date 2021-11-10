


#' Plot the output of \code{compare_lineups()} to display the statistical differences between the lineups
#'
#'
#' @param lineup_data data.frame as returned by \code{compare_lineups()}
#' @palette the RColorBrewer fill palette to use ('Set2' by default)
#' @export
#' @family plotting_functions
#'
#' @return a list with game data and multiple plots of lineup data
#'

plot_lineup_comparision <- function(lineup_data, palette = 'Set2') {

  ## because the 8 factors are all rates, it makes sense to do they separately
  colors <- RColorBrewer::brewer.pal(4, palette)
  names(colors) <- c('both', 'neither', 'lineup1', 'lineup2')

  factors_plot <- lineup_data %>% dplyr::select(lineup, efp, defp, tr, dtr, rr, drr, ftr, dftr) %>%
    tidyr::pivot_longer(2:9) %>%
    dplyr::mutate(name = factor(name, levels = c('efp', 'defp', 'rr', 'drr', 'tr', 'dtr', 'ftr', 'dftr'))) %>%
    ggplot2::ggplot(ggplot2::aes(x = lineup, y = value, fill = lineup)) + ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)), vjust = 1.1, color = 'white') +
    ggplot2::facet_wrap(~name, ncol = 2) +
    ggplot2::coord_cartesian() +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = 'lineup', values = colors) +
    ggtitle('Advanced Stats')

  ## unique line ups and unique games can be part of a table (or indicators)
  game_data <- lineup_data %>% dplyr::select(lineup, lineups, unique_lineup_count, games, unique_game_count)


  ## total minutews, +/- per minute and possesions can each be separate?
  minutes_plot <- lineup_data %>% dplyr::select(lineup, min) %>% ggplot2::ggplot(ggplot2::aes(x = reorder(lineup, -min), y = min, fill = lineup)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(min, 3)), vjust = 1.1, color = 'white') +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = 'lineup', values = colors) +
    ggplot2::xlab('lineup') +
    ggtitle('Minutes Played')

  possessions_plot <- lineup_data %>% dplyr::select(lineup, poss) %>% ggplot2::ggplot(ggplot2::aes(x = reorder(lineup, -poss), y = poss, fill = lineup)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(poss, 3)), vjust = 1.1, color = 'white') +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = 'lineup', values = colors) +
    ggplot2::xlab('lineup') +
    ggtitle('Possessions')

  pace_plot <- lineup_data %>% dplyr::select(lineup, pace) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(lineup, -pace), y = pace, fill = lineup)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(pace, 2)), vjust = 1.1, color = 'white') +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = 'lineup', values = colors) +
    ggplot2::xlab('lineup') +
    ggtitle('Pace')

  plus_minus_per_plot <- lineup_data %>% dplyr::select(lineup, `+/- min`) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(lineup, -`+/- min`), y = `+/- min`, fill = lineup)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(`+/- min`, 3)), vjust = 1.1, color = 'white') +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = 'lineup', values = colors) +
    ggplot2::xlab('lineup') +
    ggtitle('+\\-')


  return(list(factors = factors_plot,
              game_data = game_data,
              minutes = minutes_plot,
              possessions = possessions_plot,
              pace = pace_plot,
              plus_minus = plus_minus_per_plot))




}

