#' Summarize the performance of each unique lineup in the provided pbp data
#'
#' @param dat a tibble with play-by-play data
#' @param minimum_minutes lineups with fewer minutes played that this will be excluded
#' @param round round advanced stats to this many places
#' @param player_data a tibble with msf player data, used to create lineup_ids. If not provided, player ids are left uninterpreted
#' @param use_player_initials if TRUE (and if player_data provided) the lineup is shown as player initials + jersey number instead of last name
#' @export
#' @family summarize_lineup
#'
#' @return a tibble with summary statistics for each unique lineup in the provided data
#'
summarize_lineup_performance <- function(dat, minimum_minutes, round = 4, player_data = NULL, use_player_initials = FALSE) {
  lu_performance <- dat %>%
    group_by(gs_this_pof_id) %>%
    filter(!is.na(gs_this_pof_id)) %>%
    summarize(lineup_vec = list(first(gs_this_pof_vec)),
              team = interpret_team(first(team_id))$abbr,
              min = round(sum(gs_seconds_until_next_event, na.rm = TRUE) / 60, 1),
              `+/- min` = compute_average_plus_minus(.data) %>% round(round),
              poss = round(estimate_team_possessions_basic(.data), 1),
              pace = round(poss / (min / 48), round),
              efp = compute_effective_fgp(.data) %>% round(round),
              defp = compute_effective_defensive_fgp(.data) %>% round(round),
              tr = compute_turnover_rate(.data) %>% round(round),
              dtr = compute_defensive_turnover_rate(.data) %>% round(round),
              rr = compute_rebound_rate(.data) %>% round(round),
              drr = compute_defensive_rebound_rate(.data) %>% round(round),
              ftr = compute_free_throw_rate(.data) %>% round(round),
              dftr = compute_defensive_free_throw_rate(.data) %>% round(round),
              ) %>%
    arrange(desc(min)) %>%
    filter(min > minimum_minutes)


  if (!is.null(player_data)) {
    lu_performance <- lu_performance %>%
      rowwise() %>%
      mutate(lineup = if_else(use_player_initials, get_lineup_initials(lineup_vec, pd = player_data), get_lineup_last_names(lineup_vec, pd = player_data))) %>%
      select(-gs_this_pof_id) %>%
      select(lineup, everything())
  } else {
    lu_performance
  }

  return(lu_performance)
}


#' Given a set of pbp data and player id inclusions and exclusions for 2 different lineup deinfitions, return a lineup summary comparing the two provided lineups
#'
#' Any overlapping rows are removed and a summary of overlapping minutes is returned.
#'
#' @param pbp a pbp dataset
#' @param lineup1 a length two list with names "includes" and "excludes" defining the first lineup
#' @param lineup2 a length two list with names "includes" and "excludes" defining the second lineup
#'
#' @export
#' @family summarize_lineup
#' @return a 2 row data.frame of summarized pbp data, one row for each id
#'
#'
compare_lineups <- function(pbp, lineup1, lineup2, round = 4) {


  # add Boolean for first lineup

  pbp <- pbp %>% mutate(is_lineup1 = map_lgl(gs_this_pof_vec, filter_lineup, includes = lineup1$includes, excludes = lineup1$excludes),
                        is_lineup2 = map_lgl(gs_this_pof_vec, filter_lineup, includes = lineup2$includes, excludes = lineup2$excludes),
                        lineup = case_when(is_lineup1 & is_lineup2 ~ 'both',
                                           is_lineup1 ~ 'lineup1',
                                           is_lineup2 ~ 'lineup2',
                                           TRUE ~ 'neither'))

  summary <- pbp %>%
    dplyr::group_by(lineup) %>%
    dplyr::summarize(lineups = list(unique(gs_this_pof_id)),
                     unique_lineup_count = length(unlist(lineups)),
                     games = list(unique(game_id)),
                     unique_game_count = length(unlist(games)),
                     team = interpret_team(first(team_id))$abbr,
                     min = round(sum(gs_seconds_until_next_event, na.rm = TRUE) / 60, 1),
                     `+/- min` = compute_average_plus_minus(.data) %>% round(round),
                     poss = round(estimate_team_possessions_basic(.data), 1),
                     pace = round(poss / (min / 48), round),
                     efp = compute_effective_fgp(.data) %>% round(round),
                     defp = compute_effective_defensive_fgp(.data) %>% round(round),
                     tr = compute_turnover_rate(.data) %>% round(round),
                     dtr = compute_defensive_turnover_rate(.data) %>% round(round),
                     rr = compute_rebound_rate(.data) %>% round(round),
                     drr = compute_defensive_rebound_rate(.data) %>% round(round),
                     ftr = compute_free_throw_rate(.data) %>% round(round),
                     dftr = compute_defensive_free_throw_rate(.data) %>% round(round),
    )

  return(summary)
}

