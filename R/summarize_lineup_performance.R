# summarize lineup performance



#' Summarize the performance of each unique lineup in the provided pbp data
#'
#' @param dat a tibble with play-by-play data
#' @param minimum_minutes lineups with fewer minutes played that this will be excluded
#' @param round round advanced stats to this many places
#' @param player_data a tibble with msf player data, used to create lineup_ids. If not provided, player ids are left uninterpreted
#' @export
#'
#' @return a tibble with summary statistics for each unique lineup in the provided data
#'
summarize_lineup_performance <- function(dat, minimum_minutes, round = 4, player_data = NULL) {
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
      mutate(lineup = get_lineup_last_names(lineup_vec, pd = player_data)) %>%
      select(-gs_this_pof_id) %>%
      select(lineup, everything())
  } else {
    lu_performance
  }

  return(lu_performance)
}
