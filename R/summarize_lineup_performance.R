# summarize lineup performance



#' Summarize the performance of each unique lineup in the provided pbp data
#'
#' @param dat a tibble with play-by-play data
#' @param minimum_minutes lineups with fewer minutes played that this will be excluded
#'
#' @return a tibble with summary statistics for each unique lineup in the provided data
#'
summarize_lineup_performance <- function(dat, minimum_minutes) {
  lu_performance <- dat %>%
    group_by(gs_this_pof_id) %>%
    filter(!is.na(gs_this_pof_id)) %>%
    summarize(lineup_vec = first(gs_this_pof_vec),
              team = interpret_team(first(team_id))$abbr,
              minutes_played = round(sum(gs_seconds_until_next_event, na.rm = TRUE) / 60, 1),
              possessions = round(estimate_team_possessions_basic(.data), 1),
              pace = round(possessions / (minutes_played / 48), 4),
              efp = compute_effective_fgp(.data) %>% round(4),
              defp = compute_effective_defensive_fgp(.data) %>% round(4),
              tr = compute_turnover_rate(.data) %>% round(4),
              dtr = compute_defensive_turnover_rate(.data) %>% round(4),
              rr = compute_rebound_rate(.data) %>% round(4),
              drr = compute_defensive_rebound_rate(.data) %>% round(4),
              ftr = compute_free_throw_rate(.data) %>% round(4),
              dftr = compute_defensive_free_throw_rate(.data) %>% round(4),
              pm_per_min = compute_average_plus_minus(.data) %>% round(4)) %>%
    arrange(desc(minutes_played)) %>%
    filter(minutes_played > minimum_minutes)

  return(lu_performance)
}
