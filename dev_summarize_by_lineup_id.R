




#' subset a pbp datset according to players that must be, or must not be, in the current lineup
#'
#' @param pbp a processed play-by-play dataset
#' @param includes a vector of player ids that must be in  \code{gs_this_pof_vec}
#' @param excludes a vector of player ids that myst not be in \code{gs_this_pof_vec}
#' @export
#' @family lineup_helpers
#' @return a pbp dataset subset according to includes and excludes
#'

subset_pbp_by_player_ids <- function(pbp, includes, exludes) {
  return(pbp %>% filter(filter_lineup(gs_this_pof_vec, includes, exludes)))
}



#' Given a subset of pbp data return a data.frame of advances stats summarizing the data
#'
#' @pbp_dub subset of pbp data to summarize
#' @export
#' @family lineup_helpers
#' @return a data.frame summarizing key statistics from the provided pbp file
#'
#'
#' need to think about:
#' how do we handle two lineups with overlapping minutes (i.e. some lineups are TRUE for both lineup definitions)
#' for now: tag every row as T/F with a new column for each lineup definition
#'
#' if we allow overlap to occur we can't just group by because we will need some rows 2x
#' if we try to prevent overlap it's easy enough if the 2 comparison case, but
#' what if we try to compare 3 lineups? We can remove rows that match for all three definitions
#' but what about rows that match for 2 definitions
#' to do 3 you would need to:
#' ------
#' simplest thing to do is retrict to 2 lineup definitions
#' this allows you to just drop rows that are overlap (after summarizing them)
#' or perhaps makign this a function parameter
#' also drop rows that match neither id (XOR!)
#' then you can group by the boolean (either one) and summarize
#' ------
#' however, to allow for comparisions between multiple ids would require
#' for EACH lineup id:
#'  - subset pbp to rows matching that id
#'  - inner join each subset pbp with each other subset pbp and summarize total minutes in each inner join? (or do a complete summarize_lineup on it?)
#'  - summarize each subset of lineup data
#'  - stack and summarize the individual lineup performances
#'  final summary will indicate how mnay minutes of overlap each lineup definition has with each other
#'  but is this enough?
#'
#'
#'  FOR NOW ----------
#'   option A because it is easier, but if we want to expand later it's like starting over

summarize_pbp_subset <- function(pbp_sub) {
 summ <- pbp_sub %>% summarize(lineups = list(gs_this_pof_id),
                               unique_lineup_count =
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
}



