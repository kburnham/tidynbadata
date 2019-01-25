#' Compute basic team level stats from play-by-play data
#'
#' Compute the number of points scored by a team and its opponent in a
#' given chunk of pbp data
#'
#' @param pbp_data a chunk of play-by-play data
#'
#' @export
#' @return a tibble summarizing basic team level stats typically found in a box score
#'

compute_team_stats <- function(pbp_data) {
  team_stats <- pbp_data %>%
    summarize(points_this = sum(gs_this_points_row, na.rm = TRUE),
              points_opp = sum(gs_opp_points_row, na.rm = TRUE),
              fga_this = sum(gs_event_type_detail == "fga_this", na.rm = TRUE),
              fga_opp = sum(gs_event_type_detail == "fga_opp", na.rm = TRUE),
              fga_made_this = sum(gs_event_type_detail == "fga_this" &
                                    fga_result == "make", na.rm = TRUE),
              fga_made_opp = sum(gs_event_type_detail == "fga_opp" &
                                   fga_result == "make", na.rm = TRUE),
              three_att_this = sum(gs_event_type_detail == "fga_this" &
                                 fga_points == 3, na.rm = TRUE),
              three_att_opp = sum(gs_event_type_detail == "fga_opp" &
                                fga_points == 3, na.rm = TRUE),
              three_made_this = sum(gs_event_type_detail == "fga_this" &
                                  fga_points == 3 & fga_result == "make",
                                  na.rm = TRUE),
              three_made_opp = sum(gs_event_type_detail == "fga_opp" &
                                  fga_points == 3 & fga_result == "make",
                                  na.rm = TRUE),
              total_reb_this = sum(gs_event_type_detail %in% c("oreb_this", "dreb_this")),
              total_reb_opp = sum(gs_event_type_detail %in% c("oreb_opp", "dreb_opp")),
              def_reb_this = sum(gs_event_type_detail == "dreb_this" &
                                   !is.na(reb_player), na.rm = TRUE),
              def_reb_opp = sum(gs_event_type_detail == "dreb_opp" &
                                  !is.na(reb_player), na.rm = TRUE),
              off_reb_this = sum(gs_event_type_detail == "oreb_this" &
                                   !is.na(reb_player), na.rm = TRUE),
              off_reb_opp = sum(gs_event_type_detail == "oreb_opp" &
                                  !is.na(reb_player), na.rm = TRUE),
              fta_this = sum(gs_event_type_detail == "fta_this"),
              fta_made_this = sum(gs_event_type_detail == "fta_this" &
                                    fta_result == "make", na.rm = TRUE),
              fta_opp = sum(gs_event_type_detail == "fta_opp"),
              fta_made_opp = sum(gs_event_type_detail == "fta_opp" &
                                   fta_result == "make", na.rm = TRUE),
              assists_this = sum(gs_event_type_detail == "fga_this" &
                                   !is.na(fga_assist_player)),
              assists_opp = sum(gs_event_type_detail == "fga_opp" &
                                  !is.na(fga_assist_player)),
              turnovers_this = sum(gs_event_type_detail == "to_this"),
              turnovers_opp = sum(gs_event_type_detail == "to_opp"),
              steals_this = sum(gs_event_type_detail == "to_opp" & to_isstolen),
              steals_opp = sum(gs_event_type_detail == "to_this" & to_isstolen),
              blocks_this = sum(gs_event_type_detail == "fga_opp" &
                                  fga_result == "block"),
              blocks_opp = sum(gs_event_type_detail == "fga_this" &
                                 fga_result == "block"),
              foul_this = sum(gs_event_type_detail == "foul_this", na.rm = TRUE),
              foul_opp = sum(gs_event_type_detail == "foul_opp", na.rm = TRUE),
              foul_tech_this = sum(gs_event_type_detail == "foul_this" &
                                     foul_istechnical, na.rm = TRUE),
              foul_tech_opp = sum(gs_event_type_detail == "foul_opp" &
                                    foul_istechnical, na.rm = TRUE),
              foul_flag1_this = sum(gs_event_type_detail == "foul_this" &
                                      foul_isflagrant1, na.rm = TRUE),
              foul_flag1_opp = sum(gs_event_type_detail == "foul_opp" &
                                     foul_isflagrant1, na.rm = TRUE),
              foul_flag2_this = sum(gs_event_type_detail == "foul_this" &
                                      foul_isflagrant2, na.rm = TRUE),
              foul_flag2_opp = sum(gs_event_type_detail == "foul_opp" &
                                     foul_isflagrant2, na.rm = TRUE),

              )

 return(team_stats)
}





