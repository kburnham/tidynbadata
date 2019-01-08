#' Basic data on 30 NBA teams
#'
#' a dataseat containing basic identification information for 30 NBA teams
#'
#' @format a data.frame with 30 rows and 4 variables:
#' \describe{
#'   \item{id}{two digit id (from mysportsfeeds) for identifying the team}
#'   \item{city}{the city (or other place name) with which the team is associated}
#'   \item{name}{The team's name (e.g. "Hawks", "Knicks", "Celtics")}
#'   \item{abbr}{three capital letters that identify the team (e.g "ATL", "NYK", "BOS")}
#' }
#' @source mysportsfeeds
"tidynbadata_team_info"


#' A sample data.frame of play-by-play data
#'
#' play-by-play data of game 47899. The data are from the perspective of the home
#' team (NY Knicks) so that 'this' columns refer to the Knicks and 'opp'
#' columns to the Bucks. Note that you can get play-by-play data from the same game,
#'  from the perspective of the Bucks with load_pbp(game_id = 47899, team = "Bucks')
#'
#' @format a data.frame with 584 rows and 65 columns of play-by-play data from a
#' single nba game
#' \describe{
#' \item{gs_description}{a short text description of the event}
#' \item{jb_wonby_team}{indicates who won the jump ball. Will be NA, 'this', 'opp' or 'neither'}
#' \item{jb_tippedtoplayer}{id of player to whom jump ball was tipped}
#' \item{jb_this_player}{id of player on reference team taking the jump ball}
#' \item{jb_opp_player}{id of player on opponent team taking the jump ball}
#' \item{gs_quarter}{the integer number of the quarter or overtime period (5 is first OT)}
#' \item{gs_quarter_seconds_elapsed}{number of game seconds elapsed since start of this quarter}
#' \item{gs_total_seconds_elapsed}{number of game seconds elapsed since start of this game}
#' \item{gs_this_pof_vec}{a length 5 vector of players on the floor for the reference team}
#' \item{gs_opp_pof_vec}{a length 5 vector of players on the floor for the opposing team}
#' \item{gs_this_pof_id}{a hyphen separated string of player ids, sorted low to high, concatenated from gs_this_pof_vec}
#' \item{gs_opp_pof_id}{a hyphen separated string of player ids, sorted low to high, concatenated from gs_opp_pof_vec}
#' \item{gs_seconds_until_next_event}{an integer indicating how many seconds
#'  until the occurence of the next event (row) in the data. Use this column to compute playing time}
#'}
"tidynbadata_pbp"
