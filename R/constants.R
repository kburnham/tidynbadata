#' tidynbadata constant environment
#'
#' A cache environment to store ours constants that are referenced in package functions.
#'
#' @details use \code{ls(tidynbadata)} to see a list of currently available objects in this
#'   environment
#'
#' @export
tidynbadata <- new.env()

tidynbadata$DEFAULT_TEAM <- 'NYK'
tidynbadata$CURRENT_SEASON <- '2018-2019-regular'
tidynbadata$MSF_VERSION <- '2.0'
tidynbadata$ARCHIVE_DIR <- file.path(Sys.getenv('HOME'), 'tidynbadata_archive')
# set the hour after which daily updates are made (0 is midnight)
tidynbadata$UPDATE_TIME <- 4


## hard code teaminfo - actually better to create a data set

## this is how to get the hard data:

# standings <- msf_get_results(version = '2.0',
#                              league = 'nba',
#                              season = tidynbadata$CURRENT_SEASON,
#                              feed = 'seasonal_standings')
#
#
# standings$api_json$teams %>%
#   select(id = team.id,
#          city = team.city,
#          name = team.name,
#          abbr = team.abbreviation) %>%
#   dput()

tidynbadata$TEAM_DATA <- structure(list(team_id = c(91L, 82L, 84L, 93L, 89L, 86L, 108L, 99L,
                                               88L, 101L, 109L, 87L, 102L, 105L, 107L, 92L, 90L, 100L, 110L,
                                               83L, 96L, 95L, 85L, 104L, 97L, 103L, 106L, 81L, 98L, 94L),
                                        city = c("Atlanta",
                                                 "Boston", "Brooklyn", "Charlotte", "Chicago", "Cleveland", "Dallas",
                                                 "Denver", "Detroit", "Golden State", "Houston", "Indiana", "Los Angeles",
                                                 "Los Angeles", "Memphis", "Miami", "Milwaukee", "Minnesota",
                                                 "New Orleans", "New York", "Oklahoma City", "Orlando", "Philadelphia",
                                                 "Phoenix", "Portland", "Sacramento", "San Antonio", "Toronto",
                                                 "Utah", "Washington"),
                                        name = c("Hawks", "Celtics", "Nets", "Hornets",
                                                 "Bulls", "Cavaliers", "Mavericks", "Nuggets", "Pistons", "Warriors",
                                                 "Rockets", "Pacers", "Clippers", "Lakers", "Grizzlies", "Heat",
                                                 "Bucks", "Timberwolves", "Pelicans", "Knicks", "Thunder", "Magic",
                                                 "76ers", "Suns", "Trail Blazers", "Kings", "Spurs", "Raptors",
                                                 "Jazz", "Wizards"),
                                        abbr = c("ATL", "BOS", "BRO", "CHA", "CHI",
                                                "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL",
                                                "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKL", "ORL", "PHI",
                                                "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")),
                                        class = "data.frame",
                                   row.names = c(NA, 30L), .Names = c("id", "city", "name", "abbr"))

