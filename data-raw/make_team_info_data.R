## make the team data tibble


tidynbadata_team_info <- structure(list(team_id = c(91L, 82L, 84L, 93L, 89L, 86L, 108L, 99L,
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
devtools::use_data(tidynbadata_team_info)
