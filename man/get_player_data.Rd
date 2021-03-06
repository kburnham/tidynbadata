% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_acquisition.R
\name{get_player_data}
\alias{get_player_data}
\title{downloads a data.frame of NBA player data from mysportsfeeds}
\usage{
get_player_data()
}
\arguments{
\item{force_reload}{when TRUE will ignore the archive and download new data}
}
\value{
a data.frame of player data
}
\description{
downloads a data.frame of NBA player data from mysportsfeeds
}
\seealso{
Other data_acquisition: \code{\link{get_lineup}},
  \code{\link{get_raw_msf_box_score}},
  \code{\link{get_raw_pbp}},
  \code{\link{get_team_schedule}}
}
\concept{data_acquisition}
