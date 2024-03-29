% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pbp_audit_functions.R
\name{generate_substitution_row_from_desc}
\alias{generate_substitution_row_from_desc}
\title{Given a sentence of substitution data, this function returns a row of substitution data to be insterted into a pbp. This function combines the two functions \code{get_player_ids_from_desc()} and
\code{generate_substitution_row()} to generate a single row of substitution data from a short description of the substition.}
\usage{
generate_substitution_row_from_desc(
  desc,
  player_data,
  quarter,
  elapsed_time_in_quarter
)
}
\arguments{
\item{desc}{a seneence that describes the substition, be in the format "x enters the game for y"}

\item{player_data}{a data.frame of player data, as returned by \code{get_player_data()$api_json$players}}
}
\value{
a 1 row data.frame of substitution data
}
\description{
Given a sentence of substitution data, this function returns a row of substitution data to be insterted into a pbp. This function combines the two functions \code{get_player_ids_from_desc()} and
\code{generate_substitution_row()} to generate a single row of substitution data from a short description of the substition.
}
\seealso{
Other audit_functions: 
\code{\link{generate_substitution_row}()},
\code{\link{get_player_ids_from_desc}()},
\code{\link{read_boxscore_screen_shot}()},
\code{\link{remove_pbp}()}
}
\concept{audit_functions}
