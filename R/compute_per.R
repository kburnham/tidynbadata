


#' Compute the PER (player efficiency ranking) for a single player
#' see https://en.wikipedia.org/wiki/Player_efficiency_rating
#' @param thp number of three pointers made
#' @param pf number of personal fouls
#' @param min number of minutes played
#' @param fg number of field goals made
#' @param ft number of free throws made
#' @param fta number of free throws attempted
#' @param ast number of assists
#' @param lg.ft number of league free throws
#' @param lg.pf number of league personal fouls
#' @param tm.ast number of team assists
#' @param tm.fg number of team field goals
#' @param orb number of offensive rebounds
#' @param trb number of total rebounds
#' @param to number of turnovers
#' @param vop value of play (see \code{compute_leauge_vop()})
#' @param drbp league defensize rebound percentage (see \code{compute_league_drbp})
#' @param factor league based adjustment factor based on assists, filed goals and free throws
#'
#'
compute_uper <- function(thp, pf, min, fg, ft, fta, ast, lg.ft,
                        lg.pf, tm.ast, tm.fg, orb, trb,
                        to, vop, drbp, factor) {
  # see https://en.wikipedia.org/wiki/Player_efficiency_rating
  uper <- 1/min * (thp + (2/3 * ast) + ((2 - factor * (tm.ast/tm.fg)) * fg)) -
    (vop * to) - (vop * drbp * (fga - fg)) - (vop * .44 * (.44+  (.56 * drbp)) * (fta - ft)) +
    (vop * (1 - drbp) * (trb - orb)) + (vop * drbp * orb) + (vop * stl) + (vop * drbp * blk) -
    (pf * ((lg.ft/lg.pf) -  (.44 * (lg.fta/lg.pf) * vop )))

  return(uper)

}

#' Convert the uPER rating for a player to PER
#'
#' @param uper the unadjusted per for the player
#' @param lg.pace league pace (possesions per game)
#' @param tm.pace team pace (possesions per game)
#' @param lgu.per league PER average
#'
#'
adjust_uper <- function(uper, lg.pace, tm.pace, lgu.per)
  uper * (lg.pave / tm.pace) * (15 / lgu.per)


#' Compute the league value of play (for the PER rating - see \code{compute_per()})
#'
#' @param lg.pts total leaguge points scored
#' @param lg.fga number of league field goals attempted
#' @param lg.orb number of league offensive rebounds
#' @param lg.to number of league turnovers
#' @param lg.fta number of league free throw attempts

compute_league_vop <- function(lg.pts, lg.fga, lg.orb, lg.to, lg.fta)
  lg.pts / (lg.fga - lg.orb + lg.to + .44 + lg.fta)


#' Compute league wide defensive rebounding percentage
#'
#' @param lg.trb total league rebounds
#' @param lg.orb total league offensive rebounds
#'
compute_league_drbd <- function(lg.trb, lg.orb) (lg.trb - lg.orb) / lg.trb






#' Compute the league factor for inclusion in Hollinger PER computation
#'
#' @param lg.ast total league assists
#' @param lg.fg total league field goals
#' @param

