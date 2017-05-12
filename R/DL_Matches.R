#' DL_Matches
#'
#' wrapper function that downloads multiple seasons for multiple league from one of the source websites
#'
#' @export
#' @import tidyverse
#' @param divs e.g. c("ENG1","FRA1")
#' @param season1 e.g. ("2001-2002")
#' @param season2 e.g. ("2005-2006")
#' @param website e.g. "FootballDataCoUk"
#' @return tibble for multiple seasons and leagues with all matches, results and bookmaker odds (if available)
#'

DL_Matches = function(divs,season1,season2,website="FootballDataCoUk"){
  if(website=="FootballDataCoUk"){
    Tt = DL_FootballDataCoUk(divs=divs,season1 = season1,season2 = season2)
  } else if(website=="XScores"){
    # TO DO
  } else if(website=="Soccerway"){
    # TO DO
  } else if(website=="BetExplorer"){
    # TO DO
  }
  Tt = CL_AddSeason(data = Tt)
  return(Tt)
}
