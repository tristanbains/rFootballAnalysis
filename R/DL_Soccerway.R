#' DL_Soccerway
#'
#' download multiple seasons for multiple leagues from www.soccerway.com
#'
#'
#' @export
#' @import tidyverse
#' @param divs e.g. c("ENG1","FRA1")
#' @param season1 e.g. ("2001-2002")
#' @param season2 e.g. ("2005-2006")
#' @param type what to download? Value can be "geo", "logo" "stadium", or "champion"
#' @param DL.input usually output of DL_Soccerway_data(). If NA then the function is called for all combinations of the other inputs
#' @return tibble for multiple seasons and leagues with all matches, results and bookmaker odds
#'

DL_Soccerway = function(divs,season1,season2,type,DL.input=NA){

}
