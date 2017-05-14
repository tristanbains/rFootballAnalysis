#' DL_Soccerway
#'
#' download multiple seasons for multiple leagues from www.soccerway.com
#'
#'
#' @export
#' @import tidyverse
#' @param divs e.g. c("ENG1","FRA1")
#' @param season1 e.g. "2012-2013"
#' @param season2 e.g. "2013-2014"
#' @param type what to download? Value can be "geo", "logo" "stadium", or "champion"
#' @param DL.input usually output of DL_Soccerway_data(). If NA then the function is called for all combinations of the other inputs
#' @example DL_Soccerway(divs=c("ENG1","FRA1"),season1="2012-2013",season2="2013-2014",type="geo")
#' @return tibble for multiple seasons and leagues with all matches, results and bookmaker odds
#'

DL_Soccerway = function(divs,season1,season2,type,DL.input=NA){
  # Go into a loop per season:
  if(type != "champion"){
    # Download the appropriate url data if DL.input==NA :
    y1s = as.numeric(substr(season1,1,4)):as.numeric(substr(season2,1,4))
    if(suppressWarnings(max(dim(DL.input)))==-Inf){
      dldf = DL_Soccerway_data(divs = divs,year1 = y1s)
    } else {
      dldf = DL.input %>% filter(Div %in% divs,Year %in% y1s)
    }
    for (i in 1:nrow(dldf)){
      countryCode.i = substr(dldf$Div[i],1,3)
      if(type=="geo"){
        Tt.i = DL_Soccerway_season_geo(countryCode = countryCode.i,url = dldf$URL.geo[i])
      } else if(type=="logo"){
        Tt.i = DL_Soccerway_season_logo(countryCode = countryCode.i,url = dldf$URL.logo[i])
      } else if(type=="stadium"){
        Tt.i = DL_Soccerway_season_stadium(countryCode = countryCode.i,url = dldf$URL.stadium[i])
      }
      if(i==1){
        Tt = Tt.i
      } else {
        Tt = full_join(Tt,Tt.i)
      }
    }
  }
  # Go into a loop per league:
  if(type == "champion"){
    y1s = max(as.numeric(substr(season1,1,4)),as.numeric(substr(season2,1,4)))
    if(suppressWarnings(max(dim(DL.input)))==-Inf){
      dldf = DL_Soccerway_data(divs = divs,year1 = y1s)
    } else {
      dldf = DL.input %>% filter(Div %in% divs,Year %in% y1s)
    }
    for(i in 1:nrow(dldf)){
      Tt.i = DL_Soccerway_season_champion(div = dldf$Div[i],url = dldf$URL.archive[i])
      if(i==1){
        Tt = Tt.i
      } else {
        Tt = full_join(Tt,Tt.i)
      }
    }
  }
  return(Tt)
}
