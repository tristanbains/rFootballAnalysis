#' DL_FootballDataCoUk
#'
#' download multiple seasons for multiple league from www.football-data.co.uk
#'
#' @export
#' @import tidyverse
#' @param divs e.g. c("ENG1","FRA1")
#' @param season1 e.g. ("2001-2002")
#' @param season2 e.g. ("2005-2006")
#' @param DL.input usually output of DL_FootballDataCoUk_data()
#' @return tibble for multiple seasons and leagues with all matches, results and bookmaker odds
#'

DL_FootballDataCoUk = function(divs,season1,season2,DL.input=DL_FootballDataCoUk_data()){
  y1 = as.numeric(substr(season1,1,4))
  y2 = as.numeric(substr(season2,1,4))
  ys = y1:y2
  dlinput = DL.input %>%
    filter(Year%in%ys,ISO4%in%divs)
  for(i in 1:nrow(dlinput)){

    div = dlinput$ISO4[i]
    url = dlinput$URL[i]

    message(paste0("DL_FootballDataCoUk:  ",div,"  ",dlinput$Year[i]))

    Ttt = suppressWarnings(DL_FootballDataCoUk_season(div = div,url = url))
    if(i==1){
      Tt = Ttt
    } else {
      Tt = full_join(Tt,Ttt)
    }
  }
  Tt = Tt %>%
    arrange(Div,Date,HomeTeam)
  return(Tt)
}
