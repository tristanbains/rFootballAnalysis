#' DL_Soccerway_season_champion
#'
#' creates tibble with all the champions in a league. url input usually comes from DL_Soccerway_season_data(), the URL.archive column
#' df = DL_Soccerway_data("NLD1"); DL_Soccerway_season_champion("NLD",df$URL.archive)
#'
#' @import tidyverse
#' @export
#' @param div e.g. "NLD1"
#' @param url e.g. "http://nl.soccerway.com/national/netherlands/eredivisie/c1/archive"
#' @return tibble with columns Div ("ESP1"), Season, Champion ("Barcelona"), RunnerUp ("Real Madrid")
#'

DL_Soccerway_season_champion = function(div,url){
  Ttt=readHTMLTable(url,stringsAsFactors=FALSE)
  Ttt=Ttt[[1]] %>%
    mutate(Div=div) %>%
    mutate(Seizoen=gsub("/","-",Seizoen))
  colnames(Ttt)=c("Season","Champion","RunnerUp","Div")
  Ttt=Ttt[,c("Div","Season","Champion","RunnerUp")]
  return(Ttt)
}
