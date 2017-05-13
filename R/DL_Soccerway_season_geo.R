#' DL_Soccerway_season_geo
#'
#' creates tibble with lat-long pairs per club. url input usually comes from DL_Soccerway_season_data(), the URL.geo column
#' df = DL_Soccerway_data("NLD1"); DL_Soccerway_season_geo("NLD",df$URL.geo)
#'
#' @import tidyverse
#' @export
#' @param countryCode e.g. "ESP"
#' @param url e.g. "http://nl.soccerway.com/national/spain/segunda-division/20152016/regular-season/r32028/map"
#' @return tibble with columns CountryCode ("ESP"), Team ("UD Almería"), Lat, Long
#'

DL_Soccerway_season_geo = function(countryCode,url){
  Ttt=readLines(url)
  txt="        setMarker("
  Team=Ttt[which(Ttt==txt)+1]
  Lat=Ttt[which(Ttt==txt)+2]
  Long=Ttt[which(Ttt==txt)+3]
  Tt=tibble(CountryCode=countryCode,Team,Lat,Long) %>%
    mutate(Team=gsub( '[",]', "",Team)) %>%
    mutate(Team=sub('^\\s*','',Team)) %>%
    mutate(Team=sub('*\\s$','',Team)) %>%
    mutate(Lat=sub(' *([0-9]*.[0-9]*), *','\\1',Lat)) %>%
    mutate(Lat=as.numeric(Lat)) %>%
    mutate(Long=sub(' *([0-9]*.[0-9]*), *','\\1',Long)) %>%
    mutate(Long=as.numeric(Long))
  return(Tt)
}
