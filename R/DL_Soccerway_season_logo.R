#' DL_Soccerway_season_logo
#'
#' creates tibble with lat-long pairs per club. url input usually comes from DL_Soccerway_season_data(), the URL.geo column
#' df = DL_Soccerway_data("NLD1"); DL_Soccerway_season_logo("NLD",df$URL.logo)
#'
#' @import tidyverse
#' @export
#' @param countryCode e.g. "ESP"
#' @param url e.g. "http://nl.soccerway.com/national/spain/segunda-division/20152016/regular-season/r32028/map"
#' @return tibble with columns CountryCode ("ESP"), Team ("UD Almería"), Logo ("http://cache.images.core.optasports.com/soccer/teams/150x150/2049.png")
#'

DL_Soccerway_season_logo = function(countryCode,url){
  Ttt=readLines(url)
  txt="        setMarker("
  Team=Ttt[which(Ttt==txt)+1]
  Logo=Ttt[which(Ttt==txt)+8]
  Tt = data.frame(CountryCode=countryCode,Team,Logo,stringsAsFactors=FALSE) %>%
    mutate(Team=gsub( '[",]', "",Team)) %>%
    mutate(Team=sub('^\\s*','',Team)) %>%
    mutate(Team=sub('*\\s$','',Team)) %>%
    mutate(Logo=sub('(.*")(.*?)(", *)','\\2',Logo)) %>%
    mutate(Logo=gsub("30x30","150x150",Logo))
  return(Tt)
}
