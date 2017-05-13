#' DL_Soccerway_season_stadium
#'
#' creates tibble with stadium info per club. url input usually comes from DL_Soccerway_season_data(), the URL.stadium column
#' df = DL_Soccerway_data("NLD1"); DL_Soccerway_season_stadium("NLD",df$URL.stadium)
#' Output can have multiple stadiums, often the case for 2nd team of a club
#'
#' @import tidyverse
#' @export
#' @param countryCode e.g. "ESP"
#' @param url e.g. "http://nl.soccerway.com/national/spain/segunda-division/20152016/regular-season/r32028/venues"
#' @return tibble with columns CountryCode ("ESP"), Team ("UD Almería"), Stadium, Capacity, City, Address
#'
#'

DL_Soccerway_season_stadium = function(countryCode,url){
  Ttt=readLines(url)
  Team=Ttt[which(grepl("</a></h3>",Ttt))]
  Stadium=Ttt[which(grepl("</a></h4>",Ttt))]
  City=Ttt[which(grepl("<dt>Plaats:</dt>",Ttt))+1]
  Capacity=Ttt[which(grepl("<dt>Capaciteit:</dt>",Ttt))+1]
  Address=Ttt[which(grepl("<dt>Adres:</dt>",Ttt))+1]
  Tt=tibble(CountryCode=countryCode,Team,Stadium,Capacity,City,Address) %>%
    mutate(Team=sub('(.*[0-9]{1,}/\\\">)(.*?)(</a></h3>)','\\2',Team)) %>%
    mutate(Stadium=sub('(.*venue/\\\">)(.*?)(</a></h4>)','\\2',Stadium)) %>%
    mutate(Capacity=sub('(<dd>)(.*?)(</dd>)','\\2',Capacity)) %>%
    mutate(Capacity=as.numeric(Capacity)) %>%
    mutate(City=sub('(.*<dd>)(.*?)(</dd>)','\\2',City)) %>%
    mutate(Address=sub('(.*<dd>)(.*?)(</dd>)','\\2',Address))
  return(Tt)
}
