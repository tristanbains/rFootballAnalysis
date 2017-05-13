#' DL_Soccerway_data
#'
#' creates tibble with the string that is needed to plot correctly
#'
#' @export
#' @import tidyverse
#' @return tibble with columns Div (e.g. "NLD1"),"Season",Year (e.g. 2005) URL.geo, URL.logo, URL.stadium
#'

DL_Soccerway_data = function(divs="DEU3",year1=(as.numeric(format(Sys.Date(),"%Y"))-1)){

  # Data per league:

  # NW
  NLD1 = tibble(Div="NLD1",URL="http://nl.soccerway.com/national/netherlands/eredivisie")
  NLD2 = tibble(Div="NLD2",URL="http://nl.soccerway.com/national/netherlands/eerste-divisie")
  BEL1 = tibble(Div="BEL1",URL="http://nl.soccerway.com/national/belgium/pro-league")
  BEL2 = tibble(Div="BEL2",URL="http://nl.soccerway.com/national/belgium/second-division")
  DEU1 = tibble(Div="DEU1",URL="http://nl.soccerway.com/national/germany/bundesliga")
  DEU2 = tibble(Div="DEU2",URL="http://nl.soccerway.com/national/germany/2-bundesliga")
  DEU3 = tibble(Div="DEU3",URL="http://nl.soccerway.com/national/germany/3-liga")
  NW = rbind(NLD1,NLD2,BEL1,BEL2,DEU1,DEU2,DEU3)

  # S
  FRA1 = tibble(Div="FRA1",URL="http://nl.soccerway.com/national/france/ligue-1")
  FRA2 = tibble(Div="FRA2",URL="http://nl.soccerway.com/national/france/ligue-2")
  FRA3 = tibble(Div="FRA3",URL="http://nl.soccerway.com/national/france/national")
  ESP1 = tibble(Div="ESP1",URL="http://nl.soccerway.com/national/spain/primera-division")
  ESP2 = tibble(Div="ESP2",URL="http://nl.soccerway.com/national/spain/segunda-division")
  PRT1 = tibble(Div="PRT1",URL="http://nl.soccerway.com/national/portugal/portuguese-liga-")
  PRT2 = tibble(Div="PRT2",URL="http://nl.soccerway.com/national/portugal/liga-de-honra")
  ITA1 = tibble(Div="ITA1",URL="http://nl.soccerway.com/national/italy/serie-a")
  ITA2 = tibble(Div="ITA2",URL="http://nl.soccerway.com/national/italy/serie-b")
  GRC1 = tibble(Div="GRC1",URL="http://nl.soccerway.com/national/greece/super-league")
  GRC2 = tibble(Div="GRC2",URL="http://nl.soccerway.com/national/greece/football-league")
  TUR1 = tibble(Div="TUR1",URL="http://nl.soccerway.com/national/turkey/super-lig")
  TUR2 = tibble(Div="TUR2",URL="http://nl.soccerway.com/national/turkey/1-lig")
  S = rbind(FRA1,FRA2,FRA3,ESP1,ESP2,PRT1,PRT2,ITA1,ITA2,GRC1,GRC2,TUR1,TUR2)

  # UK
  ENG1 = tibble(Div="ENG1",URL="http://nl.soccerway.com/national/england/premier-league")
  ENG2 = tibble(Div="ENG2",URL="http://nl.soccerway.com/national/england/championship")
  ENG3 = tibble(Div="ENG3",URL="http://nl.soccerway.com/national/england/league-one")
  ENG4 = tibble(Div="ENG4",URL="http://nl.soccerway.com/national/england/league-two")
  SCO1 = tibble(Div="SCO1",URL="http://nl.soccerway.com/national/scotland/premier-league")
  SCO2 = tibble(Div="SCO2",URL="http://nl.soccerway.com/national/scotland/first-division")
  SCO3 = tibble(Div="SCO3",URL="http://nl.soccerway.com/national/scotland/second-division")
  SCO4 = tibble(Div="SCO4",URL="http://nl.soccerway.com/national/scotland/third-division")
  UK = rbind(ENG1,ENG2,ENG3,ENG4,SCO1,SCO2,SCO3,SCO4)

  # N
  SWE1 = tibble(Div="SWE1",URL="http://nl.soccerway.com/national/sweden/allsvenskan")
  SWE2 = tibble(Div="SWE2",URL="http://nl.soccerway.com/national/sweden/superettan")
  NOR1 = tibble(Div="NOR1",URL="http://nl.soccerway.com/national/norway/eliteserien")
  NOR2 = tibble(Div="NOR2",URL="http://nl.soccerway.com/national/norway/1-division")
  DNK1 = tibble(Div="DNK1",URL="http://nl.soccerway.com/national/denmark/superliga")
  DNK2 = tibble(Div="DNK2",URL="http://nl.soccerway.com/national/denmark/1st-division")
  FIN1 = tibble(Div="FIN1",URL="http://nl.soccerway.com/national/finland/veikkausliiga")
  FIN2 = tibble(Div="FIN2",URL="http://nl.soccerway.com/national/finland/ykkonen")
  N = rbind(SWE1,SWE2,NOR1,NOR2,DNK1,DNK2,FIN1,FIN2)

  # C
  CHE1 = tibble(Div="CHE1",URL="http://nl.soccerway.com/national/switzerland/super-league")
  CHE2 = tibble(Div="CHE2",URL="http://nl.soccerway.com/national/switzerland/challenge-league")
  AUT1 = tibble(Div="AUT1",URL="http://nl.soccerway.com/national/austria/bundesliga")
  AUT2 = tibble(Div="AUT2",URL="http://nl.soccerway.com/national/austria/1-liga")
  CZE1 = tibble(Div="CZE1",URL="http://nl.soccerway.com/national/czech-republic/czech-liga")
  CZE2 = tibble(Div="CZE2",URL="http://nl.soccerway.com/national/czech-republic/2-liga")
  SVK1 = tibble(Div="SVK1",URL="http://nl.soccerway.com/national/slovakia/super-liga")
  SVK2 = tibble(Div="SVK2",URL="http://nl.soccerway.com/national/slovakia/i-liga")
  HUN1 = tibble(Div="HUN1",URL="http://nl.soccerway.com/national/hungary/nb-i")
  HUN2 = tibble(Div="HUN2",URL="http://nl.soccerway.com/national/hungary/nb-ii")
  C = rbind(CHE1,CHE2,AUT1,AUT2,CZE1,CZE2,SVK1,SVK2,HUN1,HUN2)

  # SE
  SRB1 = tibble(Div="SRB1",URL="http://nl.soccerway.com/national/serbia/super-liga")
  SRB2 = tibble(Div="SRB2",URL="http://nl.soccerway.com/national/serbia/prva-liga")
  HRV1 = tibble(Div="HRV1",URL="http://nl.soccerway.com/national/croatia/1-hnl")
  HRV2 = tibble(Div="HRV2",URL="http://nl.soccerway.com/national/croatia/2-hnl")
  BIH1 = tibble(Div="BIH1",URL="http://nl.soccerway.com/national/bosnia-herzegovina/premier-liga")
  SVN1 = tibble(Div="SVN1",URL="http://nl.soccerway.com/national/slovenia/1-snl")
  SVN2 = tibble(Div="SVN2",URL="http://nl.soccerway.com/national/slovenia/2-snl")
  ROU1 = tibble(Div="ROU1",URL="http://nl.soccerway.com/national/romania/liga-i")
  ROU2 = tibble(Div="ROU2",URL="http://nl.soccerway.com/national/romania/liga-ii")
  BGR1 = tibble(Div="BGR1",URL="http://nl.soccerway.com/national/bulgaria/a-pfg")
  BGR2 = tibble(Div="BGR2",URL="http://nl.soccerway.com/national/bulgaria/b-pfg")
  ALB1 = tibble(Div="ALB1",URL="http://nl.soccerway.com/national/albania/super-league")
  ALB2 = tibble(Div="ALB2",URL="http://nl.soccerway.com/national/albania/league-1")
  ISR1 = tibble(Div="ISR1",URL="http://nl.soccerway.com/national/israel/ligat-haal")
  ISR2 = tibble(Div="ISR2",URL="http://nl.soccerway.com/national/israel/liga-leumit")
  SE = rbind(SRB1,SRB2,HRV1,HRV2,ROU1,ROU2,BGR1,BGR2,ALB1,ALB2,ISR1,ISR2,SVN1,SVN2)

  # E
  POL1 = tibble(Div="POL1",URL="http://nl.soccerway.com/national/poland/ekstraklasa")
  POL2 = tibble(Div="POL2",URL="http://nl.soccerway.com/national/poland/i-liga")
  BLR1 = tibble(Div="BLR1",URL="http://nl.soccerway.com/national/belarus/premier-league")
  BLR2 = tibble(Div="BLR2",URL="http://nl.soccerway.com/national/belarus/1-division")
  UKR1 = tibble(Div="UKR1",URL="http://nl.soccerway.com/national/ukraine/premier-league")
  UKR2 = tibble(Div="UKR2",URL="http://nl.soccerway.com/national/ukraine/persha-liga")
  RUS1 = tibble(Div="RUS1",URL="http://nl.soccerway.com/national/russia/premier-league")
  RUS2 = tibble(Div="RUS2",URL="http://nl.soccerway.com/national/russia/1-division")
  E = rbind(POL1,POL2,BLR1,BLR2,UKR1,UKR2,RUS1,RUS2)

  Tt = rbind(NW,S,N,C,E,SE) %>% arrange(Div)

  urls = Tt %>% filter(Div %in% divs)

  # Get the urls per league:

  suppressWarnings(rm(Tt.i))

  for (i in 1:nrow(urls)){
    url.i = urls$URL[i]
    div.i = urls$Div[i]
    Ttt.i=readLines(url.i)
    urls.i = Ttt.i[grepl("Stadion",Ttt.i)]
    url.archive.i = paste0("http://nl.soccerway.com/national",gsub("(.*Stadions.*national)(.*)(archive.*)", "\\2", urls.i),"archive")
    Ttt.archive.i = readLines(url.archive.i)
    url.suffix.i = sub("(http://nl.soccerway.com)(.*)","\\2",url.i)

    # Get all URLs for the season:
    urls.season.i = tibble(StringURL = Ttt.archive.i) %>%
      filter(grepl(url.suffix.i,StringURL)) %>%                     # get the urls
      filter(grepl("[0-9]{4}</a>",StringURL)) %>%                   # get the available seasons
      mutate(Div = div.i) %>%
      mutate(Season=sub(".*>(.*)</a>$","\\1",StringURL)) %>%        # extract the season
      mutate(Season=sub("/","-",Season)) %>%
      mutate(Y1 = as.numeric(substr(Season,1,4))) %>%
      mutate(URL=sub('.*href="(.*)".*',"\\1",StringURL)) %>%        # extract the URL
      mutate(URL=paste0("http://nl.soccerway.com",URL)) %>%
      unique() %>%
      dplyr::select(-StringURL) %>%
      filter(Y1 %in% year1)

    # Inner loop: use urls.league.i to get urls for matches, stadiums, etc.
    suppressWarnings(rm(Tt.j))
    if(nrow(urls.season.i)>0){
      for (j in 1:nrow(urls.season.i)){

        div.j = urls.season.i$Div[j]
        season.j = urls.season.i$Season[j]
        y1.j = urls.season.i$Y1[j]
        Ttt.season.j=readLines(urls.season.i$URL[j])

        urls.season.j=tibble(StringURL=Ttt.season.j) %>%
          filter(grepl("regular-season",StringURL)) %>%
          mutate(URL=sub('.*href="(.*)".*',"\\1",StringURL)) %>%
          filter(!grepl(">",URL)) %>%
          mutate(URL=sub('(.*regular-season/.*?/).*',"\\1",URL)) %>%
          mutate(URL=paste0("http://nl.soccerway.com",URL)) %>%
          filter(grepl("season/r",URL)|grepl("season/s",URL)) %>%
          dplyr::select(URL) %>%
          unique() %>%
          mutate(Div=div.j,
                 Season=season.j,
                 Year=y1.j,
                 URL.geo=paste0(URL,"map"),
                 URL.logo=URL.geo,
                 URL.stadium=paste0(URL,"venues"),
                 URL.archive=url.archive.i) %>%
          dplyr::select(-URL)

        if(j==1){
          Tt.j = urls.season.j
        } else {
          Tt.j = full_join(Tt.j,urls.season.j)
        }
      }
    }

    #
    if(nrow(urls.season.i)>0){
      if(!exists("Tt.i")){
        Tt.i = Tt.j
      } else {
        Tt.i=full_join(Tt.i,Tt.j)
      }
    }
  }

  Tt.i = Tt.i %>%
    dplyr::select(Div,Season,Year,URL.geo,URL.logo,URL.stadium,URL.archive)

  return(Tt.i)
}
