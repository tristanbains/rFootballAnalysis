#' PL_MapData
#'
#' creates tibble with the string that is needed to plot correctly
#'
#' @export
#' @import tidyverse
#' @return tibble with columns "CountryCode","MapSource","MapString","xmin","xmax","ymin","ymax"
#'

PL_MapData = function(){
  # map('worldHires',c('UK', 'Ireland', 'Isle of Man','Isle of Wight'),xlim=c(-11,3), ylim=c(49,60.9))	UK
  # map('worldHires',c('UK'),xlim=c(-5.7,1.75), ylim=c(49.9,55.5))	ENG

  # Countries that can be defined by regions:

  # NW
  DEU = tibble(CountryCode="DEU",MapString=c("Germany"))
  NLD = tibble(CountryCode="NLD",MapString=c("Netherlands"))
  BEL = tibble(CountryCode="BEL",MapString=c("Belgium"))
  NW = rbind(DEU,NLD,BEL)

  # S
  ESP = tibble(CountryCode="ESP",MapString=c("Spain","Canary"))
  PRT = tibble(CountryCode="PRT",MapString=c("Portugal","Madeira"))
  ITA = tibble(CountryCode="ITA",MapString=c("Italy","Sicily","Sardinia"))
  GRC = tibble(CountryCode="GRC",MapString=c("Greece"))
  TUR = tibble(CountryCode="TUR",MapString=c("Turkey"))
  S = rbind(ESP,PRT,ITA,GRC,TUR)

  # N
  SWE = tibble(CountryCode="SWE",MapString=c("Sweden"))
  FIN = tibble(CountryCode="FIN",MapString=c("Finland"))
  N = rbind(SWE,FIN)

  # C
  AUT = tibble(CountryCode="AUT",MapString=c("Austria"))
  CHE = tibble(CountryCode="CHE",MapString=c("Switzerland"))
  # CZE = tibble(CountryCode="CZE",MapString=c("Czech"))
  # SVK = tibble(CountryCode="SVK",MapString=c("Slovakia"))
  HUN = tibble(CountryCode="HUN",MapString=c("Hungary"))
  C = rbind(AUT,CHE,HUN)

  # E
  POL  = tibble(CountryCode="POL",MapString=c("Poland"))
  E = rbind(POL)

  # SE
  # SRB  = tibble(CountryCode="SRB",MapString=c("Serbia"))
  # BIH  = tibble(CountryCode="BIH",MapString=c("Bosnia"))
  # HRV  = tibble(CountryCode="HRV",MapString=c("Croatia"))
  # SVN  = tibble(CountryCode="SVN",MapString=c("Slovenia"))
  ALB  = tibble(CountryCode="ALB",MapString=c("Albania"))
  ROM  = tibble(CountryCode="ROM",MapString=c("Romania"))
  BGR  = tibble(CountryCode="BGR",MapString=c("Bulgaria"))
  #  = tibble(CountryCode="",MapString=c(""))
  SE = rbind(ALB,ROM,BGR)

  Tt = rbind(NW,S,C,N,E,SE)

  # Countries with long/lat restrictions:

  # FRA
  # PRT
  # ENG
  # SCO
  # DNK = tibble(CountryCode="DNK",MapString=c("Denmark"))
  # NOR = tibble(CountryCode="NOR",MapString=c("Norway"))
  # restr = rbind(ESP,FRA,PRT,ENG,SCO,DNK,NOR)

  # map("worldHires",Tt$MapString[Tt$CountryCode=="ITA"])


  return(Tt)
}

