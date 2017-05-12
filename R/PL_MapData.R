#' PL_MapData
#'
#' creates tibble with the string that is needed to plot correctly
#'
#' @export
#' @import tidyverse, mapdata, maps
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
  ITA = tibble(CountryCode="ITA",MapString=c("Italy","Sicily","Sardinia"))
  GRC = tibble(CountryCode="GRC",MapString=c("Greece"))
  TUR = tibble(CountryCode="TUR",MapString=c("Turkey"))
  S = rbind(ITA,GRC,TUR)

  # N
  SWE = tibble(CountryCode="SWE",MapString=c("Sweden"))
  NOR = tibble(CountryCode="NOR",MapString=c("Norway"))
  DNK = tibble(CountryCode="DNK",MapString=c("Denmark"))
  FIN = tibble(CountryCode="FIN",MapString=c("Finland"))
  N = rbind(SWE,NOR,DNK,FIN)

  # SE
  #  = tibble(CountryCode="",MapString=c(""))

  # C
  AUT = tibble(CountryCode="AUT",MapString=c("Austria"))
  CHE = tibble(CountryCode="CHE",MapString=c("Switzerland"))
  C = rbind(AUT,CHE)


  # Countries with long/lat restrictions:
  # ESP
  # FRA
  # PRT
  # ENG
  # SCO
  # restr = rbind(ESP,FRA,PRT,ENG,SCO)

  # map("worldHires",Tt$MapString[Tt$CountryCode=="ITA"])

  Tt = rbind(NW,S,C,N)
  return(Tt)
}

