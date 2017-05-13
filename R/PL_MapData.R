#' PL_MapData
#'
#' creates tibble with the string that is needed to plot correctly
#'
#' @import tidyverse maps mapdata
#' @export
#' @return tibble with columns "CountryCode","MapSource","MapString","xmin","xmax","ymin","ymax"
#'

PL_MapData = function(){

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

  ENG = tibble(CountryCode="ENG",MapString=c("UK"),xmin=-5.7,xmax=1.75,ymin=49.9,ymax=55.5)
  SCO = tibble(CountryCode="SCO",MapString=c("UK"),xmin=-7.5,xmax=1.75,ymin=54.5,ymax=61)
  FRA = tibble(CountryCode="FRA",MapString=c("France","Corsica"),xmin=-5,xmax=10,ymin=41,ymax=51)
  DNK = tibble(CountryCode="DNK",MapString=c("Denmark"),xmin=8,xmax=12.8,ymin=53.5,ymax=58)
  NOR = tibble(CountryCode="NOR",MapString=c("Norway"),xmin=4.5,xmax=34,ymin=58,ymax=71)
  Tt.restr = rbind(ENG,SCO,FRA,SCO,DNK,NOR)

  Tt = full_join(Tt,Tt.restr)

  # map('worldHires',c('France',"Corsica"),xlim=c(-5,10), ylim=c(41,51))
  # cts = c("ESP","PRT");map("worldHires",Tt$MapString[Tt$CountryCode %in% cts])
  # map("worldHires",Tt.restr$MapString,xlim=c(min(Tt.restr$xmin),max(Tt.restr$xmax)),ylim=c(min(Tt.restr$ymin),max(Tt.restr$ymax)))

  Tt = Tt %>%
    mutate(MapSource = "worldHires") %>%
    dplyr::select(CountryCode,MapSource,MapString,xmin,xmax,ymin,ymax) %>%
    arrange(CountryCode,MapString,MapSource)

  return(Tt)
}

