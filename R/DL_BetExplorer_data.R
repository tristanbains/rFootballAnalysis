#' DL_BetExplorer_data
#'
#' creates tibble with URLs per league per season
#'
#' @export
#' @import tidyverse zoo
#' @return tibble with columns Div (e.g. "NLD1"),"Season",Year (e.g. 2005) and URL
#'

# TO DO: huidige seizoen ook toevoegen, staat geen seizoen in de url, na.locf(fromLast=TRUE)?
# TO DO: als input ITA3 is dan zoeken op ITA3A en ITA3B en evt ITA3
# TO DO: meer landen toevoegen
# TO DO: check binnen een results om verschillende tabbladen met main, playoffs e.d te detecteren, zie bv http://www.betexplorer.com/soccer/italy/lega-pro-group-a/results/
# TO DO: puntenaftrek herkennen, zie bv http://www.betexplorer.com/soccer/italy/lega-pro-c2-a-2011-2012/?stage=lMeU6nWP

DL_BetExplorer_data = function(divs,year1=(as.numeric(format(Sys.Date(),"%Y"))-1)){

  # NW
  # NLD = tibble(Div=c("","","",""),CountryName="",League=c("","","",""))
  DEU = tibble(Div=c("DEU1","DEU2","DEU3","DEU3N","DEU3S","DEU3BY","DEU3NO","DEU3SW","DEU3W"),CountryName="germany",
               League=c("bundesliga","2-bundesliga","3-liga","regionalliga-north","regionalliga-south","regionalliga-bayern","regionalliga-nordost","regionalliga-sudwest","regionalliga-west"))
  NW = rbind(DEU)

  # S
  ITA = tibble(Div=c("ITA1","ITA2","ITA3A","ITA3B","ITA3A","ITA3B","ITA3C","ITA3A","ITA3B"),CountryName="italy",
               League=c("serie-a","serie-b","serie-c1-a","serie-c1-b","lega-pro-group-a","lega-pro-group-b","lega-pro-group-c","lega-pro-c1-a","lega-pro-c1-b"))
  # ESP
  S = rbind(ITA)

  Ttt = rbind(NW,S) %>% arrange(Div,League)

  dldf = Ttt %>%
    filter(Div %in% divs) %>%
    mutate(URLCountry=paste0("http://www.betexplorer.com/soccer/",CountryName))
  dldf.ct = dldf %>%
    dplyr::select(URLCountry) %>%
    unique()

  for(i in 1:nrow(dldf.ct)){
    dldf.i = inner_join(dldf,dldf.ct[i,]) %>%
      mutate(URLpartial=paste(CountryName,League,sep="/"))
    cn = dldf.i$CountryName[1]

    Ttt.i = tibble(URL.long = readLines(dldf.ct$URLCountry[i])) %>%
      filter(grepl("^.*-[0-9]{4}/*",URL.long) | grepl(paste(dldf.i$URLpartial,collapse = "|"),URL.long)) %>%
      mutate(URL=paste0("http://www.betexplorer.com/soccer/",sub("(^.*soccer/)(.*[0-9]/)(.*)","\\2",URL.long),"results"),
             Year=as.numeric(sub(".*?([0-9]{4}).*", "\\1",URL)),
             League=sub(paste0("http://www.betexplorer.com/soccer/",cn),"",URL)) %>%
      mutate(League=sub("/(.*?)([0-9]{4}).*","\\1",League)) %>%
      mutate(League=substr(League,1,nchar(League)-1)) %>%
      mutate(League=sub(paste0("(.*soccer/",cn,")(.*)"),"\\2",League)) %>%
      mutate(League=sub("(^.*)(\">.*)","\\1",League)) %>%
      mutate(League=gsub("/","",League)) %>%
      inner_join(dldf.i %>%
                   dplyr::select(Div,League)) %>%
      mutate(Season = paste(Year,(Year+1),sep="-")) %>%
      mutate(Season=ifelse(Season=="NA-NA",as.character(NA),Season)) %>%
      dplyr::select(Div,Season,Year,URL,League) %>%
      mutate(URL=paste0(sub("(^.*)(\">.*)","\\1",URL),"results")) %>%
      mutate(URL=sub("resultsresults","results",URL)) %>%
      mutate(URL=ifelse(is.na(Year),paste("http://www.betexplorer.com/soccer",cn,League,"results",sep="/"),URL)) %>%
      dplyr::select(-League)

    if(i==1){
      Tt = Ttt.i
    } else {
      Tt = full_join(Tt,Ttt.i)
    }
  }

  Tt = Tt %>%
    unique() %>%
    arrange(Div,-Year)

  return(Tt)
}
