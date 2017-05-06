#' AN_EloStartingQ_season
#'
#' provides starting Qs for multiple levels for a country
#'
#' @export
#' @import tidyverse
#' @param data match data tibble that should contain one season for one country with columns "Div","Season","Date","HomeTeam","AwayTeam" and "Res"
#' @param level1.baseQ the mean Q at the highest level
#' @param level1.spread the maximum Q delta between the best and worst team at the highest level
#' @param level1.maxQ the maximum Q of the best team at the highest level
#' @return a tibble with columns "Team" and "Q0"

# TO DO: als meerdere levels en verschillende startseizoenen
# check op negatieve Q

# TO DO

AN_EloStartingQ_season=function(data,level1.baseQ=2000,level1.spread=500,level1.maxQ=Inf){
  Tt = data %>%
    dplyr::select(Div,Season,Date,Res,HomeTeam,AwayTeam) %>%
    arrange(Div,Date) %>%
    group_by(Div) %>%
    filter(Season==Season[1]) %>%       # If data contains multiple seasons then only consider the first season
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    mutate(Result=ifelse(Location=="HomeTeam",Res,-Res)) %>%
    mutate(Pts=ifelse(Result>=0,3^Result,0)) %>%
    mutate(Level=as.numeric(substr(Div,4,4))) %>%
    mutate(CountryCode=substr(Div,1,3)) %>%
    ungroup() %>%
    group_by(CountryCode,Team) %>%
    summarise(Pts=sum(Pts),
              Level=min(Level)) %>%
    ungroup() %>%

    # HIER GEBLEVEN

    group_by(Level) %>%
    mutate(PtsScaled=Pts-min(Pts)) %>%
    arrange(PtsScaled) %>%
    mutate(PtsScaledDelta=PtsScaled-lag(PtsScaled,default = 0)) %>%
    ungroup() %>%
    arrange(-Level,PtsScaled) %>%
    mutate(PtsOverall=cumsum(PtsScaledDelta)) %>%
    mutate(Q=ifelse(Level==min(Level)&PtsScaled==0,level1.baseQ-level1.spread,NA)) %>%
    mutate(Q=ifelse(PtsOverall==max(PtsOverall),level1.baseQ+level1.spread,Q))

  Tt = data %>%
    arrange(Div,Date) %>%
    group_by(Div) %>%
    filter(Season==Season[1]) %>%       # If data contains multiple seasons then only consider the first season
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    mutate(Result=ifelse(Location=="HomeTeam",Res,-Res)) %>%
    mutate(Pts=ifelse(Result>=0,3^Result,0)) %>%
    mutate(Level=as.numeric(substr(Div,4,4))) %>%
    mutate(CountryCode=substr(Div,1,3)) %>%
    ungroup() %>%
    group_by(CountryCode,Team) %>%
    summarise(Pts=sum(Pts),Level=min(Level)) %>%
    group_by(Level) %>%
    mutate(PtsScaled=Pts-min(Pts)) %>%
    arrange(PtsScaled) %>%
    mutate(PtsScaledDelta=PtsScaled-lag(PtsScaled,default = 0)) %>%
    ungroup() %>%
    arrange(-Level,PtsScaled) %>%
    mutate(PtsOverall=cumsum(PtsScaledDelta)) %>%
    mutate(Q=ifelse(Level==min(Level)&PtsScaled==0,level1.baseQ-level1.spread,NA)) %>%
    mutate(Q=ifelse(PtsOverall==max(PtsOverall),level1.baseQ+level1.spread,Q))
  QPts = (2*level1.spread)/(max(Tt$PtsOverall[!is.na(Tt$Q)])-min(Tt$PtsOverall[!is.na(Tt$Q)]))
  Tt = Tt %>%
    mutate(Q=max(Q,na.rm=TRUE)-(max(PtsOverall)-PtsOverall)*QPts) %>%
    mutate(Q=round(Q,0)) %>%
    dplyr::select(Team,Level,Q) %>%
    arrange(-Q)
  if(!is.na(level1.maxQ)){
    Tt = Tt %>%
      mutate(Q=Q-(max(Q)-level1.maxQ))
  }
  return(Tt)
}
