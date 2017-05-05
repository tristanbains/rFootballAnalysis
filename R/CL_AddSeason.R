#' CL_AddSeason
#'
#' determine the seasons for the entire data frame and fill the Season column
#'
#' @export
#' @import tidyverse
#' @param data match data tibble that should contain columns Div, Season, Date, HomeTeam, AwayTeam
#' @return same as data input but with Season filled in
#'

CL_AddSeason=function(data){
  Ttt = data %>%
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    mutate(M=as.numeric(format(Date,"%m")),
           Y=as.numeric(format(Date,"%Y"))) %>%
    group_by(Div,Team,Y,M) %>%
    summarise(minDate=min(Date),
              maxDate=max(Date)) %>%
    ungroup() %>%
    arrange(Div,Y,M,Team) %>%
    group_by(Div,Y,M) %>%
    summarise(Teams=paste(Team,collapse=" "),
              minDate=min(minDate),
              maxDate=max(maxDate)) %>%
    ungroup() %>%
    arrange(Div,Y,M) %>%
    group_by(Div) %>%
    mutate(DateDiv=minDate-lag(maxDate,default = 1000),
           NewSeason=ifelse(lead(Teams,2)!=lag(Teams,2)&DateDiv>22,1,0)) %>%
    mutate(NewSeason=ifelse(row_number()==1,1,NewSeason)) %>%
    mutate(SeasonNum=cumsum(NewSeason)) %>%
    ungroup() %>%
    group_by(Div,SeasonNum) %>%
    mutate(minDate=min(minDate)) %>%
    mutate(Season=ifelse(as.numeric(format(minDate,"%m"))<6,
                         format(minDate,"%Y"),
                         paste(format(minDate,"%Y"),as.numeric(format(minDate,"%Y"))+1,sep="-"))) %>%
    ungroup() %>%
    dplyr::select(Div,Y,M,Season)

  cn=colnames(data)

  Tt = data %>%
    mutate(M=as.numeric(format(Date,"%m")),
           Y=as.numeric(format(Date,"%Y"))) %>%
    dplyr::select(-Season) %>%
    merge(Ttt,all=TRUE)

  Tt=Tt[,cn] %>%
    group_by(Div,Date,HomeTeam) %>%
    filter(!(n()>1&is.na(Res))) %>%
    ungroup() %>%
    arrange(Div,Date,HomeTeam)
  rownames(Tt)=NULL
  return(Tt)
}
