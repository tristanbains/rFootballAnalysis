#' AN_Elo_season
#'
#' wrapper function that downloads multiple seasons for multiple league from one of the source websites
#'
#' @export
#' @import tidyverse
#' @param data match data tibble that should contain one season for one league with columns "Div","Season","Date","HomeTeam","AwayTeam" and "Res"
#' @param startingQs tibble with columns "Team" and "Q0"
#' @param a division factor
#' @param b1 home team correction factor
#' @param b2 away team correction factor
#' @param k k-factor
#' @return tibble with columns "Div","Season","Date","HomeTeam","AwayTeam","Res","HWpct","Dpct","AWpct","QH0","QH1","QA0","QA1"
#'

AN_Elo_season=function(data,startingQs=NA,a=1/1400,b1=0.05,b2=-0.6,k=35){

  # Make sure the startingQs are in order:
  if(suppressWarnings(max(dim(startingQs))==-Inf)){
    SQ = tibble(Team=unique(data$HomeTeam),Q0=2000)
  } else {
    SQ = startingQs
  }

  Qcols=c("Div","Season","Date","HomeTeam","AwayTeam","Res")

  # Initialize the df:
  Tt = data %>%
    dplyr::select_(.dots=Qcols) %>%
    arrange(Date) %>%
    mutate(ID=row_number()) %>%
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    arrange(ID) %>%
    group_by(Team) %>%
    mutate(MatchRound = row_number()) %>%
    ungroup() %>%
    left_join(SQ,by = "Team") %>%
    mutate(Q1=as.numeric(NA),HWpct=as.numeric(NA),Dpct=as.numeric(NA),AWpct=as.numeric(NA),Exp=as.numeric(NA)) %>%
    mutate(Q0=ifelse(MatchRound==1,Q0,NA))

  # Calculate the Elo ratings and percentages:
  for(i in seq(1,nrow(Tt),2)){
    if(Tt$MatchRound[i]==1){
      Qh=Tt$Q0[i]
    } else {
      Qh=unlist(Tt[Tt$Team==Tt$Team[i]&Tt$MatchRound==(Tt$MatchRound[i]-1),c("Q1")])
    }
    if(Tt$MatchRound[(i+1)]==1){
      Qa=Tt$Q0[i+1]
    } else {
      Qa=unlist(Tt[Tt$Team==Tt$Team[i+1]&Tt$MatchRound==(Tt$MatchRound[i+1]-1),c("Q1")])
    }
    Res=Tt$Res[i]
    dQ=Qh-Qa

    hpct=pnorm(a*dQ+b1)
    apct=pnorm(-a*dQ+b2)
    dpct=1-hpct-apct
    Exp=hpct-apct
    dQ1=round((Res-Exp)*k,0)
    Qht=Qh+dQ1      #round(Qh+(Res-Exp)*k,0)
    Qat=Qa-dQ1      #round(Qa-(Res-Exp)*k,0)

    Tt$Q0[i]=Qh
    Tt$Q0[i+1]=Qa
    Tt$Q1[i]=Qht
    Tt$Q1[i+1]=Qat
    Tt$HWpct[i:(i+1)]=round(hpct,3)
    Tt$Dpct[i:(i+1)]=round(dpct,3)
    Tt$AWpct[i:(i+1)]=round(apct,3)
    Tt$Exp[i:(i+1)]=round(Exp,3)
  }

  output = Tt %>%
    mutate(QH0=ifelse(Location=="HomeTeam",Q0,NA),
           QH1=ifelse(Location=="HomeTeam",Q1,NA),
           QA0=ifelse(Location=="AwayTeam",Q0,NA),
           QA1=ifelse(Location=="AwayTeam",Q1,NA)) %>%
    dplyr::select(-Q0,-Q1,-MatchRound,-Exp) %>%
    group_by(ID) %>%
    mutate(QH0=max(QH0,na.rm=TRUE),
           QH1=max(QH1,na.rm=TRUE),
           QA0=max(QA0,na.rm=TRUE),
           QA1=max(QA1,na.rm=TRUE)) %>%
    ungroup() %>%
    spread(Location,Team) %>%
    dplyr::select(Div,Season,Date,HomeTeam,AwayTeam,Res,HWpct,Dpct,AWpct,QH0,QH1,QA0,QA1)

  return(output)
}
