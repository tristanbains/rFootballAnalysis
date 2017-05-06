#' AN_EloMatrix_season
#'
#' converts a match data tibble into a matrix for faster optimalisation
#'
#' @export
#' @import tidyverse
#' @param data match data tibble that should contain one season for one league with columns "Div","Season","Date","HomeTeam","AwayTeam" and "Res"
#' @param startingQs tibble with columns "Team" and "Q0"
#' @return a matrix with columns
#'

# TO DO

AN_EloMatrix_season=function(data,startingQs){
  Tt=data %>%
    arrange(Date) %>%
    mutate(Sn=match(Season,table = unique(Season)))
  tms=unlist(sort(unique(c(Tt$HomeTeam,Tt$AwayTeam))))
  TttQs=startingQs %>%
    mutate(n=match(Team,tms))
  TttQs=merge(TttQs,data.frame(n=1:length(tms)),all=TRUE)

  Ttt=cbind(Tt$Sn,match(Tt$HomeTeam,tms),match(Tt$AwayTeam,tms),Tt$Res)
  TttPcts=matrix(0,nrow(Ttt),3)
  TttQ0=matrix(0,nrow(Ttt),length(tms))
  TttQ0[1,]=TttQs$Q
  TttdQ=matrix(0,nrow(Ttt),length(tms))
  Ttt=cbind(Ttt,TttPcts,TttQ0,TttdQ)
  return(Ttt)
}
