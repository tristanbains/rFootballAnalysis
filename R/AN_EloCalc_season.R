#' AN_EloCalc_season
#'
#' wrapper function that downloads multiple seasons for multiple league from one of the source websites
#'
#' @export
#' @import tidyverse
#' @param data.m matrix (!) that should have columns "Res","RowPrev","Q0","Q1", "HWpct", "Dpct", "AWpct"
#' "RowPrev" should be equal to row_number() if it is the first occurrence
#' "Q0" should be filled for the first occurrence per team
#' "Q1" should be equal to Q0
#' nrow(data.m) should be even, odd rows for home teams, even rows for away teams
#' @param startingQs tibble with columns "Team" and "Q0"
#' @param a division factor
#' @param b1 home team correction factor
#' @param b2 away team correction factor
#' @param k k-factor
#' @return tibble with columns "Div","Season","Date","HomeTeam","AwayTeam","Res","HWpct","Dpct","AWpct","QH0","QH1","QA0","QA1"
#'

# TO DO

AN_EloCalc_season=function(data.m,k=35,a=1/1400,factors=c(0.05,-0.6)){
  Tt.m=data.m
  is=seq(1,nrow(Tt.m),2)
  b1=factors[1]
  b2=factors[2]
  for(i in is){
    Tt.m[i:(i+1),"Q0"]=Tt.m[Tt.m[i:(i+1),"RowPrev"],"Q1"]
    dQ=Tt.m[i,"Q0"]-Tt.m[(i+1),"Q0"]
    Tt.m[i:(i+1),"HWpct"]=round(pnorm(a*dQ+b1),3)
    Tt.m[i:(i+1),"AWpct"]=round(pnorm(-a*dQ+b2),3)
    Tt.m[i:(i+1),"Dpct"]=1-Tt.m[i,"HWpct"]-Tt.m[i,"AWpct"]
    expRes=Tt.m[i,"HWpct"]-Tt.m[i,"AWpct"]
    tQ=round((Tt.m[i,"Res"]-expRes)*k,0)
    Tt.m[i,"Q1"]=Tt.m[i,"Q0"]+tQ
    Tt.m[(i+1),"Q1"]=Tt.m[(i+1),"Q0"]-tQ
  }
  return(Tt.m)
}
