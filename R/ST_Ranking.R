#' ST_Ranking
#'
#' creates season rankings
#'
#' @export
#' @import tidyverse
#' @param data match data tibble that should contain columns "Div","Season","Date","HomeTeam","AwayTeam","FTHG","FTAG" and "Res"
#' @return tibble witH columns "Rank","Div","Season","Date","Team","M","W","D","L","Pts","GF","GA","GD","HM","HW","HD","HL","HPts","HGF","HGA","HGD","AM","AW","AD","AL","APts","AGF","AGA","AGD"
#'

ST_Ranking=function(data,ordering=ST_RankingOrdering()){

  # Create all the columns (except for aggregates):
  Tt = data %>%
    dplyr::select(Div,Season,HomeTeam,AwayTeam,FTHG,FTAG,Res) %>%
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    mutate(H=ifelse(Location=="HomeTeam",1,0),
           A=ifelse(Location=="AwayTeam",1,0)) %>%
    mutate(HGF = H * FTHG,
           HGA = H * FTAG,
           HGD = HGF-HGA,
           HW = ifelse(HGD>0,1,0),
           HD = H * ifelse(HGD==0,1,0),
           HL = ifelse(HGD<0,1,0),
           HPts = HW*3 + HD,
           AGF = A * FTAG,
           AGA = A * FTHG,
           AGD = AGF-AGA,
           AW = ifelse(AGD>0,1,0),
           AD = A * ifelse(AGD==0,1,0),
           AL = ifelse(AGD<0,1,0),
           APts = AW*3 + AD) %>%
    group_by(Div,Season,Team) %>%
    summarise(HM = sum(H),
              HW = sum(HW),
              HD = sum(HD),
              HL = sum(HL),
              HPts = sum(HPts),
              HGF = sum(HGF),
              HGA = sum(HGA),
              HGD = sum(HGD),
              AM = sum(A),
              AW = sum(AW),
              AD = sum(AD),
              AL = sum(AL),
              APts = sum(APts),
              AGF = sum(AGF),
              AGA = sum(AGA),
              AGD = sum(AGD)) %>%
    ungroup() %>%
    mutate(M = HM+AM,
           W = HW+AW,
           D = HD+AD,
           L = HL+AL,
           Pts = HPts+APts,
           GF = HGF+AGF,
           GA = HGA+AGA,
           GD = HGD+AGD)

  # Reordering columns:
  hcols = colnames(Tt)[grepl("^H",colnames(Tt))]
  acols = colnames(Tt)[grepl("^A",colnames(Tt))]
  mcols = setdiff(colnames(Tt),c(hcols,acols))
  Tt = Tt[,c(mcols,hcols,acols)]

  # Calculate aggregate results:
  Tt.agg = Tt %>%
    dplyr::select(Div,Season,Team,Pts) %>%
    arrange(Div,Season,-Pts) %>%
    group_by(Div,Season,Pts) %>%
    mutate(GroupPts=n(),
           GroupNumber=row_number()) %>%
    ungroup() %>%
    filter(GroupPts>1) %>%
    mutate(GroupNumber=ifelse(GroupNumber==1,1,0),
           GroupNumber=cumsum(GroupNumber)) %>%
    dplyr::select(-Pts,-GroupPts)

  Tt.aggH = data %>% right_join(Tt.agg %>% dplyr::rename(HomeTeam=Team))
  Tt.aggA = data %>% right_join(Tt.agg %>% dplyr::rename(AwayTeam=Team))
  Tt.agg = inner_join(Tt.aggH,Tt.aggA) %>%
    dplyr::select(Div,Season,HomeTeam,AwayTeam,FTHG,FTAG,Res,GroupNumber) %>%
    gather(Location,Team,HomeTeam:AwayTeam) %>%
    group_by(Div,Season,Team,GroupNumber) %>%
    mutate(Loc=ifelse(Location=="HomeTeam",1,-1),
           Pts.agg=ifelse(Loc==Res,3,0),
           Pts.agg=ifelse(Res==0,1,Pts.agg),
           GF.agg=ifelse(Loc==1,FTHG,FTAG),
           GD.agg=ifelse(Loc==1,FTHG-FTAG,FTAG-FTHG),
           AGF.agg=ifelse(Loc==-1,FTAG,0)) %>%
    summarise(Pts.agg=sum(Pts.agg),
              GD.agg=sum(GD.agg),
              GF.agg=sum(GF.agg),
              AGF.agg=sum(AGF.agg)) %>%
    ungroup() %>%
    dplyr::select(-GroupNumber)

  Tt = Tt %>%
    full_join(Tt.agg)

  rm(Tt.agg,Tt.aggH,Tt.aggA)
  gc()

  colsTt = colnames(Tt)

  # Determine column ordering:
  order = ordering %>%
    filter(Div%in%c(Tt$Div,"ENG1"),Season%in%Tt$Season) %>%
    arrange(Div,Season,Order)
  orderUnique = unique(order[,c("Div","Season")])

  # HIER GEBLEVEN
  # order %>% mutate(OrderL=sprintf("Order%02d",Order)) %>% dplyr::select(-Order) %>% spread(OrderL,Parameter)
  # sub("-","",unique(order$Parameter))
  Tt.order = Tt[,c("Div","Season","Team",sub("-","",unique(order$Parameter)))]

  # Add ranking:
  for (j in 1:nrow(orderUnique)){
    order.j = order %>%
      inner_join(orderUnique[j,]) %>%
      arrange(Order)
    order.j = order.j$Parameter

    Tt.order.j = Tt
  }


  return(Tt)

}
