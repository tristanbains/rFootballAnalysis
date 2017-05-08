#' ST_RankingOrdering
#'
#' creates tibble with ranking criteria per league
#'
#' @export
#' @import tidyverse
#' @return tibble with columns "Div","Season","Parameter","Order"
#'

ST_RankingOrdering=function(){

  # Create seasons:
  Y1=1980:(as.numeric(format(Sys.Date(),"%Y")))
  Y2=Y1+1
  ss = paste(Y1,Y2,sep="-")

  # Ranking criteria for European leagues:
  rank.ENG1 = c("-Pts -GD -GF -Pts.agg -GD.agg -GF.agg -AGF.agg")   # Default for all leagues
  rank.BEL1 = c("-Pts -W -GF")
  rank.UKR1 = rank.BEL1
  rank.ESP1 = c("-Pts -Pts.agg -GD.agg -GF")
  rank.ITA1 = rank.ESP1
  rank.PRT1 = rank.ESP1
  rank.GRC1 = c("-Pts -Pts.agg -GD.agg -GF")
  rank.TUR1 = c("-Pts -Pts.agg -GD.agg -GD -GF")
  rank.RUS1 = c("-Pts -Pts.agg -GD.agg -GF.agg -AGF.agg -GD -GF -AGF")

  # Create the output:
  Tt = tibble(Div=c("ENG1","BEL1","UKR1","ESP1","ITA1","PRT1","GRC1","TUR1","RUS1"),
                   OrderL=c(rank.ENG1,rank.BEL1,rank.UKR1,rank.ESP1,rank.ITA1,rank.PRT1,rank.GRC1,rank.TUR1,rank.RUS1)) %>%
    separate(OrderL,sprintf("Order%02d", 1:20),sep=" ") %>%
    gather(OrderString,Parameter,Order01:Order20) %>%
    arrange(Div,OrderString) %>%
    filter(!is.na(Parameter)) %>%
    group_by(Div) %>%
    mutate(Order=row_number()) %>%
    ungroup()

  Tt = full_join(Tt,expand.grid(Div=unique(Tt$Div),Season=ss,stringsAsFactors = FALSE)) %>%
    dplyr::select(Div,Season,Parameter,Order) %>%
    arrange(Div,Season,Order)

  return(Tt)
}

