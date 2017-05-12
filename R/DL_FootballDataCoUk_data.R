#' DL_FootballDataCoUk_data
#'
#' download multiple seasons for multiple league from www.football-data.co.uk
#'
#' @export
#' @import zoo
#' @return tibble with columns ISO4 (e.g. "NLD1"), Year (e.g. "2005") and URL
#'

DL_FootballDataCoUk_data = function(){
  ENG=tibble::tibble(ISO4=paste0("ENG",1:4),
             CSV=paste0("E",0:3),
             Year=1993)
  SCO=tibble::tibble(ISO4=paste0("SCO",1:4),
             CSV=paste0("SC",0:3),
             Year=1997)
  DEU=tibble::tibble(ISO4=c("DEU1","DEU2"),
             CSV=c("D1","D2"),
             Year=1993)
  ESP=tibble::tibble(ISO4=c("ESP1","ESP2"),
             CSV=c("SP1","SP2"),
             Year=1996)
  ITA=tibble::tibble(ISO4=c("ITA1","ITA2"),
             CSV=c("I1","I2"),
             Year=1997)
  FRA=tibble::tibble(ISO4=c("FRA1","FRA2"),
             CSV=c("F1","F2"),
             Year=1996)
  NLD=tibble::tibble(ISO4="NLD1",
             CSV="N1",
             Year=1993)
  BEL=tibble::tibble(ISO4="BEL1",
             CSV="B1",
             Year=1995)
  PRT=tibble::tibble(ISO4="PRT1",
             CSV="P1",
             Year=1994)
  GRC=tibble::tibble(ISO4="GRC1",
             CSV="G1",
             Year=1994)
  TUR=tibble::tibble(ISO4="TUR1",
             CSV="T1",
             Year=1994)

  Tt = rbind(ENG,SCO,DEU,ESP,ITA,FRA,NLD,BEL,PRT,GRC,TUR) %>%
    arrange(ISO4)

  Y1 = 1990
  Y2 = as.numeric(format(Sys.Date(),"%Y"))+1
  Ys = expand.grid(ISO4=unique(Tt$ISO4),Year=Y1:Y2,stringsAsFactors = FALSE)

  Tt = Tt %>%
    full_join(Ys) %>%
    arrange(ISO4,Year) %>%
    group_by(ISO4) %>%
    mutate(BooleanNA=ifelse(is.na(CSV),0,1),
           BooleanNA=cumsum(BooleanNA)) %>%
    filter(BooleanNA>0) %>%
    mutate(CSV=na.locf(CSV)) %>%
    ungroup() %>%
    mutate(URL=paste0("http://www.football-data.co.uk/mmz4281","/",paste0(substr(Year,3,4),substr(Year+1,3,4)),"/",CSV,".csv")) %>%
    dplyr::select(ISO4,Year,URL)

  # "http://www.football-data.co.uk/mmz4281/1516/E0.csv"

  return(Tt)
}
