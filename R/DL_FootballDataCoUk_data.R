#' DL_FootballDataCoUk_data
#'
#' download multiple seasons for multiple league from www.football-data.co.uk
#'
#' @export
#' @import zoo
#' @return tibble with columns ISO3 (e.g. "NLD1"), Year (e.g. "2005") and URL
#'
#'
DL_FootballDataCoUk_data = function(){
  ENG=tibble(ISO3=paste0("ENG",1:4),
             CSV=paste0("E",0:3),
             Year=1993)
  SCO=tibble(ISO3=paste0("SCO",1:4),
             CSV=paste0("SC",0:3),
             Year=1997)
  DEU=tibble(ISO3=c("DEU1","DEU2"),
             CSV=c("D1","D2"),
             Year=1993)
  ESP=tibble(ISO3=c("ESP1","ESP2"),
             CSV=c("SP1","SP2"),
             Year=1996)
  ITA=tibble(ISO3=c("ITA1","ITA2"),
             CSV=c("I1","I2"),
             Year=1997)
  FRA=tibble(ISO3=c("FRA1","FRA2"),
             CSV=c("F1","F2"),
             Year=1996)
  NLD=tibble(ISO3="NLD1",
             CSV="N1",
             Year=1993)
  BEL=tibble(ISO3="BEL1",
             CSV="B1",
             Year=1995)
  PRT=tibble(ISO3="PRT1",
             CSV="P1",
             Year=1994)
  GRC=tibble(ISO3="GRC1",
             CSV="G1",
             Year=1994)
  TUR=tibble(ISO3="TUR1",
             CSV="T1",
             Year=1994)

  Tt = ENG %>%
    full_join(SCO) %>%
    full_join(DEU) %>%
    full_join(ESP) %>%
    full_join(ITA) %>%
    full_join(FRA) %>%
    full_join(NLD) %>%
    full_join(BEL) %>%
    full_join(PRT) %>%
    full_join(GRC) %>%
    full_join(TUR) %>%
    arrange(ISO3)

  Y1 = 1990
  Y2 = as.numeric(format(Sys.Date(),"%Y"))+1
  Ys = expand.grid(ISO3=unique(Tt$ISO3),Year=Y1:Y2,stringsAsFactors = FALSE)

  Tt = Tt %>%
    full_join(Ys) %>%
    arrange(ISO3,Year) %>%
    group_by(ISO3) %>%
    mutate(BooleanNA=ifelse(is.na(CSV),0,1),
           BooleanNA=cumsum(BooleanNA)) %>%
    filter(BooleanNA>0) %>%
    mutate(CSV=na.locf(CSV)) %>%
    ungroup() %>%
    mutate(URL=paste0("http://www.football-data.co.uk/mmz4281","/",paste0(substr(Year,3,4),substr(Year+1,3,4)),"/",CSV,".csv")) %>%
    dplyr::select(ISO3,Year,URL)

  # "http://www.football-data.co.uk/mmz4281/1516/E0.csv"


  return(Tt)
}
