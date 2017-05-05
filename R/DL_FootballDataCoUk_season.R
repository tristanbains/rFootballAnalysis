#' Add together two numbers.
#'
#' @param div e.g. ENG0
#' @param url e.g. http://www.football-data.co.uk/mmz4281/1516/E0.csv
#' @return tibble with
#'
#'
DL_FootballDataCoUk_season=function(div,url){
  Tttt=read.csv(url,stringsAsFactors=FALSE)
  Ttt = Tttt %>%
    filter(!is.na(HomeTeam),
           HomeTeam!="",
           !is.na(HomeTeam)) %>%
    mutate(Div=div,
           Res=sign(FTHG-FTAG),
           Season="",
           Date=ifelse(nchar(Date)==8,as.Date(Date,format="%d/%m/%y"),as.Date(Date,format="%d/%m/%Y")),
           HomeTeam=sub("(^ *)(.*)","\\2",HomeTeam),
           AwayTeam=sub("(^ *)(.*)","\\2",AwayTeam)) %>%
    mutate(Date=as.Date(Date))
  # determine best odds and bookmakers:
  cn = colnames(Ttt)
  bcols = as.data.frame(table(substr(cn,1,nchar(cn)-1)),stringsAsFactors=FALSE) %>%
    mutate(Var2=substr(Var1,1,3)) %>%
    group_by(Var2) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(Freq==3,n==1) %>%
    dplyr::select(Var1)
  if(nrow(bcols)>0){
    hcols=intersect(cn,paste(bcols$Var1,"H",sep=""))
    dcols=intersect(cn,paste(bcols$Var1,"D",sep=""))
    acols=intersect(cn,paste(bcols$Var1,"A",sep=""))
    HO = Ttt %>%
      dplyr::select(one_of(hcols)) %>%
      mutate(None=0,
             None2=0)
    BHO=apply(HO,1,max,na.rm=TRUE)
    DO = Ttt %>%
      dplyr::select(one_of(dcols)) %>%
      mutate(None=0,
             None2=0)
    BDO=apply(DO,1,max,na.rm=TRUE)
    AO = Ttt %>%
      dplyr::select(one_of(acols)) %>%
      mutate(None=0,
             None2=0)
    #AO.ranked=data.frame(t(apply(-AO, 1, rank, ties.method='min')))
    #AO.ranked=data.frame(t(apply(-AO, 1, rank, ties.method='first')))
    BAO=apply(AO,1,max,na.rm=TRUE)
    OV=1/BHO+1/BDO+1/BAO
    boolean.OVcorrection=(OV<0.99)
    BHO2=apply(HO,1,function(x) sort(x, decreasing = TRUE)[2])
    BDO2=apply(DO,1,function(x) sort(x, decreasing = TRUE)[2])
    BAO2=apply(AO,1,function(x) sort(x, decreasing = TRUE)[2])
    # TO DO: correctie om NA en -Inf
    BBHO=colnames(HO)[apply(HO,1,which.max)]
    BBDO=colnames(DO)[apply(DO,1,which.max)]
    BBAO=colnames(AO)[apply(AO,1,which.max)]
    maxn = function(n) function(x) order(x, decreasing = TRUE)[n]
    BBHO2=colnames(HO)[apply(HO,1,maxn(2))]
    BBDO2=colnames(DO)[apply(DO,1,maxn(2))]
    BBAO2=colnames(AO)[apply(AO,1,maxn(2))]
    Ttt = Ttt %>%
      mutate(BHO=ifelse(OV<0.99,BHO2,BHO),
             BDO=ifelse(OV<0.99,BDO2,BDO),
             BAO=ifelse(OV<0.99,BAO2,BAO),
             BHO=ifelse(BHO==0,NA,BHO),
             BDO=ifelse(BDO==0,NA,BDO),
             BAO=ifelse(BAO==0,NA,BAO),
             BHO=round(BHO,2),
             BDO=round(BDO,2),
             BAO=round(BAO,2),
             BBHO=ifelse(OV<0.99,BBHO2,BBHO),
             BBDO=ifelse(OV<0.99,BBDO2,BBDO),
             BBAO=ifelse(OV<0.99,BBAO2,BBAO),
             BBHO=substr(BBHO,1,nchar(BBHO)-1),
             BBDO=substr(BBDO,1,nchar(BBDO)-1),
             BBAO=substr(BBAO,1,nchar(BBAO)-1),
             BBHO=gsub("Non.*",NA,BBHO),
             BBDO=gsub("Non.*",NA,BBDO),
             BBAO=gsub("Non.*",NA,BBAO))
  } else {
    Ttt = Ttt %>%
      mutate(BHO=NA,
             BDO=NA,
             BAO=NA,
             BBHO=NA,
             BBDO=NA,
             BBAO=NA)
  }
  # create final output:
  cols=c("Div","Season","Date","HomeTeam","AwayTeam","FTHG","FTAG","Res","HTHG","HTAG","BHO","BDO","BAO","BBHO","BBDO","BBAO")
  cn=intersect(cols,colnames(Ttt))
  Ttt=Ttt[,cn] %>%
    mutate(HomeTeam=sub("^ *","",HomeTeam),
           HomeTeam=sub(" *$","",HomeTeam),
           AwayTeam=sub("^ *","",AwayTeam),
           AwayTeam=sub(" *$","",AwayTeam))
  return(Ttt)
}
