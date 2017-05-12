context("AN_Elo_season")

data = suppressMessages(suppressWarnings(
  DL_Matches("NLD1",season1 = "2015-2016",season2 = "2015-2016")))

startingQs=NA
a=1/1400
b1=0.05
b2=-0.6
k=35

result = suppressMessages(suppressWarnings(
  AN_Elo_season(data=data,startingQs = startingQs,a = a,b1 = b1,b2 = b2,k = k)))
