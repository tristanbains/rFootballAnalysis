context("DL_FootballDataCoUk")

divs = c("ENG1","FRA1")
season1 = "1990-1991"
season2 = "1996-1997"

result = suppressMessages(suppressWarnings(
  DL_FootballDataCoUk(divs=divs,season1=season1,season2=season2)))
