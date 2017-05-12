context("DL_FootballDataCoUk_season")

# library(tidyverse)
div="ENG0"
url="http://www.football-data.co.uk/mmz4281/1516/E0.csv"

result = suppressMessages(suppressWarnings(
  DL_FootballDataCoUk_season(div = div,url = url)))
