% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AN_Elo_season.R
\name{AN_Elo_season}
\alias{AN_Elo_season}
\title{AN_Elo_season}
\usage{
AN_Elo_season(data, startingQs = NA, a = 1/1400, b1 = 0.05, b2 = -0.6,
  k = 35)
}
\arguments{
\item{data}{match data tibble that should contain one season for one league with columns "Div","Season","Date","HomeTeam","AwayTeam" and "Res"}

\item{startingQs}{tibble with columns "Team" and "Q0"}

\item{a}{division factor}

\item{b1}{home team correction factor}

\item{b2}{away team correction factor}

\item{k}{k-factor}
}
\value{
tibble with columns "Div","Season","Date","HomeTeam","AwayTeam","Res","HWpct","Dpct","AWpct","QH0","QH1","QA0","QA1"
}
\description{
calculates all Elo ratings and home, draw and away percentages per match
}
