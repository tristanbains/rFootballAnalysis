% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DL_Matches.R
\name{DL_Matches}
\alias{DL_Matches}
\title{DL_Matches}
\usage{
DL_Matches(divs, season1, season2, website = "FootballDataCoUk")
}
\arguments{
\item{divs}{e.g. c("ENG1","FRA1")}

\item{season1}{e.g. ("2001-2002")}

\item{season2}{e.g. ("2005-2006")}

\item{website}{e.g. "FootballDataCoUk"}
}
\value{
tibble for multiple seasons and leagues with all matches, results and bookmaker odds (if available)
}
\description{
wrapper function that downloads multiple seasons for multiple league from one of the source websites
}
