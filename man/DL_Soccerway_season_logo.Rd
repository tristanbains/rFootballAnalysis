% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DL_Soccerway_season_logo.R
\name{DL_Soccerway_season_logo}
\alias{DL_Soccerway_season_logo}
\title{DL_Soccerway_season_logo}
\usage{
DL_Soccerway_season_logo(countryCode, url)
}
\arguments{
\item{countryCode}{e.g. "ESP"}

\item{url}{e.g. "http://nl.soccerway.com/national/spain/segunda-division/20152016/regular-season/r32028/map"}
}
\value{
tibble with columns CountryCode ("ESP"), Team ("UD Almería"), LogoURL ("http://cache.images.core.optasports.com/soccer/teams/150x150/2049.png")
}
\description{
creates tibble with logo url per club. url input usually comes from DL_Soccerway_season_data(), the URL.logo column
df = DL_Soccerway_data("NLD1"); DL_Soccerway_season_logo("NLD",df$URL.logo)
}
