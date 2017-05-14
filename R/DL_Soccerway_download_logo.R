#' DL_Soccerway_download_logo
#'
#' Download logos to the specified folder, which is a subfolder of the working directory.
#' If the folder doesn't exist then create it.
#' @examples df = DL_Soccerway_data("NLD1"); df.logos = DL_Soccerway_season_logo("NLD",df$URL.logo);
#' DL.input = df.logos %>% mutate(FileName=paste(CountryCode,Team,sep=" - "));DL_Soccerway_download_logo(DL.input,"LogoTest")
#'
#' @import tidyverse
#' @export
#' @param DL.input a tibble that should contain columns FileName (string, without extension) and LogoURL.
#' @param subfolder the name of the subfolder of the current working directory, e.g. "Logos"
#' @param overwrite if TRUE then download all logos and overwrite existing logos in the folder with the same name
#'

DL_Soccerway_download_logo = function(DL.input,subfolder,overwrite=FALSE){
  currentFolder = getwd()
  newFolder = paste(currentFolder,subfolder,sep="/")
  # create folder if necessary
  dir.create(file.path(currentFolder, subfolder), showWarnings = FALSE)
  setwd(newFolder)
  existingFiles=list.files(path = getwd())
  # HIER GEBLEVEN
  setwd(currentFolder)
}
