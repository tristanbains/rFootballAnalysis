#' DL_Soccerway_download_logo
#'
#' Download logos to the specified folder, which is a subfolder of the working directory.
#' If the folder doesn't exist then create it.
#' @examples setwd("/Users/tristanbains/Desktop/R/rFootballAnalysis");lg="RUS2";
#' lgs=substr(lg,1,3);df = DL_Soccerway_data(lg); df.logos = DL_Soccerway_season_logo(lgs,df$URL.logo);
#' DL.input = df.logos %>% mutate(FileName=paste0(CountryCode," - ",Team,".png"));DL_Soccerway_download_logo(DL.input)
#'
#'
#'
#' @import tidyverse
#' @export
#' @param DL.input a tibble that should contain columns FileName (string, with extension) and LogoURL.
#' @param subfolder the name of the subfolder where to store thelogos, e.g. "~/Desktop/Logos"
#' @param overwrite if TRUE then download all logos and overwrite existing logos in the folder with the same name
#'

DL_Soccerway_download_logo = function(DL.input,subfolder="~/Desktop/Logos",overwrite=FALSE){
  currentFolder = getwd()
  # create folder if necessary
  dir.create(file.path(subfolder), showWarnings = FALSE)
  setwd(subfolder)
  existingFiles=list.files(path = getwd())
  newFiles=setdiff(DL.input$FileName,existingFiles)
  if(overwrite){
    dldf = DL.input
  } else {
    dldf = DL.input %>% filter(FileName %in% newFiles)
  }
  # Download the files:
  for(i in 1:nrow(dldf)){
    message(paste0("DL_Soccerway_download_logo: folder ",subfolder," adding ",dldf$FileName[i]))
    download.file(url=dldf$LogoURL[i],destfile=dldf$FileName[i],mode = "wb",quiet = TRUE)
  }
  # Remove empty logos and placeholders:
  existingFiles=list.files(path = getwd())
  fileInfo=file.info(existingFiles)
  fileInfo$FileName=row.names(fileInfo)
  noOfficialLogo = fileInfo %>%
    filter(grepl(".png",FileName)) %>%
    filter(size==9817|size==13176)
  if(nrow(noOfficialLogo)>0){
    message(paste0("DL_Soccerway_download_logo: folder ",subfolder," deleting some logos because they are empty/placeholders",dldf$FileName[i]))
    file.remove(noOfficialLogo$FileName)
  }
  setwd(currentFolder)
}
