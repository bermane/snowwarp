#' Download SnowWarp Data
#'
#' This function downloads imagery from Google Earth Engine that will be input into the SnowWarp algorithm. Please refer to the
#' instruction manual \url{https://github.com/bermane/snowwarp},
#'and follow the steps to acquire the necessary imagery. You may have issues if Google Chrome is not
#' your default web browser.
#'
#' @param mail Your Google Drive account email
#' @param link The URL to the Google Drive folder that contains the SnowWarp input imagery.
#' @param folder The folder where you want the imagery to be downloaded to. This will be the same folder you use for the other
#' SnowWarp functions.
#' @return The download_snowwarp_data function downloads imagery pre-processed on Google Earth Engine. It is necessary to download the imagery
#' before running the \code{\link{get_snowwarp_tiles}} and \code{\link{process_snowwarp}} functions.
#' @seealso \code{\link{get_snowwarp_tiles}}, \code{\link{process_snowwarp}}, \code{\link{extract_snowwarp_stats}}
#' @examples
#' download_snowwarp_data(
#' mail = "saverio.francini@unifi.it",
#' link = "https://drive.google.com/drive/u/1/folders/1nWauSi-6dGkdn9htUodSszskbi1F9NHz",
#' folder = file.path(getwd(), "snowwarp_data"))
#'
#' @export

download_snowwarp_data <- function(
  mail, # your drive account e-mail
  link = "https://drive.google.com/drive/u/1/folders/1nWauSi-6dGkdn9htUodSszskbi1F9NHz", # the link of your drive folder
  folder = file.path(getwd(), "snowwarp_data"),
  overwrite = T) {
  requiredPackages <- c("googledrive")
  new.packages <-
    requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages)

  googledrive::drive_auth(mail)
  print(googledrive::drive_user())
  filteredFiles <- googledrive::drive_ls(googledrive::as_id(link))
  if (dir.exists(folder) == F)
    dir.create(folder)
  outFileNames <- file.path(folder, filteredFiles$name)
  lapply(1:nrow(filteredFiles), function(i) {
    googledrive::drive_download(googledrive::as_id(filteredFiles$id[i]), outFileNames[i], overwrite = overwrite)
  })
  return(outFileNames)

}
