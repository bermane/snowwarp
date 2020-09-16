#' Download SnowWarp Data
#'
#' This function downloads imagery from Google Earth Engine that will be input into the SnowWarp algorithm. Please refer to the
#' instruction manual \url{https://htmlpreview.github.io/?https://github.com/bermane/snowwarp/blob/master/start_snowwarp.html},
#'and follow the steps to acquire the necessary imagery. You may have issues if Google Chrome is not
#' your default web browser.
#'
#' @param mail Your Google Drive account email
#' @param link The shareable link to the Google Drive folder that contains the SnowWarp input imagery. (1) Right click on the Google Drive folder.
#' (2) Obtain shareable link.
#' @param folder The folder where you want the imagery to be downloaded to. This will be the same folder you use for the other
#' SnowWarp functions.
#' @return The download_snowwarp_data function downloads imagery pre-processed on Google Earth Engine. It is necessary to download the imagery
#' before running the \code{\link{get_snowwarp_tiles}} and \code{\link{process_snowwarp}} functions.
#' @seealso \code{\link{get_snowwarp_tiles}}, \code{\link{process_snowwarp}}, \code{\link{extract_snowwarp_stats}}
#' @examples
#' For Mac:
#' download_snowwarp_data(mail = 'myemail at server.com',
#' link = 'shareable google drive folder link',
#' folder = '/Users/MyUser/Documents/GEE_snow_download')
#'
#' For Windows:
#' process_snowwarp(mail = 'myemail at server.com',
#' link = 'shareable google drive folder link',
#' folder = 'D:/GEE_snow_download')
#'
#' @export

download_snowwarp_data <- function(
  mail,                                  # your drive account e-mail
  link,                            # (1) right-click on the drive folder (2) obtain shareable link
  folder = file.path(getwd(), "snowwarp_data"),   # OPTIONAL: specify the directory path where you want to imagery
  overwrite = T
  ){

googledrive::drive_auth(mail); print(googledrive::drive_user())
filteredFiles <- googledrive::drive_ls(googledrive::as_id(link))
if(dir.exists(folder)==F) dir.create(folder)
outFileNames <- file.path(folder, filteredFiles$name)
lapply(1:nrow(filteredFiles), function(i){googledrive::drive_download(as_id(filteredFiles$id[i]), outFileNames[i], overwrite = overwrite)})
return(outFileNames)

}
