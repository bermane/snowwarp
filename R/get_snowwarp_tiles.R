#' Get the number of SnowWarp tiles to be processed
#'
#' This function prints a message with the number of SnowWarp tiles to be processed. The number can be used to split up the processing.
#'
#' @param folder The main directory where the Google Earth Engine files are located on your computer. There should already be an 'output' folder
#' inside this directory with the data from the process_snowwarp function.
#' @return The get_num_tiles function returns the number of tiles that need to be processed using the process_snowwarp function.
#' @seealso \code{\link{process_snowwarp}}, \code{\link{extract_snowwarp_stats}}, \code{\link{download_snowwarp_data}}
#' @examples
#' For Mac:
#' get_snowwarp_tiles(folder = '/Users/MyUser/Documents/GEE_snow_download')
#'
#' For Windows:
#' get_snowwarp_tiles(folder = 'D:/GEE_snow_download')
#'

get_snowwarp_tiles <- function(folder) { #main directory where data is located

  #add an error if the folder is not a character vector
  if(is.character(folder) == F) stop("'folder' is not a character vector")

  #add an error if the folder directory doesn't exist
  if(dir.exists(folder) == F) stop("'folder' does not point to a valid directory")

  #set wd
  setwd(folder)

  #load file names
  ls_files <- list.files(pattern = 'FscaLandsat', full.names = T)

  #return error if there are no files, otherwise print number of tiles
  if(length(ls_files) == 0) {
    stop("No tiles from Google Earth Engine were found. Please verify directory or re-download.")
  } else print(paste0("There are ", length(ls_files), " tiles."))

} #end of function
