#' Extract SnowWarp Annual Statistics
#'
#' This function extracts SnowWarp annual statistics from the SnowWarp output generated using the function process_snowwarp. The three statistics
#' calculated are date of snow accumulation, date of snow melt, and number of days with snow in winter year (from Aug 1st - July 31st).
#' Annual stats will be extracted for all the years of SnowWarp that were processed.
#'
#' @param folder The main directory where the Google Earth Engine files are located on your computer. There should already be an 'output' folder
#' inside this directory with the data from the process_snowwarp function.
#' @param cpus The number of cpus to use for parallel processing. This should not be greater than the number of tiles since each tile is
#' processed in parallel.
#' @param file_ext File name descriptor that you added to the process_snowwarp function. No descriptor by default.
#' @param tiles Tiles that you have already processed using the process_snowwarp function.
#' @param maxmemory The amount of memory allocated for the raster package. Defaults to 1e+10, which is slightly more than default.
#' @return The extract_snowwarp_stats function returns .tif rasters with annual snow statistics (for each winter year). Each file
#' contains three bands. Band 1 is the date of snow accumulation, Band 2 is the date of snow melt, and Band 3 is the
#' number of days with snow in the winter year. These files are written to 'folder/output'.
#' @seealso \code{\link{process_snowwarp}}, \code{\link{get_snowwarp_tiles}}, \code{\link{download_snowwarp_data}}
#' @examples
#' For Mac:
#' extract_snowwarp_stats(folder = '/Users/MyUser/Documents/GEE_snow_download',
#' cpus = 8,
#' file_ext = 'studyarea1',
#' tiles = 1:8)
#'
#' For Windows:
#' extract_snowwarp_stats(folder = 'D:/GEE_snow_download',
#' cpus = 8,
#' file_ext = 'studyarea1',
#' tiles = 1:8)
#'
#' @export

extract_snowwarp_stats <- function(
  folder, #main directory where data is located
  cpus, #number of cpus to use for parallel processing
  file_ext = '', #file extension added to snowwarp output and to add to stats output
  tiles = NULL, #option to process a certain number of tiles. make sure already ran process_snowwarp on these tiles.
  maxmemory = 1e+10 #maximum memory used by raster package
){

  ##################################
  ###CHECK FOR PACKAGES INSTALLED###
  ##################################

  #check for packages and return error if missing
  packages <- c('tidyverse', 'raster', 'doParallel', 'foreach', 'zoo', 'ArgumentCheck')
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) stop(paste0("\n Package ", new_packages," needed for this function to work. Please install it."))

  #define local %operators%
  `%dopar%` <- foreach::`%dopar%`
  `%>%` <- stringr::`%>%`

  ###########################
  ###CHECK INPUT VARIABLES###
  ###########################

  #add check argument
  check <- ArgumentCheck::newArgCheck()

  #add an error if the folder is not a character vector
  if(is.character(folder) == F) {
    ArgumentCheck::addError(
      msg = "'folder' is not a character vector",
      argcheck = check
    )
  }

  #add an error if the folder directory doesn't exist
  if(dir.exists(folder) == F) {
    ArgumentCheck::addError(
      msg = "'folder' does not point to a valid directory",
      argcheck = check
    )
  }

  #add an error if cpus is not numeric and is longer than 1
  if(is.numeric(cpus) == F) {
    ArgumentCheck::addError(
      msg = "'cpus' is not a numeric vector",
      argcheck = check
    )
  } else if(length(cpus) != 1) {
    ArgumentCheck::addError(
      msg = "'cpus' does not contain a single value",
      argcheck = check
    )
  }

  #add an error if file_ext is not character and is longer than 1
  if(is.character(file_ext) == F) {
    ArgumentCheck::addError(
      msg = "'file_ext' is not a character vector",
      argcheck = check
    )
  } else if(length(file_ext) != 1) {
    ArgumentCheck::addError(
      msg = "'file_ext' does not contain a single value",
      argcheck = check
    )
  }

  #add an error if maxmemory is not numeric and is longer than 1
  if(is.numeric(maxmemory) == F) {
    ArgumentCheck::addError(
      msg = "'maxmemory' is not a numeric vector",
      argcheck = check
    )
  } else if(length(maxmemory) != 1) {
    ArgumentCheck::addError(
      msg = "'maxmemory' does not contain a single value",
      argcheck = check
    )
  }

  #Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(check)

  ###################
  ###INITIAL SETUP###
  ###################

  #set raster options to use a bit more memory
  raster::rasterOptions(maxmemory = maxmemory)

  #set wd
  setwd(folder)

  #set output file_ext with underscore
  if(file_ext != '') file_ext = stringr::str_c('_', file_ext)

  #read in snowwarp data
  snow_files <- Sys.glob(file.path('output', stringr::str_c('snowwarp_[0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9]_*',
                                                   file_ext, '.tif')))

  #check that there are actually snow files to extract stats from
  if(length(snow_files) == 0) stop(stringr::str_c('No SnowWarp files found. Please verify the folder, file extention, ',
                                   'and that the process_snowwarp function ran successfully.'))

  #subset only specified tiles
  if(is.null(tiles) == F) {
    if(is.numeric(tiles) == F) {
      stop("'tiles' is not a numeric vector.")
    } else {
      snow_files2 <- character()
      for(i in 1:length(tiles)) {
        snow_check <- stringr::str_subset(snow_files, stringr::str_c('tile_', tiles[i]))
        if(length(snow_check) == 0) stop(stringr::str_c("No files found for tile ", tiles[i], '. Please check that tile numbers match ',
                                                        'the tiles processed using the process_snowwarp function.'))
        snow_files2 <- append(snow_files2, stringr::str_subset(snow_files, stringr::str_c('tile_', tiles[i])))
      }
      snow_files <- snow_files2
    }
  }

  #define moving window size
  window <- 31

  #define number of interations
  num_iter = 100

  #register parallel backend
  cl <- parallel::makeCluster(cpus)
  doParallel::registerDoParallel(cl)

  #loop through landsat files
  invisible(foreach::foreach(l = 1:length(snow_files)) %dopar% {

    #load snowwarp brick
    ras <- raster::brick(snow_files[l])

    #allocate output array
    stats <- array(as.integer(NA), dim = c(nrow(ras), ncol(ras), 3))

    #find segment of rows to run
    row_seg <- split(1:nrow(ras), factor(sort(rank(1:nrow(ras))%%100)))

    for(iter in 1:num_iter){

      #load in snowwarp chunk
      snow <- raster::getValuesBlock(ras, row = min(row_seg[[iter]]), nrows = length(row_seg[[iter]]),
                             col = 1)

      #make snowwarp binary
      snow[snow < 15] = 0;
      snow[snow >= 15] = 1;

      #find number of days with snow
      snow_days <- apply(snow, 1, FUN = function(x) sum(x, na.rm = T))

      #add NA pad at beginning and end of time series
      snow2 <- matrix(as.integer(NA), nrow = nrow(snow), ncol = ncol(snow) + 30)

      #fill with snow values
      snow2[,16:380] <- snow
      rm(snow)

      #calculate moving average along rows
      snow_roll <- apply(snow2, 1, FUN = function(x) zoo::rollapply(x, window, FUN = function(y) mean(y, na.rm = T)))

      #segment snow roll into fall and spring
      snow_fall = snow_roll[1:153,]
      snow_spring = snow_roll[154:365,]
      rm(snow_roll)

      #find fall and spring transtion dates
      doy_fall <- apply(snow_fall, 2, FUN = function(x) if(sum(x > .5, na.rm = T) == 0) NA else min(which(x > .5)))
      doy_spring <- apply(snow_spring, 2, FUN = function(x) if(sum(x > .5, na.rm = T) == 0) NA else max(which(x > .5)) + 153)

      #restructure to enter into stats array
      snow_days <- array(as.integer(snow_days),
                         dim = c(ncol(ras), length(row_seg[[iter]]), 1))

      doy_fall <- array(as.integer(doy_fall),
                         dim = c(ncol(ras), length(row_seg[[iter]]), 1))

      doy_spring <- array(as.integer(doy_spring),
                         dim = c(ncol(ras), length(row_seg[[iter]]), 1))

      #change to row/column/layer
      snow_days <- aperm(snow_days, c(2, 1, 3))
      doy_fall <- aperm(doy_fall, c(2, 1, 3))
      doy_spring <- aperm(doy_spring, c(2, 1, 3))

      #add to stats
      stats[row_seg[[iter]],,1] <- doy_fall
      stats[row_seg[[iter]],,2] <- doy_spring
      stats[row_seg[[iter]],,3] <- snow_days

    }

    #create output raster
    out <- raster::brick(ras, nl = 3)

    #fill with stats values
    out <- raster::setValues(out, aperm(stats, c(2,1,3)))

    #write raster
    raster::writeRaster(out, filename = str_replace(snow_files[l], 'snowwarp', 'snowwarp_stats'),
                format = 'GTiff', datatype = 'INT2U', overwrite = T, NAflag = 65534)

  })

  #stop parallel cluster
  parallel::stopCluster(cl)

} #end of function
