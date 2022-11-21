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
#' @param tiles Tiles that you have already processed using the process_snowwarp function.
#' @return The extract_snowwarp_stats function returns .tif rasters with annual snow statistics (for each winter year). Each file
#' contains three bands. Band 1 is the date of snow accumulation, Band 2 is the date of snow melt, and Band 3 is the
#' number of days with snow in the winter year. These files are written to 'folder/output'.
#' @seealso \code{\link{process_snowwarp}}, \code{\link{get_snowwarp_tiles}}, \code{\link{download_snowwarp_data}}
#' @examples
#' For Mac:
#' extract_snowwarp_stats(folder = '/Users/MyUser/Documents/GEE_snow_download',
#' cpus = 8,
#' tiles = 1:8)
#'
#' For Windows:
#' extract_snowwarp_stats(folder = 'D:/GEE_snow_download',
#' cpus = 8,
#' tiles = 1:8)
#'
#' @export

extract_snowwarp_stats <- function(
  folder, #main directory where data is located
  cpus, #number of cpus to use for parallel processing
  tiles = NULL #option to process a certain number of tiles. make sure already ran process_snowwarp on these tiles.
){

  #define local %operators%
  `%dopar%` <- foreach::`%dopar%`
  `%>%` <- stringr::`%>%`

  ###################
  ###INITIAL SETUP###
  ###################

  #set wd
  setwd(folder)

  #read in snowwarp data
  snow_files <- Sys.glob(file.path('output', stringr::str_c('snowwarp_[0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9]_*.tif')))

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

  #loop through landsat files
  for(l in 1:length(snow_files)) {

    if(isFALSE(dir.exists("temp"))) dir.create("temp")
    
    #create output file base
    out_f <- stringr::str_replace(snow_files[l], 'snowwarp', 'snowwarp_stats') %>%
      stringr::str_replace(., 'output', 'temp')

    #load snowwarp brick
    ras <- raster::brick(snow_files[l])

    #define number of interations
    num_iter = 100
    if(nrow(ras)<300){
      num_iter <- round(nrow(ras)/4)
    }
    
    #find segment of rows to run
    row_seg <- split(1:nrow(ras), factor(sort(rank(1:nrow(ras))%%num_iter)))

    ## clean parallel computing
    unregister_dopar <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    }

    #register parallel backend
    cl <- parallel::makeCluster(cpus)
    doParallel::registerDoParallel(cl)

    #process in parallel over row iters
    invisible(foreach::foreach(iter = 1:num_iter) %dopar% {

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

      #put output layers into single array
      ls_out <- abind::abind(doy_fall, doy_spring, snow_days)

      #create output raster template
      ls <- raster::crop(ras[[1:3]], raster::extent(ras, min(row_seg[[iter]]), max(row_seg[[iter]]),
                                                     1, ncol(ras)))

      #fill output raster with values
      ls <- raster::brick(ls_out, xmn = raster::extent(ls)[1], xmx = raster::extent(ls)[2],
                          ymn = raster::extent(ls)[3], ymx = raster::extent(ls)[4], crs = raster::crs(ls))

      #create specific output file
      out_f_row <- stringr::str_replace(out_f, '.tif', stringr::str_c('_r', min(row_seg[[iter]]), '_r', max(row_seg[[iter]]), '.tif'))

      #write chunk to disk
      raster::writeRaster(ls, filename = out_f_row, format = 'GTiff',
                          datatype = 'INT2U', NAflag = 65534, overwrite = T,
                          options = rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE"))

      }) #end of parallel loop over iter

    #stop parallel cluster
    parallel::stopCluster(cl)
    unregister_dopar()

    ###########################
    ###MOSAIC LANDSAT OUTPUT###
    ###########################

    #load file list of raster chunks
    mos_f <- Sys.glob(stringr::str_replace(out_f, '.tif', '_*.tif'))

    #mosaic rasters
    invisible(gdalUtils::mosaic_rasters(gdalfile = mos_f, dst_dataset = stringr::str_replace(out_f, 'temp', 'output'),
                              NUM_THREADS = cpus))

    #print processing complete message
    print(stringr::str_c('Processing of SnowWarp stats for ', snow_files[l], ' finished at ', Sys.time(), '.'))

    #delete all files from temp folder
    do.call(file.remove, list(list.files('temp', full.names = TRUE)))

} #end of loop over landsat files

} #end of function
