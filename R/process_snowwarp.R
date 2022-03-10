# Process SnowWarp Data
#'
#' last modified: Jan 13, 2022
#'
#' This function processes SnowWarp using imagery downloaded from Google Earth Engine. Please refer to instruction manual
#' \url{https://htmlpreview.github.io/?https://github.com/bermane/snowwarp/blob/master/start_snowwarp.html},
#' to ensure you have properly downloaded the necessary imagery.
#'
#' @param folder The main directory where the Google Earth Engine files are located on your computer.
#' @param years The years of data you want processed. Must be numeric and between 2000-2019. Snowwarp runs on winter years (Aug 1st - July 31st)
#' instead of calendar years, so years = 2005 means August 1, 2005 - July 31, 2006, etc.
#' It is better to run all the years you want at once, and less tiles.
#' @param cpus The number of cpus to use for parallel processing. We recommend using at least half of the cpus on your machine. More if
#' you are able. For example, note that often an "8" core machine means "16" cpus. Therefore this value should be >= 8.
#' @param otb_dir The main directory where the Orfeo Toolbox is located on your computer.
#' @param max_ram The maximum ram (in MB) available to the smoothing function from Orfeo Toolbox. Defaults to 256 and can only be increased.
#' It is strongly advised to increase this number to at least half your computer's RAM. Consider using the same proportion of RAM as porportion of CPUS/threads.
#' @param tiles Option to process a certain number of tiles. Run get_snowwarp_tiles first to get the total number of tiles.
#' It is better to run the function over a smaller number of tiles and all the years desired. SNOWWARP processes one tile at a time
#' so you can run a test on the first tile. Defaults to processing all tiles.
#' @return The process_snowwarp function returns .tif rasters with daily values of fractional snow covered area. Each file
#' contains values from August 1st - July 31st of the subsequent year. These daily values can be used
#' in of themselves, or can be further input into the extract_snowwarp_stats function to calculate annual statistics.
#' These files are written to 'folder/output'. Temporary files generated during processing are stored in 'folder/temp'
#' and organized by tile. These need to be deleted manually! Once the first tile is finished, check the output folder
#' to make sure everything is working correctly before deleting the temporary files.
#' **Please note that there is a small bug that sometimes returns values greater than 100.
#' After processing snowwarp you should set all values greater than 100 to NA**
#' @seealso \code{\link{get_snowwarp_tiles}}, \code{\link{extract_snowwarp_stats}}, \code{\link{download_snowwarp_data}}
#' @examples
#' For Mac:
#' process_snowwarp(folder = '/Users/MyUser/Documents/GEE_snow_download',
#' years = 2004:2008,
#' cpus = 8,
#' otb_dir = '/Users/MyUser/Users/OTB-7.1.0-Darwin64'
#' max_ram = 8000
#' tiles = 1:8)
#'
#' For Windows:
#' process_snowwarp(folder = 'D:/GEE_snow_download',
#' years = 2004:2008,
#' cpus = 8,
#' oct_dir = 'C:/OTB-7.1.0-Win64'
#' max_ram = 8000
#' tiles = 1:8)
#'
#'
#' @export

process_snowwarp <-
  function(folder,
           #main directory where data is located
           years,
           #years of processing output. 2005 means August 1, 2005 till July 31, 2006 etc. Must be between 2000-2019
           cpus,
           #number of cpus to use for parallel processing
           otb_dir,
           #main directory where Orfeo Toolbox is located
           max_ram = 256,
           #maximum ram (in MB) available for smoothing. Defaults to 256.
           tiles = NULL #option to process a certain number of tiles. run get_num_tiles first.
           ) {
           ##################################
           ###CHECK FOR PACKAGES INSTALLED###
           ##################################
           
           #check for packages and return error if missing
           requiredPackages <-
             c(
               'tidyverse',
               'gdalUtils',
               'raster',
               'doParallel',
               'foreach',
               'pspline',
               'smoothie',
               'dtw',
               'parallel'
             )
           new.packages <-
             requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
           if (length(new.packages))
             install.packages(new.packages)
           
           #define local %operators%
           `%dopar%` <- foreach::`%dopar%`
           `%>%` <- stringr::`%>%`
           
           ###################
           ###INITIAL SETUP###
           ###################
           
           #read in landsat and modis data
           ls_bands <-
             list.files(folder, pattern = 'BandNamesLandsat', full.names = T)
           mod_bands <-
             list.files(folder, pattern = 'BandNamesModis', full.names = T)
           
           
           #stop and print error if band files are missing
           if (length(ls_bands) == 0)
             stop('Files from Google Earth Engine are missing. Please verify or re-download.')
           if (length(mod_bands) == 0)
             stop('Files from Google Earth Engine are missing. Please verify or re-download.')
           
           ls_bands <-
             read.table(ls_bands, sep = ",")
           mod_bands <-
             read.table(mod_bands, sep = ",")
           
           #transpose and remove useless columns
           ls_bands <- ls_bands %>% t
           ls_bands <-
             as.character(ls_bands[2:(NROW(ls_bands) - 1), 1])
           mod_bands <- mod_bands %>% t
           mod_bands <-
             as.character(mod_bands[2:(NROW(mod_bands) - 1), 1])
           
           #set data years. Code will pull data from
           #Aug 1st of start year until July 31st of end year
           data_yrs <-
             mod_bands %>% stringr::str_extract(pattern = '[:digit:]{4}') %>% as.numeric
           data_yrs <- min(data_yrs):max(data_yrs)
           
           #load file names
           ls_files <-
             list.files(folder, pattern = 'FscaLandsat', full.names = T)
           mod_files <-
             list.files(folder, pattern = 'FscaModis', full.names = T)
           
           #stop and print error if raster files are missing
           if (length(ls_files) == 0)
             stop('Files from Google Earth Engine are missing. Please verify or re-download.')
           if (length(mod_files) == 0)
             stop('Files from Google Earth Engine are missing. Please verify or re-download.')
           
           #check if there is a tiles variables and that it contains values within the number of tiles
           #else set tiles to total number
           if (is.null(tiles) == F) {
             if (is.numeric(tiles) == F) {
               stop("'tiles' is not a numeric vector")
             } else if (length(tiles[!(tiles %in% 1:length(ls_files))])) {
               stop(stringr::str_c(
                 "'tiles' does not only have values between 1-",
                 length(ls_files)
               ))
             }
           } else
             tiles <- 1:length(ls_files)
           
           #build virtual raster for MODIS if there are multiple files
           if (length(mod_files) > 1) {
             invisible(gdalUtils::gdalbuildvrt(gdalfile = mod_files, output.vrt = "mod_ras.vrt"))
             mod_ras <-
               raster::brick('mod_ras.vrt')
           } else {
             mod_ras <- raster::brick(mod_files)
           }
           
           #############################################
           ###CHECK FOR MODIS DATA CONTINUITY AND FIX###
           #############################################
           
           #create date seq based on data years input
           dates <-
             seq(as.Date(stringr::str_c(data_yrs[1], "-08-01")), as.Date(stringr::str_c(data_yrs[length(data_yrs)], "-07-31")), by =
                   "days")
           
           #remove leap year values
           dates <- dates[!grepl('02-29', dates)]
           
           #parse mod_bands as a date object
           dates_m <-
             mod_bands %>% stringr::str_extract('[:digit:]{4}_[:digit:]{2}_[:digit:]{2}')
           dates_m <- lubridate::ymd(dates_m)
           
           #find location of leap year values
           dates_leap <-
             stringr::str_which(dates_m, '[:digit:]{4}-02-29')
           
           #remove leap year values from MODIS dates
           dates_m <- dates_m[-dates_leap]
           
           #map modis band dates to the correct full vector of dates
           dates_id <- match(dates_m, dates)
           
           rm(dates, dates_m)
           
           ##################################################
           ###CREATE MODIS ID RASTER FOR ENTIRE STUDY AREA###
           ##################################################
           
           #create modis id raster
           raster::writeRaster(
             mod_ras[[1]],
             filename = file.path(folder, 'mod_id'),
             datatype = 'INT4S',
             overwrite = T
           )
           
           #fill with ID values
           mod_id <-
             raster::raster(file.path(folder, 'mod_id'), values = F)
           mod_id <-
             raster::writeStart(
               mod_id,
               filename = file.path(folder, 'mod_id'),
               format = 'raster',
               datatype = 'INT4S',
               overwrite = T
             )
           tr <- raster::blockSize(mod_id)
           for (i in 1:tr$n) {
             if (i != tr$n) {
               v <-
                 ((tr$row[i] - 1) * ncol(mod_id) + 1):((tr$row[i + 1] - 1) * ncol(mod_id))
             } else {
               v <- ((tr$row[i] - 1) * ncol(mod_id) + 1):raster::ncell(mod_id)
             }
             mod_id <-
               raster::writeValues(mod_id, v, tr$row[i])
           }
           mod_id <- raster::writeStop(mod_id)
           rm(tr, i)
           
           #keep mod_id as a matrix as well
           mod_id_m <- raster::as.matrix(mod_id)
           
           ###################################################
           ###RUN ALGORITHM ON EACH LANDSAT FILE SEPARATELY###
           ###################################################
           
           ################################
           ###LOOP THROUGH LANDSAT FILES###
           ################################
           
           temporaryFolder <-
             file.path(folder, 'temp')
           outputFolder <-
             file.path(folder, 'output')
           
           #create temp and output folders
           if (dir.exists(temporaryFolder) == F)
             dir.create(temporaryFolder)
           if (dir.exists(outputFolder) == F)
             dir.create(outputFolder)
           
           #loop through landsat files
           for (l in tiles) {
             #print processing started message
             print(stringr::str_c('Processing of Landsat tile ', l, ' started at ', Sys.time(), '.'))
             
             #create temp dir for tile and years if doesn't already exist
             tileDir <-
               file.path(temporaryFolder, paste0("tile_", l))
             if (dir.exists(tileDir) == F)
               dir.create(tileDir)
             
             invisible(sapply(1:length(years), function(i) {
               yearDir <- file.path(tileDir, paste0(years[i], "_", years[i] + 1))
               if (dir.exists(yearDir) == F) {
                 dir.create(yearDir)
               }
             }))
             
             #load landsat file
             ls_ras <- raster::brick(ls_files[l])
             
             ##################################
             ###BUILD LANDSAT MODIS RELATION###
             ##################################
             
             #fill cells with values from modis id and convert to matrix
             ls_id <-
               raster::resample(mod_id, ls_ras[[1]], method = "ngb") %>% raster::as.matrix(.)
             
             #create vector of modis rows and columns to process based on overlap with landsat tile
             mod_loc <-
               sapply(unique(as.vector(ls_id)), function(x)
                 which(mod_id_m == x, arr.ind = T)) %>% t
             colnames(mod_loc) <- c('row', 'col')
             
             ##################################################
             ###SET UP PARALLEL PROCESSING OVER MODIS PIXELS###
             ##################################################
             
             #register parallel backend
             cl <- parallel::makeCluster(cpus)
             doParallel::registerDoParallel(cl)
             
             #loop through each modis pixel
             invisible(foreach::foreach(id = 1:nrow(mod_loc)) %dopar% {
               #####################
               ###TIME WARP MODIS###
               #####################
               
               #set initial parameters
               window <- 30
               threshold <- 30
               main_perc <- .5
               
               #set row and col to process
               mod_row <-
                 mod_loc[id, 1] %>% as.integer
               mod_col <-
                 mod_loc[id, 2] %>% as.integer
               
               #define MODIS neighborhood range
               r_grab <- 2
               c_grab <- 2
               r_low <- mod_row - 1
               r_high <- mod_row + 1
               c_low <- mod_col - 1
               c_high <- mod_col + 1
               
               if (r_low < 1)
                 r_low <- 1
               r_grab <- 1
               if (c_low < 1)
                 c_low <- 1
               c_grab <- 1
               if (r_high > NROW(mod_ras))
                 r_high <- NROW(mod_ras)
               if (c_high > NCOL(mod_ras))
                 c_high <- NCOL(mod_ras)
               
               #load in MODIS chunk all years
               mod_p <-
                 raster::getValuesBlock(
                   mod_ras,
                   row = r_low,
                   nrows = (r_high - r_low + 1),
                   col = c_low,
                   ncols = (c_high - c_low + 1)
                 )
               
               #restructure as array
               mod_p <-
                 array(as.numeric(mod_p), dim = c((c_high - c_low + 1),
                                                  (r_high - r_low + 1),
                                                  raster::nbands(mod_ras)
                 ))
               
               #mask bad values
               mod_p[mod_p >= 200] <- NA
               
               #change to row/col/layer for processing
               mod_p <-
                 aperm(mod_p, c(2, 1, 3), resize = T)
               
               #calculate pixel value based on target pixel and neighborhood (main_perc above dictates %)
               mod_p <-
                 apply(mod_p, 3, function(x) {
                   pix_main <- x[r_grab, c_grab]
                   x[r_grab, c_grab] <- NA
                   pix_neigh <- mean(x, na.rm = T)
                   if (is.na(pix_main) == 0 &
                       is.na(pix_neigh) == 0)
                     (pix_main * main_perc) + (pix_neigh * (1 - main_perc))
                   else
                     NA
                 }) %>%
                 as.numeric
               
               #fix MODIS chunk to match full date seq
               #remove leap year values
               mod_p <- mod_p[-dates_leap]
               
               #allocate vector of full date seq
               mod_pix <-
                 as.numeric(rep(NA, times = ((
                   length(data_yrs) - 1
                 ) * 365)))
               
               #fill with modis values
               mod_pix[dates_id] <- mod_p
               rm(mod_p)
               
               #segment array into matrix by winter year
               #code needs to screen at beginning that the full data is downloaded and ready to go in order, or else shouldn't process!!!
               mod_pix <-
                 matrix(mod_pix, nrow = 365, ncol = (length(data_yrs) - 1))
               
               #set parameters to interpolate daily values after spline
               
               #set gap size and gaussian level
               gap_size = 7
               gauss_lvl = 2
               
               #find NA values and determine gap sizes
               vals_na <- is.na(mod_pix)
               rle_na <-
                 apply(vals_na, 2, function(x) {
                   rle_hold <- rle(x)
                   loc <-
                     cumsum(rle_hold$lengths) + 1
                   loc <- append(loc, 1, after = 0)
                   loc <- loc[-length(loc)]
                   len <-
                     rle_hold$lengths[rle_hold$values == T]
                   loc <- loc[rle_hold$values == T]
                   loc <- loc[len > gap_size]
                   len <- len[len > gap_size]
                   matrix(c(loc, len),
                          ncol = 2,
                          dimnames = list(NULL, c("loc", "len")))
                 })
               rm (vals_na)
               
               #fit initial spline to MODIS fsca, resulting in daily values
               #sm.spline chooses the amount of smoothing based on the supplied values
               #smooth.Pspline lets you choose the spar value... possibly better to have it be constant?
               #mod_pix <- apply(mod_pix, 2, function(pix) {
               #  c(predict(sm.spline(x = which(is.na(pix) == 0),
               #                      y = pix[is.na(pix) == 0]), 1:365))
               #})
               
               #only apply to pixels if there are enough modis values to run the function
               if (sum(apply(mod_pix, 2, function(x)
                 sum(is.na(x))) < 320) != 0) {
                 mod_pix <- apply(mod_pix, 2, function(pix) {
                   c(predict(
                     pspline::smooth.Pspline(
                       x = which(is.na(pix) == 0),
                       y = pix[is.na(pix) == 0],
                       norder = 2,
                       method = 1,
                       spar = (1 - 0.5) / 0.5     # p given in Matlab csaps
                     ),
                     1:365
                   ))
                 })
                 
                 #since there are some NA values at beginning of years, refit using another interpolation spline
                 mod_pix <-
                   apply(mod_pix, 2, function(pix)
                     spline(
                       x = 1:365,
                       y = pix,
                       xout = 1:365
                     ) %>% .[['y']])
                 
                 #use linear interpolation for gaps bigger than gap size
                 #use for loops for now but try to vectorize later!!!
                 #or not since it runs super fast...
                 for (j in 1:ncol(mod_pix)) {
                   if (nrow(rle_na[[j]]) > 0) {
                     for (i in 1:nrow(rle_na[[j]])) {
                       if (rle_na[[j]][, 'loc'][i] == 1) {
                         #if values start at beginning of vector
                         mod_pix[(rle_na[[j]][, 'loc'][i]):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i]), j] <-
                           approx(
                             x = c(rle_na[[j]][, 'loc'][i], rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i]),
                             y = c(mod_pix[rle_na[[j]][, 'loc'][i], j], mod_pix[rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i], j]),
                             xout = (rle_na[[j]][, 'loc'][i]):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i])
                           )$y
                       } else if ((rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i]) == nrow(mod_pix) + 1) {
                         #if values end at the end of vector
                         mod_pix[(rle_na[[j]][, 'loc'][i] - 1):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i] - 1), j] <-
                           approx(
                             x = c(rle_na[[j]][, 'loc'][i] - 1, rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i] - 1),
                             y = c(mod_pix[rle_na[[j]][, 'loc'][i] - 1, j], mod_pix[rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i] - 1, j]),
                             xout = (rle_na[[j]][, 'loc'][i] - 1):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i] - 1)
                           )$y
                       } else {
                         #if the values are in the middle of vector
                         mod_pix[(rle_na[[j]][, 'loc'][i] - 1):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i]), j] <-
                           approx(
                             x = c(rle_na[[j]][, 'loc'][i] - 1, rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i]),
                             y = c(mod_pix[rle_na[[j]][, 'loc'][i] - 1, j], mod_pix[rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i], j]),
                             xout = (rle_na[[j]][, 'loc'][i] - 1):(rle_na[[j]][, 'loc'][i] + rle_na[[j]][, 'len'][i])
                           )$y
                       }
                     }
                   }
                 }
                 
                 #smooth using a gaussian filter
                 if (gauss_lvl != 0) {
                   mod_pix <-
                     apply(mod_pix, 2, function(pix) {
                       #window taken from matlab default filter size for imgaussfilt
                       smoothie::kernel2dsmooth(
                         matrix(pix),
                         kernel.type = 'gauss',
                         nx = 365,
                         ny = 1,
                         sigma = gauss_lvl
                       ) %>%
                         scales::rescale(., to = c(min(pix), max(pix)))
                     })
                 }
                 
                 #round, set values > 100 to 100, values < 0 to 0
                 mod_pix <- mod_pix %>% round
                 mod_pix[mod_pix > 100] <- 100
                 mod_pix[mod_pix < 0] <- 0
                 
                 #apply dynamic time warping algorithm
                 #the output is warp paths for every processing year
                 #the value of the warp paths is where each index DOY gets warped to
                 
                 #create index of which years are target years
                 target_id <-
                   which(data_yrs %in% years)
                 
                 #generate warp paths, applied over target_id
                 warp_paths <-
                   sapply(target_id, function(target) {
                     #vector of query years not including target
                     query_id <-
                       seq_along(mod_pix[1, ])
                     
                     #apply dtw over query years for each target year
                     sapply(query_id, function(query) {
                       #if query_id is target, return DOY 1:365 since no warp path
                       if (query == target) {
                         1:365
                       } else{
                         #run dtw
                         d <-
                           dtw::dtw(
                             mod_pix[, query],
                             mod_pix[, target],
                             window.type = dtw::sakoeChibaWindow,
                             window.size = window
                           )
                         
                         #simplify warping function to get warp_paths
                         #when a single DOY from query year warps to multiple target
                         #year DOYs, take mean target DOY
                         sapply(1:365, function(query_doy) {
                           target_doy <- d$index2[d$index1 == query_doy] %>% mean %>% round
                           
                           #remove warp values if difference between target and query values is
                           #greater than threshold
                           if (abs(mod_pix[target_doy, target] - mod_pix[query_doy, query]) > threshold) {
                             target_doy <- NA
                           }
                           
                           #return target_doy value
                           target_doy
                         })
                       }
                     })
                   }, simplify = "array")
                 
                 ###########################
                 ###APPLY WARP TO LANDSAT###
                 ###########################
                 
                 #find landsat pixels the correspond to MODIS pixel warped
                 #the pixels will be a rectangle since MODIS and LS in same projection
                 #this means we can process all pixels enclosed in the min/max row/col vals
                 ls_mask <-
                   which(ls_id == mod_id[mod_row, mod_col], arr.ind = T)
                 
                 #only run the warp processing if the mask sum is greater then zero
                 #which should be always given the extents are the same
                 #but just to be sure
                 if (sum(ls_mask) > 0) {
                   #load in landsat chunk all years
                   ls_p <-
                     raster::getValuesBlock(
                       ls_ras,
                       row = min(ls_mask[, 1]),
                       nrows = (max(ls_mask[, 1]) - min(ls_mask[, 1]) + 1),
                       col = min(ls_mask[, 2]),
                       ncols = (max(ls_mask[, 2]) - min(ls_mask[, 2]) + 1)
                     )
                   
                   #restructure as array
                   ls_p <-
                     array(as.numeric(ls_p), dim = c((max(ls_mask[, 2]) - min(ls_mask[, 2]) + 1),
                                                     (max(ls_mask[, 1]) - min(ls_mask[, 1]) + 1),
                                                     raster::nbands(ls_ras)
                     ))
                   
                   #change to row/column/layer for processing
                   ls_p <-
                     aperm(ls_p, c(2, 1, 3), resize = T)
                   
                   #set no data values
                   ls_p[ls_p == 255] <- NA
                   
                   #extract date values from landsat bands
                   ls_dates <-
                     stringr::str_extract(ls_bands, '[:digit:]{8}')
                   ls_dates <-
                     lubridate::ymd(ls_dates)
                   
                   #create vector of landsat DOY
                   ls_doy <-
                     lubridate::yday(ls_dates)
                   
                   #subtract 1 in leap years to keep DOY uniform
                   ls_doy[lubridate::leap_year(ls_dates) == T &
                            ls_doy > 59] <-
                     ls_doy[lubridate::leap_year(ls_dates) == T &
                              ls_doy > 59] - 1
                   
                   #subtract 212 and add 365 from negative DOY to convert
                   #to winter DOY (WDOY)
                   ls_doy <- ls_doy - 212
                   ls_doy[ls_doy < 1] <-
                     ls_doy[ls_doy < 1] + 365
                   
                   #create vector of landsat year
                   ls_yr <-
                     lubridate::year(ls_dates)
                   
                   #change year to match WDOY -- so if winter starts in year 2000
                   #Jan-July 2001 should read as year 2000 to facilitate warping
                   #since winter years strattle calendar years
                   ls_yr[lubridate::month(ls_dates) < 8] <-
                     ls_yr[lubridate::month(ls_dates) < 8] - 1
                   
                   #apply warp paths to each target year of data
                   #output is landsat time-series for each target year
                   ls_out <-
                     sapply(1:dim(warp_paths)[3], function(x) {
                       #convert warp paths into vector for each year
                       warp <-
                         warp_paths[, , x] %>% as.vector
                       
                       #create index of the landsat data we have
                       #in the same shape as the warp paths
                       warp_id <-
                         (365 * (ls_yr - min(data_yrs))) + ls_doy
                       
                       #only keep the warp values we have data for
                       warp_doy <- warp[warp_id]
                       
                       #create id vector length of ls_dates
                       #and remove values where we don't have landsat
                       #data to use for warp
                       band_id <-
                         1:length(ls_dates) %>% .[is.na(warp_doy) == F]
                       
                       #remove warp_doy values with NA
                       warp_doy <-
                         warp_doy[is.na(warp_doy) == F]
                       
                       #create vector of unique days to warp to
                       unique_doy <-
                         unique(warp_doy)
                       
                       #create array of warped landsat data
                       #take mean if more than one landsat band warps
                       #to a given DOY in target year
                       snow <-
                         sapply(unique_doy, function(doy) {
                           apply(
                             ls_p[, , band_id[warp_doy == doy]],
                             c(1, 2),
                             FUN = function(x)
                               mean(x, na.rm = T)
                           ) %>% round
                         }, simplify = "array")
                       
                       #change NaN to NA
                       snow[is.nan(snow)] <- NA
                       
                       #create output snow matrix for each year
                       out <-
                         array(as.integer(NA), dim = c(dim(ls_p)[1], dim(ls_p)[2], 365))
                       
                       #fill ls_out with snow values from warp
                       out[, , unique_doy] <- snow
                       
                       #send matrix out
                       out
                       
                     }, simplify = "array")
                   
                   #apply spline to output landsat pixels, resulting in daily values
                   ls_out <-
                     sapply(1:dim(ls_out)[4], function(yr) {
                       #apply over all years
                       sapply(1:dim(ls_out)[2], function(c) {
                         #apply over all columns
                         sapply(1:dim(ls_out)[1], function(r) {
                           #apply over all rows
                           
                           #load in single pixel time series
                           pix <-
                             ls_out[r, c, , yr]
                           
                           #only apply spline if have more than 45 data points
                           if (sum(is.na(pix)) < 320) {
                             #find NA values and determine gap sizes
                             vals_na <- is.na(pix)
                             rle_hold <-
                               rle(vals_na)
                             loc <-
                               cumsum(rle_hold$lengths) + 1
                             loc <-
                               append(loc, 1, after = 0)
                             loc <-
                               loc[-length(loc)]
                             len <-
                               rle_hold$lengths[rle_hold$values == T]
                             loc <-
                               loc[rle_hold$values == T]
                             loc <-
                               loc[len > gap_size]
                             len <-
                               len[len > gap_size]
                             rle_na <-
                               matrix(c(loc, len),
                                      ncol = 2,
                                      dimnames = list(NULL, c("loc", "len")))
                             
                             #apply initial spline
                             pix <- c(predict(
                               pspline::smooth.Pspline(
                                 x = which(is.na(pix) == 0),
                                 y = pix[is.na(pix) == 0],
                                 norder = 2,
                                 method = 1,
                                 spar = (1 - 0.5) / 0.5     # p given in Matlab csaps
                               ),
                               1:365
                             ))
                             
                             #since there are some NA values at beginning of years, refit using another interpolation spline
                             pix <-
                               spline(x = 1:365,
                                      y = pix,
                                      xout = 1:365) %>% .[['y']]
                             
                             #use linear interpolation for gaps bigger than gap size
                             #use for loops for now but try to vectorize later!!!
                             #or not since it runs super fast...
                             if (nrow(rle_na) > 0) {
                               for (i in 1:nrow(rle_na)) {
                                 if (rle_na[, 'loc'][i] == 1) {
                                   #if values start at beginning of vector
                                   pix[(rle_na[, 'loc'][i]):(rle_na[, 'loc'][i] + rle_na[, 'len'][i])] <-
                                     approx(
                                       x = c(rle_na[, 'loc'][i], rle_na[, 'loc'][i] + rle_na[, 'len'][i]),
                                       y = c(pix[rle_na[, 'loc'][i]], pix[rle_na[, 'loc'][i] + rle_na[, 'len'][i]]),
                                       xout = (rle_na[, 'loc'][i]):(rle_na[, 'loc'][i] + rle_na[, 'len'][i])
                                     )$y
                                 } else if ((rle_na[, 'loc'][i] + rle_na[, 'len'][i]) == length(pix) + 1) {
                                   #if values end at the end of vector
                                   pix[(rle_na[, 'loc'][i] - 1):(rle_na[, 'loc'][i] + rle_na[, 'len'][i] - 1)] <-
                                     approx(
                                       x = c(rle_na[, 'loc'][i] - 1, rle_na[, 'loc'][i] + rle_na[, 'len'][i] - 1),
                                       y = c(pix[rle_na[, 'loc'][i] - 1], pix[rle_na[, 'loc'][i] + rle_na[, 'len'][i] - 1]),
                                       xout = (rle_na[, 'loc'][i] - 1):(rle_na[, 'loc'][i] + rle_na[, 'len'][i] - 1)
                                     )$y
                                 } else {
                                   #if the values are in the middle of vector
                                   pix[(rle_na[, 'loc'][i] - 1):(rle_na[, 'loc'][i] + rle_na[, 'len'][i])] <-
                                     approx(
                                       x = c(rle_na[, 'loc'][i] - 1, rle_na[, 'loc'][i] + rle_na[, 'len'][i]),
                                       y = c(pix[rle_na[, 'loc'][i] - 1], pix[rle_na[, 'loc'][i] + rle_na[, 'len'][i]]),
                                       xout = (rle_na[, 'loc'][i] - 1):(rle_na[, 'loc'][i] + rle_na[, 'len'][i])
                                     )$y
                                 }
                               }
                             }
                             
                             #smooth using a gaussian filter
                             if (gauss_lvl != 0) {
                               pix <-
                                 smoothie::kernel2dsmooth(
                                   matrix(pix),
                                   kernel.type = 'gauss',
                                   nx = 365,
                                   ny = 1,
                                   sigma = gauss_lvl
                                 ) %>%
                                 scales::rescale(., to = c(min(pix), max(pix)))
                             }
                             
                             #round, set values > 100 to 100, values < 0 to 0
                             pix <- pix %>% round
                             pix[pix > 100] <- 100
                             pix[pix < 0] <- 0
                             
                             #send pix out
                             pix
                             
                           } else {
                             #if not enough data points set values to no data
                             pix[] <- 255
                             
                             #send out pix
                             pix
                           }
                         })
                       }, simplify = 'array')
                     }, simplify = 'array')
                   
                   #change ls_out back to array dimensions we want
                   ls_out <-
                     aperm(ls_out, c(2, 3, 1, 4))
                   
                   #just in case extra NA values set them to no data (shouldn't be any NA)
                   ls_out[is.na(ls_out)] <- 255
                   
                 } else{
                   ls_out <-
                     array(data = 255,
                           dim = c(
                             length(min(ls_mask[, 1]):max(ls_mask[, 1])),
                             length(min(ls_mask[, 2]):max(ls_mask[, 2])),
                             365,
                             length(years)
                           ))
                 }
                 
                 #change data back to columns first
                 #ls_out <- aperm(ls_out, c(2, 1, 3, 4))
                 
                 #change to integer
                 #storage.mode(ls_out) <- 'integer'
                 
                 #write out data in small chunks. will merge after processing
                 invisible(sapply(1:dim(ls_out)[4], function(i) {
                   #create output raster template
                   ls <-
                     raster::crop(ls_ras[[1]],
                                  raster::extent(
                                    ls_ras,
                                    min(ls_mask[, 1]),
                                    max(ls_mask[, 1]),
                                    min(ls_mask[, 2]),
                                    max(ls_mask[, 2])
                                  ))
                   
                   #fill output raster with values
                   ls <-
                     raster::brick(
                       ls_out[, , , i],
                       xmn = raster::extent(ls)[1],
                       xmx = raster::extent(ls)[2],
                       ymn = raster::extent(ls)[3],
                       ymx = raster::extent(ls)[4],
                       crs = raster::crs(ls)
                     )
                   
                   #write chunk to disk
                   yearDir <-
                     file.path(tileDir, paste0(years[i], "_", years[i] + 1))
                   filNameYear_i <-
                     file.path(yearDir, paste0(id, ".tif"))
                   
                   raster::writeRaster(
                     ls,
                     filename = filNameYear_i,
                     format = 'GTiff',
                     datatype = 'INT1U',
                     NAflag = 255,
                     overwrite = T,
                     options = rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
                   )
                   
                   rm(ls)
                   
                 }))
               } else {
                 #end of if function to ensure enough MODIS pixels. if not output NA
                 
                 #define landsat mask
                 ls_mask <-
                   which(ls_id == mod_id[mod_row, mod_col], arr.ind = T)
                 
                 #write out data in small chunks. will merge after processing
                 invisible(sapply(1:length(years), function(i) {
                   #create output raster template
                   ls <-
                     raster::crop(ls_ras[[1]],
                                  raster::extent(
                                    ls_ras,
                                    min(ls_mask[, 1]),
                                    max(ls_mask[, 1]),
                                    min(ls_mask[, 2]),
                                    max(ls_mask[, 2])
                                  ))
                   
                   #fill output raster with values
                   ls <-
                     raster::brick(
                       nrows = NROW(ls),
                       ncols = NCOL(ls),
                       xmn = raster::extent(ls)[1],
                       xmx = raster::extent(ls)[2],
                       ymn = raster::extent(ls)[3],
                       ymx = raster::extent(ls)[4],
                       nl = 365,
                       crs = raster::crs(ls)
                     )
                   ls[] <- NA
                   
                   #write chunk to disk
                   yearDir <-
                     file.path(tileDir, paste0(years[i], "_", years[i] + 1))
                   filNameYear_i <-
                     file.path(yearDir, paste0(id, ".tif"))
                   
                   raster::writeRaster(
                     ls,
                     filename = filNameYear_i,
                     format = 'GTiff',
                     datatype = 'INT1U',
                     NAflag = 255,
                     overwrite = T,
                     options = rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
                   )
                   
                   rm(ls)
                   
                 }))
                 
               } #end of function that only outputs NA if not enough modis values
               
               
               
               
             }) #end of loop through each MODIS pixel
             
             #stop parallel cluster
             parallel::stopCluster(cl)
             
             ######################################
             ###MOSAIC AND SMOOTH LANDSAT OUTPUT###
             ######################################
             
             # to avoid error with too many temp files being joined together at once, mosaic temp files in smaller
             # groups first, then mosaic smaller, intermediate .vrt files together
             for (i in 1:length(years)) {
               yearDir <-
                 file.path(tileDir, paste0(years[i], "_", years[i] + 1)) # filNameYear_i <- file.path(yearDir, paste0(id, ".tif"))
               
               # get dimensions of boundary area
               dimension <- dim(mod_ras)
               ncol <- dimension[2]
               nrow <- dimension[1]
               ntiles <-
                 length(grep(
                   list.files(path = yearDir,
                              pattern = 'tif'),
                   pattern = '_',
                   invert = TRUE
                 ))
               
               # set initial value
               n <- 1
               # determine number of temp files per .vrt (value < 500)
               while (round(ncol / 2 ^ n) * nrow > 500) {
                 n <- n + 1
               }
               
               vrt_size <-
                 nrow * round(ncol / 2 ^ n)
               last_vrt <- ntiles %% vrt_size
               
               # number of temp files in each sub .vrt
               if (last_vrt == 0) {
                 num_vrt <- ntiles %/% vrt_size
               } else {
                 num_vrt <- ntiles %/% vrt_size + 1
               }
               
               num_cols <- vrt_size / nrow
               num_cols_last <- last_vrt / nrow
               
               mos_f <-
                 matrix(, nrow = num_vrt, ncol = vrt_size)
               
               
               # make intermediate .vrt's that will be in  ./temp directory
               for (n in 1:num_vrt) {
                 if (last_vrt != 0) {
                   if (n < num_vrt) {
                     mos_f[n,] <-
                       Sys.glob(paths = file.path(yearDir, paste0(((1:vrt_size) + vrt_size * (n - 1)
                       ), '.tif')),
                       dirmark = T)
                     
                     invisible(
                       gdalUtils::gdalbuildvrt(
                         gdalfile = mos_f[n,],
                         output.vrt = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                         srcnodata = '255',
                         vrtnodata = '255',
                         NUM_THREADS = cpus,
                         overwrite = T
                       )
                     )
                     invisible(
                       gdalUtils::gdal_translate(
                         src_dataset = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                         dst_dataset = file.path(yearDir, paste0('tif_', n, '.tif'))
                       )
                     )
                   }
                   else if (n == num_vrt) {
                     mos_f[n, 1:last_vrt] <-
                       Sys.glob(paths = file.path(yearDir, paste0(((1:last_vrt) + vrt_size * (n - 1)
                       ), '.tif')),
                       dirmark = T)
                     
                     invisible(
                       gdalUtils::gdalbuildvrt(
                         gdalfile = mos_f[n, 1:last_vrt],
                         output.vrt = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                         srcnodata = '255',
                         vrtnodata = '255',
                         NUM_THREADS = cpus,
                         overwrite = T
                       )
                     )
                     invisible(
                       gdalUtils::gdal_translate(
                         src_dataset = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                         dst_dataset = file.path(yearDir, paste0('tif_', n, '.tif'))
                       )
                     )
                   }
                 }
                 else if (last_vrt == 0) {
                   mos_f[n,] <-
                     Sys.glob(paths = file.path(yearDir, paste0(((1:vrt_size) + vrt_size * (n - 1)
                     ), '.tif')),
                     dirmark = T)
                   invisible(
                     gdalUtils::gdalbuildvrt(
                       gdalfile = mos_f[n,],
                       output.vrt = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                       srcnodata = '255',
                       vrtnodata = '255',
                       NUM_THREADS = cpus,
                       overwrite = T
                     )
                   )
                   invisible(
                     gdalUtils::gdal_translate(
                       src_dataset = file.path(yearDir, paste0('vrt_', n, '.vrt')),
                       dst_dataset = file.path(yearDir, paste0('tif_', n, '.tif'))
                     )
                   )
                 }
                 
               }
               
               # load intermediate tif files
               mos_tif <-
                 Sys.glob(paths = file.path(yearDir, paste0('tif_', 1:num_vrt, '.tif')),
                          dirmark = T)
               
               # build large, final vrt by mosaicking intermediate vrt files
               invisible(
                 gdalUtils::gdalbuildvrt(
                   gdalfile = mos_tif,
                   output.vrt = file.path(yearDir, paste0('vrt_final.vrt')),
                   srcnodata = '255',
                   vrtnodata = '255',
                   NUM_THREADS = cpus,
                   overwrite = T
                 )
               )
               
               #run smoothing function
               invisible(system2(
                 command = stringr::str_c(
                   list.files(
                     path = stringr::str_c(otb_dir, '/bin'),
                     pattern = 'otbcli_Smoothing',
                     include.dirs = T,
                     full.names = T
                   )
                 ),
                 args = c(
                   stringr::str_c(
                     '-in ',
                     folder,
                     '/temp/tile_',
                     l,
                     '/',
                     years[i],
                     '_',
                     years[i] + 1,
                     '/vrt_final.vrt'
                   ),
                   stringr::str_c(
                     '-out ',
                     folder,
                     '/output/snowwarp_',
                     years[i],
                     "_",
                     years[i] + 1,
                     '_tile_',
                     l,
                     '.tif uint8'
                   ),
                   '-type mean',
                   '-type.mean.radius 1',
                   stringr::str_c('-ram ', max_ram)
                 )
               ))
               
               # clean up the values that are over 100
               all_files <-
                 raster::brick(
                   stringr::str_c(
                     folder,
                     '/output/snowwarp_',
                     years[i],
                     "_",
                     years[i] + 1,
                     '_tile_',
                     l,
                     '.tif'
                   )
                 )
               all_files [all_files > 100] <- NA
               raster::writeRaster(
                 all_files,
                 stringr::str_c(
                   folder,
                   '/output/snowwarp_',
                   years[i],
                   "_",
                   years[i] + 1,
                   '_tile_',
                   l,
                   '.tif'
                 ),
                 datatype = 'INT1U',
                 format = 'GTiff',
                 overwrite = T,
                 options = rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
               )
             }
             
             
             #print processing complete message
             print(stringr::str_c(
               'Processing of Landsat tile ',
               l,
               ' finished at ',
               Sys.time(),
               '.'
             ))
             
             #delete all files from temp folder
             #for now make users manually delete files to ensure things are working
             #invisible(unlink(stringr::str_c('temp/tile_', l), recursive = T))
             
             
           } #end of loop over landsat tiles
           
           } #end of function
