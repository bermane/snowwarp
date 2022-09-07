# Process SnowWarp Data
#'
#' last modified: Mar 08, 2022
#'
#' This function calculates daily snow covered areas over a specific area of interest using the \code{\link{process_snowwarp}} outputs
#'
#' @param folder The main directory where the Google Earth Engine files are located on your computer (i.e., the same folder you used for process_snowwarp)
#' @param aoi The shapefile of the an Area Of Interest
#' @return The calculate_snowwarp_sca function returns a csv file that indicates for each day in the time series the percentage of the aoi that is covered by snow
#' @seealso \code{\link{get_snowwarp_tiles}}, \code{\link{extract_snowwarp_stats}}, \code{\link{download_snowwarp_data}}, \code{\link{process_snowwarp}}
#' @examples
#' calculate_snowwarp_sca(
#' folder = 'snowWarpFolder',
#' aoi = raster::shapefile("aoi.shp")
#' )
#' @export

calculate_snowwarp_sca <- function(folder, aoi) {
  #check for packages and return error if missing
  requiredPackages <-
    c('snowfall',
      'data.table',
      'raster',
      'doParallel')
  new.packages <-
    requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages)

  infiles <- list.files(file.path(folder, "output"), full.names = T)
  inr <- lapply(infiles, raster::stack)
  s <- do.call(raster::stack, inr)
  names <- as.character(as.Date("2000-08-01") + (0:364))
  names <- substr(names, 6, 10)
  names <- rep(names, nlayers(s) / 365)
  y <- c(rep(2000, 153), rep(2001, 212))
  y <- lapply(0:((nlayers(s) / 365) - 1), function(x) {
    x + y
  })
  y <- do.call(c, y)
  names <- paste0(y, "-", names)
  names(s) <- names
  rm(inr, y, infiles)
  gc()
  gc()

  s <- unstack(s) # convert to list of single raster layers

  # perform the extraction
  sfInit(parallel = TRUE, cpus = 30)
  sfLibrary(raster)
  sfLibrary(rgdal)
  extracted <- sfSapply(s, extract,  y = aoi)
  sfStop()

  names(extracted) <- names
  rm(s, aoi)
  gc()

  data <- do.call(rbind, extracted)
  data <- t(data)
  write.table(
    data,
    file.path(folder, "extracted.csv"),
    row.names = F,
    col.names = T
  )

  data <- fread(file.path(folder, "extracted.csv"))

  # variazione SCA
  calculateSCA = function(x) {
    x <- as.double(x)
    x[x > 100] <- NA
    x[x < 15] <- 0
    x[x >= 15] <- 1
    snowPixels <- sum(x, na.rm = T)
    npixels <- length(x)
    return((snowPixels / npixels) * 100)
  }

  SCA <- apply(data, 2, calculateSCA)

  write.table(SCA, file.path(folder, "SCA.csv"))

}
