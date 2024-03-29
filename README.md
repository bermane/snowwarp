## The SnowWarp tool

Welcome to the snowwarp package! This package was created to provide open-source access to SnowWarp fractional snow covered area (fSCA) data and accompanying annual statistics. SnowWarp is a new data fusion approach to create a daily time-series of 30 m snow cover observations. It was created to meet the data needs of applied researchers, who desire a snow cover dataset with a high enough spatial and temporal resolution to match the scale of their work. The product is derived by leveaging daily MODIS snow cover data, which captures the temporal dynamics of snow cover, and Dynamic Time Warping (DTW), which is used to re-order historical Landsat observations to account for inter-annual variability. For more information on the SnowWarp algorithm and it's methods see [Berman et al. (2018)](https://www.sciencedirect.com/science/article/abs/pii/S0034425718303626) and [Vaglio Laurin et al. (2022)](https://www.sciencedirect.com/science/article/abs/pii/S1364815222001815). For methods of key annual snow statistics and a novel application of SnowWarp please see [Berman et al. (2019)](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0215243). 

Please note the following when using SnowWarp:

1. fSCA data from MODIS and Landsat is now used as the input into the SnowWarp algorithm. fSCA is calculated using a linear regression on NDSI following the methods of [Aalstad et al. (2020)](https://www.sciencedirect.com/science/article/pii/S0034425719306388). The regression formula is fSCA = (1.45) * NDSI -0.01. This was not the orginal input data used in Berman et al. (2018) and this data has not been validated! Please use at your own discretion. That being said, the new fSCA approach should help to deal with the systematic underestimation of fSCA that was found in Landsat TMSCAG data.
2. Data is pre-processed in Google Earth Engine (GEE) in WGS84 projection. Output files are GeoTiff (.tif) and ~30 meters resolution.
3. Data is currently available from August 1, 2000 until July 31, 2021. New data will be added at the end of each annual 'winter' year (i.e. late summer).
3. Use of SnowWarp data is free but must be cited. Please cite:

  i) [Vaglio Laurin et al. (2022)](https://www.sciencedirect.com/science/article/abs/pii/S1364815222001815)

  ii) [Berman et al. (2018)](https://www.sciencedirect.com/science/article/abs/pii/S0034425718303626)

The following instructions should be followed to ensure proper use of the snowwarp package. The general workflow is:

0. Download the snowwarp R package and Orfeo Toolbox.
1. Organize and pre-process data on Google Earth Engine.
2. Download pre-processed data.
3. Process SnowWarp daily fSCA.
4. Extract key SnowWarp annual statistics.

## 0. Donwload the snowwarp R package and Orfeo Toolbox.

The snowwarp R package is available on Github in [this repository](https://github.com/bermane/snowwarp). To download and install in R, use the following code: devtools::install_github("bermane/snowwarp"). You need to first install the devtools package if you don't have it already.

The code also requires the Orfeo Toolbox, which can be [downloaded here](https://www.orfeo-toolbox.org/). Note the installation directory as it is needed later on to run the package in R.

## 1. Organize and pre-process data on Google Earth Engine

1. Create and/or sign-in to your Google Earth Engine Account
2. Open the SnowWarpPackage API by using [this link](https://code.earthengine.google.com/?accept_repo=users/sfrancini/SnowWarpPackage). Opening the link will create a script on your GEE account that can be used to pre-process data. The script will appear in a "Reader" repository in your "Scripts" panel.
3.  Click on the "SnowWarp-API" script and click "Run". This should open the SnowWarp API and allow you to specify a shapefile (study area) and Google Drive output folder.
4. Follow the instructions to upload a shapefile of your study area and download the data to Google Drive. Please note that a free account only has 15 GB of space on Google Drive. If your study area is too large (i.e. when you try to pre-process the data on GEE you get an error), try cutting the area into smaller parts and running separately. Once you download the imagery to your hard drive, you can delete the data from Google Drive and run the next study area chunk.

## 2. Download pre-processed data

Once the data is pre-processed on GEE and located on your Google Drive account, the rest of the process takes place in R. Use the download_snowwarp_data function (run "?download_snowwarp_data" for details) to download the pre-processed imagery to a local hard drive. The folder where you save the imagery will be the "folder" variable for the rest of the snowwarp functions.

## 3. Process SnowWarp daily fSCA

This is main part of the SnowWarp algorithm and also the most time consuming. Depending on your processing power, read/write speed, and allocation of cpus for parallel processing, this function can run from anywhere from a few days to a couple weeks. Before getting started, run the get_snowwarp_tiles function (run "?get_snowwarp_tiles" for details) function, which will return the number of Landsat tiles you need to process. When you run the process_snowwarp function (run "process_snowwarp" for details), you can specify the tile numbers you want to process. The code processes data in parallel on a single tile at a time, so the time it takes to run the first tile multiplied by the number of tiles will give a good estimate of total processing time. It is always better to process all the years of data you want at the same time, so if you have space constraints, start by restricting the number of tiles processed in each iteration. 

Snowwarp processes a lot of data and it is important to optimize the resources available to the function. When setting the number of cpus, we recommend using at least half of the cpus on your machine. More if you are able! For example, note that often an "8" core machine means "16" cpus. Therefore this value should be >= 8. This value will have a huge impact on the time it takes the function to run. Consider trying higher cpu values and testing how well your computer handles it when trying the first tile. The same goes for the maximum ram (in MB) available to the smoothing function from Orfeo Toolbox (variable "max_ram"). It defaults to 256 but it is strongly advised to increase this number to at least half your computer's RAM. Consider using the same proportion of RAM as porportion of CPUS/threads. This value is less important than number of cpus.

The other thing you can do to speed up processing is increase the ram available to the "raster" package in R using the "maxmemory" option. See [this blog](https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-13/) for an example. You need to change this option in R before running the snowwarp function, and each time you open R.

SnowWarp outputs daily fSCA and each raster contains 365 bands corresponding to August 1 - July 31. For example, if you run years = c(2000, 2004), you will get two rasters with data from August 1, 2000 - July 31, 2001 and August 1, 2004 - July 31, 2005. Leap years are not accounted for (365 days every year). The output will be located inside "folder/output", where "folder" is the directory with the pre-processed imagery from GEE. Temporary files generated will be located in "folder/temp" and organized by tile. These need to be deleted manually! Once the first tile is finished, check the output folder to make sure everything is working correctly before deleting the temporary files. Note that the function does have some built it checks for what has already been processed, so if your computer shuts down while in the middle of processing a tile, the code will resume that tile when you start it again. Note if a tile is totally finished processing (i.e. you have the outputs in the "folder/output" location), the code will start processing the tile from the beginning if you indicate to process that tile number. The function will print a message when it starts and completes each tile, along with the time of message. 

**Please note that there is a small bug that sometimes returns values greater than 100. After processing snowwarp you should set all values greater than 100 to NA.

## 4. Extract key annual statistics

The SnowWarp dataset contains daily values at ~30 m spatial resolution -- This is a LOT of data!!! For many applications, users may only be interested in annual statistics that characterize snow dynamics. The extract_snowwarp_stats function (run "?extract_snowwarp_stats" for details) processes three key statistics and outputs them into an additional raster file (one per winter year) in the "output" folder with 3 bands. The 3 bands are:

1. Date of snow accumulation
2. Date of snow melt
3. Number of days with snow cover in year

Many other statistics can be calculated from SnowWarp, but these should get you started!

## 5. SnowWarp workflow example 

For the sake of the user, we have prepared a "run.R" [code](https://github.com/bermane/snowwarp/inst/test/run.R)
Once installed the SnowWarp package, you'll find the run.R code in a "test" folder.
run.R can be executed for testing the package with example data we provide (the data that is downloaded by default using our GEE user interface) or you can replace parameters for specifying you own input data and obtaining outputs over your specific area of interest.

## That's it!

Thanks for your interest in these data! We are very excited to share SnowWarp with the research community and make it available to data users for many applications. This is a new package and deals with a large amount of data and processing, and we would definitely appreciate your feedback about it's usability and functionality. Any effort to help improve functionality is welcome!

Best of luck,

Ethan Berman, Saverio Francini, and Nicholas Coops. 2022.
