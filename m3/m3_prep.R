# setup ========================================================================
env_dir <- '../processed_data'
source('../01functions.R')
library(optparse)
load('../processed_data/subsample.RData')
subsample <- dplyr::select(subsample, 
                           checklist_id,
                           species_code, 
                           species_observed,
                           observation_date,
                           latitude, 
                           longitude)
print('subsample loaded')
tmean_path <- file.path(env_dir, 'tmean_all.tif')
precip_path <- file.path(env_dir, 'precip_all.tif')
print('file paths set')

# simplify modal NLCD map to 8 categories from 13 ==============================
lc <- terra::rast(file.path(env_dir, 'land_cover_mode.tif'))
classify_mat <- matrix(c(10, 19, 10,
                         20, 29, 20,
                         30, 39, 30,
                         40, 49, 40,
                         50, 59, 50,
                         70, 79, 70,
                         80, 89, 80,
                         90, 99, 90),
                       ncol = 3, byrow = TRUE)

simple <- terra::classify(lc, rcl = classify_mat)
names(simple) <- 'modal_lc'
print('Land cover is loaded')
rm(lc)

# ITERATE OVER DAYS ----
days <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
        substr(6, 10)

temp <- NA
temp_df <- NA

spat_df <- data.frame(day = NA,
                      x = NA, 
                      y = NA, 
                      num_lists = NA,
                      num_obs = NA,
                      modal_lc = NA,
                      precip = NA,
                      tmean = NA
                      )

for(s in c('fiscro', 'ribgul', 'amerob')){
  temp <- dplyr::filter(subsample, species_code == s) |>
          dplyr::distinct()
  for(i in 1:365){
    # get climate data for day i
    clim <- c(terra::rast(precip_path)[[i]], terra::rast(tmean_path)[[i]])
    names(clim) <- c('precip', 'tmean')
    window <- get_window(i)
    daily <- dplyr::filter(temp,
                           lubridate::yday(observation_date) %in% window)
    
    # make this spatial
    sp::coordinates(daily) <- ~longitude+latitude
    daily <- terra::vect(daily)
    
    # make sure CRS and extent matches
    terra::crs(daily) <- terra::crs(clim)
  
     # now get raster layers
    list_layer <- terra::rasterize(daily, clim, 
                                   field = 'species_observed', 
                                   fun = length)
    names(list_layer) <- 'num_lists'
    obs_layer <- terra::rasterize(daily, clim,
                                  field = 'species_observed',
                                  fun = sum, na.rm = TRUE)
    names(obs_layer) <- 'num_obs'
    daily <- c(list_layer, obs_layer) |>
             c(simple) |>
             c(clim)
    
    # add this to the data frame
    temp_df <- na.omit(terra::as.data.frame(daily, xy = TRUE))
    temp_df$day <- rep(days[i], dim(temp_df[1]))
    
    spat_df <- rbind(spat_df, temp_df)
    rm(daily, temp_df, list_layer, obs_layer, mean_layer, clim) # save some RAM
  }
  saveRDS(spat_df, file = file.path(env_dir, paste0('m3_prep_', s, '.RDS')))
  cat('Finished with species ', s)
}


# SCRATCH FROM DOING ALL SPECIES IN A LIST ----
#purrr::map(spec_list, \(x) dplyr::filter(x, lubridate::yday(observation_date) %in% window))
# sp::coordinates(temp$amerob) <- ~longitude+latitude
# sp::coordinates(temp$ribgul) <- ~longitude+latitude
# terra::crs(temp$fiscro) <- terra::crs(clim)
# terra::crs(temp$amerob) <- terra::crs(clim)
# terra::crs(temp$ribgul) <- terra::crs(clim)
# list_layer <- purrr::map(temp, \(x) terra::rasterize(x, clim,
#                                                      field = 'species_observed',
#                                                      fun = length))
# obs_layer <- purrr::map(temp, \(x) terra::rasterize(x, clim,
#                                                     field = 'species_observed',
#                                                     fun = sum, na.rm = TRUE))
# mean_layer <- purrr::map2(obs_layer, list_layer, \(x, y) x/y)
# names(mean_layer$fiscro) <- 'obs_freq'
# names(mean_layer$ribgul) <- 'obs_freq'
# names(mean_layer$amerob) <- 'obs_freq'


