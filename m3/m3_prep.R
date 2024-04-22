# setup ========================================================================
env_dir <- './processed_data'
source('01functions.R')
library(optparse)
load('.processed_data/subsample.RData')
subsample <- dplyr::select(subsample, 
                           checklist_id,
                           species_code, 
                           species_observed,
                           observation_date,
                           latitude, 
                           longitude)

tmean_path <- file.path(env_dir, 'tmean_all.tif')
precip_path <- file.path(env_dir, 'precip_all.tif')
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
# colors from https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
terra::coltab(simple) <- data.frame(value = c(10, 20, 30, 40, 50, 70, 80, 90),
                                    col = c('#4f6c9e', 
                                            '#DD3021',
                                            '#B2AEA5',
                                            '#336437',
                                            '#CDBB89',
                                            '#EDECCF',
                                            '#DCD85B',
                                            '#BFD5EB'))
terra::plot(simple)
rm(lc)

# ITERATE OVER DAYS ----
days <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
        substr(6, 10)

temp <- NA
spat_list <- list()
for(s in c('fiscro', 'ribgul', 'amerob')){
  temp <- dplyr::filter(subsample, species_code == s) |>
          dplyr::distinct()
  for(i in 365){
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
    obs_layer <- terra::rasterize(daily, clim,
                                  field = 'species_observed',
                                  fun = sum, na.rm = TRUE)
    
    mean_layer <- obs_layer/list_layer
    names(mean_layer) <- 'obs_freq'
    daily <- c(list_layer, obs_layer) |>
             c(mean_layer) |>
             c(simple) |>
             c(clim)
    
    # make this into a dataframe stored in spat_list
    spat_list[[i]] <- terra::as.data.frame(daily, xy = TRUE) |> na.omit()
    remove(daily, list_layer, obs_layer, mean_layer, clim)
  }
  names(spat_list) <- days
  saveRDS(spat_list, file = file.path(env_dir, paste0('m3_prep_', s, '.RDS')))
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


