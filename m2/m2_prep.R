library(prism)
library(terra)
env_dir <- '../data/env_vars/prism'

# LOAD DATA --------------------------------------------------------------------
# gets all of the mean temperature rasters, one for each day of the year
tmean <- terra::rast(list.files(env_dir, pattern = 'tmean', full.names = TRUE))

# same for precipitation
precip <- terra::rast(list.files(env_dir, pattern = 'precip', full.names = TRUE))

# for this preliminary model, we will average these all together
tmean <- terra::mean(tmean)
precip <- terra::mean(precip)

names(tmean) <- 'mean_temp'
names(precip) <- 'mean_precip'

load('../data/subsample.RData')

subsample <- dplyr::select(subsample, 
                           checklist_id,
                           species_code, 
                           species_observed, 
                           latitude, 
                           longitude)

fiscro <- dplyr::filter(subsample, species_code == 'fiscro') |> dplyr::distinct()
ribgul <- dplyr::filter(subsample, species_code == 'ribgul') |> dplyr::distinct()
amerob <- dplyr::filter(subsample, species_code == 'amerob') |> dplyr::distinct()

# put these in a list to make spatial stuff easier
spec_list <- list(fiscro = fiscro, ribgul = ribgul, amerob = amerob)

# Spatial occurrence data ----------------------------
sp::coordinates(spec_list$fiscro) <- ~longitude+latitude
sp::coordinates(spec_list$ribgul) <- ~longitude+latitude
sp::coordinates(spec_list$amerob) <- ~longitude+latitude

spec_list <- lapply(spec_list, terra::vect)

terra::crs(spec_list$fiscro) <- terra::crs(tmean)
terra::crs(spec_list$amerob) <- terra::crs(tmean)
terra::crs(spec_list$ribgul) <- terra::crs(tmean)

precip <- terra::crop(precip, spec_list$fiscro)
tmean <- terra::crop(tmean, spec_list$fiscro)

list_layer <- purrr::map(spec_list, \(x) terra::rasterize(x, tmean,
                                                           field = 'species_observed',
                                                           fun = length
                                                           )
                         )
           
obs_layer <- purrr::map(spec_list, \(x) terra::rasterize(x, tmean,
                                                           field = 'species_observed',
                                                           fun = sum, na.rm = TRUE
                                                          )
                        )

mean_layer <- purrr::map2(list_layer, obs_layer, \(x, y) y/x)
names(mean_layer$fiscro) <- 'obs_freq'
names(mean_layer$ribgul) <- 'obs_freq'
names(mean_layer$amerob) <- 'obs_freq'

spec_list <- purrr::map2(list_layer, obs_layer, c) |>
             purrr::map2(mean_layer, c) |>
             purrr::map2(tmean, c) |>
             purrr::map2(precip, c)

# make STAN compatible ---------------------------------------------------------
spec_stan <- lapply(spec_list, terra::as.data.frame, xy = TRUE) # mean = obs freq, species_observed = nlists
spec_stan <- lapply(spec_stan, na.omit)

# save this for easy loading ---------------------------------------------------
saveRDS(spec_stan, file = './m2/m2_prepped_data.RDS')
