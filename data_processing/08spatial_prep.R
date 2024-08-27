# DESCRIPTION ------------------------------------------------------------------
#>
#> This script is meant to be run once. It crops environmental raster data
#> to the study region, reducing file size and improving run times later on.
#>
# ------------------------------------------------------------------------------
env_dir <- file.path('.', 'processed_data', 'env_vars')

# state boundaries =============================================================
state_bounds <- terra::vect(file.path(env_dir, 'tl_2012_us_state.shp'))
# select only the states we have data for
states <- c('CT', 'DE', 'MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VA', 'VT')
state_bounds <- state_bounds[state_bounds$STUSPS %in% states]

# environmental data ===========================================================
# bioclimatic data
# bioclim_files <- list.files(path = env_dir, pattern = 'wc2.1', full.names = TRUE)
bioclim <- terra::rast(file.path(env_dir, 'prism', '01-01_precip.tif'))

# land cover data
land_cover_files <- list.files(path = file.path('..', 'data', 'nlcd'), 
                               pattern = '.img', 
                               full.names = TRUE)
land_cover <- terra::rast(land_cover_files)

# match land cover CRS by projecting
land_cover_proj <- terra::project(x = land_cover,
                                  y = bioclim,
                                  method = 'near')

# make sure ALL CRS's match!
state_bounds_proj <- terra::project(state_bounds, terra::crs(land_cover_proj))

# crop data roughly first
land_cover_crop <- terra::crop(land_cover_proj, terra::ext(bioclim))
#bioclim_crop <- terra::crop(bioclim, terra::ext(-85, -65, 35, 48))

# mask to the study region (faster than masking before cropping!)
land_cover_masked <- terra::mask(land_cover_crop, state_bounds_proj)
#bioclim_final <- terra::mask(bioclim_crop, state_bounds_proj)

# now take the mode of these years
mode_lc <- terra::modal(land_cover_masked)
terra::coltab(mode_lc) <- terra::coltab(land_cover_masked)[[1]]
# save the masked raster data
terra::writeRaster(mode_lc,
                   filename = file.path(env_dir, 'land_cover_mode.tif'),
                   overwrite = TRUE)

# terra::writeRaster(land_cover_final, 
#                    filename = file.path(env_dir, 'land_cover_final.tif'))
# terra::writeRaster(bioclim_final,
#                    filename = file.path(env_dir, 'bioclim_final.tif'))

tmean <- terra::rast(list.files(file.path(env_dir, 'prism'),
                                pattern = 'tmean',
                                full.names = TRUE))
names(tmean) <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
                substr(6, 10)

terra::writeRaster(tmean, './processed_data/tmean_all.tif')

precip <- terra::rast(list.files(file.path(env_dir, 'prism'),
                                pattern = 'precip',
                                full.names = TRUE))
names(precip) <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
                 substr(6, 10)

terra::writeRaster(precip, './processed_data/precip_all.tif')
