# DESCRIPTION ------------------------------------------------------------------
#>
#> This script is meant to be run once. It crops environmental raster data
#> to the study region, reducing file size and improving run times later on.
#>
# ------------------------------------------------------------------------------
env_dir <- file.path('..', 'data', 'env_vars')

# state boundaries =============================================================
state_bounds <- terra::vect(file.path(env_dir, 'tl_2012_us_state.shp'))
# select only the states we have data for
states <- c('CT', 'DE', 'MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VA', 'VT')
state_bounds <- state_bounds[state_bounds$STUSPS %in% states]

# environmental data ===========================================================
# bioclimatic data
bioclim_files <- list.files(path = env_dir, pattern = 'wc2.1', full.names = TRUE)
bioclim <- terra::rast(bioclim_files)

# land cover data
land_cover <- terra::rast(file.path(env_dir, 'nlcd_2021_lc.img'))

# match land cover CRS by projecting -- faster to project the smaller data set 
#> (land cover) than the larger one (bioclimatic data)

land_cover_proj <- terra::project(x = land_cover,
                                  y = bioclim,
                                  method = 'near')

# make sure ALL CRS's match!
state_bounds_proj <- terra::project(state_bounds, terra::crs(land_cover_proj))

# crop data roughly first
land_cover_crop <- terra::crop(land_cover_proj, terra::ext(-85, -65, 35, 48))
bioclim_crop <- terra::crop(bioclim, terra::ext(-85, -65, 35, 48))

# mask to the study region (faster than masking before cropping!)
land_cover_final <- terra::mask(land_cover_crop, state_bounds_proj)
bioclim_final <- terra::mask(bioclim_crop, state_bounds_proj)

# save the masked raster data
terra::writeRaster(land_cover_final, 
                   filename = file.path(env_dir, 'land_cover_final.tif'))
terra::writeRaster(bioclim_final,
                   filename = file.path(env_dir, 'bioclim_final.tif'))
