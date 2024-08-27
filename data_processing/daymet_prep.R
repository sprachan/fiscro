library(prism)
env_dir <- '../data/env_vars'
prism_set_dl_dir(tempdir()) # prism files go in a temporary directory 

# get days of the year 01-01 thru 12-31
days <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
        substr(6, 10)
years <- 2010:2022 |> as.character()
rast_years <- list()


# MEAN TEMPERATURE ----------------------------------------
for(d in 6:length(days)){
  # get the dates we want data from: day D from 2010-2022
  dates <- rep(NA, 13)
  for(i in 1:13){
    dates[i] <- paste0(years[i], '-', days[d])
  }
  print(days[d])
  # get the prism data for these 13 days
  get_prism_dailys(type = 'tmean',
                   dates = dates,
                   keepZip = FALSE)
  
  # load these as rasters
  rasts <- terra::rast(pd_to_file(prism_archive_subset(type = 'tmean', 
                                                       temp_period = 'daily',
                                                       dates = dates)))
  # store the mean of these rasters as the appropriate entry in the list of all
  #> rasters
  rast_years[[d]] <- terra::mean(rasts)
  names(rast_years)[d] <- days[d]
}

purrr::map(1:365, \(x) terra::plot(rast_years[[x]], main = names(rast_years)[x]))

# now crop these
state_bounds <- terra::vect(file.path(env_dir, 'tl_2012_us_state.shp'))
# select only the states we have data for
states <- c('CT', 'DE', 'MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VA', 'VT')
state_bounds <- state_bounds[state_bounds$STUSPS %in% states]
state_bounds_proj <- terra::project(state_bounds, terra::crs(rast_years[[1]]))

# rough crop
proc_rasts <- lapply(rast_years, terra::crop, terra::ext(-85, -65, 35, 48))

# now mask
proc_rasts <- lapply(proc_rasts, terra::mask, state_bounds_proj)

# finally, save
purrr::map(1:365, \(x) terra::writeRaster(proc_rasts[[x]], 
                                          filename = file.path(env_dir,
                                                               'prism',
                                                               paste0(names(proc_rasts)[x], 
                                                                      '_tmean.tif')
                                                               )
                                          )
           )

# PRECIPITATION -----
rast_years_p <- list()
for(d in 318:length(days)){
  # get the dates we want data from: day D from 2010-2022
  dates <- rep(NA, 13)
  for(i in 1:13){
    dates[i] <- paste0(years[i], '-', days[d])
  }
  print(days[d])
  # get the prism data for these 13 days
  # get_prism_dailys(type = 'ppt',
  #                  dates = dates,
  #                  keepZip = FALSE)
  
  # load these as rasters
  rasts <- terra::rast(pd_to_file(prism_archive_subset(type = 'ppt', 
                                                       temp_period = 'daily',
                                                       dates = dates)))
  # store the mean of these rasters as the appropriate entry in the list of all
  #> rasters
  rast_years_p[[d]] <- terra::mean(rasts)
  names(rast_years_p)[d] <- days[d]
}

# rough crop
proc_rasts_p <- lapply(rast_years_p, terra::crop, terra::ext(-85, -65, 35, 48))

# now mask
proc_rasts_p <- lapply(proc_rasts_p, terra::mask, state_bounds_proj)

# finally, save
purrr::map(1:365, \(x) terra::writeRaster(proc_rasts_p[[x]], 
                                          filename = file.path(env_dir,
                                                               'prism',
                                                               paste0(names(proc_rasts_p)[x], 
                                                                      '_precip.tif')
                                          )
)
)

