# setup ========================================================================
env_dir <- '../data/env_vars'
source('01functions.R')
library(optparse)

# options ======================================================================
option_list <- list(make_option(c('-s', '--speciesCode'),
                                type = 'character',
                                action = 'store',
                                help = 'species for analysis'),
                    make_option(c('-d', '--dayOfYear'),
                                type = 'integer',
                                action = 'store',
                                help = 'integer day of year, 1-365'
                                )
                   )

opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

opt <- list(s = 'fiscro', d = 1) # for testing purposes
# simplify modal NLCD map to 8 categories from 13 ==============================
lc <- terra::rast('../data/env_vars/land_cover_mode.tif')
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

# get temperature and precipitation data =======================================
days <- seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by = 'day') |>
        substr(6, 10)


# one layer will be temp, the other precip
clim <- terra::rast(list.files(file.path(env_dir, 'prism'), 
                              pattern = days[opt$d], 
                              full.names = TRUE))
names(clim) <- c('precip', 'tmean')

# get sliding window average data ==============================================  
load('../data/subsample.RData')
window <- get_window(opt$d)
daily <- subsample |> dplyr::filter(species_code == opt$s,
                                    lubridate::yday(observation_date) %in% window) |>
                      dplyr::select(longitude,
                                    latitude,
                                    species_code,
                                    observation_count,
                                    species_observed,
                                    observation_date)
rm(subsample)
sp::coordinates(daily) <- ~longitude+latitude
spat <- terra::vect(daily)
terra::crs(spat) <- terra::crs(clim)

list_layer <- terra::rasterize(spat, clim,
                               field = 'species_observed',
                               fun = length)
obs_layer <- terra::rasterize(spat, clim,
                              field = 'species_observed',
                              fun = sum, na.rm = TRUE)
names(list_layer) <- 'nlists'
names(obs_layer) <- 'obs'

mean_layer <- obs_layer/list_layer
names(mean_layer) <- 'obs_freq'

# combine all layers together ==================================================
all_spat <- c(list_layer, obs_layer) |> c(mean_layer) |> c(clim) |>
            terra::aggregate(fact = 2, fun = 'mean', na.rm = TRUE) |> # improve patchiness
            terra::as.data.frame(xy = TRUE) |>
            na.omit()


