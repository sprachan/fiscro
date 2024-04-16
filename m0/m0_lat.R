library(terra)
library(optparse)
# LOAD DATA --------------------------------------------------------------------
env_dir <- './processed_data/env_vars/prism'
# load this as a raster template
tmean <- terra::rast(list.files(env_dir, pattern = 'tmean', full.names = TRUE)[1])

load('./processed_data/subsample.RData')



subsample <- dplyr::select(subsample, 
                           checklist_id,
                           species_code, 
                           species_observed, 
                           latitude, 
                           longitude)

option_list <- list(make_option(c('-s', '--species', 
                                  action = 'store',
                                  type = 'character',
                                  help = '6 letter species code')))
opt <- parse_args(OptionParser(option_list = option_list))

df <- dplyr::filter(subsample, species_code == opt$s) |> dplyr::distinct()


# Spatial occurrence data ----------------------------
sp::coordinates(df) <- ~longitude+latitude

df <- terra::vect(df)
terra::crs(df) <- terra::crs(tmean)

list_layer <- terra::rasterize(df, tmean,
                               field = 'species_observed',
                               fun = length)
names(list_layer) <- 'num_lists'

obs_layer <- terra::rasterize(df, tmean,
                              field = 'species_observed',
                              fun = sum,
                              na.rm = TRUE)
names(obs_layer) <- 'occ'

mean_layer <- obs_layer/list_layer
names(mean_layer) <- 'obs_freq'

rast_data <- c(list_layer, obs_layer) |> c(mean_layer) 

# make STAN compatible ---------------------------------------------------------
stan_df <- terra::as.data.frame(rast_data, xy = TRUE) |> na.omit()

model_obj <- list(long = stan_df$x,
                  lat = stan_df$y,
                  occ = stan_df$occ,
                  checklists = stan_df$num_lists,
                  N = length(stan_df$x))
model <- rstan::stan_model('m0/m0_lat.stan')
fit = rstan::sampling(model, 
                      model_obj, 
                      iter = 5000, 
                      chains = 3,
                      cores = 3,
                      verbose = TRUE)
str(fit)
saveRDS(fit, file = file.path('~', 'eBird_project', 'model_outputs', paste0(opt$s, '_m0_lat_fit.RDS')))