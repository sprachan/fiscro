# DESCRIPTION ------------------------------------------------------------------
#>
#> This script fits a very simple GLM to the data: model species occurrence
#> using one climate variable (mean diurnal temperature range). Species 
#> occurrence is
#> modelled as a binomial, with N from the data and p given by a logistic reg
#> on temperature seasonality.
#> 
#> (1) load occurrence and environmental data
#> (2) make sure all data aligns spatially
#> (3) fit the STAN MODEL, saving the outputs to a .csv
#>
# ------------------------------------------------------------------------------
# Dependencies =================================================================
## packages ----
library(optparse)
## functions ----
# source('01functions.R')
## get options ----
option_list <- list(make_option(c('-s', '--speciesCode'),
                                type = 'character',
                                action = 'store',
                                help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);
species <- opt$s

## file paths ------
env_dir <- file.path('.', 'processed_data', 'env_vars')
output_dir <- '.'
# Load data ====================================================================
## occurrence data ------
load('./processed_data/subsample.RData')

# filter to species of interest
occ_spatial <- subsample |> 
  dplyr::select(species_code,
                observation_date,
                observation_count,
                species_observed,
                latitude,
                longitude) |>
  dplyr::filter(species_code == opt$s)# get this as small as possible to speed modeling

# get rid of extremely high values and limit to just years we have full months of data
q <- quantile(occ_spatial$observation_count, c(0, 0.999), na.rm = TRUE, names = FALSE)
q
occ_spatial <- dplyr::filter(occ_spatial,
                             lubridate::year(observation_date) %in% seq(2010, 2022),
                             observation_count %in% seq(q[1], q[2]))

rm(subsample) # free up some space

# make data into a SpatVector
sp::coordinates(occ_spatial) <- ~longitude+latitude # make into a spatial object
occ_spatial <- terra::vect(occ_spatial) # specifically SpatVec


# environmental data ------
bioclim <- terra::rast(file.path(env_dir, 'bioclim_final.tif'))

# Data Checks ==================================================================
# check extents, CRS, resolution
bioclim 

# make sure ALL CRS's match!
terra::crs(occ_spatial) <- terra::crs(bioclim) # both should be WGS84
occ_spatial

# bioclimatic variables --------------------------------------------------------
# only use mean diurnal range (mean of monthly max temp - min temp) (BIO2), 
#> temperature seasonality (std dev x100; BIO4), annual precip (BIO12), and 
#> precipitation seasonality (BIO15)

bioclim_simple <- bioclim$wc2.1_30s_bio_2
names(bioclim_simple) <- 'diurnal_temp_range'
rm(bioclim)

# Rasterize occurrence data for modeling =======================================
occ_rast <- terra::rasterize(occ_spatial, 
                             field = 'species_observed',
                             fun = sum,
                             terra::rast(nlyrs = 1, 
                                         nrows = 1560, 
                                         ncol = 2400, 
                                         crs = terra::crs(bioclim_simple), 
                                         extent = terra::ext(bioclim_simple)))

list_rast <- terra::rasterize(occ_spatial,
                              field = 'species_observed',
                              fun = length,
                              terra::rast(nlyrs = 1, 
                                          nrows = 1560, 
                                          ncol = 2400, 
                                          crs = terra::crs(bioclim_simple), 
                                          extent = terra::ext(bioclim_simple)))

# Get data into STAN ===========================================================
# make all raster data into vectors with names that correspond to STAN file

## environmental vars
tdr <- terra::as.matrix(bioclim_simple$diurnal_temp_range) |> as.vector()

## occurrence data
occ <- terra::as.matrix(occ_rast) |> as.vector()
checklists <- terra::as.matrix(list_rast) |> as.vector()

## make into a dataframe and get rid of rows where any var is NA
vars <- data.frame(tdr = tdr,
                   occ = occ,
                   checklists = checklists) |>
  na.omit()

# make model object. 
model_obj <- list(tdr = vars$tdr,
                  occ = vars$occ+1, # stan doesn't like 0's in binomial
                  checklists = vars$checklists+1,
                  N = length(vars$tdr))

# free as much RAM as possible
rm(bioclim_simple, list_rast, occ_rast, occ_spatial, opt, vars)

# Run Model ====================================================================
m0_tdr <- rstan::stan_model('M0_tdr.stan')
m0_tdr
occ_fit = rstan::sampling(m0_tdr, 
                          model_obj, 
                          iter = 5000, 
                          chains = 4,
                          #sample_file = file.path(output_dir, 'sample.csv'),
                          #diagnostic_file = file.path(output_dir, 'diagnostics.csv'),
                          verbose = TRUE)
str(occ_fit)
#occ_fit$save_object(file = file.path('~', 'eBird_project', 'model_outputs', paste0(species, 'M0_tdr_fit.RDS')))
saveRDS(occ_fit, file = file.path('~', 'eBird_project', 'model_outputs', paste0(species, '_M0_tdr_fit.RDS')))

