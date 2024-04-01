# DESCRIPTION ------------------------------------------------------------------
#>
#> This script fits a simple model to the data: model species occurrence
#> on land cover categories. Species occurrence is
#> modeled as a binomial, with N from the data and p given by a logistic reg
#> on land cover data.
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
source('01functions.R')
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
output_dir <- file.path('~', 'eBird_project', 'model_outputs', 'm1b')
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
                dplyr::filter(species_code == opt$s)

rm(subsample) # free up some space

# make data into a SpatVector
sp::coordinates(occ_spatial) <- ~longitude+latitude # make into a spatial object
occ_spatial <- terra::vect(occ_spatial) # specifically SpatVec


# environmental data ------
landcover <- terra::rast(file.path(env_dir, 'land_cover_final.tif'))

# reclassify to just 8 classes
classify_mat <- matrix(c(11, 10,
                         21, 20,
                         22, 20,
                         23, 20,
                         24, 20,
                         31, 30,
                         41, 40,
                         42, 40,
                         43, 40,
                         52, 50,
                         71, 70,
                         81, 80,
                         82, 80,
                         90, 90,
                         95, 90), 
                       ncol = 2,
                       byrow = TRUE) 
landcover_simple <- terra::classify(landcover, rcl = classify_mat, others = NA)
levels(landcover_simple) <- data.frame(index = c(10, 20, 30, 40, 50, 70, 80, 90), 
                                       value = c('water', 
                                                   'developed', 
                                                   'barren', 
                                                   'forest',
                                                   'shrub', 
                                                   'grassland', 
                                                   'planted_cultivated', 
                                                   'wetland'))                    
rm(landcover, classify_mat) # free RAM

# Data Checks ==================================================================
# check extents, CRS, resolution
landcover_simple

# make sure ALL CRS's match!
terra::crs(occ_spatial) <- terra::crs(landcover_simple) # both should be WGS84
occ_spatial

# Rasterize occurrence data for modeling =======================================
occ_rast <- terra::rasterize(occ_spatial, 
                             field = 'species_observed',
                             fun = sum,
                             terra::rast(nlyrs = 1, 
                                         nrows = 1560, 
                                         ncol = 2400, 
                                         crs = terra::crs(landcover_simple), 
                                         extent = terra::ext(landcover_simple)))

list_rast <- terra::rasterize(occ_spatial,
                              field = 'species_observed',
                              fun = length,
                              terra::rast(nlyrs = 1, 
                                          nrows = 1560, 
                                          ncol = 2400, 
                                          crs = terra::crs(landcover_simple), 
                                          extent = terra::ext(landcover_simple)))

# Prep data for STAN ===========================================================
# make all raster data into vectors with names that correspond to STAN file

## environmental vars
lc <- terra::as.matrix(landcover_simple$value) |> as.vector()

## occurrence data
occ <- terra::as.matrix(occ_rast) |> as.vector()
checklists <- terra::as.matrix(list_rast) |> as.vector()

## make into a dataframe to make it easy to get rid of NA rows
vars <- data.frame(lc = lc,
                   occ = occ,
                   checklists = checklists) |>
  na.omit()

# now code land cover as 8 categorical yes/no dummy variables.
# following Gelman et al (2008) about weakly informative priors for logistic,
#> standardize these inputs so that they have mean 0 and range 1

vars <- dplyr::mutate(vars,
                      water = dplyr::case_when(lc == 10 ~ 1,
                                               .default = 0),
                      water = water - mean(water),
                      developed = dplyr::case_when(lc == 20 ~ 1,
                                                   .default = 0),
                      developed = developed - mean(developed),
                      barren = dplyr::case_when(lc == 30 ~ 1,
                                                .default = 0),
                      barren = barren - mean(barren),
                      forest = dplyr::case_when(lc == 40 ~ 1,
                                                .default = 0),
                      forest = forest - mean(forest),
                      shrub = dplyr::case_when(lc == 50 ~ 1,
                                               .default = 0),
                      shrub = shrub - mean(shrub),
                      grassland = dplyr::case_when(lc == 70 ~ 1,
                                                   .default = 0),
                      grassland = grassland - mean(grassland),
                      planted_cultivated = dplyr::case_when(lc == 80 ~ 1,
                                                            .default = 0),
                      planted_cultivated = planted_cultivated - mean(planted_cultivated),
                      wetlands = dplyr::case_when(lc == 90 ~ 1,
                                                  .default = 0),
                      wetlands = wetlands - mean(wetlands)
                      )



# wrote STAN script to take land cover as a matrix
lc <- matrix(c(vars$water, 
               vars$developed, 
               vars$barren,
               vars$forest,
               vars$shrub,
               vars$grassland,
               vars$planted_cultivated,
               vars$developed), ncol = 8)


# make model object. 
model_obj <- list(lc = lc,
                  occ = vars$occ+1, # stan doesn't like 0's in binomial
                  checklists = vars$checklists+1,
                  N = length(vars$occ),
                  K = length(unique(vars$lc)))
str(model_obj)

# free as much RAM as possible
rm(landcover_simple, list_rast, occ_rast, occ_spatial, opt, vars)

# Run Model ====================================================================
model <- rstan::stan_model('M1B.stan')
occ_fit = rstan::sampling(model, 
                          model_obj, 
                          iter = 5000, 
                          chains = 4,
                          #sample_file = file.path(output_dir, 'sample.csv'),
                          #diagnostic_file = file.path(output_dir, 'diagnostics.csv'),
                          verbose = TRUE)
str(occ_fit)
occ_fit$save_object(file = file.path('~', 'eBird_project', 'model_outputs', paste0(species, 'M1B_fit.RDS')))



