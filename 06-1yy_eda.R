# This is some more advanced EDA on spatially unbiased data to look at
# Species counts over space and time.

# Load dependencies and parse options ==========================================
library(tidyverse) # data manipulation tools
library(viridis) # for color-blind friendly visuals
library(zoo) # helpful for year_mon object and related tools
library(optparse) # for flexibility in options when called through command line
# get options
option_list <- list(
  make_option(c('-s', '--speciesCode'), type = 'character',
              action = 'store', help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

source('01functions.R')

# Load Data ====================================================================
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
                    year(observation_date) %in% years,
                    species_code == opt$s)
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
                      year_mon = as.yearmon(observation_date)) |>
  group_by(year_mon, long_bin, lat_bin) |>
  summarize(obs_freq = sum(species_observed)/n())
print('summarized')

# free up some RAM
remove(subsample)

# Flat Smooth ==================================================================
yy_compare_flat <- smooth_compared_df(ym_obs_freq,
                                      comparison = 'y',
                                      years = years,
                                      smooth_type = 'flat',
                                      scope = 1)

yy_compare_geom <- smooth_compared_df(ym_obs_freq,
                                      comparison = 'y',
                                      years = years,
                                      smooth_type = 'geom',
                                      scope = 1)

