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

# Smooth =======================================================================
yy_compare_flat <- compare_years(ym_obs_freq, smooth_type = 'flat')
print('flat smooth done')
yy_compare_geom <- compare_years (ym_obs_freq, 'geom')
print('geom smooth done')

# Plot as maps =================================================================
# yy_plot <- ggplot(yy_compare_flat,
#                   aes(x = long_bin,
#                       y = lat_bin,
#                       fill = transform_diff))+
#            geom_raster()+
#            ggforce::facet_wrap_paginate(facets = vars(comparison),
#                                         nrow = 3,
#                                         ncol = 4)+
#            scale_fill_distiller(palette = 'RdBu',
#                                 direction = -1,
#                                 na.value = '#cccccc')+
#            theme_bw()+
#            theme(legend.direction = 'horizontal',
#                  legend.position = 'bottom')
# print('created gg object for flat year-on-year comparisons')
# 
# save_pages(yy_plot, type = 'flat_smoothed_yy',
#            facets = vars(comparison),
#            nrow = 3,
#            ncol = 4,
#            species = opt$s,
#            directory = 'comparisons')
# print('saved pages for flat smoothed year-on-year comparisons')
# 
# remove(yy_plot)
# yy_plot <- ggplot(yy_compare_geom,
#                   aes(x = long_bin,
#                       y = lat_bin,
#                       fill = transform_diff))+
#   geom_raster()+
#   ggforce::facet_wrap_paginate(facets = vars(comparison),
#                                nrow = 3,
#                                ncol = 4)+
#   scale_fill_distiller(palette = 'RdBu',
#                        direction = -1,
#                        na.value = '#cccccc')+
#   theme_bw()+
#   theme(legend.direction = 'horizontal',
#         legend.position = 'bottom')
# print('created gg object for geom year-on-year comparisons')
# 
# save_pages(yy_plot, type = 'geom_smoothed_yy',
#            facets = vars(comparison),
#            nrow = 3,
#            ncol = 4,
#            species = opt$s,
#            directory = 'comparisons')
# print('saved pages for geom smoothed year-on-year comparisons')
# remove(yy_plot)

# Plot histograms of differences ===============================================
yy_hist <- yy_compare_flat |>
           mutate(transform_diff = case_when(transform_diff == 0 ~ NA,
                                             .default = transform_diff)) |>
           ggplot(aes(x = transform_diff))+
           geom_histogram(bins = 200)+
           ggforce::facet_wrap_paginate(facets = vars(comparison),
                                        nrow = 3,
                                        ncol = 4)+
           theme_bw()

save_pages(yy_hist, type = 'flat_yy_hist',
           facets = vars(comparison),
           nrow = 3,
           ncol = 4,
           species = opt$s,
           directory = 'comparisons')
remove(yy_hist)

yy_hist <- yy_compare_geom |>
           mutate(transform_diff = case_when(transform_diff == 0 ~ NA,
                                             .default = transform_diff)) |>
           ggplot(aes(x = transform_diff))+
           geom_histogram(bins = 200)+
           ggforce::facet_wrap_paginate(facets = vars(comparison),
                                        nrow = 3,
                                        ncol = 4)+
           theme_bw()

save_pages(yy_hist, type = 'geom_yy_hist',
           facets = vars(comparison),
           nrow = 3,
           ncol = 4,
           species = opt$s,
           directory = 'comparisons')