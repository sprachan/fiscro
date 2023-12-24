# This is some more advanced EDA on spatially unbiased data to look at
# Species counts over space and time.

# Load dependencies and parse options ==========================================
## libraries -------------------------------------------------------------------
# data manipulation tools
library(tidyverse)

# for color-blind friendly visuals
library(viridis) 

# provides year_mon object type and related tools
library(zoo) 

# flexibility in options when called through command line, 
#> makes script play nicer with HPC
library(optparse) 

## get options -----------------------------------------------------------------
option_list <- list(
  make_option(c('-s', '--speciesCode'), type = 'character',
              action = 'store', help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

## load functions and set base file path ---------------------------------------
source('01functions.R')

fp <- file.path('~', 'eBird_project', 'plots')

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
yy_plot <- ggplot(yy_compare_flat,
                  aes(x = long_bin,
                      y = lat_bin,
                      fill = transform_diff))+
           geom_raster()+
           ggforce::facet_wrap_paginate(facets = vars(comparison),
                                        nrow = 3,
                                        ncol = 4)+
           scale_fill_distiller(palette = 'RdBu',
                                direction = -1,
                                na.value = '#cccccc')+
           theme_bw()+
           theme(legend.direction = 'horizontal',
                 legend.position = 'bottom')
print('created gg object for flat year-on-year comparisons')

save_pages(yy_plot,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_flat_smoothed_yy.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(year_mon))
print('saved pages for flat smoothed year-on-year comparisons')

remove(yy_plot)
yy_plot <- ggplot(yy_compare_geom,
                  aes(x = long_bin,
                      y = lat_bin,
                      fill = transform_diff))+
  geom_raster()+
  ggforce::facet_wrap_paginate(facets = vars(comparison),
                               nrow = 3,
                               ncol = 4)+
  scale_fill_distiller(palette = 'RdBu',
                       direction = -1,
                       na.value = '#cccccc')+
  theme_bw()+
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom')
print('created gg object for geom year-on-year comparisons')

save_pages(yy_plot,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_geom_smoothed_yy.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(year_mon))

print('saved pages for geom smoothed year-on-year comparisons')
remove(yy_plot)

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

save_pages(yy_hist,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_flat_yy_hist.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(year_mon))

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

save_pages(yy_hist,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_geom_yy_hist.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(year_mon))