# DESCRIPTION ------------------------------------------------------------------
#>
#> This is some more advanced EDA on spatially unbiased data to look at
#> species counts over space and time.
#> This script does year-on-year comparisons of observation frequencies,
#> taking the difference (within the same month) between the observation 
#> frequencyin a given cell in consecutive years. Differences are always taken 
#> as the earlier year subtracted from the later year; for example, 
#> January 2022 - January 2021. 
#>
# ------------------------------------------------------------------------------

# Load dependencies and parse options ==========================================
## libraries -------------------------------------------------------------------
# data manipulation tools
library(dplyr)

# plotting
library(ggplot2)

# for color-blind friendly visuals
library(viridis) 

# provides year_mon object type and related tools
library(zoo) 

# flexibility in options when called through command line, 
#> makes script play nicer with HPC
library(optparse) 

## get options -----------------------------------------------------------------
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

## load functions and set base file path ---------------------------------------
source('01functions.R')

fp <- file.path('~', 'eBird_project', 'plots')

# Load Data ====================================================================
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
                    lubridate::year(observation_date) %in% years,
                    species_code == opt$s)
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
                      year_mon = as.yearmon(observation_date)) |>
               group_by(year_mon, long_bin, lat_bin) |>
               summarize(obs_freq = sum(species_observed)/n()) |>
               mutate(obs_freq = as.numeric(obs_freq))
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
                      #fill = transform_diff)
                      fill = diff_log))+
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
           facets = vars(comparison))
print('saved pages for flat smoothed year-on-year comparisons')

remove(yy_plot)
yy_plot <- ggplot(yy_compare_geom,
                  aes(x = long_bin,
                      y = lat_bin,
                      #fill = transform_diff)
                      fill = diff_log))+
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
           facets = vars(comparison))

print('saved pages for geom smoothed year-on-year comparisons')
remove(yy_plot)

# Plot histograms of differences ===============================================
yy_hist <- yy_compare_flat |>
           mutate(diff_log = case_when(diff_log == 0 ~ NA,
                                       .default = diff_log)) |>
           ggplot(aes(#x = transform_diff)
                       x = diff_log))+
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
           facets = vars(comparison))

remove(yy_hist)

yy_hist <- yy_compare_geom |>
           mutate(diff_log = case_when(diff_log == 0 ~ NA,
                                             .default = diff_log)) |>
           ggplot(aes(#x = transform_diff)
                      x = diff_log))+
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
           facets = vars(comparison))

# Summarize difference distribution as zero/non-zero ===========================
out_flat <- summarize(yy_compare_flat,
                      decrease = sum(diff_log < 0, na.rm = TRUE)/n(),
                      no_change = sum(diff_log == 0, na.rm = TRUE)/n(),
                      increase = sum(diff_log > 0, na.rm = TRUE)/n(),
                      na = sum(is.na(diff_log))/n())

out_geom <- summarize(yy_compare_geom,
                      decrease = sum(diff_log < 0, na.rm = TRUE)/n(),
                      no_change = sum(diff_log == 0, na.rm = TRUE)/n(),
                      increase = sum(diff_log > 0, na.rm = TRUE)/n(),
                      na = sum(is.na(diff_log))/n())

out <- bind_rows(list(flat = out_flat, geom = out_geom),
                 .id = 'smooth_type')
# output -- print and save as file
print(out)
write.csv(out, file = file.path('~', 'eBird_project', 'summary_output', 
                                paste0(species, '_yy_diffs.csv')
                                ))