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
               summarize(obs_freq = sum(species_observed)/n(),
                         n_lists = n()) |>
               mutate(obs_freq = as.numeric(obs_freq))
print('summarized')

# free up some RAM
remove(subsample)

# Smooth =======================================================================
yy_compare_flat <- compare(ym_obs_freq, time_type = 'yy', smooth_type = 'flat')
print('flat smooth done')

yy_compare_geom <- compare(ym_obs_freq, time_type = 'yy', smooth_type = 'geom')
print('geom smooth done')

# Plot as maps =================================================================
yy_plot <- map_compared(yy_compare_flat)
print('made flat compared map ggobj')

save_pages(yy_plot,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_yy_flat.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(comparison))
print('saved pages for flat smoothed year-on-year comparisons')
remove(yy_plot)

yy_plot <- map_compared(yy_compare_geom)
print('made geom smoothed map ggobj')

save_pages(yy_plot,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_yy_geom.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(comparison))

print('saved pages for geom smoothed year-on-year comparisons')
remove(yy_plot)

# Plot histograms of differences ===============================================
yy_hist <- hist_compared(yy_compare_flat)
print('made flat smoothed hist ggobj')

save_pages(yy_hist,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_yy_flat_hist.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(comparison))
print('saved pages for flat hists, year-on-year comparisons')
remove(yy_hist)

yy_hist <- hist_compared(yy_compare_geom)
print('made geom smoothed hist ggobj')
save_pages(yy_hist,
           path = file.path(fp, 'comparisons', species),
           name = paste0(species, '_yy_geom_hist.pdf'),
           nrow = 3,
           ncol = 4,
           facets = vars(comparison))
print('saved pages for geom hists, year-on-year comparisons')