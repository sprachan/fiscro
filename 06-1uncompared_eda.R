# DESCRIPTION ------------------------------------------------------------------
#>
#> This is some more advanced EDA on spatially unbiased data to look at
#> species counts over space and time.
#> This script does not plot any comparisons; rather, it plots maps for 
#> each month for each year and plots histograms of observation frequency on a 
#> monthly and yearly basis.
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
                                help = 'species for analysis'),
                    make_option(c('-e', '--epsilon',
                                  type = 'double',
                                  action = 'store',
                                  help = 'adjustment value for log10(obs freq)'))
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);
species <- opt$s
eps <- opt$e

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

# Unsmoothed Data EDA ==========================================================
# mapping
month_plot <- map_uncompared(ym_obs_freq, epsilon = eps)

save_pages(month_plot,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_raw.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw monthly plots')

remove(month_plot) # free up some RAM

# histogram
month_hist <- hist_uncompared(ym_obs_freq, epsilon = eps)

save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw monthly histograms')
                       


## flat smoothed data ----------------------------------------------------------
smoothed_df <- df_smoother(ym_obs_freq, smooth_type = 'flat', over = 'ym')
# mapping
month_plot <- map_uncompared(smoothed_df, epsilon = eps)

save_pages(month_plot,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_flat.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved flat smoothed monthly plots')

remove(month_plot) # free up some RAM

# histogram
month_hist <- hist_uncompared(smoothed_df, epsilon = eps)

save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_flat_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved flat smoothed monthly histograms')

## geom smoothed data ----------------------------------------------------------
smoothed_df <- df_smoother(ym_obs_freq, smooth_type = 'geom', over = 'ym')
# mapping
month_plot <- map_uncompared(smoothed_df, epsilon = eps)

save_pages(month_plot,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_geom.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved geom smoothed monthly plots')

remove(month_plot) # free up some RAM

# histogram
month_hist <- hist_uncompared(smoothed_df, epsilon = eps)

save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_geom_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved geom smoothed monthly histograms')