# DESCRIPTION ------------------------------------------------------------------
#>
#> This is some more advanced EDA on spatially unbiased data to look at
#> species counts over space and time. This script plots raw data for each
#> year/month combination. It also does month-on-month comparisons of 
#> observation frequencies, taking the difference (within the same year) 
#> between the observation frequency in a given cell in consecutive months. 
#> Differences are always taken as the earlier month subtracted from the later;
#> for example, February 2022 - January 2022.
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

# Wrangle data =================================================================
# load data
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
            		    lubridate::year(observation_date) %in% years,
            		    species_code == species)
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
            		      year_mon = as.yearmon(observation_date)) |>
            		      group_by(year_mon, long_bin, lat_bin) |>
            		      summarize(obs_freq = sum(species_observed)/n(),
            		                n_lists = n()) |>
                      filter(n_lists >= 20)
print('summarized')


# free up some RAM
remove(subsample)


# Smooth and compare ===========================================================
mm_compare_flat <- compare(ym_obs_freq, time_type = 'mm', smooth_type = 'flat')
print('flat smooth done')

mm_compare_geom <- compare(ym_obs_freq, time_type = 'mm', smooth_type = 'geom')
print('geom smooth done')


# Plot as maps =================================================================
save_pages_break(mm_compare_flat,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_mm_flat.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'map')
print('saved pages for flat smoothed month-on-month comparisons')


save_pages_break(mm_compare_geom,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_mm_geom.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'map')
print('saved pages for geom smoothed month-on-month comparisons')


# Plot histograms of differences ===============================================
save_pages_break(mm_compare_flat,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_mm_flat_hist.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'hist')
print('saved pages for flat smoothed month-on-month hists')

save_pages_break(mm_compare_geom,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_mm_geom_hist.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'hist')
print('saved pages for geom smoothed month-on-month hists')