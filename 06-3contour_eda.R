# DESCRIPTION ------------------------------------------------------------------
#>
#> This is some more advanced EDA on spatially unbiased data to look at
#> species counts over space and time.
#> This script plots contours of observation frequency to get a sense of how
#> dispersed populations are. This is done on all data together (not separated
#> by time period -- yet).
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

# Plot =========================================================================
## cutoffs ----
phis <- c(0.999, 0.6, 0.3, 1e-3)

plots <- purrr::map(phis, \(x) cutoff_plot(data_in = ym_obs_freq, 
                                           cutoff = x, 
                                           title = as.character(x))) |>
         purrr::set_names(phis)

species <- opt$s
name <- paste0(species, '_cutoff', '.pdf')

fp <- file.path('~', 'eBird_project', 'plots', 'cutoffs', species, name)
pdf(fp, width = 11, height = 8.5)
  patchwork::wrap_plots(plots, nrow = 2, ncol = 2)
dev.off()

# ggplot contours ----
p <- ggplot(ym_obs_freq, aes(x = long_bin, y = lat_bin, z = obs_freq))+
     geom_contour_filled()+
     scale_fill_viridis_d(option = 'B')

name <- paste0(species, '_contour', '.pdf')

fp <- file.path('~', 'eBird_project', 'plots', 'cutoffs', species, name)
pdf(fp, width = 11, height = 8.5)
  print(p)
dev.off()