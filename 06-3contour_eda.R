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
phis <- c(1e-3,0.2, 0.4, 0.6, 0.8, 0.999)

plots <- map(phis, \(x) cutoff_plot(data_in = ym_obs_freq, 
                                    cutoff = x, 
                                    title = as.character(x))) |>
         set_names(phis)

species <- opt$s
name <- paste0(species, '.pdf')

fp <- file.path('~', 'eBird_project', 'plots', 'cutoffs', species, name)
pdf(fp, width = 11, height = 8.5)
  patchwork::wrap_plots(plots, nrow = 2, ncol = 3)
dev.off()
