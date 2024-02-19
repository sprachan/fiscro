# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes the average observation frequency between years for each 
#> latitude/longitude cell for each week of the year.
#>
# ------------------------------------------------------------------------------

# Dependencies =================================================================
## packages ----
library(dplyr)
library(optparse) 
## functions ----
source('01functions.R')

## options ----

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

# Load and Wrangle Data ========================================================
# testing purposes:
load('./processed_data/subsample.RData')

# add week, year columns
weekly <- subsample |> filter(species_code == opt$s,
                              lubridate::year(observation_date) >= 2010) |>
                       select(long_bin,
                              lat_bin,
                              species_code,
                              observation_count,
                              species_observed,
                              observation_date
                              ) |>
                       mutate(week = lubridate::week(observation_date),
                              year = lubridate::year(observation_date)) |>
                       group_by(long_bin, lat_bin, week, year) |>
                       summarize(observation_count = sum(observation_count),
                                 species_observed = sum(species_observed),
                                 n_lists = n(),
                                 obs_freq = species_observed/n_lists)

# Average Movements ============================================================
wks <- rep('', 52)
for(i in 1:52){
  wks[i] <- paste0('wk', i)
}
cols <- viridis::viridis(200, option = 'inferno')
avg_mats <- purrr::map(1:52, \(x) df_to_mat(weekly, over = x, nest_by = 'week')) |>
            purrr::set_names(wks)
smoothed <- lapply(avg_mats, geom_smooth)

pdf(file = '~/week_plots.pdf')
lapply(avg_mats, image, col = cols)
dev.off()

pdf(file = '~/week_plots2.pdf')
lapply(smoothed, image, col = cols)
dev.off()