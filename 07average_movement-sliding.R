# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes the average observation frequency between years for each 
#> latitude/longitude cell for sliding 7-day windows starting on each day.
#> Produces 365 images in a PDF, one per page, that can then be animated
#> into a gif.
#>
# ------------------------------------------------------------------------------
fp <- file.path('~', 'eBird_project', 'plots', 'weekly')
# Dependencies =================================================================
## packages ----
library(dplyr)
library(optparse) 
## functions ----
source('01functions.R')

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

daily <- subsample |> filter(species_code == opt$s,
                             lubridate::year(observation_date) >= 2010) |>
                      select(long_bin,
                             lat_bin,
                             species_code,
                             observation_count,
                             species_observed,
                             observation_date)|>
                      mutate(day = lubridate::yday(observation_date),
                             year = lubridate::year(observation_date)
                      ) |>
                      group_by(long_bin, lat_bin, day, year) |>
                      summarize(observation_count = sum(observation_count),
                                species_observed = sum(species_observed),
                                n_lists = n(),
                                obs_freq = species_observed/n_lists)

# Average Movements ============================================================
day_name <- rep('', 365)
for(i in 1:365){
  day_name[i] <- paste0('day', i)
}
avg_mats <- purrr::map(1:365, \(x) df_to_slide_mat(daily, x)) |>
            purrr::set_names(day_name) |>
            lapply(geom_smooth)
cols <- viridis::viridis(200, option = 'inferno')
n <- paste0(opt$s, '_slide.pdf')
pdf(file = file.path(fp, n))
purrr::map(1:365, \(x) image(avg_mats[[x]],
                             col = cols,
                             main = as.character(x),
                             cex.main = 0.8))
dev.off()