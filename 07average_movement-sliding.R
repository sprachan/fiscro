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
# set up titles and colors
names <- lubridate::make_date(2022, 1, 1)+lubridate::days(1:365-1)
names <- format(names, '%b-%d')
cols <- viridis::viridis(200, option = 'inferno')

# get matrices
avg_mats <- purrr::map(1:365, \(x) df_to_slide_mat(daily, x)) |>
            purrr::set_names(names) |>
            lapply(geom_smooth)

# convert matrices to a data frame for nice ggplot plotting
avg_df <- mats_to_vecdf(avg_mats, enf_name = 'day', enf_value = 'obs_freq') |>
          tidyr::unnest_longer(obs_freq) |>
          dplyr::mutate(long_bin = rep(rep(1:200, each = 200), 365),
                        lat_bin = rep(rep(1:200, times = 200), 365))

n <- paste0(opt$s, '_slide.pdf')
pdf(file = file.path(fp, n))
purrr::map(names, \(x) map_uncompared(avg_df, 
                                      epsilon = 1e-3, 
                                      year_mon = FALSE,
                                      over = x))
dev.off()

n <- paste0(opt$s, 'slide_cutoffs.pdf')
pdf(file = file.path(fp, n))
purrr::map(1:5, \(x) cutoff_plot(data_in = dplyr::filter(avg_df, day == names[x]),
                                   cutoff = -1.5,
                                   title = '-1.5',
                                   log = TRUE,
                                   epsilon = 1e-3))
dev.off()
