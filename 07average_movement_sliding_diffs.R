# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes calculates the average observation frequency across
#> all years for each latitude/longitude cell for sliding 7-day windows starting 
#> on each day. Then, it takes the differences in average observation frequencies
#> between consecutive windows (eg., Jan 2 - Jan 1).
#> Produces 364 images in a PDF, one per page, that can then be animated
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
            purrr::set_names(names) 

diffs <- list()
comparison <- rep('', 364)

# this will be slow -- maybe try to figure out a faster way later
for(i in 1:364){
  diffs[[i]] <- geom_smooth(avg_mats[[i+1]]+avg_mats[[i]]) 
  comparison[i] <- paste0(names(avg_mats)[i+1], '_', names(avg_mats[i]))
}
diffs <- purrr::set_names(diffs, comparison)


# convert matrices to a data frame for nice ggplot plotting
diff_df <- mats_to_vecdf(diffs, enf_name = 'comparison', enf_value = 'diff') |>
           tidyr::unnest_longer(diff) |>
           dplyr::mutate(long_bin = rep(rep(1:200, each = 200), 364),
                         lat_bin = rep(rep(1:200, times = 200), 364))


n <- paste0(opt$s, 'slide_diffs.pdf')
pdf(file = file.path(fp, n))
purrr::map(1:364, \(x) map_compared(diff_df,
                                    use_facets = FALSE,
                                    over = comparison[x]))
dev.off()
