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
eps <- opt$e |> as.numeric()
print(eps)
## load functions and set base file path ---------------------------------------
source('01functions.R')

fp <- file.path('~', 'eBird_project', 'plots')

# Load Data ====================================================================
ym_obs_freq <- load_data()
# Unsmoothed Data EDA ==========================================================
# mapping
month_plot <- map_uncompared(ym_obs_freq, epsilon = eps)
print('ggobj created for raw monthly maps')

save_pages(month_plot,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_raw.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw monthly plots')

remove(month_plot) # free up some RAM

# histogram
month_hist <- filter(ym_obs_freq, n_lists >= 20) |>
              hist_uncompared(epsilon = eps)
print('ggobj created for histograms')
save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_monthly_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw monthly histograms')
                       


# flat smoothed data ----------------------------------------------------------
smoothed_df <- df_smoother(df = ym_obs_freq, df_type = 'raw', smooth_type = 'flat')
# mapping
month_plot <- map_uncompared(smoothed_df, epsilon = eps)
print('made flat smoothed map ggobj')

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
print('made flat smoothed hist ggobj')

save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_flat_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved flat smoothed monthly histograms')

# geom smoothed data ----------------------------------------------------------
smoothed_df <- df_smoother(df = ym_obs_freq, df_type = 'raw', smooth_type = 'geom')
# mapping
month_plot <- map_uncompared(smoothed_df, epsilon = eps)
print('made geom smoothed map ggobj')

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
print('made geom smoothed hist ggobj')

save_pages(month_hist,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_geom_hist.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved geom smoothed monthly histograms')