# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes the average observation frequency between years for each 
#> latitude/longitude cell for each week of the year.
#>
# ------------------------------------------------------------------------------
fp <- file.path('~', 'eBird_project', 'plots', 'monthly')
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
ym_obs_freq <- load_data()
names <- ym_obs_freq$year_mon
# Plot =========================================================================
n <- paste0(opt$s, '_year_geom.pdf')
pdf(file = file.path(fp, n))
purrr::map(names, \(x) map_uncompared(avg_df,
                                      epsilon = 1e-3,
                                      plot_facet = FALSE,
                                      keep = ym_obs_freq$year_mon == x,
                                      over = x))
dev.off()

