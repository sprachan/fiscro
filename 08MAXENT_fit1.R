# DESCRIPTION ------------------------------------------------------------------
#>
#> This script fits a MAXENT model to the ebird data.
#>
# ------------------------------------------------------------------------------
fp <- file.path('~', 'eBird_project', 'plots', 'weekly')
# Dependencies =================================================================
## packages ----
library(dplyr) # data wrangling
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

# testing purposes
species <- 'fiscro'

# Load data ====================================================================
# HPC Version: load('./processed_data/combined_zf.RData')
# My computer version
load('../data/combined_zf.RData')

# clean up and turn into presence-only data
combined_zf <- combined_zf |> 
               dplyr::select(-X) |>
               dplyr::filter(species_code == species,
                             species_observed == TRUE,
                             lubridate::year(observation_date) %in% seq(2010, 2023))
# will not remove duplicate locations -- this might get rid of checklists at
#> "hotspots" within the same day