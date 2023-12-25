# DESCRIPTION ------------------------------------------------------------------
#>
#> Given an input EBD and SED, return zero-filled dataframe: one row for each
#> unique checklist-species combination; columns the same as the EBD, with 
#> the addition of a (logical) species_observed column. I also clean up some
#> variables:
#> use 6-letter banding code instead of scientific name; 
#> convert observation counts to numbers, and make "X"'s into NAs; 
#> convert stationary NA effort distances to 0.
#>
# ------------------------------------------------------------------------------

# Dependencies =================================================================
library(auk)
library(dplyr)
library(optparse)


# Parse Command Line Options ===================================================

#> Necessary because I used Bowdoin's HPC to process these large files,
#> because my personal computer couldn't (effectively) handle them.

option_list <- list(
  make_option(c("-e", "--ebdinput"), type = 'character',
              action = 'store',
              help="ebd input file name"),
  make_option(c("-s", "--sedinput"), type = 'character',
              action = 'store',
              help="sed input file name"),
  make_option(c("-z", "--zerofilloutput"), type = 'character',
              action = 'store',
              help = "zerofill output prefix, eg., ./processed_data/ct_")
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

input_ebd <- opt$ebdinput
input_sed <- opt$sedinput
output_zf <- opt$zerofilloutput

# check that that worked as intended
cat('EBD input path: ', input_ebd, '\n',
    'SED input path: ', input_sed, '\n',
    'ZF output path: ', output_zf, '\n')


# Zerofilling ==================================================================



species <- c('Mourning Dove',
             'American Crow',
             'Fish Crow',
             'American Robin',
             'Ring-Billed Gull')

ebd_zf <- auk_zerofill(input_ebd, sampling_events = input_sed,
                       species = species, collapse = TRUE) |>
  mutate(species_code = ebird_species(scientific_name, 'code'),
         observation_count = na_if(observation_count, 'X'),
         observation_count = as.numeric(observation_count),
         effort_distance_km = case_when(protocol_type == 'Stationary' ~ 0,
                                        .default = as.numeric(effort_distance_km))
  ) |>
  select(checklist_id,
         observer_id,
         sampling_event_identifier,
         species_code,
         observation_count,
         species_observed,
         state_code,
         latitude,
         longitude,
         protocol_type,
         observation_date,
         time_observations_started,
         duration_minutes,
         effort_distance_km,
         number_observers) |>
  filter(protocol_type %in% c('Stationary', 'Traveling'),
         !is.na(time_observations_started),
         number_observers <=10)
print('zf, filtered, and selected columns')
# write file
write.csv(ebd_zf, file = output_zf)
print('written')


combined_zf  <- list.files(path='./processed_data', pattern='zf',
                           all.files=TRUE, full.names=TRUE) |>
  purrr::map(function(x) read.csv(x)) |>
  dplyr::bind_rows()

save(combined_zf, file='./processed_data/combined_zf.RData')
