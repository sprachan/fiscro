# DESCRIPTION ------------------------------------------------------------------
#>
#> Given an input EBD and SED, output an EBD and SED that are filtered in 
#> the same way. Filters data down to only species of interest as well as 
#> by some effort variables (duration, distance) to keep only higher quality 
#> data, as suggested by Best Practices for using eBird Data 
#> (https://ebird.github.io/ebird-best-practices/)
#>
# ------------------------------------------------------------------------------

# Load dependencies and parse options ==========================================
# Dependencies ----
# tools for working with eBird data
library(auk)

# get and use options so I can run scripts flexibly from the command line
library(optparse)

# data manipulation tools
library(dplyr)

# Options ----
# get arguments from the command line input
option_list <- list(
  make_option(c("-a", "--ebdinput"), type = 'character',
              action = 'store',
              help="ebd input file name"),
  make_option(c("-b", "--sedinput"), type = 'character',
              action = 'store',
              help="sed input file name"),
  make_option(c("-c", "--ebdoutput"), type = 'character',
              action = 'store',
              help="ebd output file name"),
  make_option(c("-d", "--sedoutput"), type = 'character',
              action = 'store',
              help = "sed output file name"),
  make_option(c("-e", "--zerofilloutput"), type = 'character',
              action = 'store',
              help = "zerofill output prefix, eg., ./processed_data/ct_")
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

#store those arguments as easier to access variables
input_ebd <- opt$ebdinput
input_sed <- opt$sedinput
output_ebd <- opt$ebdoutput
output_sed <- opt$sedoutput
zf_output <- opt$zerofilloutput
#output will go to the command line, making this easier to debug and trace.
print('names are set:')
cat('input ebd: ', input_ebd, '\n', 
    'input sed: ', input_sed, '\n',
    'output ebd: ', output_ebd, '\n',
    'output sed: ', output_sed, '\n',
    'prefix: ', prefix, '\n')

# Setup variables for filters =================================================
species <- c('Mourning Dove',
             'American Crow',
             'Fish Crow',
             'American Robin',
             'Ring-Billed Gull')

duration <- c(5, 300)
distance <- c(0, 5)

# Filter =======================================================================
#> Will overwrite existing filtered files if they exist in the destination
filtered <- auk_ebd(file = input_ebd, file_sampling = input_sed) |>
  auk_species(species) |>
  auk_duration(duration) |>
  auk_distance(distance=distance) |>
  auk_complete() |>
  auk_filter(file = output_ebd, file_sampling = output_sed,
             overwrite=TRUE)
print('filtered')

