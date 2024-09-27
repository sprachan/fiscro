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
require(auk) # tools for working with eBird data
require(optparse) # allows me to set arguments on command line Rscript calls
require(dplyr) # data manipulation tools

# Options ----
# get arguments from the command line input
option_list <- list(
  make_option(c("-a", "--ebdinput"), type = 'character',
              action = 'store',
              help="ebd input file name (do not include directory)"),
  make_option(c("-b", "--sedinput"), type = 'character',
              action = 'store',
              help="sed input file name (do not include directory)"),
  make_option(c("-c", "--ebdoutput"), type = 'character',
              action = 'store',
              help="ebd output file name (do not include directory)"),
  make_option(c("-d", "--sedoutput"), type = 'character',
              action = 'store',
              help = "sed output file name (do not include directory)")
  )

# parse
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

#store those arguments as easier to access variables
input_ebd_name <- opt$ebdinput
input_sed_name <- opt$sedinput
output_ebd_name <- opt$ebdoutput
output_sed_name <- opt$sedoutput

output_ebd <- file.path('.', 'filtered_data', output_ebd_name)
output_sed <- file.path('.', 'filtered_data', output_sed_name)
input_ebd <- file.path('.', 'data', input_ebd_name)
input_sed <- file.path('.', 'data', input_sed_name)

#output will go to the command line, making this easier to debug and trace.
print('names are set:')
cat('input ebd name: ', input_ebd_name, '\n', 
    'input sed name: ', input_sed_name, '\n',
    'output ebd name: ', output_ebd_name, '\n',
    'output sed name: ', output_sed_name, '\n')

print('Input directory is assumed ./data and output directory is assumed 
      ./filtered_data, so final paths are: ')
cat('input ebd path: ', input_ebd, '\n',
    'input sed path: ', input_sed, '\n',
    'output ebd path: ', output_ebd, '\n',
    'output sed path: ', output_sed, '\n')

# Setup variables for filters =================================================
species <- c('Fish Crow',
             'Ring-Billed Gull',
             'American Robin',
             'Tufted Titmouse',
             'Blue Jay',
             'American Crow')

duration <- c(5, 180) # 5 minutes to 3 hours
distance <- c(0, 5) # 0 to 5km
# Filter =======================================================================
#> Will overwrite existing filtered files if they exist in the destination
filters <- auk_ebd(file = input_ebd, file_sampling = input_sed) |>
           auk_state('US-ME') |>
           auk_species(species) |>
           auk_duration(duration) |>
           auk_distance(distance=distance) |>
           auk_complete()
print(filters)

filtered <- auk_filter(filters,
                       file = output_ebd, 
                       file_sampling = output_sed,
                       overwrite = TRUE)

print('filtered')

