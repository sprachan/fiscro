# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes a whole, large, zero-filled eBird dataset, assigns each 
#> checklist a discrete longitude and latitude bins (number of bins in both
#> directions given by num_bins, passed in command line), calculates a sampled
#> "p" as (# lists in a long-lat bin combination)/(# of all lists), then 
#> resamples the data with weight given by 1/(p+epsilon). This produces a less
#> spatially biased subsample with size sample_size (from command line). We 
#> write the subsample to parquet files partitioned by species.
#> 
# ------------------------------------------------------------------------------

# Load dependencies, parse options =======================================
# data manipulation tools
library(arrow) # for working with the parquet files
library(dplyr)

# get options from command line for flexibility
library(optparse)
source('functions.R')
## Parse command line options ----
option_list <- list(
  make_option(c('-n', '--numbins'), type = 'integer', 
              action = 'store', help = 'number of bins for KDE2D'),
  make_option(c('-e', '--epsilon', type = 'double',
                action = 'store', help = 'probability weight adjustment')),
  make_option(c('-s', '--samplesize', type = 'integer',
                action = 'store', help = 'subsample size')),
  make_option(c('-t', '--plottag', type = 'character',
                action = 'store', help = 'suffix for plots'))
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

# store these in R in an easier to access way
num_bins <- opt$numbins
epsilon <- opt$epsilon |> as.numeric()
sample_size <- opt$samplesize |> as.numeric()
tag <- opt$plottag

# print out parameters used for rasterizing
print(num_bins)
print(epsilon)
print(sample_size)
print(tag)

# File Paths ===================================================================
plot_path <- file.path('~', 'eBird_project', 'plots', 'subsampling')
text_path <- file.path('~', 'eBird_project', 'subsampling_metrics')
pq_path <- file.path('.', 'processed_data')

# Subsample ====================================================================
# use the data that has already been binned into a 500x500 grid
ebd_ds <- arrow::open_dataset(file.path(pq_path, 'species'))
list_ds <- open_dataset(file.path(pq_path, 'checklists')) 

# get a random sample from these data
list_subsample <- dplyr::select(list_ds, checklist_id, cell) |>
                  dplyr::slice_sample(n = sample_size)

print('Subsampled checklists')


dplyr::right_join(ebd_ds, 
                  list_subsample, 
                  by = dplyr::join_by(checklist_id)) |>
dplyr::mutate(long_bin = cell %/% num_bins + 1,
             lat_bin = cell %% num_bins,
             lat_bin = dplyr::case_when(lat_bin != 0 ~ lat_bin,
                                        .default = num_bins)) |>
write_dataset(path = file.path(pq_path, 'subsamples', 'random'),
              partitioning = c('species_code'),
              existing_data_behavior = 'overwrite')

print('Used checklist subsample to randomly subset whole dataset')
