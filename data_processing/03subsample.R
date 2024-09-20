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
  make_option(c('-f', '--folder', type = 'character',
                action = 'store', help = 'subfolder for subsample'))
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

# store these in R in an easier to access way
num_bins <- opt$numbins
epsilon <- opt$epsilon |> as.numeric()
sample_size <- opt$samplesize |> as.numeric()
folder <- opt$folder

# print out parameters used for rasterizing
print(num_bins)
print(epsilon)
print(sample_size)
print(folder)

# File Paths ===================================================================
plot_path <- file.path('~', 'eBird_project', 'plots', 'subsampling')
text_path <- file.path('~', 'eBird_project', 'subsampling_metrics')
pq_path <- file.path('.', 'processed_data')


# Subsample ====================================================================
ebd_ds <- arrow::open_dataset(file.path(pq_path, 'species'))

## Process data into longitude and latitude bins, num_bins of each ----
ext <- dplyr::select(ebd_ds, longitude, latitude) |>
       dplyr::summarize(long_min = min(longitude, na.rm = TRUE),
                        long_max = max(longitude, na.rm = TRUE),
                        lat_min = min(latitude, na.rm = TRUE),
                        lat_max = max(latitude, na.rm = TRUE)) |>
       dplyr::collect()

long_breaks <- seq(ext$long_min, ext$long_max, length.out = num_bins)
lat_breaks <- seq(ext$lat_min, ext$lat_max, length.out = num_bins)

print('Longitude and latitude break vectors created') # progress check


# write this directly to a parquet file to keep things relatively faster.
dplyr::select(ebd_ds, checklist_id, longitude, latitude) |>
dplyr::distinct() |>
dplyr::collect() |>
dplyr::mutate(long_bin = findInterval(longitude, long_breaks),
              lat_bin = findInterval(latitude, lat_breaks),
              cell = (long_bin - 1)*num_bins + lat_bin) |>
write_dataset(path = file.path(pq_path, 'checklists'),
              format = 'parquet',
              existing_data_behavior = 'overwrite')

print('Wrote checklist and bins to parquet file')

## using this binned/rasterized data, calculate sampled p adjusted by epsilon ----
list_ds <- open_dataset(file.path(pq_path, 'checklists'))

weights <- dplyr::select(list_ds, cell) |>
           dplyr::group_by(cell) |>
           dplyr::summarize(num_lists = dplyr::n()) |>
           dplyr::mutate(sampled_p = num_lists/sum(num_lists))

## subsample checklists using this sampled p ----
list_subsample <- dplyr::select(list_ds, checklist_id, cell) |>
                  dplyr::left_join(weights, by = dplyr::join_by(cell)) |>
                  dplyr::select(checklist_id, cell, sampled_p) |>
                  dplyr::collect() |>
                  dplyr::slice_sample(n = sample_size,
                                      weight_by = 1/(sampled_p+epsilon))

print('Subsampled checklists')


# Subsample the whole data =====================================================
# right_join keeps all the observations in y, but not x; that is, we keep the
#> subsampled checklists, but not the other ones.
dplyr::right_join(ebd_ds, 
                  list_subsample, 
                  by = dplyr::join_by(checklist_id)) |>
  dplyr::mutate(long_bin = cell %/% num_bins + 1,
                lat_bin = cell %% num_bins,
                lat_bin = dplyr::case_when(lat_bin != 0 ~ lat_bin,
                                           .default = num_bins)) |>
  write_dataset(path = file.path(pq_path, 'subsamples', folder),
                format = 'parquet',
                partitioning = c('species_code'),
                existing_data_behavior = 'overwrite')

print('Used checklist subsample to subset whole dataset')
