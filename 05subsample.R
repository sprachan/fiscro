# DESCRIPTION ------------------------------------------------------------------
#>
#> This script uses a kernel density estimator to estimate checklist density
#> over a 200x200 spatial grid, then samples with weights inversely proportional 
#> to the estimated density in order to reduce spatial bias in the data.
#>
# ------------------------------------------------------------------------------


# Load dependencies, parse options =======================================
## Dependencies ----
# data manipulation tools
library(dplyr)

# plotting tools
library(ggplot2)

# color-blind friendly color scales
library(viridis)
cols <- viridis(100)

# kde2d function and associated help
library(MASS)

# get options from command line for flexibility
library(optparse)
source('01functions.R')
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

# test that this worked
print(num_bins)
print(epsilon)
print(sample_size)
print(tag)

# Load and wrangle data ========================================================
# Load combined data -- this takes a minute
load('./processed_data/combined_zf.RData')
str(combined_zf)

# only need checklist id and location information (lat and long)
lists_location <- dplyr::select(combined_zf,
                                checklist_id,
                                latitude,
                                longitude) |>
                  distinct(.keep_all = TRUE)
str(lists_location)

# Sampling =====================================================================
# kde2d to get a probability density estimate for a given latitude and longitude
probs <- kde2d(lists_location$longitude, 
               lists_location$latitude,
               n = num_bins)
print('done kde2d')

file_name <- paste0('prob_map_', tag, '.png')
fp <- file.path('~', 'eBird_project', 'plots', 'subsampling', file_name)
png(filename = fp)
image(probs, col = cols)
title(main = paste(num_bins, epsilon, sample_size, sep = ', ')) 
dev.off()

## "flatten" the probability matrix into a vector
#> going across each row, then down to the next.
prob_vec <- as.vector(t(probs$z))

# Need to assign each checklist a spatial "bin" that matches a "bin"
#> in the kde2d lat/long.

long_bins_1 <- lapply(lists_location$longitude, get_bin, probs$x) |>
               unlist()
lat_bins_1  <- lapply(lists_location$latitude, get_bin, probs$y) |>
               unlist()
lists_location <- mutate(lists_location,
                         long_bin = long_bins_1,
                         lat_bin = lat_bins_1,
            		         cell = (long_bin-1)*num_bins+lat_bin,
            		         weight = 1/(prob_vec[cell]+epsilon))
str(lists_location)

print('bin check: ')
print(sum(lists_location$cell == 0))


# get some RAM back
remove(long_bins_1)
remove(lat_bins_1)


# sample! do this on as small a df as possible to speed up runtimes
list_subsample <- lists_location |> 
                  dplyr::select(checklist_id, long_bin, lat_bin, weight) |>
	                slice_sample(n = sample_size, 
						                   weight_by = lists_location$weight)
	
print('sampled')
str(list_subsample)
save(list_subsample, file = './processed_data/list_subsample.RData')

# plot new checklist distribution
p <- list_subsample |> group_by(long_bin, lat_bin) |>
                       summarize(num_lists = n()) |>
                       ggplot()+
                       geom_point(aes(x = long_bin, y = lat_bin, color = num_lists),
				                          size = 0.55)+
                       scale_color_viridis()


file_name <- paste0('subsample_map_', tag, '.png')
fp <- file.path('~', 'eBird_project', 'plots', 'subsampling',  file_name)
ggsave(filename = fp, plot = p, device = 'png', dpi = 300)


print('distribution plotted')



# save subsample metrics =======================================================
prob_var <- var(prob_vec)
file_name <- paste0('subsamp_output_', tag, '.txt')
fp <- file.path('~', 'eBird_project', file_name)
cat(prob_var, paste(num_bins, epsilon, sample_size, sep = ', '), 
    file = fp)
print('outputted')

# use checklist sample to subset the whole data set ============================
load('./processed_data/combined_zf.RData')

subsample <- left_join(list_subsample, 
                       combined_zf, 
                       by = join_by(checklist_id))

str(subsample)

save(subsample, file = './processed_data/subsample.RData')


