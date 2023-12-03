#=========================================================================
#
# This is some more advanced EDA on spatially unbiased data to look at
# Species counts over space and time.
#
#=========================================================================

#=========================================================================
# Load dependencies and parse options 
#==========================================================================
library(tidyverse) # data manipulation tools
library(viridis) # for color-blind friendly visuals
library(zoo) # helpful for year_mon object and related tools
library(optparse) # for flexibility in options when called through command line
# get options
option_list <- list(
  make_option(c('-s', '--speciesCode'), type = 'character',
              action = 'store', help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

# functions=============================================================== 
#=========================================================================

# make it easier to save plots onto separate pages
save_pages <- function(ggobj, type, directory, ncol, nrow, species, facets,
		       page = 'portrait'){
  all_plots <- list()
  for(j in 1:ggforce::n_pages(ggobj))
  {
	p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
                                                     ncol = ncol,
                                                     nrow = nrow,
    						     page = j)
  	all_plots[[j]] <- p_save
  }
  name <- paste0(species, '_', type, '.pdf')
  fp <- file.path('~', 'eBird_project', 'plots', directory, species, name)
  pdf(fp, width = 11, height = 8.5)
  for (k in seq_along(all_plots)) 
  {
       print(all_plots[[k]])
  }
  dev.off()
}

# turn the dataframe to a matrix for easy use in lapply/maps
df_to_mat <- function(df, ym, n = 200){
  # make sure the data frame has every lat-long combination represented
  temp <- ungroup(df) |>
    complete(nesting(year_mon),
             long_bin = 1:n,
             lat_bin = 1:n
    ) |>
    filter(year_mon == ym) |>
    arrange(long_bin, lat_bin)

  # turn the data frame to a matrix
  temp <- matrix(temp$obs_freq, nrow = n, ncol = n, byrow = TRUE)
  return(temp)
}

# smooth the data by averaging a cell with its neighbors; does not
#> propagate NA values.
flat_smooth <- function(matrix_in){
  # assumes a square matrix (which we have)
  N <- length(matrix_in[1,]) # should be 200
  out <- matrix(NA, nrow = N, ncol = N)

  for(j in 1:N){
    for(k in 1:N){
      # do interior cells first
      if(1 < k && k < N && 1 < j && j < N){
        out[j, k] <- mean(matrix_in[(j-1):(j+1), (k-1):(k+1)], na.rm = TRUE)
      }else{
        valid_rows <- (j-1):(j+1)
        valid_rows <- valid_rows[(j-1):(j+1) >= 1 & (j-1):(j+1) <= N]

        valid_cols <- (k-1):(k+1)
        valid_cols <- valid_cols[(k-1):(k+1) >= 1 & (k-1):(k+1) <= N]

        out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
      }
    }
  }
  return(out)
}

#=========================================================================
# wrangle data and plot
#=========================================================================

# load data
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
		    year(observation_date) %in% years,
		    species_code == opt$s)
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
		      year_mon = as.yearmon(observation_date)) |>
	       group_by(year_mon, long_bin, lat_bin) |>
	       summarize(obs_freq = sum(species_observed)/n())
print('summarized')

# free up some RAM
remove(subsample)


# plot the unprocessed data
#month_plot <- ggplot(ym_obs_freq, aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+0.1)))+
#	      geom_raster()+
#	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
#	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
#	      theme_bw()+
#	      theme(legend.direction = 'horizontal',
#                    legend.position = 'bottom')

#print('made raw month gg object')

#save_pages(month_plot, type = 'month_raw',
#           facets = vars(year_mon),
#	   ncol = 6,
#           nrow = 4,
#           species = opt$s,
#           directory = 'monthly',
#	   page = 'landscape')
#print('saved raw month plot')

#remove(month_plot)

# turn the data into matrices and smooth
yms <- unique(ym_obs_freq$year_mon)
smooth_mats <- map(yms, ~ df_to_mat(ym_obs_freq, .x)) |>
	       map(flat_smooth) |>
	       set_names(yms)
print('smoothed matrices created')
# free up some RAM
remove(ym_obs_freq)
	       
#=========================================================================
# monthly distributions
#=========================================================================

# make the matrices into a data frame for plotting
#smoothed_df <- smooth_mats |>
#               lapply(t) |>
#               lapply(as.vector) |>
#              # put the vectors in a data frame with year_mon column and obs_freq
#              #> column; the obs_freq cells are vectors
#               enframe(name = 'year_mon', value = 'obs_freq') |>
#               unnest_longer('obs_freq') |>
#               mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
#                                           is.na(obs_freq) ~ NA,
#                                           !is.na(obs_freq) ~ obs_freq))
#n <- length(unique(smoothed_df$year_mon))
#smoothed_df <- smoothed_df |> 
#               mutate(long_bin = rep(rep(1:200, each = 200), n),
#                      lat_bin = rep(rep(1:200, times = 200), n),
#                      year_mon = as.yearmon(year_mon)
#                      )

# create ggplot object for smoothed data
#month_plot <- ggplot(data = smoothed_df, aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+0.1)))+
#	      geom_raster()+
#	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
#	     # scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
#	      theme_bw()+
#	      theme(legend.direction = 'horizontal',
#                    legend.position = 'bottom')
	
#print('made ggplot object for months')

# save smoothed plots
#save_pages(month_plot, type = 'month_smoothed',
#           facets = vars(year_mon),
#	   ncol = 6,
#           nrow = 4,
#           species = opt$s,
#           directory = 'monthly',
#	   page = 'landscape')

# free up some RAM
#remove(month_plot)



#=========================================================================
# comparing distributions between months and years
#=========================================================================

# subset the data to every other year
years <- seq(2010, 2022, by = 2)

evens <- year(as.yearmon(names(smooth_mats))) %in% years

smooth_mats <- smooth_mats[evens]

# get a data frame that has every 3-, 6-, and 9-month comparison (only in the
#> forward time direction)
mm_compare <- smooth_mats |> 
              lapply(t) |>
	      lapply(as.vector) |>
	      enframe(name = 'year_mon', value = 'obs_freq') |>
              expand(nesting(year_mon = as.yearmon(year_mon), obs_freq),
                     nesting(year_mon2 = as.yearmon(year_mon), obs_freq2 = obs_freq)) |>
	      filter(year(year_mon) <= year(year_mon2),
                     month(year_mon) == month(year_mon2) & year(year_mon) != year(year_mon2) |
                     month(year_mon) == month(year_mon2) - 3 |
                     month(year_mon) == month(year_mon2) - 6 |
                     month(year_mon) == month(year_mon2) - 9) |>
	      mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
                     diff = map2(obs_freq2, obs_freq, `-`)) |>
              select(-obs_freq, -obs_freq2) |>
              unnest_longer(diff) |>
              mutate(diff = case_when(is.nan(diff) ~ NA,
                                      !is.nan(diff) ~ diff),
                     transform_diff = case_when(is.na(diff) ~ NA,
                                                diff < 0 ~ -sqrt(abs(diff)),
                                                diff == 0 ~ 0,
                                                diff > 0 ~ sqrt(abs(diff))
						)
		     )
print('monthly comparison df created')
n <- length(unique(mm_compare$comparison)) 

# add long_bin and lat_bin columns
mm_compare <- mm_compare |>
              mutate(long_bin = rep(rep(1:200, each = 200), n),
                     lat_bin = rep(rep(1:200, times = 200), n))

# reorder the comparison factor chronologically
mm_compare$comparison <- factor(mm_compare$comparison,
                                unique(mm_compare$comparison[order(mm_compare$year_mon, mm_compare$year_mon2, mm_compare$comparison)])
				)
# plot

mm_plot <- ggplot(mm_compare, aes(x = long_bin, y = lat_bin, fill = transform_diff))+
           geom_raster()+
	   ggforce::facet_wrap_paginate(facets = vars(comparison), nrow = 6, ncol = 6)+
           scale_fill_distiller(palette = 'RdBu', direction = -1, na.value = '#cccccc')+
           theme_bw()+
           theme(legend.direction = 'horizontal',
                 legend.position = 'bottom')
print('created gg object for monthly comparisons')

save_pages(mm_plot, type = 'smoothed_compared',
           facets = vars(comparison),
	   ncol = 6,
           nrow = 6,
           species = opt$s,
           directory = 'comparisons',
	   page = 'landscape')

