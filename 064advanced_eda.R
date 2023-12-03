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
save_pages <- function(ggobj, type, directory, ncol, nrow, species, facets)
              {
	         all_plots <- lapply(1:ggforce::n_pages(ggobj), function(j){
			 p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
								      ncol = ncol,
								      nrow = nrow,
								      page = j)
			 return(p_save)
		 })

                  name <- paste0(species, '_', type, '.pdf')

	          fp <- file.path('~', 'eBird_project', 'plots', directory, species, name)
		  pdf(fp, width = 11, height = 8.5)
		  lapply(all_plots, print)
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

# smooth by applying a weighted average
geom_smooth <- function(matrix_in, w = 0.75){
	N <- length(matrix_in[1,])
	out <- matrix(NA, nrow = N, ncol = N)
	for(j in 1:N){
	  for(k in 1:N){
	  	temp <- matrix_in
	  	temp[j, k] <- NA
		valid_rows <- (j-1):(j+1)
      		valid_rows <- valid_rows[(j-1):(j+1) >= 1 & (j-1):(j+1) <= N]
      
      		valid_cols <- (k-1):(k+1)
      		valid_cols <- valid_cols[(k-1):(k+1) >= 1 & (k-1):(k+1) <= N]
      		neighbors <- temp[valid_rows, valid_cols]
      		n <- length(neighbors)-1
      
      # if current cell is NA, do a regular average
      	        if(is.na(matrix_in[j, k])){
        		out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
      		}else{
      # otherwise, do the weighted average
      			out[j, k] <- w*matrix_in[j, k]+((1-w)/n)*sum(neighbors, na.rm = TRUE)
      		}
	   }
	}
return(out)
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
	       map(geom_smooth) |>
	       set_names(yms)
print('smoothed matrices created')
# free up some RAM
remove(ym_obs_freq)
	       
#=========================================================================
# monthly distributions
#=========================================================================

# make the matrices into a data frame for plotting
smoothed_df <- smooth_mats |>
               lapply(t) |>
               lapply(as.vector) |>
              # put the vectors in a data frame with year_mon column and obs_freq
              #> column; the obs_freq cells are vectors
               enframe(name = 'year_mon', value = 'obs_freq') |>
 	       unnest_longer('obs_freq') |>
               mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
                                           is.na(obs_freq) ~ NA,
                                           !is.na(obs_freq) ~ obs_freq))
n <- length(unique(smoothed_df$year_mon))
smoothed_df <- smoothed_df |> 
               mutate(long_bin = rep(rep(1:200, each = 200), n),
                      lat_bin = rep(rep(1:200, times = 200), n),
                      year_mon = as.yearmon(year_mon)
                      )

# create ggplot object for smoothed data
month_plot <- ggplot(data = smoothed_df, aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+0.1)))+
	      geom_raster()+
	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
	      theme_bw()+
	      theme(legend.direction = 'horizontal',
                    legend.position = 'bottom')
	
print('made ggplot object for months')

# save smoothed plots
save_pages(month_plot, type = 'month_geom_smoothed',
           facets = vars(year_mon),
	   ncol = 6,
           nrow = 4,
           species = opt$s,
           directory = 'monthly')

# free up some RAM
remove(month_plot)

remove(smoothed_df)


