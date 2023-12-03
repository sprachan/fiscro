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
#> lapply version from ChatGPT
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
#  all_plots <- list()
  
# for(j in 1:ggforce::n_pages(ggobj))
#   {
#	p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
#                                                     ncol = ncol,
#                                                     nrow = nrow,
#    						     page = j)
#  	all_plots[[j]] <- p_save
#  }
#  name <- paste0(species, '_', type, '.pdf')
#  fp <- file.path('~', 'eBird_project', 'plots', directory, species, name)
#  pdf(fp, width = 11, height = 8.5)
#  for (k in seq_along(all_plots)) 
#  {
#       print(all_plots[[k]])
#  }
#  dev.off()
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
	 valid_rows <- (j-1):(j+1)
      	 valid_rows <- valid_rows[(j-1):(j+1) >= 1 & (j-1):(j+1) <= N]
      
      	 valid_cols <- (k-1):(k+1)
      	 valid_cols <- valid_cols[(k-1):(k+1) >= 1 & (k-1):(k+1) <= N]
      
         # if current cell is NA, do a regular average
      	 if(is.na(matrix_in[j, k])){
            out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
         }else{
            temp <- matrix_in
	    temp[j, k] <- NA
      	 
	    neighbors <- temp[valid_rows, valid_cols]
      	    n <- sum(!is.na(neighbors)) 
           
	    if(n > 0){
            # if there are any neighbors with information, take the weighted average
               out[j, k] <- w*matrix_in[j, k]+((1-w)/n)*sum(neighbors, na.rm = TRUE)
      	    }else{
            # otherwise if the current cell is an "island", it gets itself back
               out[j, k] <- matrix_in[j, k]
	    }
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



# turn the data into matrices and smooth
yms <- unique(ym_obs_freq$year_mon)

smooth_df <- map(yms, ~df_to_mat(ym_obs_freq, .x)) |>
             map(geom_smooth) |>
             set_names(yms) |>
	     lapply(t) |>
	     lapply(as.vector) |>
	     enframe(name  = 'year_mon', value = 'obs_freq') |>
	     unnest_longer(obs_freq) |>
	     mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
					 !is.nan(obs_freq) ~ obs_freq)
	           )
n <- length(unique(smooth_df$year_mon))
smooth_df <- smooth_df |> 
             mutate(long_bin = rep(rep(1:200, each = 200), n),
		    lat_bin = rep(rep(1:200, times = 200), n),
		    year_mon = as.yearmon(year_mon)
		   )

month_plot <- ggplot(smooth_df,
		     aes(x = long_bin, y = lat_bin, fill = obs_freq))+
              geom_raster()+
	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
	      theme_bw()+
	      theme(legend.direction = 'horizontal',
                    legend.position = 'bottom')+
	      ggforce::facet_wrap_paginate(facets = vars(year_mon),
					   ncol = 6,
					   nrow = 4)

save_pages(month_plot, type = 'month_geom_smoothed',
           facets = vars(year_mon),
	   ncol = 6,
           nrow = 4,
           species = opt$s,
           directory = 'monthly')

# free up some RAM
remove(month_plot)
remove(smooth_df)
#=========================================================================
# year-on-year comparisons 
#=========================================================================

# get a data frame that has comparisons between the same month in
#> every year, only doing the forward comparison (eg., Jan 2022-Jan 2020)

yy_compare <- map(yms, ~ df_to_mat(ym_obs_freq, .x)) |>
	      map(geom_smooth) |>
              set_names(yms) |>
              lapply(t) |>
	      lapply(as.vector) |>
	      enframe(name = 'year_mon', value = 'obs_freq') |>
              expand(nesting(year_mon = as.yearmon(year_mon), obs_freq),
                     nesting(year_mon2 = as.yearmon(year_mon), obs_freq2 = obs_freq)) |>
	      filter(year(year_mon) < year(year_mon2),
		     month(year_mon) == month(year_mon2)) |>
	      mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
                     diff = map2(obs_freq2, obs_freq, `-`)) |>
              select(-obs_freq, -obs_freq2) |>
              unnest_longer(diff) |>
              mutate(diff = case_when(is.nan(diff) ~ NA,
                                      !is.nan(diff) ~ diff
				     ),
                     transform_diff = case_when(is.na(diff) ~ NA,
                                                diff < 0 ~ -sqrt(abs(diff)),
                                                diff == 0 ~ 0,
                                                diff > 0 ~ sqrt(abs(diff))
					       )
		     )
print('year-on-year comparison df created')
n <- length(unique(yy_compare$comparison)) 

# add long_bin and lat_bin columns
yy_compare <- yy_compare |>
              mutate(long_bin = rep(rep(1:200, each = 200), n),
                     lat_bin = rep(rep(1:200, times = 200), n))

# reorder the comparison factor chronologically
yy_compare$comparison <- factor(yy_compare$comparison,
                                unique(yy_compare$comparison[order(yy_compare$year_mon, yy_compare$year_mon2, yy_compare$comparison)])
				)
# plot
#> each year-mon will be compared to 11 others (and itself so there are 12 plots per year_mon)
#> want 2 year-mons per page (24 plots per page)
yy_plot <- ggplot(yy_compare, aes(x = long_bin, y = lat_bin, fill = transform_diff))+
           geom_raster()+
	   ggforce::facet_wrap_paginate(facets = vars(comparison), nrow = 4, ncol = 6)+
           scale_fill_distiller(palette = 'RdBu', direction = -1, na.value = '#cccccc')+
           theme_bw()+
           theme(legend.direction = 'horizontal',
                 legend.position = 'bottom')
print('created gg object for year-on-year comparisons')

save_pages(yy_plot, type = 'geom_smoothed_yy',
           facets = vars(comparison),
	   ncol = 6,
           nrow = 4,
           species = opt$s,
           directory = 'comparisons')

