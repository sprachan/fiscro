# This is some more advanced EDA on spatially unbiased data to look at
# Species counts over space and time.

# Load dependencies and parse options ==========================================

library(tidyverse) # data manipulation tools
library(viridis) # for color-blind friendly visuals
library(zoo) # helpful for year_mon object and related tools
library(optparse) # for flexibility in options when called through command line

# get options
option_list <- list(
  make_option(c('-s', '--speciesCode'), 
              type = 'character',
              action = 'store', 
              help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);

species <- opt$s
# functions
source('01functions.R')

# baseline file path for plots (change this if running on different system)
fp <- file.path('~', 'Library', 'CloudStorage', 'OneDrive-BowdoinCollege', 'ebird_plots')


# Wrangle data =================================================================
# load data
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
            		    year(observation_date) %in% years,
            		    species_code == species)
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
            		      year_mon = as.yearmon(observation_date)) |>
            		      group_by(year_mon, long_bin, lat_bin) |>
            		      summarize(obs_freq = sum(species_observed)/n())
print('summarized')
yms <- unique(ym_obs_freq$year_mon)

# free up some RAM
remove(subsample)


eps <- 1e-4
# Plot unprocessed data ========================================================
month_plot <- ggplot(ym_obs_freq,
                     aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+eps)))+
	            geom_raster()+
	            ggforce::facet_wrap_paginate(facets = vars(year_mon),
	                                         ncol = 6,
	                                         nrow = 4)+
	            scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
	            theme_bw()+
	            theme(legend.direction = 'horizontal',
              legend.position = 'bottom')+
              labs(fill = 'log(OF+1e-4)')

print('made raw month gg object')



save_pages(month_plot,
           path = file.path(fp, 'monthly'),
           name = paste0(species, '_month_raw.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw month plot')

remove(month_plot)


# Flat Smoothing ===============================================================
## prep and plot smoothed data -------------------------------------------------
# prep smoothed data
smoothed_df <- map(yms, ~df_to_mat(ym_obs_freq, .x)) |>
               map(flat_smooth) |>
               set_names(yms) |>
               lapply(t) |>
               lapply(as.vector) |>
               enframe(name = 'year_mon', value = 'obs_freq') |>
               unnest_longer('obs_freq') |>
               mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
                                           is.na(obs_freq) ~ NA,
                                           !is.na(obs_freq) ~ obs_freq))
n <- length(unique(smoothed_df$year_mon))
smoothed_df <- smoothed_df |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                                     lat_bin = rep(rep(1:200, times = 200), n),
                                     year_mon = as.yearmon(year_mon)
                                     )

# create ggplot object for smoothed data
month_plot <- ggplot(data = smoothed_df,
                     aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+eps)))+
      	      geom_raster()+
      	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
      	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
      	      theme_bw()+
      	      theme(legend.direction = 'horizontal',
                    legend.position = 'bottom')+
              labs(fill = 'log(OF+1e-4)')

print('made ggplot object for months')

# save smoothed plots
save_pages(month_plot,
           path = file.path(fp, 'monthly'),
           name = paste0(species, '_month_smoothed.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))

# free up some RAM
remove(month_plot)

# ## Month-on-Month comparisons ----
# mm_compare_flat <- compare_months(ym_obs_freq,
#                                   years = years,
#                                   smooth_type = 'flat')
# # 
# # plot
# save_pages_break(mm_compare_flat, type = 'flat_smoothed_mm',
#                  facets = vars(comparison),
#                  nrow = 3,
#                  ncol = 4,
#                  species = opt$s,
#                  directory = 'comparisons')
# print('saved pages for flat smoothed month-on-month comparisons')

# Geometric Smoothing ==========================================================
## prep and plot smoothed data ----

# plot smoothed data
smoothed_df <- map(yms, ~df_to_mat(ym_obs_freq, .x)) |>
               map(geom_smooth) |>
               set_names(yms) |>
               lapply(t) |>
               lapply(as.vector) |>
               enframe(name = 'year_mon', value = 'obs_freq') |>
               unnest_longer('obs_freq') |>
               mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
                                           is.na(obs_freq) ~ NA,
                                           !is.na(obs_freq) ~ obs_freq))
n <- length(unique(smoothed_df$year_mon))
smoothed_df <- smoothed_df |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                                     lat_bin = rep(rep(1:200, times = 200), n),
                                     year_mon = as.yearmon(year_mon)
                                     )

# create ggplot object for smoothed data
month_plot <- ggplot(data = smoothed_df,
                     aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+eps)))+
      	      geom_raster()+
      	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
      	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
      	      theme_bw()+
      	      theme(legend.direction = 'horizontal',
                    legend.position = 'bottom')+
              labs(fill = 'log(OF+1e-4)')

print('made ggplot object for months')

# save smoothed plots
save_pages(month_plot,
           path = file.path(fp, 'monthly'),
           name = paste0(species, '_month_geom_smoothed.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
# free up some RAM
remove(month_plot)

## Month-on-Month comparisons --------------------------------------------------
# mm_compare_geom <- compare_months(ym_obs_freq, 
#                                   years = years, 
#                                   smooth_type = 'geom')
# 
# # plot
# save_pages_break(mm_compare_geom,
#                  type = 'geom_smoothed_mm',
#                  facets = vars(comparison),
#                  nrow = 3,
#                  ncol = 4,
#                  species = opt$s,
#                  directory = 'comparisons')
# print('saved pages for geom smoothed month-on-month comparisons')

# Plot histograms of differences ===============================================
# save_pages_break(mm_compare_flat,
#                  type = 'flat_mm_hist',
#                  facets = vars(comparison),
#                  nrow = 3,
#                  ncol = 4,
#                  species = opt$s,
#                  directory = 'comparisons',
#                  plot_type = 'hist')
# 
# 
# save_pages_break(mm_compare_geom,
#                  type = 'geom_mm_hist',
#                  facets = vars(comparison),
#                  nrow = 3,
#                  ncol = 4,
#                  species = opt$s,
#                  directory = 'comparisons',
#                  plot_type = 'hist')