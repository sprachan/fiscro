# DESCRIPTION ------------------------------------------------------------------
#>
#> This is some more advanced EDA on spatially unbiased data to look at
#> species counts over space and time. This script plots raw data for each
#> year/month combination. It also does month-on-month comparisons of 
#> observation frequencies, taking the difference (within the same year) 
#> between the observation frequency in a given cell in consecutive months. 
#> Differences are always taken as the earlier month subtracted from the later;
#> for example, February 2022 - January 2022.
#>
# ------------------------------------------------------------------------------

# Load dependencies and parse options ==========================================
## libraries -------------------------------------------------------------------
# data manipulation tools
library(dplyr)

# plotting
library(ggplot2)

# for color-blind friendly visuals
library(viridis) 

# provides year_mon object type and related tools
library(zoo) 

# flexibility in options when called through command line, 
#> makes script play nicer with HPC
library(optparse) 

## get options -----------------------------------------------------------------
option_list <- list(make_option(c('-s', '--speciesCode'), 
                                type = 'character',
                                action = 'store', 
                                help = 'species for analysis')
)

# create a parser object
opt_parser = OptionParser(option_list = option_list);

# make a list of the arguments passed via command line
opt = parse_args(opt_parser);
species <- opt$s

## load functions and set base file path ---------------------------------------
source('01functions.R')

fp <- file.path('~', 'eBird_project', 'plots')

# Wrangle data =================================================================
# load data
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2010, 2022, by = 1)
subsample <- filter(subsample, 
            		    lubridate::year(observation_date) %in% years,
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


# Plot unprocessed data ========================================================
month_plot <- ggplot(ym_obs_freq,
                     aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+1e-2)))+
	            geom_raster()+
	            ggforce::facet_wrap_paginate(facets = vars(year_mon),
	                                         ncol = 6,
	                                         nrow = 4)+
	            scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
	            theme_bw()+
	            theme(legend.direction = 'horizontal',
              legend.position = 'bottom')+
              labs(fill = 'log(OF+1e-2)')

print('made raw month gg object')



save_pages(month_plot,
           path = file.path(fp, 'monthly', species),
           name = paste0(species, '_month_raw.pdf'),
           ncol = 6,
           nrow = 4,
           facets = vars(year_mon))
print('saved raw month plot')

remove(month_plot)


# Flat Smoothing ===============================================================
## prep and plot smoothed data -------------------------------------------------
# prep smoothed data
# smoothed_df <- purrr:::map(yms, \(x) df_to_mat(ym_obs_freq, over = x)) |>
#                purrr::map(flat_smooth) |>
#                purrr::set_names(yms) |>
#                lapply(t) |>
#                lapply(as.vector) |>
#                tibble::enframe(name = 'year_mon', value = 'obs_freq') |>
#                tidyr::unnest_longer('obs_freq') |>
#                mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
#                                            is.na(obs_freq) ~ NA,
#                                            !is.na(obs_freq) ~ obs_freq))
# n <- length(unique(smoothed_df$year_mon))
# smoothed_df <- smoothed_df |> mutate(long_bin = rep(rep(1:200, each = 200), n),
#                                      lat_bin = rep(rep(1:200, times = 200), n),
#                                      year_mon = as.yearmon(year_mon)
#                                      )
# 
# # create ggplot object for smoothed data
# month_plot <- ggplot(data = smoothed_df,
#                      aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+eps)))+
#       	      geom_raster()+
#       	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
#       	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
#       	      theme_bw()+
#       	      theme(legend.direction = 'horizontal',
#                     legend.position = 'bottom')+
#               labs(fill = 'log(OF+1e-4)')
# 
# print('made ggplot object for months')
# 
# # save smoothed plots
# save_pages(month_plot,
#            path = file.path(fp, 'monthly', species),
#            name = paste0(species, '_month_smoothed.pdf'),
#            ncol = 6,
#            nrow = 4,
#            facets = vars(year_mon))
# 
# # free up some RAM
# remove(month_plot)

## Month-on-Month comparisons ----
mm_compare_flat <- compare_months(ym_obs_freq,
                                  years = years,
                                  smooth_type = 'flat')

# plot
save_pages_break(mm_compare_flat,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_flat_mm.pdf'),
                 ncol = 3,
                 nrow = 4,
                 facets = vars(comparison),
                 plot_type = 'map')
print('saved pages for flat smoothed month-on-month comparisons')

# Geometric Smoothing ==========================================================
# plot smoothed data
# smoothed_df <- purrr::map(yms, \(x) df_to_mat(ym_obs_freq, over = x)) |>
#                purrr::map(geom_smooth) |>
#                purrr::set_names(yms) |>
#                lapply(t) |>
#                lapply(as.vector) |>
#                tibble::enframe(name = 'year_mon', value = 'obs_freq') |>
#                tidyr::unnest_longer('obs_freq') |>
#                mutate(obs_freq = case_when(is.nan(obs_freq) ~ NA,
#                                            is.na(obs_freq) ~ NA,
#                                            !is.na(obs_freq) ~ obs_freq))
# n <- length(unique(smoothed_df$year_mon))
# smoothed_df <- smoothed_df |> mutate(long_bin = rep(rep(1:200, each = 200), n),
#                                      lat_bin = rep(rep(1:200, times = 200), n),
#                                      year_mon = as.yearmon(year_mon)
#                                      )
# 
# # create ggplot object for smoothed data
# month_plot <- ggplot(data = smoothed_df,
#                      aes(x = long_bin, y = lat_bin, fill = log10(obs_freq+eps)))+
#       	      geom_raster()+
#       	      ggforce::facet_wrap_paginate(facets = vars(year_mon), ncol = 6, nrow = 4)+
#       	      scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
#       	      theme_bw()+
#       	      theme(legend.direction = 'horizontal',
#                     legend.position = 'bottom')+
#               labs(fill = 'log(OF+1e-4)')
# 
# print('made ggplot object for months')
# 
# # save smoothed plots
# save_pages(month_plot,
#            path = file.path(fp, 'monthly', species),
#            name = paste0(species, '_month_geom_smoothed.pdf'),
#            ncol = 6,
#            nrow = 4,
#            facets = vars(year_mon))
# # free up some RAM
# remove(month_plot)

## Month-on-Month comparisons --------------------------------------------------
mm_compare_geom <- compare_months(ym_obs_freq,
                                  years = years,
                                  smooth_type = 'geom')

# plot
save_pages_break(mm_compare_geom,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_geom_mm.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'map')
print('saved pages for geom smoothed month-on-month comparisons')

# Plot histograms of differences ===============================================
save_pages_break(mm_compare_flat,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_flat_mm_hist.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'hist')


save_pages_break(mm_compare_geom,
                 path = file.path(fp, 'comparisons', species),
                 name = paste0(species, '_geom_mm_hist.pdf'),
                 ncol = 4,
                 nrow = 3,
                 facets = vars(comparison),
                 plot_type = 'hist')