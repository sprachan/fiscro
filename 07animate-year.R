# DESCRIPTION ------------------------------------------------------------------
#>
#> This script takes the average observation frequency between years for each 
#> latitude/longitude cell for each week of the year.
#>
# ------------------------------------------------------------------------------
fp <- file.path('~', 'eBird_project', 'plots', 'monthly')
# Dependencies =================================================================
## packages ----
library(dplyr)
library(optparse) 
library(ggplot2)
library(viridis)
## functions ----
source('01functions.R')

## get options ----
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

# Load and Wrangle Data ========================================================
ym_obs_freq <- load_data(keep_date = TRUE)
dates <- seq(lubridate::make_date(2010, 1, 1), 
             lubridate::make_date(2023, 12, 31),
             by = 'day')

# get rid of leap days -- I use their data but don't make separate maps for them
dates <- dates[-which(lubridate::month(dates) == 2 & lubridate::day(dates) == 29)] |>
         format('%Y-%b-%d')

ym_obs_freq$day <- lubridate::yday(ym_obs_freq$observation_date)
ym_obs_freq$year <- lubridate::year(ym_obs_freq$observation_date)
years <- 2010:2023
store <- array(NA, dim = c(365*14, 200, 200))
dimnames(store) <- list(date = dates, NULL, NULL)

for(year_ind in 1:14){
  for(day in 1:365){
    window <- get_window(day)
    temp_df <- dplyr::filter(ym_obs_freq, year == years[year_ind], day %in% window) |>
               dplyr::group_by(long_bin, lat_bin) |>
               dplyr::summarize(obs_freq = mean(obs_freq, na.rm = TRUE)) |>
               dplyr::ungroup() |>
               tidyr::complete(long_bin = 1:200,
                               lat_bin = 1:200) |>
               dplyr::arrange(lat_bin, long_bin) 
    temp_mat <- matrix(temp_df$obs_freq, nrow = 200, ncol = 200) |>
                geom_smooth() 
    store[365*(year_ind-1)+day,,] <- log10(temp_mat+1e-3)
  }
}




# Plot =========================================================================
n <- paste0(opt$s, '_year_geom.pdf')
pdf(file = file.path(fp, n))
  purrr::map(1:5110, 
             function(x){temp <- data.frame(long_bin = rep(1:200, each = 200),
                                            lat_bin = rep(1:200, times = 200),
                                            obs_freq = as.vector(t(store[x,,])))
                         p <- ggplot(temp)+
                              geom_raster(aes(x = long_bin, 
                                              y = lat_bin, 
                                              fill = obs_freq))+
                              scale_fill_viridis(option = 'inferno', 
                                                 na.value = '#cccccc',
                                                 limits = c(-3, 1))+
                              theme_bw()+
                              theme(legend.direction = 'horizontal',
                                    legend.position = 'bottom')+
                              ggplot2::labs(fill = 'log10(OF+0.001)',
                                            x = 'Longitude',
                                            y = 'Latitude',
                                            title = dates[x])
                         print(p)
                        }
             ) 
dev.off()

