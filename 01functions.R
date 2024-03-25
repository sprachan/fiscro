# Convenience Functions ========================================================
load_data <- function(fp = './processed_data/subsample.RData',
                      years = seq(2010, 2022, by = 1),
                      keep_date = FALSE){
  load(fp)
  print('loaded')
  str(subsample)
  # filter data
  subsample <- dplyr::filter(subsample, 
                             lubridate::year(observation_date) %in% years,
                             species_code == opt$s)
  print('filtered')
  
  # process data
  if(keep_date == FALSE){
    out <- dplyr::mutate(subsample,
                         year_mon = zoo::as.yearmon(observation_date)) |>
           dplyr::group_by(year_mon, long_bin, lat_bin) |>
           dplyr::summarize(obs_freq = sum(species_observed)/dplyr::n(),
                           n_lists = dplyr::n()) |>
           dplyr::mutate(obs_freq = as.numeric(obs_freq))
  }else{
    out <- dplyr::mutate(subsample,
                         year_mon = zoo::as.yearmon(observation_date)) |>
           dplyr::group_by(observation_date, long_bin, lat_bin) |>
           dplyr::summarize(obs_freq = sum(species_observed)/dplyr::n(),
                            n_lists = dplyr::n()) |>
           dplyr::mutate(obs_freq = as.numeric(obs_freq))
  }
  print('summarized')
  return(out)
}



# Plotting Functions ===========================================================

#> DESCRIPTION: saves ggobj input to a pdf file (letter paper in landscape)
#> with file path 'path' and file name 'name.'
#> ncol, nrow, and facets are required and control facetting and layout.

save_pages <- function(ggobj, path, name, ncol, nrow, facets){
  # error catching
  name_catch <- grepl('.pdf', name)
  stopifnot('name must include .pdf' = name_catch)
  
  # function
  all_plots <- lapply(1:ggforce::n_pages(ggobj), function(j){
    p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
                                                 ncol = ncol,
                                                 nrow = nrow,
                                                 page = j)
    return(p_save)
  }
  )
  
  pdf(file.path(path, name), width = 11, height = 8.5)
  lapply(all_plots, print)
  dev.off()
}

#> DESCRIPTION: special version of save_pages that is designed to work with
#> month-on-month comparisons. This breaks the comparisons up by year; ie.,
#> one page has a full set of monthly comparisons for a year and none from any
#> other years. 

save_pages_break <- function(data_in, path, name, ncol, nrow, facets, plot_type = 'map'){
  # error catching
  name_catch <- grepl('.pdf', name)
  stopifnot('name must include .pdf' = name_catch)
  
  type_catch <- grepl('map', plot_type)|grepl('hist', plot_type)
  stopifnot('plot_type must be map or hist' = type_catch)
  
  years <- unique(lubridate::year(data_in$year_mon))
  p_save <- list()
  if(plot_type == 'map'){
    for(j in 1:length(years)){
      p_save[[j]] <- dplyr::filter(data_in, lubridate::year(year_mon) == years[j]) |>
                     ggplot2::ggplot()+
                     ggplot2::geom_raster(ggplot2::aes(x = long_bin, 
                                                       y = lat_bin, 
                                                       fill = transform_diff))+
                     ggplot2::facet_wrap(facets = facets, ncol = ncol, nrow = nrow)+
                     ggplot2::scale_fill_distiller(palette = 'RdBu', 
                                                   direction = -1, 
                                                   na.value = '#cccccc')+
                     ggplot2::theme_bw()+
                     ggplot2::theme(legend.direction = 'horizontal',
                                    legend.position = 'bottom')
    }
  }else if(plot_type == 'hist'){
    for(j in 1:length(years)){
      p_save[[j]] <- dplyr::filter(data_in, 
                                   lubridate::year(year_mon) == years[j],
                                   diff != 0) |>
                     ggplot2::ggplot()+
                     ggplot2::geom_histogram(ggplot2::aes(x = transform_diff), 
                                             bins = 200)+
                     ggplot2::facet_wrap(facets = facets, 
                                         ncol = ncol, 
                                         nrow = nrow)+
                     ggplot2::theme_bw()
    }
  }
  
  pdf(file.path(path, name), width = 11, height = 8.5)
  lapply(p_save, print)
  dev.off()
}

#> DESCRIPTION: Given a cutoff value, create a map in ggplot where a cell is 
#> black if the observation frequency is ABOVE that value and white if
#> the observation frequency is BELOW the value.

cutoff_plot <- function(data_in, cutoff, title, log = FALSE, epsilon = NULL){
  legend_lab <- paste0('OF over ', cutoff)
  if(log == TRUE){
    temp <- data_in |> dplyr::mutate(obs_freq = log10(obs_freq + epsilon))
  }else{
    temp <- data_in
  }
  p <- temp|> dplyr::mutate(over = dplyr::case_when(obs_freq < cutoff ~ 'No',
                                                        obs_freq >= cutoff ~ 'Yes'),
                                over = ordered(over, levels = c('No', 'Yes'))) |>
    ggplot2::ggplot(ggplot2::aes(x = long_bin, y = lat_bin, fill = over))+
    ggplot2::geom_raster()+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#555555'),
                   legend.key = ggplot2::element_rect(color = "black"),
                   legend.position = 'bottom')+
    ggplot2::scale_fill_manual(values = c('black', 'white'))+
    ggplot2::labs(fill = legend_lab,
                  title = title,
                  x = 'Longitude',
                  y = 'Latitude')
  return(p)
}

#> DESCRIPTION: Given a dataframe of observation frequencies with 
#> associated long and lat bins, make a ggplot object for mapping.
#> if plot_facet is FALSE, each plot goes on its own page in the pdf.
#> In this case, each page reflects data given by keep, a vector of boolean
#> values.

map_uncompared <- function(data_in, epsilon, nrow = 4, ncol = 6, plot_facet = TRUE, keep = NULL, over = ''){
  if(plot_facet == TRUE){
    p <- dplyr::arrange(data_in, year_mon) |>
      ggplot(aes(x = long_bin, 
                 y = lat_bin,
                 fill = log10(obs_freq+epsilon)))+
      geom_raster()+
      ggforce::facet_wrap_paginate(facets = vars(year_mon),
                                   nrow = nrow,
                                   ncol = ncol)+
      viridis::scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.direction = 'horizontal',
                     legend.position = 'bottom')+
      ggplot2::labs(fill = paste0('log10(OF+', epsilon, ')'),
                    x = 'Longitude',
                    y = 'Latitude')
  }else{
    p <- data_in[keep,] |>
         ggplot2::ggplot(ggplot2::aes(x = long_bin,
                                     y = lat_bin,
                                     fill = log10(obs_freq+epsilon)))+
         ggplot2::geom_raster()+
         viridis::scale_fill_viridis(option = 'inferno', na.value = '#cccccc')+
         ggplot2::theme_bw()+
         ggplot2::theme(legend.position = 'bottom')+
         ggplot2::labs(fill = paste0('log10(OF+', epsilon, ')'),
                       title = over,
                       x = 'Longitude',
                       y = 'Latitude')
  }
  return(p)
}

#> DESCRIPTION: Given a dataframe of observation frequencies with 
#> associated long and lat bins, make a ggplot object for histograms.

hist_uncompared <- function(data_in, epsilon, nrow = 4, ncol = 6){
  p <- dplyr::arrange(data_in, year_mon) |>
       ggplot2::ggplot(ggplot2::aes(log10(obs_freq+epsilon)))+
       ggplot2::geom_histogram(bins = 100)+
       ggforce::facet_wrap_paginate(facets = ggplot2::vars(year_mon),
                                            nrow = nrow,
                                            ncol = ncol)+
       ggplot2::theme_bw()
  return(p)
}

#> DESCRIPTION: Given a dataframe of differences in observation frequencies with 
#> associated long and lat bins, make a ggplot object for mapping.
map_compared <- function(data_in, use_facets = TRUE, nrow = 3, ncol = 4, over = ''){
  if(use_facets == TRUE){
    p <- ggplot2::ggplot(data_in, 
                         ggplot2::aes(x = long_bin, 
                                      y = lat_bin,
                                      fill = transform_diff))+
      ggplot2::geom_raster()+
      ggforce::facet_wrap_paginate(facets = ggplot2::vars(comparison),
                                   nrow = nrow,
                                   ncol = ncol)+
      ggplot2::scale_fill_distiller(palette = 'RdBu', 
                                    direction = -1,
                                    na.value = '#cccccc')+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.direction = 'horizontal',
                     legend.position = 'bottom')+
      ggplot2::labs(fill = ' sign sqrt-transformed diff')
  }else{
    title_clean <- paste(substr(over, 1, 6), 'vs', substr(over, 8, 13))
    p <- dplyr::filter(data_in, comparison == over) |>
         ggplot2::ggplot(ggplot2::aes(x = long_bin, 
                                      y = lat_bin, 
                                      fill = transform_diff))+
         ggplot2::geom_raster()+
         ggplot2::scale_fill_distiller(palette = 'RdBu',
                                       direction = -1,
                                       na.value = '#cccccc',
                                       limits = c(-1, 1))+
         ggplot2::theme_bw()+
         ggplot2::theme(legend.direction = 'horizontal',
                        legend.position = 'bottom')+
         ggplot2::labs(title = title_clean,
                       x = 'Longitude',
                       y = 'Latitude',
                       fill = 'Square root transformed difference')
  }

  
  return(p)
}

#> DESCRIPTION: Given a dataframe of differences in observation frequencies with 
#> associated long and lat bins, make a ggplot object for histograms.

hist_compared <- function(data_in, nrow = 3, ncol = 4){
  p <- dplyr::filter(data_in, diff != 0) |>
       ggplot2::ggplot(ggplot2::aes(diff))+
       ggplot2::geom_histogram(bins = 150)+
       ggforce::facet_wrap_paginate(facets = ggplot2::vars(comparison),
                                    nrow = nrow,
                                    ncol = ncol)+
       ggplot2::theme_bw()
  return(p)
}
# Data Formatting Functions ====================================================

#> DESCRIPTION: Given a data set and a vector of breaks from kde2d, assign the
#> checklist data a longitude bin (and a latitude bin) according to the breaks.

get_bin <- function(data_set, breaks){
  # vector that will give the breaks above our long/lat
  upper <- which(data_set < breaks)
  # vector that will give the breaks below our long/lat
  lower <- which(data_set > breaks)
  
  # edge case: above the highest break
  if(length(upper) == 0){
    if(length(lower) == 0){
      # shouldn't be possible to be outside of the upper AND lower bound
      return(NA)
    }else{
      # give index of the max lower bound if there are no breaks above the value
      return(max(lower))
    }
    # edge case: below the lowest break
  }else if(length(lower) == 0){
    # give the index of the min upper bound 
    return(min(upper))
    # leverage the fact that if min(upper) - x < x - max(lower), we are closer
    #> to the upper bound! Arbitrarily, if we are exactly in the middle, take
    #> the upper bound
  }else if(min(upper)+max(lower) <= 2*data_set){
    # return index of the min upper bound
    return(min(upper))
  }else{
    # return index of the max lower bound
    return(max(lower))
  }
}

#> DESCRIPTION: Turn a data frame into a <n> x <n> matrix for easy use in lapply
#> and map functions. <over> is what the lapply/map is done "over", typically
#> the year-month combinations. <nest_by> gives what we should complete the
#> cells by -- that is, should each unique year-month have a full
#> set of cells ('ym'), or should each unique comparison have a full set of cells
#> ('comparison')?

df_to_mat <- function(df, over, nest_by = 'ym', n = 200){
  # error catching
  nest_check <- nest_by %in% c('ym', 'comparison', 'week')
  stopifnot('nest_by must be ym, comparison, or week' = nest_check)
  
  # function
  out <- dplyr::ungroup(df)
  if(nest_by == 'ym'){
    out <- tidyr::complete(out, 
                           tidyr::nesting(year_mon),
                           long_bin = 1:n,
                           lat_bin = 1:n) |>
      dplyr::filter(year_mon == over) |>
      dplyr::arrange(long_bin, lat_bin)
    out <- matrix(out$obs_freq, nrow = n, ncol = n, byrow = TRUE)
    return(out)
  }else if(nest_by == 'comparison'){
    out <- tidyr::complete(out, 
                           tidyr::nesting(comparison),
                           long_bin = 1:n,
                           lat_bin = 1:n) |>		     
      dplyr::filter(comparison == over) |>
      dplyr::arrange(long_bin, lat_bin) 
    out <- matrix(out$diff, nrow = n, ncol = n, byrow = TRUE)
    return(out)
  }else if(nest_by == 'week'){
    out <- tidyr::expand(out,
                         tidyr::nesting(year),
                         week = 1:52,
                         long_bin = 1:n,
                         lat_bin = 1:n) |>
           dplyr::full_join(df) |>
           dplyr::filter(week == over) |>
           dplyr::arrange(year, lat_bin, long_bin)
    out <- array(out$obs_freq, dim = c(200, 200, 14))
    return(out)
  }
}

#> DESCRIPTION: Turn a LIST of <n> x <n> matrices into a 2-column dataframe where
#> one column ("enf_name", a string) is the "category" (ie., year_mon or 
#> comparison) and the other column ("enf_value", a string) is a vector of 
#> values (observation frequencies or differences in observation frequencies).
#> enf_name should be either year_mon or comparison
#> enf_value should be obs_freq or diff

mats_to_vecdf <- function(matrix_list, enf_name, enf_value){
  out <- lapply(matrix_list, t) |>
    lapply(as.vector) |>
    tibble::enframe(name = enf_name, value = enf_value)
  return(out)
}

#> DESCRIPTION: Get the window of days from day x to 7 days after day x. This
#> "wraps around" to the beginning of the next year, if needed: for example, 
#> inputting 365 and a non-leap year will return (365, 1, 2, 3, 4, 5, 6). 

get_window <- function(day, year){
  if(366-day >= 7){
    window <- seq(day, day+7)
  }else if(day != 366){
    x <- seq(day, 365)
    y <- seq(1, 7-length(x))
    window <- c(x, y)
  }else{
    window <- c(366, seq(1, 6))
  }
  return(window)
}

# designed to be iterated over day
df_to_slide_mat <- function(df, day){
  window <- get_window(day)
  temp <- dplyr::filter(df, day %in% window) |>
          dplyr::ungroup() |>
          dplyr::group_by(long_bin, lat_bin, year) |>
          dplyr::summarize(avg_obs_freq = mean(obs_freq, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          tidyr::complete(tidyr::nesting(year),
                          long_bin = 1:200,
                          lat_bin = 1:200) |>
          dplyr::arrange(year, lat_bin, long_bin)
    arr <- array(temp$avg_obs_freq, dim = c(200, 200, length(unique(df$year))))
    out <- apply(X = arr, MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
    return(out)
}

# Smoothing Functions ==========================================================

#> DESCRIPTION: Smooth a matrix of data. For a cell x, the new "smoothed" value
#> of x is given by taking the weighted average of x (weight given by <w>) and
#> its non-diagonal neighbors within <scope> cells. If x is NA, do a regular average 
#> of the non-diagonal neighbors (otherwise the weighted average will artificially
#> lower the observation frequency).

geom_smooth <- function(matrix_in, w = 0.75, scope = 1){
  N <- length(matrix_in[1,])
  out <- matrix(NA, nrow = N, ncol = N)
  for(j in 1:N){
    for(k in 1:N){
      temp <- matrix_in
      a <- j-scope
      b <- j+scope
      temp[j, k] <- NA
      valid_rows <- a:b
      valid_rows <- valid_rows[a:b >= 1 & a:b <= N]
      
      a <- k-scope
      b <- k+scope
      valid_cols <- a:b
      valid_cols <- valid_cols[a:b >= 1 & a:b <= N]
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

#> DESCRIPTION: Smooth a matrix of data. For a cell x, the new "smoothed" value
#> of x is given by taking the average of x and its non-diagonal neighbors within
#> <scope> cells.

flat_smooth <- function(matrix_in, scope = 1){
  # assumes a square matrix (which we have)
  N <- length(matrix_in[1,]) # should be 200
  out <- matrix(NA, nrow = N, ncol = N)
  
  for(j in 1:N){
    for(k in 1:N){
      a <- j-scope
      b <- j+scope
      valid_rows <- a:b
      valid_rows <- valid_rows[a:b >= 1 & a:b <= N]
      
      a <- k-scope
      b <- k+scope
      valid_cols <- a:b
      valid_cols <- valid_cols[a:b >= 1 & a:b <= N]
      
      out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
    }
  }
  return(out)
}

#> DESCRIPTION: Given a dataframe and a smooth type, return a smoothed df for
#> each year_mon present in the data. Wrapper for geom_smooth and/or flat_smooth

df_smoother <- function(df, df_type, smooth_type){
  # check arguments
  df_catch <- df_type %in% c('raw', 'comp')
  stopifnot('df_type must be raw or comp' = df_catch)
  
  smooth_catch <- smooth_type %in% c('flat', 'geom', 'none')
  stopifnot('smooth_type must be flat or geom' = smooth_catch)
  
  if(df_type == 'raw'){
    iter <- unique(df$year_mon)
    nest_by <- 'ym'
    enf_name <- 'year_mon'
    enf_value <- 'obs_freq'
  }else if(df_type == 'comp'){
    iter <- unique(df$comparison)
    nest_by <- 'comparison'
    enf_name <- 'comparison'
    enf_value <- 'diff'
  }
  
  mats <- purrr::map(iter, \(x) df_to_mat(df, over = x, nest_by = nest_by)) |>
          purrr::set_names(iter)
  
  if(smooth_type == 'flat'){
    smooth_mats <- purrr::map(mats, flat_smooth)
  }else if(smooth_type == 'geom'){
    smooth_mats <- purrr::map(mats, geom_smooth)
  }
  
  rm(mats) # free up some RAM
  
  vec_df <- purrr::set_names(smooth_mats, iter) |>
            mats_to_vecdf(enf_name = enf_name, enf_value = enf_value)
  
  rm(smooth_mats) # free up RAM
  
  if(df_type == 'raw'){
    out <- tidyr::unnest_longer(vec_df, obs_freq)
    n <- length(unique(df$year_mon))
  }else if(df_type == 'comp'){
    out <- tidyr::unnest_longer(vec_df, diff)
    n <- length(unique(df$comparison))
  }
  
  out <- dplyr::mutate(out,
                       long_bin = rep(rep(1:200, each = 200), n),
                       lat_bin = rep(rep(1:200, times = 200), n))
  return(out)
}

# Comparison functions =========================================================

#> DESCRIPTION: Given a vector df (obs_freq is a vector of values for the given
#> year_mon), produce a df with every relevant combination of year_mons where
#> values are the difference in obs_freq from the two time periods (late-early).

get_diff <- function(vecs_in, type){
  # error catching
  catch <- grepl('yy', type)|grepl('mm', type)
  stopifnot('type must be mm or yy' = catch)
  
  # get all possible combinations of year_mons
  out <- vecs_in |> tidyr::expand(tidyr::nesting(year_mon = zoo::as.yearmon(year_mon),
                                                 obs_freq = obs_freq),
                                  tidyr::nesting(year_mon2 = year_mon,
                                                 obs_freq2 = obs_freq))
  
  # filter to just relevant combinations
  if(type == 'yy'){
    # year on year comparisons: take consecutive years for the same month
    out <- dplyr::filter(out,
                         lubridate::year(year_mon) == lubridate::year(year_mon2)-1,
                         lubridate::month(year_mon) == lubridate::month(year_mon2))
  }else if(type == 'mm'){
    # month on month comparisons: take consecutive months in the same year
    out <- dplyr::filter(out,
                         lubridate::year(year_mon) == lubridate::year(year_mon2),
                         lubridate::month(year_mon) == lubridate::month(year_mon2)-1)
  }
  
  # make a comparison column, take the differences; drop unneeded columns
  #> left with a df with the comparison in one column and the associated 
  #> difference in another.
  out <- dplyr::mutate(out,
                       comparison = paste(year_mon, year_mon2, sep = '_'),
                       diff = purrr::map2(obs_freq2, obs_freq, `-`)
  ) |>
    dplyr::select(-year_mon, -year_mon2, -obs_freq, -obs_freq2) 
  
  return(out)
}

#> DESCRIPTION: Given an observation frequency data frame, return a data frame
#> that has the difference in observation frequency between the same cell in
#> either the same month in consecutive years ('yy') or consecutive months in
#> the same year ('mm'). Wrapper for get_diff and df_smoother.

compare <- function(data_in, time_type, smooth_type){
  yms <- unique(data_in$year_mon)
  diff_vecs <- purrr::map(yms, \(x) df_to_mat(data_in, over = x, nest_by = 'ym')) |>
               purrr::set_names(yms) |>
               mats_to_vecdf(enf_name = 'year_mon', enf_value = 'obs_freq') |>
               get_diff(type = time_type)
  
  # make regular df with one row for each long bin/lat bin/comparison combination
  diff_df <- tidyr::unnest_longer(diff_vecs, diff)
  rm(diff_vecs) # free up RAM
  
  # smooth, if applicable
  if(smooth_type != 'none'){
      n <- length(unique(diff_df$comparison))
      diff_df <- dplyr::mutate(diff_df,
                               long_bin = rep(rep(1:200, each = 200), n),
                               lat_bin = rep(rep(1:200, times = 200), n)) |>
                  df_smoother(df_type = 'comp', 
                              smooth_type = smooth_type)
  }
  
  # make comparison into a factor, making sure its ordered correctly
  diff_df$comparison <- factor(diff_df$comparison, 
                               levels = unique(diff_df$comparison), 
                               ordered = TRUE)
  n <- length(unique(diff_df$comparison))
  
  # add lat/long bin columns, year_mon column, and transform_diff column
  out <- diff_df |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                                  lat_bin = rep(rep(1:200, times = 200), n),
                                  year_mon = zoo::as.yearmon(substr(comparison, 1, 8)),
                                  transform_diff = dplyr::case_when(diff < 0 ~ -sqrt(abs(diff)),
                                                                    diff == 0 ~ 0,
                                                                    diff > 0 ~ sqrt(abs(diff))))

  return(out)
}