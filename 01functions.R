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
  
  # function
  years <- unique(lubridate::year(data_in$year_mon))
  p_save <- list()
  if(plot_type == 'map'){
    for(j in 1:length(years)){
      p_save[[j]] <- dplyr::filter(data_in, lubridate::year(year_mon) == years[j]) |>
        ggplot2::ggplot()+
        ggplot2::geom_raster(ggplot2::aes(x = long_bin, 
                                          y = lat_bin, 
                                          fill = diff_log))+
        ggplot2::facet_wrap(facets = facets, ncol = ncol, nrow = nrow)+
        ggplot2::scale_fill_distiller(palette = 'RdBu', 
                                      direction = -1, 
                                      na.value = '#cccccc')+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.direction = 'horizontal',
                       legend.position = 'bottom')
    }
  }else if(plot_type == 'hist'){
    temp <- data_in |>
            dplyr::mutate(diff_log = dplyr::case_when(diff_log == 0 ~ NA,
                                                      .default = diff_log))
    for(j in 1:length(years)){
      p_save[[j]] <- dplyr::filter(temp, lubridate::year(year_mon) == years[j]) |>
                     ggplot2::ggplot()+
                     ggplot2::geom_histogram(ggplot2::aes(x = diff_log), 
                                              bins = 200)+
                     ggplot2::facet_wrap(facets = facets, 
                                         ncol = ncol, 
                                         nrow = nrow)+
                     ggplot2::theme_bw()
    }
  }

  pdf(paste0(path, name), width = 11, height = 8.5)
  lapply(p_save, print)
  dev.off()
}

#> DESCRIPTION: Given a cutoff value, create a map in ggplot where a cell is 
#> black if the observation frequency is ABOVE that value and white if
#> the observation frequency is BELOW the value.

cutoff_plot <- function(data_in, cutoff, title){
  legend_lab <- paste0('OF over ', title)
  p <- data_in |> dplyr::mutate(over = dplyr::case_when(obs_freq < cutoff ~ 'No',
                                                   obs_freq >= cutoff ~ 'Yes'),
                                over = ordered(over, levels = c('No', 'Yes'))) |>
                   ggplot2::ggplot(ggplot2::aes(x = long_bin, y = lat_bin, fill = over))+
                   ggplot2::geom_raster()+
                   ggplot2::theme_bw()+
                   ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#555555'),
                                  legend.key = ggplot2::element_rect(color = "black"))+
                   ggplot2::scale_fill_manual(values = c('white', 'black'))+
                   ggplot2::labs(fill = legend_lab)
  return(p)
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
      # this should give NA because it shouldn't be possible to be both
      #> outside of the upper AND lower bound
      return(NA)
    }else{
      # just give index of the max lower bound
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
  nest_check <- grepl('ym', nest_by)|grepl('comparison', nest_by)
  stopifnot('nest_by must be ym or comparison' = nest_check)
  
  # function
  temp <- dplyr::ungroup(df)
  if(nest_by == 'ym'){
    temp <- tidyr::complete(temp, 
                            tidyr::nesting(year_mon),
                            long_bin = 1:n,
                            lat_bin = 1:n) |>
            dplyr::filter(year_mon == over) |>
            dplyr::arrange(long_bin, lat_bin)
    temp <- matrix(temp$obs_freq, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }else if(nest_by == 'comparison'){
    temp <- tidyr::complete(temp, 
                            tidyr::nesting(comparison),
                            long_bin = 1:n,
                            lat_bin = 1:n) |>		     
            dplyr::filter(comparison == over) |>
            dplyr::arrange(long_bin, lat_bin) 
    #transform_mat <- matrix(temp$transform_diff, nrow = n, ncol = n, byrow = TRUE)
    temp <- matrix(temp$diff_log, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }
}

# Comparison functions ---------------------------------------------------------

#> DESCRIPTION: Given an observation frequency data frame, calculate the 
#> difference in observation frequency between the same bin in different 
#> years. Bins are compared between the same month of consecutive years.

compare_years <- function(data_in, smooth_type, epsilon = 1e-2){
  epsilon <- as.numeric(epsilon)
  # error catching
  type_catch <- grepl('flat', smooth_type)|grepl('geom', smooth_type)
  stopifnot('smooth_type must be flat or geom' = type_catch)
  
  # function
  yms <- unique(data_in$year_mon)
  # make a data frame that has transformed differences
  temp <- purrr::map(yms, \(x) df_to_mat(data_in, over = x, nest_by = 'ym')) |> 
          purrr::set_names(yms) |> 
          lapply(t) |>
          lapply(as.vector) |> 
          tibble::enframe(name = 'year_mon', value = 'obs_freq') |>
          tidyr::expand(tidyr::nesting(year_mon = zoo::as.yearmon(year_mon),
                                       log_of = lapply(obs_freq, \(x) log10(x + epsilon))),
                        tidyr::nesting(year_mon2 = zoo::as.yearmon(year_mon),
                                       log_of2 = log_of)) |>
          dplyr::filter(lubridate::year(year_mon) == lubridate::year(year_mon2)-1,
                        lubridate::month(year_mon) == lubridate::month(year_mon2)) |>
          dplyr::mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
                        diff_log = purrr::map2(log_of2, log_of, `-`)) |>
          dplyr::select(-log_of, -log_of2, -year_mon, -year_mon2) |>
          tidyr::unnest_longer(diff_log) |>
          dplyr::mutate(diff_log = dplyr::case_when(is.nan(diff_log) ~ NA,
                                                    !is.nan(diff_log) ~ diff_log)
                       )
  
  n <- length(unique(temp$comparison))
  # add lat_bin, long_bin columns
  temp <- temp |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                                lat_bin = rep(rep(1:200, times = 200), n))  
  
  com <- unique(temp$comparison)
  y <- purrr::map(com, \(x) df_to_mat(temp, over = x, nest_by = 'comparison')) |>
       purrr::set_names(com)
  
  if(smooth_type == 'flat'){
    y <- purrr::map(y, flat_smooth)
  }else if(smooth_type == 'geom'){
    y <- purrr::map(y, geom_smooth) 
  }
  
  y <- purrr::set_names(y, com) |>
       lapply(t) |>
       lapply(as.vector) |>
       tibble::enframe(name = 'comparison', value = 'diff_log') |>
       tidyr::unnest_longer(diff_log)
  
  n <- length(unique(y$comparison))
  # add lat_bin, long_bin columns
  y <- y |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                          lat_bin = rep(rep(1:200, times = 200), n),
                          year_mon = zoo::as.yearmon(substring(comparison, 1, 8)))|> 
            dplyr::arrange(lubridate::year(year_mon), lubridate::month(year_mon))
  
  # make comparison into a factor, making sure its ordered correctly
  y$comparison <- factor(y$comparison, 
                         levels = unique(y$comparison), 
                         ordered = TRUE)
  return(y)
}

#> DESCRIPTION: Given an observation frequency data frame and a set of years, 
#> calculate the difference in observation frequency between the same bin in 
#> different years. Bins are compared between consecutive months of the same 
#> year.

compare_months <- function(data_in, years, smooth_type, epsilon = 1e-2){
  # error catching
  type_catch <- grepl('flat', smooth_type)|grepl('geom', smooth_type)
  stopifnot('smooth_type must be flat or geom' = type_catch)
  
  # function
  yms <- unique(data_in$year_mon)
  
  # make a data frame that has transformed differences
  temp <- purrr::map(yms, \(x) df_to_mat(data_in, over = x, nest_by = 'ym')) |> 
          purrr::set_names(yms) |> 
          lapply(t) |>
          lapply(as.vector) |> 
          tibble::enframe(name = 'year_mon', value = 'obs_freq') |>
          tidyr::expand(tidyr::nesting(year_mon = zoo::as.yearmon(year_mon),
                                       log_of = lapply(obs_freq, \(x) log10(x + epsilon))),
                        tidyr::nesting(year_mon2 = zoo::as.yearmon(year_mon),
                                       log_of2 = log_of)) |>
          dplyr::filter(lubridate::year(year_mon) %in% years,
                        lubridate::year(year_mon) == lubridate::year(year_mon2),
                        lubridate::month(year_mon) == lubridate::month(year_mon2)-1) |>
          dplyr::mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
                        diff_log = purrr::map2(log_of2, log_of, `-`)) |>
          dplyr::select(-log_of, -log_of2, -year_mon, -year_mon2) |>
          tidyr::unnest_longer(diff_log)
  
  n <- length(unique(temp$comparison))
  # add lat_bin, long_bin columns
  temp <- temp |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                                lat_bin = rep(rep(1:200, times = 200), n))  
  
  com <- unique(temp$comparison)
  
  y <- purrr::map(com, \(x) df_to_mat(temp, over = x, nest_by = 'comparison')) |>
       purrr::set_names(com)
  if(smooth_type == 'flat'){
    y <- purrr::map(y, flat_smooth)
  }else if(smooth_type == 'geom'){
    y <- purrr::map(y, geom_smooth) 
  }else{
    stop('Need valid smooth type, either flat or geom')
  }
  
  y <- purrr::set_names(y, com) |>
       lapply(t) |>
       lapply(as.vector) |>
       tibble::enframe(name = 'comparison', value = 'diff_log') |>
       tidyr::unnest_longer(diff_log) |>
       dplyr::mutate(diff_log = dplyr::case_when(is.nan(diff_log) ~ NA,
                                                 !is.nan(diff_log) ~ diff_log)
                    )
  
  n <- length(unique(y$comparison))
  
  # add lat_bin, long_bin columns
  y <- y |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                          lat_bin = rep(rep(1:200, times = 200), n),
                          year_mon = zoo::as.yearmon(substring(comparison, 1, 8)))|> 
            dplyr::arrange(lubridate::year(year_mon), lubridate::month(year_mon))
  
  # make comparison into a factor, making sure its ordered correctly
  y$comparison <- factor(y$comparison, 
                         levels = unique(y$comparison), 
                         ordered = TRUE)
  return(y)
}

