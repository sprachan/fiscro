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

  pdf(file.path(path, name), width = 11, height = 8.5)
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
  nest_check <- grepl('ym', nest_by)|grepl('comparison', nest_by)
  stopifnot('nest_by must be ym or comparison' = nest_check)
  
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
  }
}

#> DESCRIPTION: Turn a list of <n> x <n> matrices into a 2-column dataframe where
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
# Comparison functions ---------------------------------------------------------

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

#> DESCRIPTION: Smooth a df of comparisons (ie., the result of unnesting a
#> get_diff output). Outputs a vector df.
smooth_compared <- function(df_in, smooth_type){
  catch <- grepl('flat', smooth_type)|grepl('geom', smooth_type)
  stopifnot('smooth_type must be flat or geom' = catch)
  
  com <- unique(df_in$comparison)
  n <- length(com)
  
  # add in lat/long bin columns for compatibility with df_to_mat, which
  #> is needed for smoothing
  out <- df_in |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                                lat_bin = rep(rep(1:200, times = 200), n))
  
  out <- purrr::map(com, \(x) df_to_mat(out, over = x, nest_by = 'comparison')) |>
         purrr::set_names(com)
  
  if(smooth_type == 'flat'){
    out <- purrr::map(out, flat_smooth)
  }else if(smooth_type == 'geom'){
    out <- purrr::map(out, geom_smooth) 
  }
  
  out <- purrr::set_names(out, com) |>
         mats_to_vecdf(enf_name = 'comparison', enf_value = 'diff') |>
         tidyr::unnest_longer(diff)
  
  return(out)
}

#> DESCRIPTION: Given an observation frequency data frame, return a data frame
#> that has the difference in observation frequency between the same cell in
#> either the same month in consecutive years ('yy') or consecutive months in
#> the same year ('mm'). Wrapper for get_diff and smooth_compare.
compare <- function(data_in, time_type, smooth_type){
  yms <- unique(data_in$year_mon)
  out <- purrr::map(yms, \(x) df_to_mat(data_in, over = x, nest_by = 'ym')) |>
         purrr::set_names(yms) |>
         mats_to_vecdf(enf_name = 'year_mon', enf_value = 'obs_freq') |>
         get_diff(type = time_type)
        
  # make regular df with one row for each long bin/lat bin/comparison combination
  out <- tidyr::unnest_longer(out, diff)
  
  # smooth, if applicable
  if(smooth_type != 'none'){
    out <- smooth_compared(out, smooth_type = smooth_type)
  }
  
  # make comparison into a factor, making sure its ordered correctly
  out$comparison <- factor(out$comparison, 
                           levels = unique(out$comparison), 
                           ordered = TRUE)
  n <- length(unique(out$comparison))
  # add lat/long bin columns
  out <- out |> dplyr::mutate(long_bin = rep(rep(1:200, each = 200), n),
                              lat_bin = rep(rep(1:200, times = 200), n))
  return(out)
}
