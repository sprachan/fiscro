# load('./processed_data/subsample.RData')
load('../data/subsample.RData')
# env_dir <- './processed_data/env_vars'
env_dir <- '../data/env_vars'

# Land Cover Data ===========
landcover <- terra::rast(file.path(env_dir, 'land_cover_final.tif'))

# reclassify to just 8 classes
classify_mat <- matrix(c(11, 10,
                         21, 20,
                         22, 20,
                         23, 20,
                         24, 20,
                         31, 30,
                         41, 40,
                         42, 40,
                         43, 40,
                         52, 50,
                         71, 70,
                         81, 80,
                         82, 80,
                         90, 90,
                         95, 90), 
                       ncol = 2,
                       byrow = TRUE) 
landcover_simple <- terra::classify(landcover, rcl = classify_mat, others = NA)
levels(landcover_simple) <- data.frame(index = c(10, 20, 30, 40, 50, 70, 80, 90), 
                                       value = c('water', 
                                                 'developed', 
                                                 'barren', 
                                                 'forest',
                                                 'shrub', 
                                                 'grassland', 
                                                 'planted_cultivated', 
                                                 'wetland'))                    
rm(landcover, classify_mat) # free RAM
nlcd_cols <- c('#4f6c9e', 
               rgb(221, 48, 33, max = 255), 
               rgb(178, 174, 165, max = 255),
               rgb(51, 100, 55, max = 255), 
               rgb(205, 187, 137, max = 255),
               rgb(237, 236, 207, max = 255),
               rgb(220, 216, 91, max = 255),
               rgb(191, 213, 235, max = 255))

terra::plot(landcover_simple, col = nlcd_cols)

# Occurrence Data =====
jan_jun_data <- dplyr::select(subsample, 
                          observation_date,
                          checklist_id, 
                          species_code,
                          observation_count,
                          species_observed,
                          latitude,
                          longitude) |>
            dplyr::filter(species_code == 'fiscro',
                          lubridate::month(observation_date) %in% c(1, 6),
                          lubridate::day(observation_date) %in% seq(1, 14)) 

# Spatial wrangling -- there's def a better way ======
jan_spat <- dplyr::filter(jan_jun_data, lubridate::month(observation_date) == 1)
jun_spat <- dplyr::filter(jan_jun_data, lubridate::month(observation_date) == 6)

sp::coordinates(jan_spat) <- ~longitude+latitude
sp::coordinates(jun_spat) <- ~longitude+latitude

lc <- terra::as.data.frame(landcover_simple, xy = TRUE) |>
      dplyr::mutate(xy = paste0(x, '_', y))

jan_spat_occ <- terra::vect(jan_spat) |> 
                terra::rasterize(landcover_simple,
                                 field = 'species_observed',
                                 fun = 'sum', na.rm = TRUE) |>
                terra::as.data.frame(xy = TRUE) |>
                dplyr::mutate(xy = paste0(x, '_', y))
colnames(jan_spat_occ)[3] <- 'obs_freq_jan'

jan_spat_list <- terra::vect(jan_spat) |> 
                 terra::rasterize(landcover_simple,
                                  field = 'species_observed',
                                  fun = 'length') |>
                terra::as.data.frame(xy = TRUE) |>
                dplyr::mutate(xy = paste0(x, '_', y))
colnames(jan_spat_list)[3] <- 'lists_jan'


jun_spat_occ <- terra::vect(jun_spat) |> 
                terra::rasterize(landcover_simple,
                                 field = 'species_observed',
                                 fun = 'sum', na.rm = TRUE) |>
                 terra::as.data.frame(xy = TRUE) |>
                 dplyr::mutate(xy = paste0(x, '_', y))
colnames(jun_spat_occ)[3] <- 'obs_freq_jun'

jun_spat_list <- terra::vect(jun_spat) |> 
                 terra::rasterize(landcover_simple,
                                  field = 'species_observed',
                                  fun = 'length') |>
                terra::as.data.frame(xy = TRUE) |>
                dplyr::mutate(xy = paste0(x, '_', y))
colnames(jun_spat_list)[3] <- 'lists_jun'

# Combine ====
df <- dplyr::full_join(jan_spat_list[,-c(1,2)], jan_spat_occ[-c(1,2)], by = 'xy') |> 
      dplyr::full_join(jun_spat_occ[-c(1,2)], by = 'xy') |> 
      dplyr::full_join(jun_spat_list[-c(1,2)], by = 'xy') |>
      dplyr::full_join(lc[-c(1,2)], by = 'xy') |> 
      dplyr::rename(lc = value) |> 
      dplyr::mutate(water = as.numeric(lc == 'water'),
                    developed = as.numeric(lc == 'developed'),
                    barren = as.numeric(lc == 'barren'),
                    forest = as.numeric(lc == 'forest'),
                    shrub = as.numeric(lc == 'shrub'),
                    grassland = as.numeric(lc == 'grassland'),
                    planted_cultivated = as.numeric(lc == 'planted_cultivated'),
                    wetland = as.numeric(lc == 'wetland'))
jan_df <- dplyr::select(df, -obs_freq_jun, -lists_jun) |> na.omit()
jun_df <- dplyr::select(df, -obs_freq_jan, -lists_jan) |> na.omit()

jan_lc <- dplyr::select(jan_df, -xy, -lists_jan, -obs_freq_jan, -lc) |>
          as.matrix(ncol = 8)

jun_lc <- dplyr::select(jun_df, -xy, -lists_jun, -obs_freq_jun, -lc) |>
  as.matrix(ncol = 8)

# Model Objects for Stan ====
jan_model_obj <- list(lc = jan_lc,
                      occ = jan_df$obs_freq_jan,
                      checklists = jan_df$lists_jan,
                      N = length(jan_df$lists_jan),
                      K = length(unique(jan_df$lc)))

jun_model_obj <- list(lc = jun_lc,
                      occ = jun_df$obs_freq_jun,
                      checklists = jun_df$lists_jun,
                      N = length(jun_df$lists_jun),
                      K = length(unique(jun_df$lc)))

# Free up RAM
rm(df ,jan_df, jan_jun_data, jan_lc, jan_spat, jan_spat_occ, jan_spat_list, 
   jun_df, jun_lc, jun_spat, jun_spat_occ, jun_spat_list, 
   landcover_simple, lc, subsample)

mean(jan_model_obj$occ/jan_model_obj$checklists)
mean(jun_model_obj$occ/jun_model_obj$checklists)

# Save these objects ====
# saveRDS(jan_model_obj, '~/Desktop/jan_model_obj.RDS')
# saveRDS(jun_model_obj, '~/Desktop/jun_model_obj.RDS')
