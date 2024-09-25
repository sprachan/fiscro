data_dir <- file.path('.', 'processed_data')

# save 
list.files(path = data_dir, 
           pattern = 'zf',
           all.files = TRUE, 
           full.names = TRUE) |>
as.list() |>
lapply(read.csv)
dplyr::bind_rows() |>
arrow::write_dataset(file.path(data_dir, 'species'),
                     format = 'parquet',
                     partitioning = c('species_code'))
