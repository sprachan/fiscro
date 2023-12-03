combined_zf  <- list.files(path='./processed_data', pattern='zf',
           all.files=TRUE, full.names=TRUE) |>
  purrr::map(function(x) read.csv(x)) |>
  dplyr::bind_rows()


save(combined_zf, file='./processed_data/combined_zf.RData')
