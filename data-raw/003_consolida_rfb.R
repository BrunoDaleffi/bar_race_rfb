library(tidyverse)

rfb_consolidada <- readr::read_rds('data-raw/rfb/rds/rfb.rds') %>%
  dplyr::mutate(ano_mes = lubridate::floor_date(dt_inicio,'month')) %>%
  dplyr::group_by(ano_mes,cnae_id,cnae_nm) %>%
  dplyr::summarise(n = dplyr::n())

readr::write_rds(rfb_consolidada,'data/rfb_consolidada.rds',compress = 'xz')