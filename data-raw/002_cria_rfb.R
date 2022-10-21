library(tidyverse)
library(progressr)

progressr::handlers(global = TRUE)

progressr::handlers(
  list(
    progressr::handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 120,
      complete = "+"
    )
  )
)

rfb <- function(files){
  
  p <- progressr::progressor(along = files)
  
  purrr::map_dfr(
    files,
    function(x){
      p()
      readr::read_rds(x)
    }
  )
  
}



files_empresa <- fs::dir_ls('data-raw/rfb/rds/empresa/')
files_estabelecimentos <- fs::dir_ls('data-raw/rfb/rds/estabelecimento/')
cnae <- readr::read_rds('data-raw/cnae/cnae.rds')
sit <- readr::read_rds('data-raw/rfb/rds/motivo_situacao.rds') %>%
  dplyr::rename(situacao = x00) %>%
  dplyr::mutate(situacao = as.numeric(situacao))

rfb_empresas <- rfb(files = files_empresa) %>%
  dplyr::distinct()

rfb_estabelecimentos <- rfb(files = files_estabelecimentos) %>%
  dplyr::filter(!is.na(situacao)) %>%
  dplyr::distinct()

readr::write_rds(rfb_empresas,'data-raw/rfb/rds/empresa/rfb_empresas_consolidado.rds')
readr::write_rds(rfb_estabelecimentos,'data-raw/rfb/rds/estabelecimento/rfb_estabelecimentos_consolidado.rds')

rfb <- rfb_estabelecimentos %>%
  dplyr::left_join(rfb_empresas, by = 'cnpj_raiz') %>%
  dplyr::left_join(cnae, by = c('cnae_principal' = 'cnae_id')) %>%
  dplyr::left_join(sit,'situacao') %>%
  dplyr::distinct()

readr::write_rds(rfb,'data-raw/rfb/rds/rfb.rds')