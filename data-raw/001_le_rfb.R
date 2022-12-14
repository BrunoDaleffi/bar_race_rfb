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

# Estabelecimentos --------------------------------------------------------


tab_estabelecimentos <- function(files){
  
  p <- progressr::progressor(along = files)
  
  purrr::walk(
    files,
    function(x){
      p()
      nm = stringr::str_extract(x,'[0-9]+')
      
      readr::read_csv(file = x,show_col_types = FALSE) %>%
        dplyr::transmute(
          cnpj_raiz,
          cnpj = paste0(cnpj_raiz,cnpj_comp,cnpj_dv),
          dt_inicio,
          situacao,
          dt_situacao,
          matriz,
          uf,
          cnae_principal
        ) %>%
        dplyr::distinct() %>%
        readr::write_rds(paste0('data-raw/rfb/rds/estabelecimento/',nm,'.rds'))
      
    }
  )
}


# Empresas ----------------------------------------------------------------

tab_empresas <- function(files){
  
  p <- progressr::progressor(along = files)
  
  purrr::walk(
    files,
    function(x){
      p()
      nm = stringr::str_extract(x,'[0-9]+')
      
      readr::read_csv(file = x,show_col_types = FALSE) %>%
        dplyr::transmute(
          cnpj_raiz,
          razao_social,
          natureza,
          capital_social,
          porte
        ) %>%
        dplyr::distinct() %>%
        readr::write_rds(paste0('data-raw/rfb/rds/empresa/',nm,'.rds'))
      
    }
  )
}


# Salva bases -------------------------------------------------------------

files_estabelecimentos <- fs::dir_ls('data-raw/rfb/brutos/estabelecimento/')
files_empresas <- fs::dir_ls('data-raw/rfb/brutos/empresa/')

tab_estabelecimentos(files_estabelecimentos)
tab_empresas(files_empresas)
