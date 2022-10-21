library(tidyverse)
library(gganimate)

#' Formatação de variável numérica
nb <- function(x) scales::number_format(decimal.mark = ',',big.mark = '.')(x)

rfb <- readr::read_rds('data-raw/rfb/rds/rfb.rds')

#' Tratamento da base.
#' A base original da RFB pesa mais de 9GB, aqui estamos usando uma base previamente tratada.
#' Os scripts de tratamento estão em data-raw/00*.R
base_artigo <-  readr::read_rds('data/rfb_consolidada.rds') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(atividade_politica = ifelse(cnae_id == '9492800','Atividades de organizações políticas','Outras atividades'),
                ano = lubridate::year(ano_mes)) %>%
  dplyr::filter(cnae_id != '8888888', ano >=2000, ano < 2022) %>%
  dplyr::select(-cnae_id) %>%
  dplyr::group_by(ano,atividade_politica) %>%
  dplyr::summarise(n = sum(n)) %>%
  dplyr::mutate(n = n/1e6,
                p = n/sum(n),
                p_lbl = scales::percent(p,accuracy = 1),
                p_lbl = ifelse(ano %in% c(2004,2008,2012,2016,2020) & atividade_politica == 'Atividades de organizações políticas',p_lbl,NA_character_))


# Figura 1 ----------------------------------------------------------------

base_artigo %>% 
  ggplot2::ggplot(aes(x = ano, y = n, fill = atividade_politica,label = p_lbl)) +
  ggplot2::geom_col(position = position_dodge2()) +
  ggplot2::geom_text(position = position_dodge2(1),hjust = 0.4,vjust = -0.2,size = 3.5) +
  ggplot2::labs(x = 'Ano', y = 'Volume de empresas abertas', fill = '') +
  ggplot2::scale_fill_manual(values = vistrnv::trnv_colors_palette(2)) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10),
                              labels = scales::number_format(big.mark = '.',decimal.mark = ',',suffix = ' M')) +
  ggplot2::theme_minimal(base_family = 'Times',base_size = 14) +
  ggplot2::theme(legend.position = 'bottom')



# Figura 2 ----------------------------------------------------------------
#'Este trecho da Figura 2  não é reprodutível, pois depende do pacote "montila",
#'um pacote de análise de sobrevivência desenolvido exclusivamente pela Terranova
library(montila)
library(survminer)

base_pol <- rfb %>%
  dplyr::filter(cnae_principal == '9492800',dt_inicio >= '2000-01-01') %>%
  dplyr::transmute(situacao,
                   situacao_nm = dplyr::case_when(
                     situacao == 1 ~ 'Nula',
                     situacao == 2 ~ 'Ativa',
                     situacao == 3 ~ 'Suspensa',
                     situacao == 4 ~ 'Inapta',
                     situacao == 5 ~ 'Atividade não regular',
                     situacao == 8 ~ 'Baixada'
                     
                   ),
                   tipo_evento = dplyr::if_else(situacao %in% c(1,8), 0,1),
                   data_inicio = dt_inicio,
                   ano_inicio = lubridate::year(data_inicio),
                   data_fim = dplyr::if_else(tipo_evento == 0, dt_situacao,lubridate::today()),
                   razao_social,
                   cnae_id = cnae_principal,
                   cnae_nm) %>%
  tibble::rowid_to_column('id')


# Tabela 1 ----------------------------------------------------------------

base_pol %>%
  dplyr::group_by(situacao_nm) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(p = scales::percent(n/sum(n),accuracy = 0.01))

set.seed(42)
base_tempo <- base_pol %>%
  montila::montibble(
    id = 'id',
    data_inicio = 'data_inicio',
    data_fim = 'data_fim',
    tipo_evento = 'tipo_evento'
  ) %>%
  dplyr::filter(inc_tempo == 'ok') %>%
  dplyr::sample_n(100000)

mediana = montila::fit_km(base_tempo) %>% 
  survminer::surv_median() %>%
  dplyr::pull(median)


base_tempo %>%
  montila:::compara_dists(vars = "1", dists = 'gaussian') %>% 
  dplyr::rename(tempo = t_obs) %>% 
  dplyr::select(strata,p, tempo) %>%
  ggplot2::ggplot(ggplot2::aes(x = tempo, y = p)) + 
  ggplot2::geom_line(size = 1,color = "#007A74") + 
  ggplot2::geom_point(x = mediana, y = 0.5,color = '#E17605') +
  ggplot2::geom_text(x = mediana+50, y = 0.5,label = 'Mediana: 165 dias',check_overlap = TRUE) +
  ggplot2::labs(x = "Tempo (dias)",y = "Probabilidade de sobrevivencia\n[P(T ≥ t)]",colour = "", linetype = "") + 
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(20),labels = purrr::partial(scales::percent, accuracy = 1)) + 
  ggplot2::scale_x_continuous(limits = c(0,365),
                              breaks = scales::pretty_breaks(20)) + 
  ggplot2::theme_minimal()


# Figura 3 ----------------------------------------------------------------

base_artigo %>% 
  dplyr::ungroup() %>%
  dplyr::filter(atividade_politica != 'Outras atividades') %>%
  dplyr::arrange(ano) %>%
  dplyr::mutate(acum = purrr::accumulate(n,sum)) %>%
  ggplot2::ggplot(aes(x = ano, y = acum)) +
  ggplot2::geom_col(fill = "#007A74") +
  ggplot2::geom_smooth(se = FALSE,color = "#00E6DA") +
  ggplot2::labs(x = 'Ano', y = 'Volume acumulado de empresas abertas\nno ramo de atividades políticas') +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  ggplot2::scale_y_continuous(limits = c(0,NA),
                              labels = scales::number_format(big.mark = '.',decimal.mark = ',',suffix = ' M'),
                              breaks = scales::pretty_breaks(20)) +
  ggplot2::theme_minimal(base_family = 'Times',base_size = 14)




# Tabela 2 ----------------------------------------------------------------
base_artigo %>%
  dplyr::filter(ano %in% c(2004,2008,2012,2016,2020)) %>%
  dplyr::group_by(atividade_politica) %>%
  dplyr::summarise(n = round(sum(n),1)) %>%
  dplyr::mutate(p = scales::percent(n/sum(n))) %>%
  dplyr::bind_rows(
    tibble::tibble(
      atividade_politica = 'Total',
      n = sum(.$n), 
      p = '100%'
    )
  ) %>%
  purrr::set_names(c("Atividade Econômica", "Volume de empresas abertas (Milhões","Proporção")) %>%
  knitr::kable()
