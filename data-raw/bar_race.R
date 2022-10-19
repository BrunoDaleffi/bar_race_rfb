library(tidyverse)
library(gganimate)

#' Formatação de variável numérica
nb <- function(x) scales::number_format(decimal.mark = ',',big.mark = '.')(x)

#' Tratamento da base.
#' A base original da RFB pesa mais de 9GB, aqui estamos usando uma base previamente tratada.
#' Os scripts de tratamento estão em data-raw/00*.R
base <- readr::read_rds('data/rfb_consolidada.rds') %>%
  dplyr::ungroup() %>%
  dplyr::filter(cnae_id != '8888888') %>%
  dplyr::select(-cnae_id) %>%
  # dplyr::mutate(ano_mes = lubridate::floor_date(x = ano_mes,unit = 'bimonth')) %>%
  dplyr::group_by(ano_mes,cnae_nm) %>%
  dplyr::summarise(n = sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    cnae_nm = stringr::str_wrap(string = cnae_nm,width = 45),
    ano_mes_lbl = format(ano_mes,"%b/%Y"),
    ano_mes_lbl = stringr::str_to_title(ano_mes_lbl),
    ano_mes_lbl = factor(ano_mes_lbl),
    ano_mes_lbl = forcats::fct_reorder(ano_mes_lbl,ano_mes)
  ) %>%
  dplyr::arrange(ano_mes,cnae_nm) %>%
  dplyr::group_by(cnae_nm) %>%
  dplyr::mutate(acum = purrr::accumulate(n,sum)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ano_mes) %>%
  dplyr::mutate(
    rank = rank(-acum,ties.method = 'first'),
    lbl = nb(acum)
  ) %>%
  dplyr::filter(rank <= 10) %>%
  dplyr::ungroup() 


my_theme <- ggplot2::theme_classic(
  base_family = "Times"
) +
  ggplot2::theme(axis.line=element_blank(),
                 axis.text.y=element_blank(),
                 axis.text.x=element_text(size = 14),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="none",
                 panel.background=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.grid.major.x = element_line(size=.3, color="black" ),
                 panel.grid.minor.x = element_line(size=.3, color="black" ),
                 plot.title=element_text(size=25, hjust = 0.5, face="bold.italic", color="black"),
                 plot.subtitle=element_text(size=20, hjust=0.5, face="bold", colour="black"),
                 plot.caption =element_text(size=12, color="black"),
                 plot.background=element_blank(),
                 plot.margin = margin(t = 2, r = 2, b = 2, l = 13, "cm")
  )


staticplot = ggplot2::ggplot(
  data = base, 
  mapping = ggplot2::aes(
    x = rank, 
    y = acum,
    group = cnae_nm,
    fill = as.factor(cnae_nm), 
    color = as.factor(cnae_nm)
  )
) +
  ggplot2::geom_tile(ggplot2::aes(y = acum/2,height = acum, width = 0.9), alpha = 0.9, color = NA) +
  ggplot2::geom_text(ggplot2::aes(y = 0, label = paste(cnae_nm, " ")),size = 5.2, hjust = 1.1) +
  ggplot2::geom_text(ggplot2::aes(y=acum,label = lbl, hjust=0, size = 5)) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  # ggplot2::scale_fill_manual(values = vistrnv::trnv_colors_palette(n = dplyr::n_distinct(base$cnae_nm),palette = '2greens')) +
  # ggplot2::scale_color_manual(values = vistrnv::trnv_colors_palette(n = dplyr::n_distinct(base$cnae_nm),palette = '2greens')) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = "none", fill = "none") +
  my_theme


anim = staticplot + 
  gganimate::transition_states(
    states = ano_mes_lbl, 
    transition_length = 1, 
    state_length = 0) +
  gganimate::view_follow(fixed_x = TRUE)  +
  ggplot2::labs(title = "Top 10 segmentos de mercado\n{closest_state}",
                subtitle  = "Volume acumulado de empreendimentos iniciados",
                caption  =  "Dados: Receita Federal\ngov.br")

fps = 20
tempo_overtaking = 5
nlines <- base$ano_mes %>% dplyr::n_distinct()
n_frames <- tempo_overtaking*fps*nlines

mp4 <- gganimate::animate(
  plot = anim,
  nframes = n_frames,
  fps = fps,
  duration = 120,
  width = 1200, 
  height = 800,
  start_pause = 1,
  end_pause = 1,
  renderer = gganimate::ffmpeg_renderer()
)
gganimate::anim_save("data-raw/rfb.mp4", animation = mp4)
