library(tidyverse)
library(ggrepel)
library(ggthemr)

ggthemr(palette = 'grape')


partidos <- read_delim('datos/2019/2019-04-10/partidos.txt', delim = '\t')

datos_anfitrion <- partidos %>%
    mutate(partido_orden = str_replace_all(partido_orden, '\\(', ''),
           partido_orden = str_replace_all(partido_orden, '\\)', ''),
           partido_orden = as.numeric(partido_orden)) %>%
    select(anio, anfitrion, partido_orden, equipo_1, equipo_2, 
           goles_equipo_1 = equipo_1_final,
           goles_equipo_2 = equipo_2_final) %>%
    gather(key = 'posicion', value = 'equipo', -anio:-partido_orden, - goles_equipo_1, - goles_equipo_2) %>%
    gather(key = 'posicion_goles', value = 'goles', -anio:-partido_orden, -posicion:-equipo) %>%
    mutate(posicion_goles = str_replace_all(posicion_goles, 'goles\\_', '')) %>%
    filter(posicion == posicion_goles) %>%
    select(-contains('posicion')) %>%
    mutate(es_anfitrion = if_else(anfitrion == equipo, 'Anfitrion', 'No Anfitrion')) %>%
    filter(partido_orden < 33) %>%
    group_by(es_anfitrion, anfitrion, anio) %>%
    summarize(media_goles = mean(goles)) %>%
    mutate(texto = if_else(es_anfitrion == 'Anfitrion' & 
                               anio %in% c(1930, 1950, 1998, 2006, 2010, 2018), 
                           paste(anfitrion, anio, sep = ' ,'), 
                           ''))

anfitrion_plot <- ggplot(datos_anfitrion, aes(x = anio, y = media_goles)) +
        geom_line(aes(group = es_anfitrion, colour = es_anfitrion), size = 2) +
        geom_label_repel(aes(label = texto), segment.alpha = 0.8, 
                         show.legend = FALSE, nudge_x = 4, nudge_y = 0.20) +
        labs(x = 'Año',
             y = 'Promedio de Goles por Partido',
             colour = '',
             title = 'Los equipos anfitriones anotan más goles en la Copa Mundial',
             subtitle = 'Datos de las fases de grupo de las Copas Mundiales 1930 - 2018',
             caption = 'Preparado por Ian Flores Siaca \n #Datosdemiercoles') +
        theme_minimal(base_size = 16) 

ggsave(anfitrion_plot, filename = 'anfitrion.png', width = 14, height = 10)

