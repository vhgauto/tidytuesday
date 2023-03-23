# paquetes ----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(here)
library(ggtext)
library(glue)
library(fontawesome)

# fuentes -----------------------------------------------------------------

font_add_google(name = "Share Tech Mono", family = "share") # nombres de programas
font_add_google(name = "Heebo", family = "heebo") # resto del texto
font_add_google(name = "Roboto Mono", family = "roboto") # título

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", here("icon/Font Awesome 5 Free-Regular-400.otf"))
font_add("fa-brands", here("icon/Font Awesome 5 Brands-Regular-400.otf"))
font_add("fa-solid", here("icon/Font Awesome 5 Free-Solid-900.otf"))

# Sys.getenv("GITHUB_PAT")
# Sys.unsetenv("GITHUB_PAT")

# funciones ---------------------------------------------------------------

# función p/agregar el punto de las unidades de mil
f_punto <- scales::label_number(big.mark = ".", decimal.mark = ",")

# función p/escribir en mono, en blanco
f_code <- function(x) {
  glue("<span style='font-family:share; color:white'>{x}</span>")
}

# datos -------------------------------------------------------------------

# browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-03-21/readme.md")
datos_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

glimpse(datos_raw)

datos_raw |> 
  drop_na(line_comment_token) |> 
  count(line_comment_token, sort = TRUE)

# selecciono y renombro
datos_saved <- datos_raw |> 
  select(title, line_comment_token, wikipedia_daily_page_views, appeared) |> 
  drop_na()

# escribo los datos y leo
write_tsv(datos_saved, here("2023/semana 12/datos_saved.tsv"))
datos_saved <- read_tsv(here("2023/semana 12/datos_saved.tsv"))

# datos p/la figura
datos <- datos_saved |> 
  group_by(line_comment_token) |> 
  slice_max(wikipedia_daily_page_views, n = 10) |> 
  mutate(suma = sum(wikipedia_daily_page_views)) |> 
  arrange(desc(suma), desc(wikipedia_daily_page_views)) |> 
  ungroup() |> 
  slice(1:50) |> 
  rename(nombre = title, comentario = line_comment_token, 
         visitas = wikipedia_daily_page_views, año = appeared) |> 
  mutate(nombre = fct_inorder(nombre)) |> 
  mutate(nombre = fct_rev(nombre)) |> 
  mutate(comentario = fct_reorder(comentario, -suma)) |> 
  mutate(decada = año - (año %% 10)) |> 
  mutate(decada = factor(decada)) |> 
  mutate(suma = f_punto(x = suma)) |> 
  mutate(comentario_label = glue("<span style='font-size:25pt;background-color:#003354'>{comentario}<br></span><br><span style='font-size:9pt;'>{suma}<br>visitas<br>totales</span>")) |> 
  mutate(comentario_label = fct_inorder(comentario_label))

# círculo que encierra el signo del comentario
circulo <- datos |> 
  distinct(comentario_label) |> 
  mutate(x = 7600,
         y = 5)

# lenguaje más nuevo/viejo
min_max <- bind_rows(datos |> slice_max(año, n = 1),
                     datos |> slice_min(año, n = 1))

# figura ------------------------------------------------------------------

# caption
icon_twitter <- "<span style='font-family:fa-brands; color:white;'>&#xf099;</span>"
icon_github <- "<span style='font-family:fa-brands; color:white;'>&#xf09b;</span>"
fuente <- "<span style='color:white;'>Datos:</span> <span style='color:#003354;'><span style='font-family:share;'>{**tidytuesdayR**}</span> semana 12</span>"
autor <- "<span style='color:white;'>Autor:</span> <span style='color:#003354;'>**Víctor Gauto**</span>"
sep <- glue("<span style = 'color:#a4cac8;'>**|**</span>")
usuario <- glue("<span style = 'color:#003354;'>**vhgauto**</span>")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# plot
g1 <- ggplot(data = datos, aes(x = visitas, y = nombre)) +
  # segmentos
  geom_segment(aes(x = 0, xend = visitas, yend = nombre), 
               linewidth = .5, color = "#0970a5") +
  # puntos de cantidad de visitas
  geom_point(aes(shape = decada), size = 1, color = "white", fill = "#a4cac8") +
  # círculo alrededor del comentario
  annotation_custom(grid::circleGrob(default.units="npc",
                                     x = 1.055,
                                     y = .7,
                                     r = .18,
                                     gp = grid::gpar(col = "white", 
                                                     vjust = 0.5, 
                                                     hjust = 0.5, 
                                                     fill = "#a4cac8"))) +
  # lenguajes más nuevo/viejo
  geom_label(data = min_max[1, ], 
             aes(x = visitas+150, y = nombre, label = glue("El lenguaje más nuevo, {nombre}, aparece en {año} ")),
             hjust = 0, vjust = .5, color = "white", family = "mono", size = 3,
             fill = "#5fb0b8", label.size = 0, label.r = unit(0, "line")) +
  geom_label(data = min_max[2, ], 
             aes(x = visitas+150, y = nombre, label = glue("El lenguaje más viejo, {nombre}, aparece en {año} ")),
             hjust = 0, vjust = .5, color = "white", family = "mono", size = 3,
             fill = "#5fb0b8", label.size = 0, label.r = unit(0, "line")) +
  # faceta
  facet_wrap(~ comentario_label, ncol = 1, scales = "free", 
             strip.position = "right") +
  # manual
  scale_x_continuous(breaks = seq(0, 7000, 1000),
                     limits = c(0, 7500),
                     expand = c(0, 0)) +
  scale_shape_manual(values = c(3, 8, 21:24)) +
  coord_cartesian(clip = "off") +
  # ejes
  labs(x = "Visitas diarias a la entrada del lenguaje en Wikipedia", y = NULL,
       shape = "Década de aparición:",
       title = "Su <span style='color:white'>comentario</span> no molesta",
       subtitle = glue(
         "Cada lenguaje de programación posee un caracter especial 
         que habilita los <span style='color:white'>comentarios</span> en el 
         script. Se muestra una selección de <span style='color:white'>5</span> 
         caracteres: [{f_code('//')}], [{f_code('#')}], [{f_code('-')}], 
         [{f_code('%')}] y [{f_code(';')}] y los 10 lenguajes más populares que 
         lo utilizan, de acuerdo a la cantidad de <span style='color:white'>visitas 
         diarias</span> de sus correspondientes entradas en 
         <span style='color:white'>Wikipedia</span>. Se indica también la 
         <span style='color:white'>decada</span> de salida del lenguaje."),
       caption = mi_caption) +
  # guide
  guides(shape = guide_legend(nrow = 1, override.aes = list(size = 3))) +
  # tema
  theme(aspect.ratio = .25,
        legend.position = c(.45, 1.03),
        legend.key.size = unit(1, "line"),
        legend.text = element_text(family = "share"),
        legend.title = element_text(hjust = .5, family = "heebo", color = "#003354"),
        legend.direction = "horizontal",
        legend.key = element_rect(fill = "#5fb0b8", color = NA),
        legend.background = element_rect(fill = "#5fb0b8", color = NA),
        plot.background = element_rect(fill = "#5fb0b8", color = "white",
                                       linewidth = 3),
        plot.title = element_markdown(size = 35, margin = margin(0, 5, 5, 5),
                                      family = "roboto", color = "#003354"),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(margin = margin(5, 5, 30, 5),
                                               color = "#003354"),
        plot.caption = element_markdown(hjust = .5, size = 10, 
                                        margin = margin(20, 0, 0, 0)),
        plot.caption.position = "plot",
        plot.margin = margin(16.4, 16, 16.4, 16),
        panel.grid = element_line(linewidth = .1, color = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#a4cac8", color = NA),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "heebo", color = "#003354",
                                    margin = margin(5, 0, 0, 0)),
        axis.text = element_text(family = "share", color = "black"),
        strip.background.y = element_rect(fill = NA, color = NA),
        strip.text.y.right = element_markdown(angle = 0, color = "#003354",
                                              family = "heebo"))

# guardo
ggsave(plot = g1,
       filename = here("2023/semana 12/viz.png"),
       width = 19,
       height = 27,
       dpi = 300,
       units = "cm")
# abro
browseURL(here("2023/semana 12/viz.png"))
