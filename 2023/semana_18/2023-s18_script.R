# paquetes ----------------------------------------------------------------

library(tidyverse)
library(ggh4x)
library(ggtext)
library(showtext)
library(glue)
library(ggpath)

# fuente ------------------------------------------------------------------

# colores
# paleta 'Bwenedictus' de MetBrewer
c1 <- "#fcebf0"
c2 <- "#f9b4c9"
c3 <- "#b93961"
c4 <- "#9a153d"
c5 <- "#1a318b"
c6 <- "#6996e3"

font_add_google(name = "Share Tech Mono", family = "share") # números
font_add_google(name = "Anek Tamil", family = "anek", db_cache = FALSE) # texto gral
font_add_google(name = "Almendra", family = "almendra", db_cache = FALSE) # título

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", "icon/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 18</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-02/readme.md")

species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')

# arreglo datos
datos <- species |> 
  # selecciono variables de interés
  select(nombre = commonname, ends_with("wgt"), -juvwgt) |> 
  # renombro removiendo 'wgt'
  rename_with(~ str_remove(.x, "wgt")) |> 
  # divido en RATAS y RATONES
  mutate(animal = case_when(
    str_detect(nombre, "rat") ~ "Rata",
    str_detect(nombre, "ouse") ~ "Ratón")) |> 
  # de los nombres de los animales, remuevo 'mouse' y 'rat'
  mutate(nombre = str_remove(nombre, "mouse|Mouse|rat")) |>
  mutate(nombre = str_trim(nombre)) |> 
  # acomodo el texto
  mutate(nombre = if_else(animal == "Rata", str_wrap(nombre, width = 12), nombre)) |> 
  # ordeno los nombres de acuerdo al peso promedio
  mutate(nombre = fct_reorder(nombre, mean))

# imágenes .png de rata y ratón
browseURL("https://emojipedia.org/openmoji/")

lista_i <- list.files("2023/semana_18/",
                      pattern = "^i_",
                      full.names = TRUE)

roedor <- tibble(min = c(Inf, Inf),
                 nombre = c(-Inf, -Inf),
                 animal = c("Rata", "Ratón"),
                 path = lista_i)

# figura ------------------------------------------------------------------

# escalas individuales del eje horizontal
escalas <- list(
  # rata
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 300)),
  # ratón
  scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)))

# separador de las palabras del título
# sin este separador queda todo muy pegado, con poco espacio
s <- glue("<span style='color:{c1}'>..</span>")

# figura
ggplot(data = datos, aes(x = min, xend = max, y = nombre, yend = nombre)) +
  # segmento que une max y min
  geom_segment(color = c2, linewidth = 2, lineend = "round") +
  # min
  geom_point(aes(x = min), color = c6, size = 4) +
  # max
  geom_point(aes(x = max), color = c5, size = 4) +
  # promedio
  geom_point(
    aes(x = mean), shape = "|", size = 4, color = c3) +
  # emoji de rata y ratón
  geom_from_path(data = roedor, aes(x = min, y = nombre, path = path),
                 inherit.aes = FALSE, width = 0.3, hjust = 1.25, vjust = .4) +
  # facetas
  facet_wrap(~ animal, scales = "free") +
  facetted_pos_scales(x = escalas) +
  # títulos
  labs(x = "Peso (g)", y = NULL,
       title = glue("DE{s}RATAS{s}Y{s}RATONES"),
       subtitle = glue(
         "Tomando datos de **roedores** que habitan el desierto de 
         Arizona (EEUU), se muestran los valores de <span style='color:{c6};'>**peso mínimo**</span> y
         <span style='color:{c5};'>**peso máximo**</span>, para cada especie. Las líneas verticales indican
         pesos promedio. Se clasificaron los datos entre **ratas** y
         **ratones**."),
       caption = mi_caption) +
  # tema
  theme_minimal() +
  theme(
    aspect.ratio = 2,
    plot.background = element_rect(fill = c1, color = c2, linewidth = 2),
    plot.title = element_markdown(
      family = "almendra", size = 60, color = c4, hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = c4, hjust = .4, family = "anek", size = 11, 
      margin = margin(20, 0, 0, 0)),
    plot.subtitle = element_textbox_simple(
      family = "anek", size = 15, color = c3, margin = margin(0, 0, 10, 0)), 
    plot.margin = margin(12, 10, 12, 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = c2, linewidth = .2),
    panel.spacing.x = unit(2, "line"),
    axis.text.x = element_text(family = "share", size = 17, color = c3),
    axis.text.y = element_text(family = "anek", color = c4, size = 13),
    axis.title.x = element_markdown(family = "anek", size = 23, color = c4),
    strip.text = element_markdown(color = c4, family = "anek", size = 25)
  )

# guardo
ggsave(
    filename = "2023/semana_18/viz.png",
    width = 30,
    height = 28.5,
    units = "cm",
    dpi = 300)

# abro
browseURL("2023/semana_18/viz.png")
