
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
library(ggpath)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#cc2c29"
c2 <- "#E4332F"
c3 <- "#FFCE00"
c4 <- "white"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# eje vertical, scoville
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# eje horizontal, temporadas
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 32. **List of Hot Ones episodes**, Wikipedia</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-08/readme.md")

sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')

# me interesa la distribución de la escala Scoville para cada salsa
#  (del 1 al 10), en todas las temporadas
d <- sauces |> 
  mutate(sauce_number = factor(sauce_number)) |> 
  select(sauce_number, scoville)

# figura ------------------------------------------------------------------

# tibble con la ubicación del logo

camino_logo <- "2023/semana_32/logo.png"

logo_tbl <- tibble(
  x = 1, 
  y = 1e6, 
  path = camino_logo)

# subtítulo
sub_tbl <- tibble(
  x = 10,
  y = 1000,
  label = glue(
    "<b style='color:{c3}'>Hot Ones</b> es un programa de **YouTube** donde celebridades son<br>
    entrevistadas mientras *disfrutan* comida picante,  con 10 salsas<br>
    en orden creciente de picor. Se muestra la intensidad de las<br>
    salsas para las 21 temporadas del programa."))

# explicación escala Scoville
scoville_tbl <- tibble(
  x = 10,
  y = 300,
  label = glue(
    "<sup style='color:white;'>**\U2020**</sup>La **escala Scoville** mide la
    intensidad de un picante.<br>El valor más bajo (1) corresponde al 
    morrón verde."))

# figura
g <- d |> 
  ggplot(aes(sauce_number, scoville, color = sauce_number)) +
  geom_jitter(width = .1, alpha = 1/1, size = 7, shape = 1) +
  geom_from_path(
    data = logo_tbl, aes(x, y, path = path),
    width = .4, inherit.aes = FALSE, hjust = 0, vjust = .5) +
  geom_richtext(
    data = sub_tbl, aes(x, y, label = label), 
    label.color = NA, fill = NA, color = c4, family = "ubuntu", size = 6,
    hjust = 1, vjust = 1) +
  geom_richtext(
    data = scoville_tbl, aes(x, y, label = label),
    label.color = NA, fill = NA, color = c3, family = "ubuntu", size = 5,
    hjust = 1, vjust = 1) +
  scale_y_log10(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(100, NA),
    expand = c(0, .1)) +
  scale_color_manual(values = rep(c(c3, c4), length.out = 10)) +
  coord_cartesian(clip = "off") +
  labs(
    x = "\\# de salsa",
    y = "Escala Scoville<sup style='color:white;'>**\U2020**</sup><br>
    <span style='font-size:15pt'>*logarítmica*</style>",
    caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.margin = margin(70.3, 10, 5.3, 10),
    plot.background = element_rect(fill = c1, color = c2, linewidth = 3),
    plot.caption = element_markdown(
      color = c4, family = "ubuntu", margin = margin(10, 0, 10, 0), 
      size = 12),
    panel.grid.major.y = element_line(
      color = c4,linetype = "f8", linewidth = .1),
    axis.title.x = element_markdown(
      size = 26, color = c3, family = "ubuntu", margin = margin(20, 0, 0, 0)),
    axis.title.y = element_markdown(
      size = 26, color = c3, family = "ubuntu", angle = 90, lineheight = .2),
    axis.text.x = element_text(
      family = "bebas",
      color = rep(c(c3, c4), length.out = 10),
      size = 30),
    axis.text.y = element_text(
      family = "victor", color = c4, size = 16, hjust = 1)
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_32/viz.png",
  width = 30,
  height = 31,
  units = "cm")

# abro
browseURL("2023/semana_32/viz.png")

