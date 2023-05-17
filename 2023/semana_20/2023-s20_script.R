
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)

# fuente ------------------------------------------------------------------

# eje vertical, años
font_add_google(name = "Bebas Neue", family = "bebas")
# eje horizontal, meses
font_add_google(name = "Inconsolata", family = "inconsolata")
# título
font_add_google(name = "Gloock", family = "gloock", db_cache = FALSE)
# resto del texto
font_add_google(name = "Schibsted Grotesk", family = "grotesk", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# MetBrewer: Monet
c1 <- "#e3cacf"
c2 <- "#c399a2"
c3 <- "#9f6d71"
c4 <- "#41507b"

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 20</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-16/readme.md")

tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

# sumas mensuales, desde 1960
datos <- tornados |> 
  group_by(yr, mo) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  filter(yr >= 1960)

# iniciales de los meses, para el eje x
m <- tibble(x = seq(ymd(20200101), ymd(20201201), by = "1 month")) |> 
  mutate(m = format(x, "%B")) |> 
  mutate(mes = str_sub(m, 1, 1) |> str_to_upper())

# mes con la mayor cantidad de tornados
max_n <- datos |>
  slice_max(order_by = n)

fecha_max <- ymd(glue("{max_n$yr}-{max_n$mo}-01"))

# figura ------------------------------------------------------------------

# breaks
b <- classInt::classIntervals(var = datos$n, n = 6, style = "pretty")$brks

# figura
g <- ggplot(data = datos, aes(x = mo, y = yr, fill = n)) +
  # tile
  geom_tile(
    color = c1, linewidth = 1) +
  # manual
  scale_y_continuous(
    breaks = seq(1950, 2020, 10),
    sec.axis = dup_axis()) +
  scale_x_continuous(
    breaks = seq(1, 12, 1),
    labels = m$mes,
    sec.axis = dup_axis()) +
  scale_fill_viridis_c(
    option = "turbo",
    breaks = b) +
  coord_fixed(ylim = c(1960, 2022), expand = FALSE, clip = "off") +
  # ejes
  labs(x = NULL, y = NULL, fill = "Cantidad\nde eventos\npor mes",
       title = "TWISTER!",
       subtitle = glue(
         "Cantidad mensual de **tornados** en EEUU, entre 1950 y 2022. El mes 
         con mayor cantidad de eventos fue en {format(fecha_max, '%B')} de 
         {format(fecha_max, '%Y')} con **{max_n$n}** tornados."),
       caption = mi_caption) +
  # tema
  theme_minimal() +
  theme(
    plot.background = element_rect(
      fill = c1, color = c2, linewidth = 3),
    plot.margin = margin(0, 55, 0, 55),
    plot.title.position = "plot",
    plot.title = element_text(
      color = c4, family = "gloock", size = 135, hjust = .5, 
      margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_textbox_simple(
      color = c4, size = 30, family = "grotesk",
      margin = margin(5, -10, 25, -40)),
    plot.caption = element_markdown(
      color = c4, hjust = .19, size = 21, family = "grotesk", 
      margin = margin(20, 0, 5, 0)),
    panel.grid = element_blank(),
    axis.text.y = element_text(
      color = c3, family = "bebas", size = 45),
    axis.text.x = element_text(
      color = c3, family = "inconsolata", size = 40),
    axis.text.x.bottom = element_text(
      margin = margin(15, 0, 0, 0), face = "bold"),
    axis.text.x.top = element_text(
      margin = margin(0, 0, 15, 0), face = "bold"),
    legend.box.margin = margin(0, 0, 0, 80),
    legend.title = element_text(
      colour = c4, size = 30, family = "grotesk"),
    legend.text = element_text(
      color = c4, size = 35, family = "inconsolata"),
    legend.key.height = unit(3.5, "cm"),
    legend.key.width = unit(2, "cm")
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_20/viz.png",
  width = 30,
  height = 75,
  units = "cm",
  dpi = 300)

# abro
browseURL("2023/semana_20/viz.png")
