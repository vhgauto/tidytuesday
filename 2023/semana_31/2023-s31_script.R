
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
library(ggrepel)

# fuentes -----------------------------------------------------------------

# colores, Lakota
c1 <- "#931E17"
c2 <- "#20235B"
c3 <- "#F0BE3D"
c4 <- "#EDC775"

# Estados
font_add_google(name = "Lato", family = "lato")
# procentajes
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Bree Serif", family = "bree", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 31. **List of states and territories of the United States**, Wikipedia</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-01/readme.md")

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')

# por c/Estado, me interesa el porcentaje de la superficie cubierta por agua
d <- states |> 
  transmute(estado = state, agua = water_area_km2/total_area_km2) |> 
  # me quedo con los que tengan al menos 1%
  filter(agua >= .01) |> 
  # agrego formato a las etiquetas de los Estados
  mutate(estado = glue("<span style='font-family:lato;font-size:15pt;color:white;'>{estado}</span>")) |> 
  # ordeno de acuerdo a la fracción de agua
  mutate(estado = fct_reorder(estado, agua)) |> 
  # convierto las fracciones a porcentajes, aplico formato
  mutate(agua_label = gt::vec_fmt_percent(agua, decimals = 1)) |> 
  mutate(agua_label = glue("<span style='font-family:victor;font-size:7pt;color:{c4};'>**{agua_label}**</span>")) |> 
  # acomodo de acuerdo a la fracción de agua
  arrange(agua) |> 
  # agrego nro de fila, para usar como eje vertical en la figura
  # si no, geom_ribbon() no funciona
  mutate(fila = row_number()) |> 
  # alterno etiquetas a izquierda y derecha de la superficie
  mutate(
    hjust = rep(c(1, 0), length.out = max(fila)), 
    vjust = .5
    ) |> 
  # alterno la posición de los Estados y porcentajes en las etiquetas
  mutate(label = if_else(
    condition = fila %% 2 != 0,
    true = glue("{agua_label} {estado}"),
    false = glue("{estado} {agua_label}")
  )) |> 
  # alterno espacio extra entre el punto y la etiqueta
  mutate(extra_x = if_else(
    condition = fila %% 2 != 0,
    true = -.0025,
    false = .0025
  )) |> 
  mutate(agua_x = agua + extra_x) |> 
  # condiciones especiales para HAWAII
  mutate(hjust = if_else(
    condition = str_detect(label, "Hawaii"),
    true = 1,
    false = hjust
  )) |> 
  mutate(label = if_else(
    condition = str_detect(label, "Hawaii"),
    true = glue("{agua_label} {estado}"),
    false = label
  )) |> 
  mutate(agua_x = if_else(
    condition = str_detect(label, "Hawaii"),
    true = agua - extra_x,
    false = agua_x
  ))

# vector con los Estados que tienen menos del 1% de agua
ultimos_estados <- states |> 
  transmute(estado = state, agua = water_area_km2/total_area_km2) |> 
  filter(agua < .01) |> 
  pull(estado) |> 
  str_flatten_comma(last = " y ")

# figura ------------------------------------------------------------------

# título y 
titulo <- "Los Estados<br>con más agua"

# subtítulo, es conveniente hacerlo como tibble, ya que puedo usar 
# geom_textbox(), para facilitar el ancho del texto
subtitulo <- glue(
  "Se muestran los Estados de **EEUU** que poseen
  mayor porcentaje de <b style='color:white;'>superficie cubierta por
  agua</b>, respecto del total.<br><br>{ultimos_estados}
  se omitieron debido a que poseen valores menores al 1%.")

subtitulo_tbl <- tibble(
  x = .2,
  y = 16,
  label = subtitulo
)

# figura
g <- ggplot(data = d, aes(x = agua, y = fila)) +
  # área de la derecha
  geom_ribbon(aes(ymin=0, ymax=fila), fill = c1) +
  # línea
  geom_line(color = "black", linetype = 1, linewidth = .25) +
  geom_line(color = "white", linetype = "88", linewidth = .25) +
  # puntos concentricos
  geom_point(size = 1.5, color = "white", shape = 19) +
  geom_point(size = 1, color = "black", shape = 19) +
  # estados y porcentajes
  geom_richtext(
    aes(x = agua_x, label = label, hjust = hjust, vjust = vjust), fill = NA, 
    label.r = unit(0, "line"), label.size = 0) +
  # título
  annotate(
    geom = "richtext", x = .2, y = 23, hjust = 0, vjust = 1, label = titulo,
    size = 19, fill = NA, color = c3, label.color = NA, family = "bree") +
  # subtítulo
  geom_textbox(
    data = subtitulo_tbl, aes(x = x, y = y, label = label),
    size = 6, fill = NA, color = c4, box.color = NA, 
    hjust = 0, vjust = 1, width = unit(13, "cm")) +
  # caption
  annotate(
    geom = "richtext", x = .4, y = 2, hjust = 1, vjust = 1, label = mi_caption,
    size = 3.5, fill = NA, color = "white", label.color = NA, family = "lato") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(d$agua)*1.003)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(d$fila))) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(12, 0, 0, 103),
    plot.background = element_rect(fill = c2, color = c1, linewidth = 3),
    panel.background = element_blank(),
    panel.grid = element_blank()
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_31/viz.png",
  width = 30,
  height = 26.8,
  units = "cm")

# abro
browseURL("2023/semana_31/viz.png")
