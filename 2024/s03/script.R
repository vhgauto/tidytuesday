
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(sf)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "white"
c2 <- "#3F6C81"
c3 <- "#EBEB99"
c4 <- "grey20"
c5 <- "grey10"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# horas, días
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 3. ",
  "The Center for Public Integrity.</span>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} 
  {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-09/readme.md")

tuesdata <- tidytuesdayR::tt_load(2024, week = 3)

datos <- tuesdata$polling_places

# me interesa la distribución de lugares de votación por condado, para las 
# elecciones de 2014 a 2020

count(datos, election_date)

# remuevo las fechas con menor cantidad de datos y selecciono los últimos 4
# años
fechas_interes <- ymd(c("2014-11-04", "2016-11-08", "2018-11-06", "2020-11-03"))

# cantidad de sitios por condado, para las cuatro fechas
d_tbl <- datos |> 
  filter(election_date %in% fechas_interes) |>
  count(election_date, state, county_name) |> 
  drop_na() |> 
  mutate(county_name = str_to_lower(county_name)) |> 
  rename(estado_abr = state, condado = county_name)

# nombre de los estados y sus abreviaturas, tibble
estados_tbl <- tibble(estado_abr = state.abb, estado = state.name) |> 
  mutate(estado = str_to_lower(estado))

# polígonos de los condados y a qué estado corresponden
condados_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE)) |> 
  separate_wider_delim(
    cols = ID, delim = ",", names = c("estado", "condado")) |> 
  rename(geometry = geom) |> 
  st_as_sf() |> 
  st_transform(crs = 2163)

# combino los condados, estados y cantidad de sitios
d_sf <- inner_join(d_tbl, estados_tbl, by = join_by(estado_abr)) |> 
  right_join(condados_sf, by = join_by(estado, condado)) |> 
  rename(fecha = election_date) |> 
  mutate(label_fecha = glue(
    "{day(fecha)} de {format(fecha, '%B')} de {year(fecha)}")) |> 
  st_as_sf() |> 
  drop_na() |> 
  mutate(label_fecha = fct_reorder(label_fecha, fecha))

# figura ------------------------------------------------------------------

# leyenda de los condados sin datos
strip_label <- tibble(
  x = st_bbox(condados_sf)["xmax"]*.5,
  y = st_bbox(condados_sf)["ymin"]*1.1,
  label = "Sin datos",
  label_fecha = fct("3 de noviembre de 2020"))

# subtítulo
mi_subtitle <- glue(
  "Cantidad de sitios de votación por<br>",
  "condado en **EE.UU.**, para cuatro<br>",
  "elecciones, entre 2014 y 2020.")

sub_label <- tibble(
  x = st_bbox(condados_sf)["xmax"]*.85,
  y = st_bbox(condados_sf)["ymin"],
  label = mi_subtitle,
  label_fecha = fct("4 de noviembre de 2014"))

# figura
g <- ggplot() +
  # condados de EEUU
  geom_sf(data = condados_sf, fill = c4, linewidth = .05, color = c1) +
  # lugares de votación
  geom_sf(data = d_sf, aes(fill = n), linewidth = .03, color = c1) +
  # subtítulo
  geom_richtext(
    data = sub_label, aes(x, y, label = label), color = c3, size = 4, 
    hjust = 0, vjust = .5, family = "ubuntu", fill = c4, label.color = NA,
    label.r = unit(0, "mm")) +
  # sin datos
  geom_tile(
    data = strip_label, aes(x, y, width = 150000, height = 150000), color = c1,
    fill = c4) +
  geom_text(
    data = strip_label, aes(x, y, label = label), color = c1, size = 4, 
    hjust = 0, family = "ubuntu", nudge_x = 85000) +
  # faceta
  facet_wrap(vars(label_fecha), nrow = 2) +
  scico::scale_fill_scico(
    palette = "nuuk", trans = "log10",
    breaks = 10^seq(0, 4, 1),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  coord_sf(expand = FALSE, clip = "off") +
  labs(caption = mi_caption, fill = "# de lugares\nde votación") +
  guides(fill = guide_colorbar(ticks.colour = c5)) +
  theme_void() +
  theme(
    plot.margin = margin(12.4, 10, 12.4, 10),
    plot.background = element_rect(fill = c5, color = c3, linewidth = 3),
    plot.caption = element_markdown(color = c1, family = "ubuntu"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.height = unit(.5, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.title = element_text(
      color = c1, family = "ubuntu", margin = margin(r = 10, b = 10)),
    legend.text = element_text(color = c1, family = "victor", face = "bold"),
    strip.text = element_text(
      color = c1, family = "ubuntu", size = 13, face = "bold"))

# guardo
ggsave(
  plot = g,
  filename = "2024/s03/viz.png",
  width = 30,
  height = 24,
  units = "cm")

# abro
browseURL("2024/s03/viz.png")
