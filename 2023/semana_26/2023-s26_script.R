
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(tidytext)
library(glue)
library(ggtext)
library(showtext)
library(ggpath)

# fuentes -----------------------------------------------------------------

# colores
c1 <- "#3C0D02"
c2 <- "#8D1C06"
c3 <- "white"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# título
font_add_google(name = "Bebas Neue", family = "bebas")
font_add_google(name = "STIX Two Text", family = "stix")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 26</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-07-04/readme.md")
us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')

# más datos
browseURL("https://www.usgs.gov/us-board-on-geographic-names/what-geographic-names-information-system-gnis#1")

# sistema de coordenadas de EEUU
crs_eeuu <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# mapa de EEUU, entero
eeuu <- giscoR::gisco_get_countries(country = "US", resolution = "01") |> 
  # transformo las coordenadas
  st_transform(crs = crs_eeuu)

# extención de la parte continental de EEUU, tbl
bb_tbl <- tibble(
  lon = c(-129, -62.5, -62.5, -129, -129),
  lat = c(20, 20, 50.5, 50.5, 20))

# convierto a sf
bb <- bb_tbl |> 
  # uso coordenadas 4326
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(geometry = st_combine(geometry)) |> 
  # convierto a polígono
  st_cast("POLYGON") |> 
  # transformo a sistema de coordenadas EEUU
  st_transform(crs = crs_eeuu)

# recorto el mapa de EEUU a la región continental
eeuu_bb <- st_crop(eeuu, bb)

# convierto los datos de nombres de lugares a sf
nombres_sf2 <- us_place_names |> 
  # remuevo sitios sin coordenadas
  drop_na(prim_lat_dec, prim_long_dec) |> 
  # indico columnas de coordenadas , CRS
  st_as_sf(coords = c("prim_long_dec", "prim_lat_dec"), crs = 4326) |> 
  # transformo a sistema de coordendas de EEUU
  st_transform(crs = crs_eeuu)

# recorto los datos a la región continental
nombres_sf <- st_crop(nombres_sf2, bb)

# me intereso en molinos (mill)
d <- nombres_sf |> 
  # convierto todo a minúscula
  mutate(feature_name = str_to_lower(feature_name)) |> 
  # molinos
  filter(str_detect(feature_name, "mill")) |> 
  # columnas de interés
  select(geometry)

# figura ------------------------------------------------------------------

# cantidad de molinos
n_molinos <- nrow(d) |> 
  gt::vec_fmt_number(dec_mark = ",", sep_mark = ".", decimals = 0)

# .png molino
molino <-("2023/semana_26/molino.svg")

# figura
g <- ggplot() +
  geom_sf(data = eeuu_bb, fill = c2, color = NA) +
  geom_sf(data = d, color = c3, alpha = .6, size = .75, shape = 16) +
  geom_from_path(aes(x = -2100000, y = -1700000, path = molino), width = .05) +
  annotate(
    geom = "richtext", x = -2200000, y = -2000000, 
    label = glue(
      "Cada punto representa la ubicación<br>de los {n_molinos}
      molinos en **EE.UU.**<br>(territorio continental)."),
    label.color = NA, color = c3, fill = NA, hjust = 0, family = "ubuntu", 
    size = 5) +
  coord_sf() +
  labs(
    title = "Molinos en EE.UU.",
    caption = mi_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c1, color = c2, linewidth = 3),
    # plot.margin = margin(7.3, 0, 7.3, 0),
    plot.margin = margin(6.9, 0, 6.9, 0),
    plot.title.position = "plot",
    plot.title = element_markdown(
      family = "stix", size = 36, color = c3, hjust = .5, 
      margin = margin(0, 0, -30, 0)),
    plot.caption = element_markdown(
      color = c4, margin = margin(0, 10, 0, 0), family = "ubuntu")
  ); ggsave(
    plot = g,
    filename = "2023/semana_26/viz.png",
    width = 30,
    height = 19,
    units = "cm",
    dpi = 300
  ); browseURL("2023/semana_26/viz.png")

