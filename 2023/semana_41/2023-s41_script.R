
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#E78429"
c2 <- "#F6C200"
c3 <- "#BAD6F9"
c4 <- "grey30"
c5 <- "grey10"
c6 <- "grey15"
c7 <- "grey90"
c8 <- "white"

# texto gral, ubicación, ciudad
font_add_google(name = "Ubuntu", family = "ubuntu")
# descripción
font_add_google(name = "Creepster", family = "creepster", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 41. Haunted Places in USA, **Tim Renner**</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-10/readme.md")

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

# me interesan los sitios relacionados a cementerios y universidades
# incluyo una (breve) descripción de c/u, ubicados en lados opuestos

# mapa de EEUU, sacando algunos territorios
us <- rgeoboundaries::gb_adm1(country = "USA")

us_otros <- c(
  "Puerto Rico", "Alaska", "American Samoa", "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands", "Guam", "Hawaii", 
  "Commonwealth of the Northern Mariana Islands")

us_cont <- us |> 
  filter(!shapeName %in% us_otros)

# hay un sitio por fuera de EEUU? Lo remuevo
lawrence <- haunted_places |> 
  filter(between(longitude, -82, -78) & between(latitude, 44, 46)) |> 
  pull(location)

# creo un objeto sf, y remuevo el sitio sospechoso
haunted_places_sf <- haunted_places |> 
  drop_na(longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  st_set_crs(value = 4326) |> 
  st_crop(st_bbox(us_cont)) |> 
  filter(location != lawrence)

# convierto a EPGS:5070, para un mapa más agradable
browseURL("https://epsg.io/5070")

d <- st_transform(haunted_places_sf, crs = 5070)
us_cont_trans <- st_transform(us_cont, crs = 5070)

# contorno de EEUU
contorno <- st_union(us_cont_trans)

# simplifico los sitios: otros, cementerio y universidad
d_lugares <- d |> 
  select(location) |> 
  mutate(location = str_to_lower(location)) |> 
  mutate(location = case_when(
    str_detect(location, "cemetery") ~ "cemetery",
    str_detect(location, "university") ~ "university",
    .default = "otros"))

# divido los datos en otros, y universidades & cementerios
d_otros <- d_lugares |> 
  filter(location == "otros")

d_interes <- d_lugares |> 
  filter(location != "otros")

# cantidades de universidades, cementerios y sitios en gral
n_uni <- d_interes |> 
  filter(location == "university") |> 
  nrow()

n_cem <- d_interes |> 
  filter(location == "cemetery") |> 
  nrow()

n_sitios <- nrow(haunted_places) |> 
  gt::vec_fmt_number(sep_mark = ".", dec_mark = ",", decimals = 0)

# descripción BREVE de cementerio y universidad
# elijo una descripción de cementerio de Florida, y una descripción de 
# universidad de Montana (lados opuestos)
ej_cem <- haunted_places |> 
  filter(state_abbrev == "FL") |> 
  mutate(location = str_to_lower(location)) |> 
  filter(str_detect(location, "cemetery")) |> 
  mutate(n = nchar(description)) |> 
  slice_min(order_by = n, n = 1) |> 
  mutate(description = str_wrap(description, 35))

ej_uni <- haunted_places |> 
  filter(state_abbrev == "MT") |> 
  mutate(location = str_to_lower(location)) |> 
  filter(str_detect(location, "university")) |> 
  mutate(n = nchar(description)) |> 
  slice_min(order_by = n, n = 1) |> 
  mutate(description = str_remove(description, "Main Hall - ")) |> 
  mutate(description = str_wrap(description, 35))

# unifico ambas descripciones y creo la etiqueta
ej_ambos <- bind_rows(ej_uni, ej_cem) |> 
  mutate(description = str_replace_all(description, "\\n", "<br>")) |> 
  mutate(location = str_to_title(location)) |> 
  mutate(label = glue(
    "<b style='font-size:25px;font-family:creepster;'>{description}</b><br>",
    "<span style='font-family:ubuntu; color:white'>*{location}*, *{city}*, *{state_abbrev}*</span>"))

# ubicación de las cajas de las descripciones
caja_longitud <- c(-110, -90)
caja_latitud <- c(50, 25)

# modifico las coordenadas de las descripciones a EPSG:5070
ej_label <- ej_ambos |> 
  mutate(location = str_to_lower(location)) |> 
  mutate(location = if_else(
    str_detect(location, "cemetery"), "cemetery", "university")) |> 
  # mutate(location = case_when(
  #   str_detect(location, "cemetery") ~ "cemetery",
  #   str_detect(location, "university") ~ "university",
  #   .default = "otros")) |> 
  mutate(latitude = caja_latitud) |> 
  mutate(longitude = caja_longitud) |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 5070) |> 
  mutate(vjust = c(0, 1)) |> 
  mutate(hjust = c(0, 0))

# ubicación de los sitios que corresponden a las descripciones
ej_puntos <- bind_rows(ej_uni, ej_cem) |> 
  mutate(location = str_to_lower(location)) |> 
  mutate(location = case_when(
    str_detect(location, "cemetery") ~ "cemetery",
    str_detect(location, "university") ~ "university",
    .default = "otros")) |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 5070)

# figura ------------------------------------------------------------------

# flechas que unen las descripciones con los sitios
# inicio de la flecha
flecha <- tibble(
  x = caja_longitud, y = caja_latitud) |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 5070) |> 
  st_coordinates() |> 
  as_tibble() |> 
  rename(x = X, y = Y)

# final de la flecha
flecha_end <- ej_puntos |> 
  st_coordinates() |> 
  as_tibble() |>
  st_as_sf(coords = c("X", "Y")) |> 
  st_set_crs(value = 5070) |> 
  st_coordinates() |> 
  as_tibble() |> 
  rename(xend = X, yend = Y)

flecha_tbl <- bind_cols(flecha, flecha_end)

# ubicación de la caja con el título
caja_tit <- tibble(x = -110, y = 26) |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 5070) |> 
  st_coordinates() |> 
  as_tibble() |> 
  rename(x = X, y = Y)

# figura
g <- ggplot() +
  # mapa de EEUU, con los Estados
  geom_sf(data = us_cont_trans, fill = c4, color = c5) +
  # contorno de EEUU
  geom_sf(data = contorno, fill = NA, color = c8) +
  # sitios 'otros'
  geom_sf(data = d_otros, alpha = 1/2, color = c5, size = .25) +
  # universidades y cementerios
  geom_sf(data = d_interes, aes(color = location), alpha = 1/3, size = 4) +
  # títulos
  annotate(
    geom = "richtext", x = caja_tit$x, y = caja_tit$y, hjust = .5, vjust = 1,
    label = glue(
      "En **EEUU** hay {n_sitios} sitios embrujados,<br>",
      "de los cuales hay {n_uni} en <b style='color:{c3};'>universidades</b> y<br>",
      "{n_cem} en <b style='color:{c1};'>cementerios</b>."),
    label.color = NA, fill = NA, family = "ubuntu", size = 6, color = c7) +
  # descripciones
  geom_richtext(
    data = ej_label, 
    aes(label = label, hjust = hjust, vjust = vjust, geometry = geometry), 
    stat = "sf_coordinates", family = "ubuntu", color = c2, fill = c6,
    lineheight = unit(2, "line"),
    label.color = NA) +
  # flechas
  geom_curve(
    data = flecha_tbl[1, ], aes(x, y, xend = xend, yend = yend), 
    inherit.aes = FALSE, color = c7, curvature = .25,
    arrow = arrow(angle = 15, length = unit(.5, "line"), type = "closed")) +
  geom_curve(
    data = flecha_tbl[2, ], aes(x, y, xend = xend, yend = yend), 
    inherit.aes = FALSE, color = c7, curvature = -.25,
    arrow = arrow(angle = 15, length = unit(.5, "line"), type = "closed")) +
  scale_color_manual(values = c(c1, c3)) +
  coord_sf(clip = "off") +
  labs(x = NULL, y = NULL, caption = mi_caption) +
  theme_minimal() +
  theme(
    plot.margin = margin(41, 6, 11, 6),
    plot.background = element_rect(fill = c5, color = c4, linewidth = 3),
    plot.caption = element_markdown(
      color = c7, family = "ubuntu", size = 10, margin = margin(70, 15, 5, 0)),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_41/viz.png",
  width = 30,
  height = 24,
  units = "cm")

# abro
browseURL("2023/semana_41/viz.png")
