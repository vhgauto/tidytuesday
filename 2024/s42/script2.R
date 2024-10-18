
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyterra)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#000000"
c2 <- "#000000"
c3 <- "#000000"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf"
)

# monoespacio & íconos
font_add(
  family = "jet", 
  regular = "fuente/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {42}, ",
  "XXX fuente de datos XXX.</span>"
)
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 42)
orcas <- tuesdata$orcas

glimpse(orcas)

count(orcas, pods_or_ecotype, sort = TRUE)

library(sf)
library(terra)

begin_coord <- vect(
  orcas, geom = c("begin_longitude", "begin_latitude"), crs = "EPSG:4326"
)

orcas_sf <- orcas |> 
  drop_na(begin_latitude, begin_longitude) |> 
  st_as_sf(
    coords = c("begin_longitude", "begin_latitude"),
    crs = st_crs(4326)
  ) |> 
  st_transform(5070)

usa_sf <- rgeoboundaries::gb_adm0(country = "USA")

cnd_sf <- rgeoboundaries::gb_adm0(country = "Canada")

aoi_sf <- rbind(usa_sf, cnd_sf) |> 
  st_transform(5070)

bb <- st_bbox(orcas_sf)

centroid <- st_cast(orcas_sf, "MULTIPOINT") |> 
  st_as_sfc() |> 
  st_union() |> 
  st_centroid()

centroid_buff <- st_buffer(centroid, 3e5)

aoi_crop_sf <- st_intersection(aoi_sf, centroid_buff)

plot(centroid_buff)
plot(aoi_crop_sf, add = TRUE)
plot(st_geometry(orcas_sf), col = "red", add = TRUE)






# figura ------------------------------------------------------------------

# figura
# g <- ggplot()

# guardo
# ggsave(
#   plot = g,
#   filename = "2024/s42/viz.png",
#   width = 30,
#   height = 30,
#   units = "cm")

# abro
# browseURL(glue("{getwd()}/2024/s42/viz.png"))
