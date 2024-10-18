
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

library(terra)

begin_coord <- vect(
  orcas, geom = c("begin_longitude", "begin_latitude"), crs = "EPSG:4326"
)

end_coord <- vect(
  orcas, geom = c("end_longitude", "end_latitude"), crs = "EPSG:4326"
)

plot(begin_coord, col = alpha("red", .6))
points(end_coord, col = alpha("blue", .6))

count(drop_na(orcas, year), year, sort = TRUE)

begin_coord$año <- factor(year(begin_coord$date))

cnd_sf <- rgeoboundaries::gb_adm0(country = "Canada") |> 
  vect() |> 
  sf::st_as_sf()

usa_sf <- rgeoboundaries::gb_adm0(country = "USA") |> 
  vect() |> 
  sf::st_as_sf()

costa <- sf::st_union(cnd_sf, usa_sf)

bb_sf <- (ext(begin_coord)*1.25) |> 
  vect(crs = "EPSG:4326") |> 
  sf::st_as_sf()

aoi_sf <- sf::st_intersection(bb_sf, costa) |> 
  sf::st_geometry()


# plot(as.lines(vect(bb_sf)), col = "red")
plot(vect(aoi_sf), axes = FALSE)
points(begin_coord, cex = .5, col = alpha("blue", .5))

cnd_crop <- crop(cnd, bb)

library(sf)

costa_sf <- rgeoboundaries::gb_adm0(country = c("USA", "Canada")) |> 
  st_geometry()

orcas_sf <- st_as_sf(begin_coord) |> 
  st_geometry()

# orcas_bb_sf <- st_bbox(orcas_sf) |> 
#   st_as_sfc() |> 
#   vect() |> 
#   ext()

# orcas_bb_sf <- vect(orcas_bb_sf*2, crs = "EPSG:4326") |> 
#   st_as_sf()

bb <- st_bbox(orcas_sf) |> 
  st_as_sfc()

costa_crop_sf <- st_intersection(costa_sf, bb)

ggplot() +
  geom_spatvector(
    data = aoi_sf, color = "darkblue", fill = "grey70"
  ) +
  geom_spatvector(
    data = orcas_sf, color = "darkred", size = 1, alpha = .6
  ) +
  theme_void(base_size = 3)





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
