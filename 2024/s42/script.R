
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(terra)
library(tidyterra)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#3961FF"
c2 <- "#9D6227"
c3 <- "darkred"
c4 <- "#E4E4E4"
c5 <- "black"
c6 <- "grey30"

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
  "Datos: <span style='color:{c2};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {42}, ",
  "<b>Center for Whale Research</b>.</span>"
)
autor <- glue("<span style='color:{c2};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c2};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 42)
orcas <- tuesdata$orcas

# me interesa los sitios de avistamiento de orcas sobre el relieve de la región

# convierto datos a vector
orcas_sf <- vect(
  orcas, geom = c("begin_longitude", "begin_latitude"), crs = "EPSG:4326"
)

# región de interés
usa_sf <- rgeoboundaries::gb_adm0(country = "USA")
cnd_sf <- rgeoboundaries::gb_adm0(country = "Canada")
aoi_sf <- rbind(usa_sf, cnd_sf) |> 
  vect()

# obtengo el relieve de la región de interés
bb <- (ext(orcas_sf)*1.2) |> 
  vect(crs = "EPSG:4326")

dem <- elevatr::get_elev_raster(
  locations = sf::st_as_sf(bb),
  z = 10,
  clip = "bbox"
) |> 
  rast()

# guardo ráster
writeRaster(dem, "2024/s42/dem.tif", overwrite = FALSE)
dem <- rast("2024/s42/dem.tif")

# figura ------------------------------------------------------------------

# subtítulo
mi_subtitulo <- glue(
  "Posiciones de avistamiento de <b style='color: {c3};'>orcas</b> en el ",
  "<b style='color: {c1}; '>Mar de los Salish</b>.<br>Entre 2017 a 2024 se ",
  "contabilizaron <b>{nrow(orcas_sf)}</b> registros."
)

# coordenadas
lon <- -125:-123
lon_label <- glue("{lon}°")

lat <- 48:50
lat_label <- glue("+{lat}°")

# figura
g <- ggplot() +
  geom_spatraster(
    data = dem, show.legend = FALSE, maxcell = size(dem)
  ) +
  geom_spatvector(
    data = orcas_sf, color = c3, alpha = .4, size = 3
  ) +
  scale_fill_hypso_c(palette = "gmt_globe") +
  scale_x_continuous(
    breaks = lon,
    labels = lon_label
  ) +
  scale_y_continuous(
    breaks = lat,
    labels = lat_label
  ) +
  coord_sf(expand = FALSE) +
  labs(subtitle = mi_subtitulo, caption = mi_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = c4, color = c1, linewidth = 3
    ),
    plot.margin = margin(t = 17, r = 20, b = 15, l = 20),
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 20, hjust = .5, margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      family = "ubuntu", color = c1, size = 15, lineheight = unit(1.2, "line"),
      margin = margin(b = 5, r = 0, t = 15)
    ),
    panel.background = element_rect(fill = NA, color = c5, linewidth = 1),
    panel.grid.major = element_line(
      color = c6, linetype = "FF", linewidth = .2
    ),
    panel.ontop = TRUE,
    axis.text = element_text(
      family = "jet", margin = margin(t = 5, r = 5), size = 15
    ),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks = element_line(color = c5, linewidth = .7)
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s42/viz.png",
  width = 30,
  height = 34,
  units = "cm"
)

# abro
browseURL(glue("{getwd()}/2024/s42/viz.png"))
