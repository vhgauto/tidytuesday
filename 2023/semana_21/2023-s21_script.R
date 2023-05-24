
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(ggtext)
library(showtext)

# fuentes -----------------------------------------------------------------

# título
font_add_google(name = "Playfair Display", family = "playfair", db_cache = TRUE)
# resto del texto
font_add_google(name = "Schibsted Grotesk", family = "grotesk", db_cache = FALSE)
# cantidad (n)
font_add_google(name = "Inconsolata", family = "inconsolata", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# MetBrewer: Manet
c1 <- "#ede2cc"
c2 <- "#7ec5f4"
c3 <- "#4585b7"
c4 <- "#215e92"
c5 <- "#d29c44"

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 21</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-23/readme.md")

ardillas <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

# convierto tibble a sf
ardillas_sf <- ardillas |> 
  st_as_sf(coords = c("X", "Y")) |> 
  st_set_crs(value = 4326)

# función que permite la rotación de las geometrías
browseURL("https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations")

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)*2), 2, 2)

# rotación de los datos
ardillas_sf_rot <- ardillas_sf %>% 
  mutate(geom_rot = st_geometry(.)*rot(-36.5*pi/180)) %>%
  st_drop_geometry() %>%
  rename(geometry = geom_rot) %>%
  st_set_geometry("geometry")

# colores
colores <- MetBrewer::met.brewer(
  palette_name = "Wissing", n = 5, override.order = TRUE) |> 
  as.character()

# traducciones de las actividades
tr <- c(Chasing = "Persiguiendo", Eating = "Comiendo", Climbing = "Escalando",
        Running = "Corriendo", Foraging = "Recolectando<br>comida")

# asigno los colores a cada actividad
names(colores) <- names(tr)

# quito los nombres del vector p/usar en ggplot()
colores2 <- colores
names(colores2) <- NULL

# acomodo datos
datos <- ardillas_sf_rot |> 
  # selecciono actividades
  select(ends_with("ing")) |> 
  # tabla larga
  pivot_longer(cols = -geometry, 
               names_to = "actividad") |> 
  # sólo las actividades que sí estaban sucediendo
  filter(value == TRUE) |> 
  select(-value) |> 
  # agrego traducciones
  mutate(ac = tr[actividad]) |> 
  # agrego colores
  mutate(col = colores[actividad]) |> 
  # agrego color a las actividades (strip)
  mutate(accion = glue("<b style='color:{col};'>{ac}</span>")) |> 
  select(-actividad, -ac)

# cantidad de observaciones por actividad
n_datos <- datos |> 
  count(accion, col)

# leo .geojson con mapa de Central Park, de Open Street Map
js <- st_read("2023/semana_21/OSM_central_park.geojson")

# selecciono únicamente los cuerpos de agua
w <- js |> 
  filter(landcover_class == "water")

# rotación de los polígonos
w_rot <- w %>% 
  mutate(geom_rot = st_geometry(.)*rot(-36.5*pi/180)) %>%
  st_drop_geometry() %>%
  rename(geometry = geom_rot) %>%
  st_set_geometry("geometry")

# región de interés de Central Park
roi <- st_bbox(datos) |> 
  st_as_sfc()

# incorporo el color al roi, p/las facetas
roi2 <- distinct(datos, col, accion) |> 
  mutate(geometry = roi) |> 
  st_as_sf()

# recordo polígonos de agua a la región de interés
w_rot_crop <- st_crop(w_rot, roi)

# figura ------------------------------------------------------------------

g <- ggplot() +
  geom_sf(data = roi2, aes(color = I(col)), fill = NA, linewidth = .25) +
  # polígonos de agua
  geom_sf(data = w_rot_crop, fill = "grey60", color = "grey40") +
  # ardillas
  geom_sf(
    data = datos, aes(color = I(col)), 
    alpha = .4, show.legend = FALSE, size = 4) +
  # cantidad de observaciones
  geom_text(
    data = n_datos, aes(label = glue("n = {n}"), x = -Inf, y = -Inf, color = col),
    hjust = 0, vjust = 0, family = "inconsolata", size = 7) +
  # faceta
  facet_wrap(~ accion, nrow = 1) +
  # ejes
  labs(title = "¿Qué hacen las ardillas en **Central Park**?",
       subtitle = glue(
         "Distribución espacial de {nrow(datos)} **ardillas** en 
         Central Park de acuerdo a las actividades que estaban 
         llevando a cabo al momento de ser avistadas. En la parte 
         inferior de cada panel se indica la cantidad de 
         observaciones. Las superficies en gris representan **cuerpos 
         de agua**."),
       caption = mi_caption) +
  scale_y_continuous(limits = c(21.53485, 21.60479), expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, .0005)) +
  # coord_sf(expand = FALSE) +
  # tema
  theme_void() +
  theme(panel.background = element_rect(color = NA, linewidth = 2),
    plot.background = element_rect(
      fill = c1, color = c5, linewidth = 3),
    plot.margin = margin(5, 25, 5, 25),
    plot.title = element_markdown(
      size = 45, family = "playfair", margin = margin(10, 5, 5, -5), color = c4),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      color = c4, size = 18, margin = margin(25, 0, 25, 0)),
    plot.caption = element_markdown(
      size = 15, hjust = .5, color = c2, margin = margin(25, 0, 5, 0)),
    panel.spacing.x = unit(2, "line"),
    strip.text = element_markdown(size = 20, vjust = 1))

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_21/viz.png",
  width = 30,
  height = 40.84,
  units = "cm",
  dpi = 300)

# abro
browseURL("2023/semana_21/viz.png")

