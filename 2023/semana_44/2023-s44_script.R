
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(tidytext)
library(ggVennDiagram)
library(showtext)
library(glue)
library(ggtext)
library(ggpath)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#1F78B4"
c2 <- "#E31A1C"
c3 <- "#6A3D9A"
c4 <- "#FF7F00"
c5 <- "#A6CEE3"
c6 <- "#CAB2D6"
c7 <- "#FB9A99"
c8 <- "#FEE0B6"
c9 <- "white"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# título
font_add_google(name = "Bebas Neue", family = "bebas")
# palabras verdadero/falso
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")
font_add("fa-regular", "icon/Font Awesome 6 Free-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 44, ",
  "**Snopes.com**</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-31/readme.md")

horror_articles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-31/horror_articles.csv')

# me interesan las palabras más comunes entre notas falsas y verdaderas, y con
# un diagrama de Venn para ver cuales coinciden

# palabras comunes (inglés)
sw <- tm::stopwords()

p <- horror_articles |> 
  unnest_tokens(input = title, output = palabra) |> 
  filter(rating %in% c("false", "true")) |> 
  count(palabra, rating, sort = TRUE) |> 
  filter(!palabra %in% sw) |> 
  filter(!str_detect(palabra, "'")) |> 
  filter(palabra != "deaths") |> 
  slice_max(order_by = n, n = 15, by = rating, with_ties = FALSE)

# creo una lista para el diagrama de Venn
p_lista <- list(
  FALSO = p |> filter(rating == "false") |> pull(palabra),
  VERDADERO = p |> filter(rating == "true") |> pull(palabra))

venn <- Venn(p_lista)
data <- process_data(venn)

# obtengo las coordenadas del centro de cada porción del diagrama de Venn
vv <- venn_region(data) |> 
  unnest(item) |> 
  mutate(cent = st_centroid(geometry)) |> 
  st_drop_geometry() |> 
  st_as_sf() %>%
  mutate(eje_x = st_coordinates(.)[, 1]) %>%
  mutate(eje_y = st_coordinates(.)[, 2])

# agrego un movimiento horizontal aleatorio, 
# por cada región del diagrama de Venn
set.seed(2023)
vv_falso <- vv |> 
  filter(name == "FALSO") %>%
  mutate(eje_y = seq(300, 700, length.out = nrow(.))) |> 
  mutate(eje_x = eje_x + runif(n(), -40, 40)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("eje_x", "eje_y"))
  
vv_verdadero <- vv |> 
  filter(name == "VERDADERO") %>%
  mutate(eje_y = seq(300, 700, length.out = nrow(.))) |> 
  mutate(eje_x = eje_x + runif(n(), -40, 40)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("eje_x", "eje_y"))

vv_vf <- vv |> 
  filter(name == "FALSO..VERDADERO") %>%
  mutate(eje_y = seq(400, 600, length.out = nrow(.))) |> 
  mutate(eje_x = eje_x + runif(n(), -20, 20)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("eje_x", "eje_y"))

# corrijo HIV
vv2 <- bind_rows(vv_falso, vv_verdadero, vv_vf) |> 
  mutate(item = str_to_sentence(item)) |> 
  mutate(name = str_to_upper(name)) |> 
  mutate(item = if_else(item == "Hiv", "HIV", item))


# figura ------------------------------------------------------------------

# título y subtítulo
mi_tit <- glue(
  "Historias de terror<br>",
  "*¿verdaderas o falsas?*")

mi_sub <- glue(
  "El sitio web **Snopes.com** cataloga sus notas en<br>",
  "*verdaderas* o *falsas*, ¿pero qué las diferencian?<br>",
  "Se tomaron las 15 palabras más comunes de los<br>",
  "titulares y se armó el siguiente diagrama de Venn.")

# logo
logo <- "2023/semana_44/logo.png"
logo_tbl <- tibble(x = 210, y = -Inf, label = glue("<img src={logo} width='100'>"))

# figura
g <- ggplot() +
  # regiones
  geom_sf(
    aes(fill = name), data = venn_region(data), color = NA,
    show.legend = FALSE) +
  # línea punteada
  geom_sf(
    size = 2, lty = "dashed", color = c9, data = venn_setedge(data),
    show.legend = FALSE) +
  # título de cada región
  geom_sf_text(
    aes(label = name), data = venn_setlabel(data), family = "ubuntu", 
    color = c(c1, c2), size = 10, nudge_y = -20) +
  # verdadero/falso
  geom_sf_text(
    data = vv2, aes(label = item, color = name), show.legend = FALSE, size = 7,
    family = "victor", fontface = "bold") +
  geom_richtext(
    data = logo_tbl, aes(x, y, label = label), vjust = 0, fill = NA, 
    label.color = NA) +
  annotate(
    geom = "richtext", x = 510, y = 800, hjust = 0, vjust = -.45, label = mi_sub,
    size = 6, family = "ubuntu", fill = c4, label.color = NA, 
    color = c9) +
  scale_fill_manual(values = c(c5, c6, c7)) +
  scale_color_manual(values = c(c1, c3, c2)) +
  labs(title = mi_tit, caption = mi_caption) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(2.8, 0, 2.8, 0),
    plot.background = element_rect(
      fill = c8, color = c3, linewidth = 3),
    plot.title = element_markdown(
      size = 50, family = "bebas", hjust = 0, color = c4,
      margin = margin(15, 0, 5, 15)),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, color = c1, margin = margin(5, 10, 5, 0))
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_44/viz.png",
  width = 30,
  height = 27,
  units = "cm")

# abro
browseURL("2023/semana_44/viz.png")
