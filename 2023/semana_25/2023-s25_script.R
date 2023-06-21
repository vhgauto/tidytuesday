
# paqute ------------------------------------------------------------------

library(tidyverse)
library(glue)
library(tidytext)
library(ggwordcloud)
library(ggtext)
library(showtext)

# fuentes -----------------------------------------------------------------

# colores
c1 <- "#341648"
c2 <- "#05A3BD"
c3 <- "#FFD352"
c4 <- "#621F5E"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu", db_cache = FALSE)

# fuente del título
font_add("starseeds", "fuente/Starseeds.ttf")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 25</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-06-20/readme.md")

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

# figura ------------------------------------------------------------------

# palabras comunes p/remover del listado de las descripciones (summary)
palabras_comunes <- c(tm::stopwords("en"), 1:100, "seen", "saw", "pd", "nuforc")

# obtengo las palabras individuales
ovni_palabras <- ufo_sightings |> 
  # conservo summary
  select(summary) |> 
  # separo p/palabra
  unnest_tokens(word, summary) |> 
  # remuevo las palabras comunes
  anti_join(tibble(word = palabras_comunes), by = join_by(word)) |> 
  # arreglo algunas palabras manualmente
  mutate(word = case_match(
    word,
    "lights" ~ "light",
    "objects" ~ "object", 
    "shaped" ~ "shape",
    .default = word)
  )

# top de palabras más frecuentes
n_ovni <- 40

# función p/generar la paleta de colores
f_color <- colorRampPalette(MetBrewer::met.brewer(palette_name = "Egypt"))

# colores
ovni_color <- f_color(n = n_ovni)

# tibble con las 'n_top_tit' palabras más frecuentes
ovni_conteo <- ovni_palabras |> 
  # cuento la cantidad de palabras
  count(word, sort = TRUE) |> 
  # conservo sólo el top
  slice_max(n, n = n_ovni, with_ties = FALSE) |> 
  # agrego color, aleatoriamente
  mutate(color = sample(ovni_color)) |> 
  # agrego el ángulo, con 40% de probabilidad de rotación de 90°
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) |> 
  # convierto a mayúscula
  mutate(word = str_to_upper(word))

# rotación de las palabras
browseURL("https://lepennec.github.io/ggwordcloud/articles/ggwordcloud.html#word-cloud-and-rotation")

# cantidad de reportes, p/el subtítulo
nro <- nrow(ufo_sightings) |> 
  format(big.mark = ".", decimal.mark = ",")

# figura de un OVNI
browseURL("https://www.svgrepo.com/svg/480707/ufo-sucking-up-something")

ovni <- "2023/semana_25/ovni.png"

# imagen p/tag
ovni_tag <- glue("<img src='{ovni}' width='40'></img>")

# figura
g_nube_tit <- ggplot(data = ovni_conteo) + 
  # palabras
  geom_text_wordcloud_area(
    aes(label = word, size = n, 
        color = I(color),
        angle = angle),
    seed = 2023, 
    shape = "square", 
    area_corr = TRUE) +
  scale_size_area(max_size = 80) +
  coord_cartesian(clip = "off") +
  labs(
    title = "OVNIS",
    subtitle = glue(
      "Nube de palabras a partir de las descripciones aportadas
      por quienes avistaron OVNIs.<br>Se analizaron {nro} testimonios en todo 
      el mundo."),
    tag = ovni_tag,
    caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = c1, color = c4, linewidth = 3),
    plot.title = element_markdown(
      size = 150, family = "starseeds", color = c3, hjust = .5,
      margin = margin(5, 0, 15, 0)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      color = c2, size = 20, family = "ubuntu", hjust = .5),
    plot.tag.position = c(.9, .83),
    plot.tag = element_markdown(),
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = c2, hjust = 1, family = "ubuntu", size = 13),
    plot.margin = margin(5, 14, 5, 14)
  )

# guardo
ggsave(
  plot = g_nube_tit,
  filename = "2023/semana_25/viz.png",
  width = 30, height = 35,
  units = "cm", dpi = 300)

# abro
browseURL("2023/semana_25/viz.png")
  
