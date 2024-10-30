
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidytext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#DF9ED4"
c2 <- "#55092A"
c3 <- "#960B35"
c4 <- "#FD8386"

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

# Creepster
font_add_google(
  name = "Creepster",
  family = "creepster"
)

# Girassol
font_add_google(
  name = "Moul",
  family = "moul"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c4};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {44}, ",
  "<b>Internet Movie Database</b>.</span>"
)
autor <- glue("<span style='color:{c4};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c4};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 44)
monster_movies <- tuesdata$monster_movies

# me interesan cómo se formas los títulos de las películas, las palabras
# anteriores y posteriores a MONSTER

# divido las títulos de las películas entre antes y después de la palabra
# MONSTRUO
mosters_pre_post <- select(monster_movies, primary_title) |> 
  mutate(primary_title = toupper(primary_title)) |> 
  mutate(primary_title = str_replace_all(primary_title, "-", "_")) |> 
  mutate(
    div = str_replace(
      primary_title, "(.*)MONSTERS|MONSTER(.*)", "\\1-\\2"
    )
  ) |> 
  separate_wider_delim(
    cols = div,
    delim = "-",
    names = c("pre_palabra", "post_palabra")
  )

# selecciono las palabras más frecuentes y remuevo las stopwords
# divido los conjuntos en pre y post palabras
n_top <- 15

stopwords <- filter(stop_words, lexicon == "SMART") |> 
  pull(word)

d_pre <- mosters_pre_post |> 
  select(pre_palabra) |> 
  unnest_tokens(
    output = "palabras",
    input = pre_palabra
  ) |> 
  filter(!palabras %in% stopwords) |> 
  count(palabras, sort = TRUE) |> 
  slice_max(order_by = n, n = n_top, with_ties = FALSE) |> 
  pull(palabras) |> 
  toupper() |> 
  str_replace("_", "-")

d_post <- mosters_pre_post |> 
  select(post_palabra) |> 
  unnest_tokens(
    output = "palabras",
    input = post_palabra
  ) |> 
  filter(!palabras %in% stopwords) |> 
  count(palabras, sort = TRUE) |> 
  slice_max(order_by = n, n = n_top, with_ties = FALSE) |> 
  pull(palabras) |> 
  toupper() |> 
  str_replace("_", "-")

# figura ------------------------------------------------------------------

# MONSTER
label_monster <- str_split("MONSTER", "")[[1]] |> 
  str_flatten(collapse = "<br>")

# parámetros de la figura
tamaño_label <- 13
tamaño_monster <- 38
ancho_linea <- .3

mi_subtitulo <- glue(
  "Términos más frecuentes antes y después de la palabra 
  <span style='font-family: creepster; color: {c1};'>MONSTER</span> en 
  <b>{nrow(monster_movies)}</b> películas."
)

# figura
g <- ggplot() +
  # líneas
  annotate(
    geom = "segment", x = -5, xend = 0, y = 1:15, yend = 7.7, color = c3,
    linewidth = ancho_linea, linetype = 2
  ) +
  annotate(
    geom = "segment", x = 5, xend = 0, y = 1:15, yend = 7.7, color = c3,
    linewidth = ancho_linea, linetype = 2
  ) +
  # pre
  annotate(
    geom = "richtext", x = -5, y = 1:15, label = d_pre, hjust = 1, color = c2,
    family = "creepster", size = tamaño_label, fill = c1, label.color = c3,
    label.size = unit(.7, "mm"), label.padding = unit(3, "mm"),
    label.r = unit(3, "mm")
  ) +
  # post
  annotate(
    geom = "richtext", x = 5, y = 1:15, label = d_post, hjust = 0, color = c2,
    family = "creepster", size = tamaño_label, fill = c1, label.color = c3,
    label.size = unit(.7, "mm"), label.padding = unit(3, "mm"),
    label.r = unit(3, "mm")
  ) +
  # MONSTER
  annotate(
    geom = "richtext", x = 0, y = 7.5, label = label_monster, hjust = .42,
    family = "moul", size = tamaño_monster, lineheight = unit(.9, "line"),
    fill = NA, label.color = NA, color = c1
  ) +
  scale_x_continuous(limits = c(-12, 12)) +
  scale_y_continuous(limits = c(0, 15)) +
  labs(subtitle = mi_subtitulo, caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(r = 5, l = 5, t = 1.5),
    plot.background = element_rect(color = c3, fill = c2, linewidth = 3),
    plot.subtitle = element_markdown(
      family = "ubuntu", color = c4, size = 20, hjust = .5,
      margin = margin(t = 15, b = 20)
    ),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, color = c1,
      margin = margin(b = 10, r = 10, t = -20)
    )
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s44/viz.png",
  width = 30,
  height = 32,
  units = "cm")

# abro
browseURL(glue("{getwd()}/2024/s44/viz.png"))
