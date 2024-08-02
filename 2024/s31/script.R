
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#A62F00"
c2 <- "#FFF178"
c3 <- "#6AD5E8"
c4 <- "#3C4B99"
c5 <- "#E7E1EF"
c6 <- "#F7F4F9"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf")

# monoespacio & íconos
font_add(
  family = "jet", 
  regular = "fuente/JetBrainsMonoNLNerdFontMono-Regular.ttf")

# Bebas Neue
font_add(
  family = "bebas",
  regular = "fuente/BebasNeue-Regular.ttf"
)

font_add_google(
  name = "Send Flowers",
  family = "flor",
  db_cache = FALSE
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c1};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {31}, ",
  "Internet Movie Database.</span>")
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 31)

summer_movie_genres <- tuesdata$summer_movie_genres
summer_movies <- tuesdata$summer_movies

# me interesan los puntajes de las películas, por género, e indicar las
# mejores puntuadas

d <- summer_movies |> 
  separate_longer_delim(
    cols = genres,
    delim = ","
  ) |> 
  drop_na()

# traducciones de los géneros
trad <- tribble(
  ~genres, ~generos,
  "Drama"      ,"Drama",
  "Comedy"     ,"Comedia",
  "Fantasy"    ,"Fantasía",
  "Romance"    ,"Romance",
  "Crime"      ,"Crimen",
  "Film-Noir"  ,"Cine negro",
  "History"    ,"Historia",
  "Music"      ,"Música",
  "Musical"    ,"Musical",
  "Action"     ,"Acción",
  "War"        ,"Bélica",
  "Documentary","Documental",
  "Animation"  ,"Animación",
  "Mystery"    ,"Misterio",
  "Thriller"   ,"Suspenso",
  "Family"     ,"Familia",
  "Sport"      ,"Deporte",
  "Adventure"  ,"Aventura",
  "Sci-Fi"     ,"Ciencia ficción",
  "Horror"     ,"Terror",
  "Biography"  ,"Biografía",
  "Western"    ,"Western",
  "Short"      ,"Cortometraje",
  "Talk-Show"  ,"Entrevistas"
)

# incorporo las traducciones y mantengo las más comunes
d_trad <- inner_join(d, trad, by = join_by(genres)) |> 
  mutate(generos = fct_reorder(generos, average_rating, max)) |> 
  mutate(n = n(), .by = generos) |> 
  filter(n > 5)

# obtengo la película con mejor puntaje p/c género y aplico estilo
d_top <- d_trad |> 
  slice_max(
    order_by = average_rating,
    by = generos,
    with_ties = FALSE
  ) |> 
  select(primary_title, year, generos, average_rating) |> 
  mutate(primary_title = str_wrap(primary_title, 40)) |> 
  mutate(primary_title = str_replace_all(primary_title, fixed("\n"), "<br>")) |> 
  mutate(
    primary_title = str_replace(
      primary_title,
      "Summer",
      glue("<b style='color:{c1}'>Summer</b>"))) |> 
  mutate(
    primary_title = str_replace(
      primary_title,
      "summer",
      glue("<b style='color:{c1}'>summer</b>"))) |> 
  mutate(
    primary_title = glue(
      "{primary_title}<br><span style='font-family: jet; color:{c4}'>",
      "({year})</span>")
)

mi_titulo <- "Películas veraniegas"
mi_subtitulo <- glue(
  "Género y puntaje de películas que tienen <span style='font-family: jet;",
  "color:{c1}'>summer</span> en el título.<br>",
  "Para cada género se indica la película con mejor valoración."
)

logo_imdb <- glue(
  "<span style='font-family:jet; font-size:90px'>&#xf2d8;</span>")
titulo_x <- glue("Puntaje<br>{logo_imdb}")

{
  g <- ggplot(d_trad, aes(average_rating, generos)) +
  geom_point(
    aes(fill = average_rating), alpha = .8, size = 5, shape = 23, 
    color = "black", stroke = .3
  ) +
  geom_richtext(
    data = d_top, aes(average_rating, generos, label = primary_title), size = 5,
    hjust = 0, nudge_x = .3, lineheight = unit(1, "line"), family = "ubuntu",
    label.color = NA, fill = c5, label.r = unit(0, "mm"),
    label.padding = unit(.16, "line")
  ) +
  scale_x_continuous(
    limits = c(.9, 10.1),
    breaks = 1:10,
    expand = c(0, 0)) +
  scale_fill_gradient2(
    low = c1,
    mid = c2,
    high = c3,
    midpoint = 5.5,
    limits = range(d_trad$average_rating)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = mi_titulo, subtitle = mi_subtitulo, x = titulo_x, y = NULL,
    caption = mi_caption) +
  theme_minimal() +
  theme(
    aspect.ratio = 2.2,
    plot.background = element_rect(
      fill = c6, color = c3, linewidth = 3
    ),
    plot.margin = margin(r = 263.6, l = 20),
    plot.title = element_text(
      family = "flor", size = 60, color = c1, margin = margin(t = 20),
      face = "bold"
    ),
    plot.title.position = "panel",
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 23, margin = margin(b = 15, t = 5),
      lineheight = unit(1.2, "line")
    ),
    plot.caption = element_markdown(
      size = 13, color = c4, margin = margin(r = -210, b = 10)
    ),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "FF", color = "grey30", linewidth = .1
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_markdown(
      color = c4, size = 20, family = "ubuntu", margin = margin(t = 15)
    ),
    axis.text.x = element_text(
      family = "jet", size = 15, margin = margin(t = 5), color = c4
    ),
    axis.text.y = element_text(family = "bebas", size = 30, color = c4),
    axis.ticks = element_blank(),
    legend.position = "none"
  ); ggsave(
    plot = g,
    filename = "2024/s31/viz.png",
    width = 30,
    height = 43,
    units = "cm")

}

browseURL(glue("{getwd()}/2024/s31/viz.png"))

# 

# figura ------------------------------------------------------------------

# figura
# g <- ggplot()

# guardo
# ggsave(
#   plot = g,
#   filename = "2024/s31/viz.png",
#   width = 30,
#   height = 30,
#   units = "cm")

# abro
# browseURL("2024/s31/viz.png")
