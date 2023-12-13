
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#CC3A6A"
c2 <- "grey90"
c3 <- "#100A2C"
c4 <- "white"
c5 <- "#DBB1D3"
c6 <- "#100A2C"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# calificación IMDB y año
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Mountains of Christmas", family = "christmas", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 50. ",
  "Internet Movie Database</span>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} 
  {usuario}")

imdb_logo <- glue(
  "<span style='font-family:fa-brands; font-size:30pt'>",
  "&#xf2d8;</span>")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-05/readme.md")

holiday_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')
holiday_movie_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv')

# me interesa ver la distribución de calificaciones de IMDb, por género
d <- holiday_movies |> 
  select(-genres) |> 
  full_join(holiday_movie_genres, by = join_by(tconst)) |> 
  filter(title_type == "movie") |> 
  select(average_rating, genres) |> 
  mutate(genres = fct_reorder(genres, average_rating)) |> 
  drop_na() |> 
  mutate(m = median(average_rating), .by = genres) |> 
  mutate(n = n(), .by = genres)

# películas con mejor puntuación, por género
d_max <- holiday_movies |> 
  select(-genres) |> 
  full_join(holiday_movie_genres, by = join_by(tconst)) |> 
  filter(title_type == "movie") |> 
  mutate(genres = fct_reorder(genres, average_rating)) |> 
  select(primary_title, year, genres, average_rating) |> 
  drop_na() |> 
  slice_max(average_rating, n = 1, by = genres, with_ties = FALSE) |> 
  mutate(average_rating = str_replace(average_rating, "\\.", ",")) |> 
  mutate(
    titulo_label = glue(
      "<span style='font-family:ubuntu; font-size:13pt; color: white'>",
      "{primary_title}</span>")) |> 
  mutate(
    año_label = glue(
      "<span style='font-family: victor; font-size:10pt; color: grey90'>",
      "{year}</span>")) |> 
  mutate(puntaje_label = glue(
    "<span style='font-family: victor; font-size:10pt'>",
    "**{average_rating}**/10</span>")) |> 
  mutate(label = glue("{titulo_label} {año_label}<br>{puntaje_label}"))

# figura ------------------------------------------------------------------

# título y subtítulo
mi_tit <- "En Navidad veamos un documental"

mi_sub <- glue(
  "Los **documentales** presentan la mejor calificación entre las películas ",
  "navideñas. Caso opuesto,<br>las de **terror** son las peores. ",
  "Para cada género se indica la película con el mejor puntaje.")

# figura
g <- d |> 
  ggplot(aes(average_rating, genres, group = genres)) +
  geom_richtext(
    data = d_max, aes(10.2, genres, label = label), hjust = 0, fill = NA, 
    label.color = NA) +
  geom_point( shape = 20, size = 5, alpha = .3, color = c2) +
  geom_point(
    aes(x = m), shape = 18, size = 6, alpha = 1, color = c4) +
  scale_x_continuous(
    breaks = seq(1, 10, 1), expand = c(0, 0), limits = c(1, 11)) +
  labs(
    title = mi_tit, subtitle = mi_sub, x = glue("Calificación {imdb_logo}"), 
    y = NULL, caption = mi_caption) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_colorsteps(
    frame.colour = c4, ticks = TRUE, ticks.colour = c4,
    ticks.linewidth = 3/.pt, frame.linewidth = 1/.pt)) +
  theme_void() +
  theme(
    plot.margin = margin(5, 280, 5, 5),
    plot.background = element_rect(
      fill = c1, color = c2, linewidth = 3),
    plot.title = element_text(
      family = "christmas", size = 60, color = c4,
      margin = margin(t = 6)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 18, color = c6, 
      margin = margin(b = 10, l = 10)),
    plot.caption = element_markdown(
      family = "ubuntu", color = c2, size = 10, 
      margin = margin(t = 10, r = -270)),
    aspect.ratio = 1.5,
    legend.key.height = unit(12, "mm"),
    axis.title.x = element_markdown(
      size = 20, hjust = .5, family = "ubuntu", color = c2, 
      margin = margin(t = 10)),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      family = "victor", size = 14, color = c2, face = "bold"),
    axis.text.y = element_text(
      family = "ubuntu", size = 15, hjust = 1, margin = margin(0, 10, 0, 0),
      color = c6),
    panel.grid.major.x = element_line(
      color = c5, linetype = "ff", linewidth = .1))

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_50/viz.png",
  width = 30,
  height = 32,
  units = "cm")

# abro
browseURL("2023/semana_50/viz.png")
