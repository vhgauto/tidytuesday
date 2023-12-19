
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#E1C59A"
c2 <- "#E59A52"
c3 <- "#660B1F"
c4 <- "#094568"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# calificación IMDB
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 51. ",
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

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-19/readme.md")

holiday_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episodes.csv')
holiday_episode_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episode_genres.csv')

# me interesa ver la relación entre la popularidad de los episodios y su 
# calificación

d <- holiday_episodes |> 
  select(year, num_votes, primary_title, parent_primary_title, average_rating) |> 
  drop_na(year) |> 
  mutate(decada = year - year %% 10) |> 
  mutate(decada = factor(decada))

# destaco el episodio más popular
d_pop <- d |> 
  slice_max(order_by = num_votes, n = 1) |> 
  mutate(
    x = 8.5, y = num_votes*.8, xend = average_rating*.99, yend = num_votes) |> 
  mutate(label = glue(
    "<span style = 'family-font:ubuntu; font-size: 15pt'>",
    "**{parent_primary_title}**</span><br>",
    "<span style = 'family-font:ubuntu; font-size: 12pt'>",
    "*{primary_title}*</span>"))

# porcentaje de episodios con puntaje >= a 7
porcentaje_7 <- d |> 
  mutate(es_bueno = average_rating >= 7) |> 
  reframe(es_bueno_prop = mean(es_bueno)) |> 
  pull() |> 
  gt::vec_fmt_percent(decimals = 0)

# figura ------------------------------------------------------------------

# título y subtítulo
mi_titulo <- "Especiales navideños<br>en las series"
mi_subtitulo <- glue(
  "El **{porcentaje_7}** de los episodios de Navidad<br>",
  "de las series tiene un puntaje<br>",
  "en IMDb de **7** o superior.")

# figura
g <- ggplot(d, aes(average_rating, num_votes)) +
  geom_point(size = 5, alpha = .4, color = c3) +
  geom_curve(
    data = d_pop, aes(x, y, xend = xend, yend = yend), curvature = -.1,
    arrow = arrow(angle = 10, length = unit(.7, "line"), type = "closed"),
    color = c3) +
  geom_richtext(
    data = d_pop, aes(x-.8, y, label = label), color = c4,
    family = "ubuntu", hjust = 0, vjust = 1, fill = c1, label.color = NA) +
  annotate(
    geom = "richtext", x = 1, y = 9e4, label = mi_titulo, family = "serif",
    color = c3, size = 20, hjust = 0, vjust = 1, lineheight = unit(.8, "line"),
    label.color = NA, fill = alpha(c1, .6)) +
  annotate(
    geom = "richtext", x = 1, y = 2e4, label = mi_subtitulo, family = "ubuntu",
    color = c4, size = 7, hjust = 0, vjust = 1, lineheight = unit(.9, "line"),
    label.color = NA, fill = alpha(c1, .6)) +
  scale_x_continuous(
    breaks = seq(1, 10, 1), limits = c(.9, 10.1), expand = c(0, 0)) +
  scale_y_log10(
    breaks = 10^((seq(0, 6, 1))),
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(9, 1.1e5), expand = c(0, 0)) +
  labs(
    x = glue("Calificación {imdb_logo}"), y = glue("Popularidad {imdb_logo}"),
    caption = mi_caption) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = c1, color = c2, linewidth = 3),
    plot.caption = element_markdown(family = "ubuntu", size = 12, color = c4),
    panel.grid = element_line(
      color = c2, linetype = "ff", linewidth = .3),
    panel.grid.minor = element_blank(),
    axis.title.x = element_markdown(
      size = 20, hjust = .5, family = "ubuntu", color = c4, 
      margin = margin(t = 10)),
    axis.title.y = element_markdown(
      size = 20, hjust = .5, family = "ubuntu", color = c4, 
      margin = margin(t = 10)),
    axis.text = element_text(family = "victor", size = 15, color = c4))

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_51/viz.png",
  width = 30,
  height = 30,
  units = "cm")

# abro
browseURL("2023/semana_51/viz.png")
