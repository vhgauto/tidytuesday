
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(ggrepel)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "grey20"
c2 <- "grey90"
c3 <- "black"

col <- MetBrewer::met.brewer(name = "Tara")

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

# bebas neue
font_add(
  family = "bebas",
  regular = "fuente/BebasNeue-Regular.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{col[1]};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {26}, ",
  "TidyRainbow.</span>")
autor <- glue("<span style='color:{col[1]};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{col[1]};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 26)
movies <- tuesdata$lgbtq_movies

# me interesa contar los géneros de las películas

# código de los géneros de las películas
# https://www.themoviedb.org/talk/5daf6eb0ae36680011d7e6ee
genero_tbl <- tribble(
  ~genero, ~genre_ids,
  "acción" ,28,
  "aventura" ,12,
  "animación" ,16,
  "comedia" ,35,
  "crimen" ,80,
  "documental" ,99,
  "drama" ,18,
  "familiar" ,10751,
  "fantasía" ,14,
  "historia" ,36,
  "terror" ,27,
  "musical" ,10402,
  "misterio" ,9648,
  "romática" ,10749,
  "ciencia ficción" ,878,
  "película para TV" ,10770,
  "thriller" ,53,
  "bélica" ,10752,
  "western" ,37
)

d <- movies |> 
  mutate(año = year(release_date)) |> 
  mutate(genre_ids = str_remove(genre_ids, "\\[")) |> 
  mutate(genre_ids = str_remove(genre_ids, "\\]")) |> 
  mutate(genre_ids = str_remove_all(genre_ids, " ")) |> 
  separate_longer_delim(cols = genre_ids, delim = ",") |> 
  mutate(genre_ids = as.numeric(genre_ids)) |> 
  inner_join(genero_tbl, by = join_by(genre_ids)) |> 
  count(genero, sort = TRUE) |> 
  mutate(color = rep(col, length.out = 19)) |> 
  mutate(label = glue("<span style='color:{color}'>{genero}</span>")) |> 
  mutate(label = fct_reorder(label, n))

# figura ------------------------------------------------------------------

# subtítulos
mi_subtitulo <- glue(
  "A partir de {nrow(movies)} películas con temática ",
  "<b style='color:{col[1]}'>LGBTQ</b>, ",
  "se encuentra que los géneros<br>",
  "más populares son los <b style='color:{col[4]}'>dramas</b> ",
  "y <b style='color:{col[4]}'>romances</b>. En el extremo ",
  "opuesto se ubican<br>",
  "los <b style='color:{col[4]}'>westerns</b> y películas ",
  "<b style='color:{col[4]}'>bélicas</b>."
)

# LGBTQ
arcoiris <- tibble(
  color = gglgbtq::palette_lgbtq("rainbow"),
  y = 5:10,
  x = 1000
)

# figura
g <- ggplot(d, aes(n, label, color = color)) +
  geom_segment(
    aes(x = 0, xend = n, yend = label, color = color), linewidth = 1,
    linetype = "11") +
  geom_point(
    size = 14, shape = 18) +
  geom_point(
    size = 3, shape = 19, color = c1) +
  geom_tile(
    data = arcoiris, aes(x, y, fill = color), color = c1, linewidth = 2,
    width = .19) +
  scale_x_log10(limits = c(10, NA)) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(
    y = NULL, x = "# de películas", caption = mi_caption, 
    subtitle = mi_subtitulo) +
  theme_void() +
  theme(
    aspect.ratio = 1.4,
    plot.margin = margin(r = 24.3, l = 24.3),
    plot.background = element_rect(fill = c1, color = col[5], linewidth = 3),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      color = c2, family = "ubuntu", size = 22,
      margin = margin(l = 10, b = 10, t = 20), lineheight = unit(1.3, "line")),
    plot.caption = element_markdown(
      color = col[4], family = "ubuntu", size = 15, 
      margin = margin(t = 20, b = 10, r = 10), lineheight = unit(1.3, "line")),
    panel.grid.major.x = element_line(
      color = c2, linewidth = .1, linetype = "FF"),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      family = "ubuntu", size = 22, color = c2, margin = margin(t = 10),
      hjust = 0),
    axis.text.y = element_markdown(
      family = "bebas", size = 30, hjust = 1, margin = margin(r = 10)),
    axis.text.x = element_text(
      family = "jet", size = 20, color = c2, margin = margin(t = 10))
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s26/viz.png",
  width = 30,
  height = 39,
  units = "cm"
)

# abro
browseURL("2024/s26/viz.png")
