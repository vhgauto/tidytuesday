
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(ggsvg)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#2B579A"
c2 <- colorspace::lighten(c1, .5)
c3 <- colorspace::lighten(c1, .8)
c4 <- "#A41620"
c5 <- "white"

# colores de las medallas
md <- "#FFD700"
mp <- "#BABABA"
mb <- "#BF8970"

pal_medallas <- c(
  Gold = md,
  Silver = mp,
  Bronze = mb
) |> 
  fct()

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

# bebas neue
font_add(
  family = "bebas", 
  regular = "fuente/BebasNeue-Regular.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c1};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {32}, ",
  "120 years of Olympic history.</span>"
)
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 32)
olympics <- tuesdata$olympics

# me interesa la participación de Argentina y las medallas recibidas
# selecciono únicamente los JJOO post 1920

# aspecto de tiles
largo <- 13
alto <- 3
filas <- 10

# bandera
bandera <- "<img src='2024/s32/arg.png' width=30>"

# participación de Argentina en los JJOO
arg_part <- olympics |>
  filter(season == "Summer" & team == "Argentina" & year > 1920) |> 
  distinct(event, year) |> 
  arrange(year) |> 
  count(year) |> 
  mutate(
    x_part = map(n, ~ rep(1:largo, length.out = .x))
  ) |> 
  mutate(
    y_part = map(n, ~ rep(1:largo, length.out = .x, each = largo))
  ) |> 
  unnest(cols = c(x_part, y_part)) |> 
  mutate(year = glue("{year} {bandera}"))

# medallas recibidas por Argentina en los JJOO
arg_med <- olympics |>
  filter(season == "Summer" & team == "Argentina" & year > 1920) |> 
  drop_na(medal) |> 
  distinct(event, year) |> 
  arrange(year) |> 
  count(year) |> 
  mutate(
    x_med = map(n, ~ rep(1:largo, length.out = .x))
  ) |> 
  mutate(
    y_med = map(n, ~ rep(1:largo, length.out = .x, each = largo))
  ) |> 
  unnest(cols = c(x_med, y_med)) |> 
  mutate(year = glue("{year} {bandera}"))

f_icon <- function(x) {
  glue(
    "<span style='font-family:jet; color: {x}; font-size: 20px'>",
    "&#xf0764;</span>")
}

# cantidad de medallas recibidas en los JJOO (oro, plata, bronce)
arg_r <- olympics |>
  filter(season == "Summer" & team == "Argentina" & year > 1920) |> 
  drop_na(medal) |> 
  distinct(event, year, medal) |> 
  mutate(col = pal_medallas[medal]) |> 
  arrange(year) |> 
  count(year, medal, col) |> 
  arrange(year, col) |> 
  mutate(
    l = glue("{f_icon(col)} {n}")
  ) |> 
  reframe(
    label = str_flatten(l, collapse = " "),
    .by = year
  ) |> 
  mutate(year = glue("{year} {bandera}"))

# figura -----------------------------------------------------------------

# logo de los JJOO, agrego a caption
jjoo <- "<img src='2024/s32/jjoo.png' width=250>"
mi_caption_jjoo <- glue("{jjoo}<br>{mi_caption}")

# subtítulo
mi_subtitle <- glue(
  "Desempeño de <b style='color: {c1}'>Argentina</b> en los",
  "<b style='font-family: jet; color: {c4}'>Juegos Olímpicos</b>.",
  "<br>{f_icon(c1)} indica participación en un evento.",
  "{f_icon(c4)} señala la obtención de una medalla.",
  .sep = " "
)

# figura
g <- ggplot(arg_part, aes(x_part, y_part)) +
  geom_tile(fill = c1, color = c2, linewidth = 2) +
  geom_tile(
    data = arg_med, aes(x = x_med, y = y_med), fill = c4, , color = c2, 
    linewidth = 2
  ) +
  # medallas DORADA, PLATEADA, BRONZE
  geom_richtext(
    data = arg_r, aes(largo+.5, filas+1.5, label = label), family = "jet",
    hjust = 1, fill = c5,, label.color = NA, vjust = 1, size = 4,
    # label.padding = unit(1, "mm"), 
    label.padding = unit(c(1, 1, .1, 1), "mm"),
    label.r = unit(0, "mm")
  ) +
  facet_wrap(vars(year), ncol = 4) +
  coord_equal(
    expand = FALSE, xlim = c(.25, largo+.75),  ylim = c(.25, filas+1.8),
    clip = "off") +
  labs(subtitle = mi_subtitle, caption = mi_caption_jjoo) +
  theme_void() +
  theme(
    plot.margin = margin(l = 30.3, r = 30.3, t = 10),
    plot.background = element_rect(fill = c3, color = c2, linewidth = 3),
    plot.title = element_markdown(),
    plot.subtitle = element_textbox_simple(
      family = "ubuntu", size = 22, color = "black", margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = "ubuntu", size = 15, color = c4, lineheight = unit(1.3, "line"),
      margin = margin(t = -130, b = 10)
    ),
    panel.background = element_rect(fill = c2, color = NA),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1, "line"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_markdown(
      size = 28, color = c4, family = "bebas", margin = margin(t = 5, b = 2),
      hjust = 0
    )
)

# guardo
ggsave(
  plot = g,
  filename = "2024/s32/viz.png",
  width = 30,
  height = 45,
  units = "cm"
)

# abro
browseURL(glue("{getwd()}/2024/s32/viz.png"))
