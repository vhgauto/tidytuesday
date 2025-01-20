
# paquetes ----------------------------------------------------------------

# {glue}, {ggtext}, {showtext}, {tidyverse}
# se cargan automáticamente

# fuente ------------------------------------------------------------------

# colores
c1 <- "grey5"
c2 <- "grey40"
c3 <- "grey70"
c4 <- "grey90"

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
mi_caption <- caption(
  fuente1 = "Himalayan Database", col = c4, week = 3
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, 3)
exped <- tuesdata$exped_tidy

# me interesan la cantidad de muertes que ocurrieron en cada año

# sumo las muertes de integrantes y personal de la expedición
d <- exped |> 
  select(PEAKID, YEAR, contains("DEATH")) |> 
  mutate(
    m = MDEATHS + HDEATHS
  ) |> 
  reframe(
    s = sum(m),
    .by = YEAR
  )

# figura ------------------------------------------------------------------

# subtítulo, nota y símbolo de calavera
mi_subtitulo <- glue(
  "Cantidad de muertes en expediciones al <b style='color: {c4}'>Himalaya</b>"
)

nota <- str_wrap(
  "Las muertes incluyen a los integrantes de la expedición y al personal 
  contratado.",
  30
)

calavera <- "<span style='font-family:jet;'>&#xf068c;</span>"

# gradiente de color para la figura
relleno <- grid::linearGradient(
  colours = colorRampPalette(c(c1, c2, c3, c4), bias =.25)(10),
  x1 = 0,
  y1 = 0,
  x2 = 0,
  y2 = 1
)

# figura
g <- ggplot(d, aes(YEAR, s)) +
  geom_area(fill = relleno) +
  annotate(
    geom = "richtext", x = 2020, y = seq(5, 25, 5), label = calavera, size = 10,
    color = c3, fill = NA, label.color = NA, hjust = 1
  ) +
  annotate(
    geom = "text", x = 2020.1, y = 24, label = nota, color = c3, size = 6, 
    hjust = 0, lineheight = .8
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    expand = c(0, 0),
    breaks = seq(0, 25, 5)
  ) +
  labs(subtitle = mi_subtitulo, caption = mi_caption) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(l = 0, r = 20),
    plot.background = element_rect(fill = c1, color = NA),
    plot.subtitle = element_markdown(
      family = "ubuntu", color = c3, size = 30, hjust = .5,
      margin = margin(b = 30, t = 20)
    ),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, color = c3, margin = margin(b = 10, t = 20),
      lineheight = 1.5
    ),
    panel.grid.major.y = element_line(
      color = c2, linewidth = .2, linetype = "FF"
    ),
    axis.text.y = element_markdown(
      color = c3, family = "jet", size = 15, margin = margin(r = 25)
    ),
    axis.text.x = element_text(
      color = c3, family = "bebas", size = 25, margin = margin(t = 15)
    )
  )

# guardo
ggsave(
  plot = g,
  filename = "2025/s03/viz.png",
  width = 30,
  height = 33,
  units = "cm"
)

# abro
browseURL(paste0(getwd(), "/2025/s03/viz.png")) 
