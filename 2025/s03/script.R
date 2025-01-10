
# paquetes ----------------------------------------------------------------

# {glue}, {ggtext}, {showtext}, {tidyverse}
# se cargan automáticamente

# fuente ------------------------------------------------------------------

# colores
c1 <- "#000000"
c2 <- "#000000"
c3 <- "#000000"

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

showtext_auto()
showtext_opts(dpi = 300)

# caption
mi_caption <- caption(
  fuente1 = "FUENTE DE DATOS", col = c1, week = 3
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, 3)
tuesdata

# figura ------------------------------------------------------------------

# figura
# g <- ggplot()

# guardo
# ggsave(
#   plot = g,
#   filename = "2025/s03/viz.png",
#   width = 30,
#   height = 30,
#   units = "cm"
# )

# abro
# browseURL(paste0(getwd(), "/2025/s03/viz.png"))
