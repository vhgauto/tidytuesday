
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
mi_caption <- caption(fuente1 = "FUENTE DE DATOS", col = c1)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(año, semana_numero)
tuesdata

# figura ------------------------------------------------------------------

# figura
# g <- ggplot()

# guardo
# ggsave(
#   plot = g,
#   filename = "semana_carpeta/viz.png",
#   width = 30,
#   height = 30,
#   units = "cm"
# )

# abro
# browseURL(paste0(getwd(), "/semana_carpeta/viz.png"))
