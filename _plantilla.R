
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
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {semana_numero}, ",
  "<b>FUENTE DE DATOS</b>.</span>"
)
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
icon_bsky <- glue("<span style='font-family:jet;'>&#xe28e;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {icon_bsky} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(año, semana_numero)

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
