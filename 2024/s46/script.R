
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#20235B"
c2 <- "#05A3BD"
c3 <- "#EAF3FF"

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
  "Datos: <span style='color:{c2};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {46}, ",
  "<span style='font-family: jet;'>{{ISOcodes}}</span>.</span>"
)
autor <- glue("<span style='color:{c2};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c2};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 46)
countries <- tuesdata$countries

# me interesa mostrar los países que inician con las mismas letras que el 
# código ISO 3166-1 de tres letras

d <- countries |> 
  select(alpha_3, name) |> 
  mutate(
    tres = str_sub(name, 1, 3) |> toupper()
  ) |> 
  mutate(
    es_igual = alpha_3 == tres
  )

# figura ------------------------------------------------------------------

# íconos de igualdad/desigualdad
desigual <- "<span style='font-family:jet;'>&#xf098e;</span>"
igual <- "<span style='font-family:jet;'>&#xf01fc;</span>"

# subtítulo, notas de igualdad/desigualdad y
# ejemples (Argentina y Japón)
mi_subtitulo <- glue(
  "El estándar <b>ISO 3166-1 alpha-3</b> define códigos de ",
  "tres letras para identificar países.<br>",
  "A veces, esa letras coinciden con el inicio del nombre del país, en inglés."
)

nota1 <- glue("Código {igual} inicio de país")
nota2 <- glue("Código {desigual} inicio de país")

argentina <- glue("ARG {igual} ARGENTINA")
japon <- glue("JPN {igual} JAPAN")

# coordenadas de las notas
pos_x <- .007
pos_y <- .15

# figura
g <- countries::quick_map(
  data = d,
  col_border = c3,
  plot_col = "es_igual"
) +
  # igual
  annotate(
    geom = "richtext", x = I(pos_x), y = I(pos_y+.06), label = nota1, size = 4,
    hjust = 0, vjust = 1, fill = c1, family = "jet", color = c3,
    label.color = NA, label.r = unit(0, "mm")
  ) +
  # desigual
  annotate(
    geom = "richtext", x = I(pos_x), y = I(pos_y), label = nota2, size = 4,
    hjust = 0, vjust = 1, fill = c2, family = "jet", color = c3,
    label.color = NA, label.r = unit(0, "mm")
  ) +
  # Argentina
  annotate(
    geom = "richtext", x = I(.31), y = I(.12), label = argentina, size = 2.7,
    hjust = 0, vjust = 1, fill = c1, family = "jet", color = c3,
    label.color = NA, label.r = unit(0, "mm")
  ) +
  # Japón
  annotate(
    geom = "richtext", x = I(.87), y = I(.68), label = japon, size = 2.7,
    hjust = 0, vjust = 1, fill = c2, family = "jet", color = c3,
    label.color = NA, label.r = unit(0, "mm")
  ) +
  scale_fill_manual(
    breaks = c(TRUE, FALSE),
    values = c(c1, c2)
  ) +
  labs(
    subtitle = mi_subtitulo,
    caption = mi_caption
  ) +
  theme(
    plot.background = element_rect(
      fill = c3, color = c2, linewidth = 3
    ),
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 13, margin = margin(b = 25, t = 4),
      lineheight = unit(1.2, "line")
    ),
    plot.caption = element_markdown(
      family = "ubuntu", size = 9, margin = margin(b = 4, r = 4),
      lineheight = unit(1.2, "line")
    ),
    panel.background = element_rect(fill = c3, color = NA),
    legend.position = "none"
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s46/viz.png",
  width = 30,
  height = 15,
  units = "cm")

# abro
browseURL(glue("{getwd()}/2024/s46/viz.png"))
