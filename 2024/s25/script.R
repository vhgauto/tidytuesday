
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#FCFFC9"
c2 <- "#E8C167"
c3 <- "#AA5D00"
c4 <- "#913640"
c5 <- "#1D0B14"
c6 <- "#FFFFFF"

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

# bebas
font_add(
  family = "bebas", 
  regular = "fuente/BebasNeue-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c2};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {25}, ",
  "Feriados en EE.UU., <b>Wikipedia</b></span>")
autor <- glue("<span style='color:{c2};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c2};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 25)

federal_holidays <- tuesdata$federal_holidays

# me interesa indicar el año y si las fechas son inamovibles

# estilos de texto y símbolo para marcar las fechas inamovibles
estilo_1 <- glue(
  "<span style='font-family:bebas; font-size:40px; color:{c1}'>")
estilo_2 <- glue(
  "<span style='font-family:jet; font-size:20px; color:{c2}'>")
simbolo_label <- glue(
  "<sup style='font-family:jet; font-size:25px; color:{c2}'>†</sup>")

d <- federal_holidays |> 
  arrange(desc(year_established)) |> 
  mutate(nro = row_number()) |> 
  mutate(official_name = str_wrap(official_name, width = 20)) |> 
  mutate(official_name = str_replace_all(official_name, "\\n", "<br>")) |> 
  mutate(
    simbolo = if_else(
      date_definition == "fixed date",
      simbolo_label,
      ""
    )
  ) |> 
  mutate(label = glue(
    "{estilo_1}{official_name}</span>{simbolo}<br>",
    "{estilo_2}{year_established}</span>")) |> 
  mutate(nro = if_else(nro == 1, .8, nro))

# figura ------------------------------------------------------------------

# subtítulo
mi_subtitulo <- glue(
  "Feriados nacionales en <b>EE.UU.</b><br>",
  "Se muestra el año en que se<br>",
  "estableció la fecha como tal.<br><br>",
  "Los feriados con fecha<br>",
  "inamovible se indican con {simbolo_label}.")

# figura
g <- ggplot(d, aes(year_established, nro, label = label)) +
  geom_richtext(
    size = 5, hjust = 0, vjust = 1, nudge_x = 4, nudge_y = .1, label.color = NA,
    fill = NA, label.padding = unit(0, "mm"), lineheight = unit(1.6, "line")) +
  geom_segment(aes(y = 0, yend = nro), color = c4, linewidth = 2) +
  geom_point(
    shape = 23, color = c4, fill = c5, size = 5, stroke = 2) +
  geom_point(
    shape = 20, color = c6, size = 1) +
  annotate(
    geom = "richtext", x = I(1), y = I(.9), label = mi_subtitulo, color = c6,
    family = "ubuntu", size = 8, label.color = NA, fill = NA, hjust = 1,
    vjust = 1
  ) +
  coord_cartesian(
    clip = "off", xlim = c(1868, 2070), ylim = c(0, 11.2), expand = FALSE) +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1.3,
    plot.margin = margin(l = 30, r = 40, t = 12),
    plot.background = element_rect(fill = c5, color = c6, linewidth = 3),
    plot.caption = element_markdown(
      family = "ubuntu", size = 14, color = c6, 
      margin = margin(t = 15, b = 10))
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s25/viz.png",
  width = 30,
  height = 38,
  units = "cm")

# abro
browseURL("2024/s25/viz.png")
