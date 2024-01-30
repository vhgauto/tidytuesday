
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(ggpath)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#EAF3FF"
c2 <- "#123E7E"
c3 <- "#5773C0"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf")

# fuente: Victor
font_add(
  family = "victor", 
  regular = "fuente/VictorMono-ExtraLight.ttf",
  bold = "fuente/VictorMono-VariableFont_wght.ttf",
  italic = "fuente/VictorMono-ExtraLightItalic.ttf")

# título
font_add_google(name = "Miltonian", family = "miltonian")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {5}. ",
  "Groundhog Day Predictions, groundhog-day.com.</span>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} 
  {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 5)

groundhogs <- tuesdata$groundhogs

# me interesan las imágenes de las marmotas únicamente
d <- groundhogs |> 
  filter(is_groundhog) |> 
  select(image, name) |> 
  arrange(name) |> 
  mutate(name = str_remove(name, " the Groundhog")) |> 
  mutate(nro = row_number()) |> 
  mutate(label = glue(
    "<b style='font-family:victor; font-size:8pt; color: {c3}'>{nro}.</b>",
    "{name}")) |> 
  mutate(label = fct_inorder(label))

# figura ------------------------------------------------------------------

# etiquetas de los paneles (facet_wrap)
nombres <- as.character(d$label)
names(nombres) <- d$nro

# título y subtítulo
mi_title <- "Marmotas pronosticadoras"

mi_subtitle <- glue(
  "El **Día de la Marmota** consiste en dos creencias populares,",
  "una sobre el **clima** (soleado o nublado) y otra sobre **animales que",
  "hibernan** (si ya están despiertos o no). La tradición moderna en esencia",
  "tiene sus raíces en el clima: si es un día soleado, la **marmota** ve su",
  "sombra, lo que significa un *invierno* más largo. Si está nublado, se",
  "adelanta la *primavera*.",
  .sep = " ") |> 
  str_wrap(width = 65) |> 
  str_replace_all(fixed("\n"), "<br>")

mi_subtitle_tbl <- tibble(
  nro = 33) |>
  mutate(subtitulo = mi_subtitle)

# figura
g <- ggplot(d, aes(0, 0, path = image)) +
  geom_from_path(width = 1) +
  geom_richtext(
    data = mi_subtitle_tbl, aes(0, 0, label = subtitulo), hjust = 0, color = c2,
    family= "ubuntu", size = 4.5, inherit.aes = FALSE, fill = NA, 
    label.color = NA) +
  facet_wrap(vars(nro), ncol = 5, labeller = as_labeller(nombres)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = mi_title, caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = c1, color = c3, linewidth = 3),
    plot.margin = margin(r = 20, l = 20, t = 10, b = 10),
    plot.title = element_text(
      family = "miltonian", size = 61, color = c2, hjust = .5,
      margin = margin(b = 15)),
    plot.caption = element_markdown(family = "ubuntu", color = c3, size = 13),
    panel.spacing.x = unit(1, "line"),
    panel.spacing.y = unit(1.1, "line"),
    strip.text = element_markdown(
      color = c2, family = "ubuntu", size = 12, face = "bold",
      hjust = 0)
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s05/viz.png",
  width = 30,
  height = 47.51,
  units = "cm")

# abro
browseURL("2024/s05/viz.png")
