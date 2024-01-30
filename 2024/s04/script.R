
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "grey90"
c2 <- "grey70"
c3 <- "grey40"
c4 <- "grey20"
c5 <- "darkviolet"
c6 <- "white"

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

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {4}. ",
  "UK Office for National Statistics.</span>")
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

tuesdata <- tidytuesdayR::tt_load(2024, 4)

english_education <- tuesdata$english_education

# me interesa la relación entre los ingresos y el nivel educativo

# descarga de los datos
# https://www.ons.gov.uk/visualisations/dvc2651b/fig3/datadownload.xlsx

# datos de logros educativos y privación en los ingresos, para ciudades de UK
privacion <- readxl::read_xlsx("2024/s04/datadownload.xlsx", skip = 4) |> 
  rename(
    "ciudad" = `Town name`,
    "educacion" = `Educational attainment score`,
    "ingreso" = `Income deprivation score`) |> 
  janitor::clean_names()

# tamaño de las ciudades
tamaño_ciudad <- english_education |> 
  select(town11cd, size_flag) |> 
  mutate(size_flag = str_to_lower(size_flag)) |> 
  mutate(size_flag = str_remove(size_flag, " towns")) |> 
  mutate(size_flag = if_else(
    str_detect(size_flag, "london"),
    "london",
    size_flag))

# combino ambos datos
d <- inner_join(privacion, tamaño_ciudad, by = join_by(town11cd))

# ciudades min/max para señalar
ciudad_max <- d |> 
  slice_max(educacion, n = 1) |> 
  mutate(ciudad = str_remove(ciudad, " BUASD")) |> 
  mutate(
    x = educacion*.85,
    y = ingreso*1.005,
    xend = educacion*.98,
    yend = ingreso,
    hjust = 1.1)

ciudad_min <- d |> 
  slice_min(ingreso, n = 1) |> 
  mutate(ciudad = str_remove(ciudad, " BUA")) |> 
  mutate(
    x = educacion*.85,
    y = ingreso,
    xend = educacion*.98,
    yend = ingreso,
    hjust = -.1)

ciudades <- rbind(ciudad_max, ciudad_min)

# figura ------------------------------------------------------------------

# rangos de datos de ambos ejes, para las flechas
rango_educacion <- diff(range(d$educacion))
rango_ingreso <- diff(range(d$ingreso))

# flechas y texto explicativo
flecha_x <- tibble(
  xend = max(d$educacion),
  y = min(d$ingreso),
  yend = min(d$ingreso)) |> 
  mutate(
    x = xend - .13*rango_educacion,
    label = "Mayores logros",
    angle = 0)

flecha_y <- tibble(
  yend = max(d$ingreso),
  x = -10,
  xend = min(d$educacion)) |> 
  mutate(
    y = yend - .13*rango_ingreso,
    label = "Menor privación",
    angle = 90)

flechas <- rbind(flecha_x, flecha_y)

# subtítulo
mi_subtitle <- glue(
  "En **Reino Unido**, Las ciudades con mayor nivel educativo tienen bajos ",
  "niveles de privación de ingresos.") |> 
  str_wrap(width = 30) |> 
  str_replace_all(pattern = fixed("\n"), "<br>")

# figura
g <- ggplot(d, aes(educacion, ingreso)) +
  # puntos
  geom_point(alpha = .6, size = 5, shape = 17, color = c5) +
  # aclaración en flechas
  geom_richtext(
    data = flechas, aes(x, y, label = label, angle = angle), family = "ubuntu",
    inherit.aes = FALSE, vjust = 0, hjust = 0, fill = c1, label.color = NA, 
    color = c3) +
  # flechas
  geom_segment(
    data = flechas, aes(x, y, xend = xend, yend = yend), inherit.aes = FALSE,
    arrow = arrow(angle = 15, length = unit(4, "mm"), type = "closed"), 
    linewidth = 1, color = c3) +
  # ciudades min/max
  geom_curve(
    data = ciudades, aes(x, y, xend = xend, yend = yend), curvature = -.1, 
    arrow = arrow(angle = 12, length = unit(3, "mm"), type = "closed"), 
    color = c2) +
  geom_text(
    data = ciudades, aes(x, y, label = ciudad, hjust = hjust), family = "ubuntu",
    size = 4, color = c3) +
  # subtítulo
  annotate(
    geom = "richtext", x = 4, y = .75, label = mi_subtitle, fill = c6, 
    color = c5, label.color = NA, hjust = 0, vjust = 1, family = "ubuntu",
    size = 6, label.r = unit(0, "mm")) +
  scale_x_continuous(
    limits = c(-11, 13), expand = c(0, 0), breaks = seq(-12, 12, 2)) +
  scale_y_continuous(
    limits = c(.65, 1), expand = c(0, 0),
    labels = scales::label_number(
      big.mark = "", decimal.mark = ",", accuracy = .1)) +
  labs(
    x = "Logros educativos", y = "Privación en ingresos", caption = mi_caption) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(t = 20.5, r = 25, l = 20, b = 10),
    plot.background = element_rect(fill = c1, color = c2, linewidth = 3),
    plot.caption = element_markdown(color = c4, family = "ubuntu", size = 11),
    panel.background = element_rect(fill = c1, color = NA),
    panel.grid.major = element_line(linetype = "FF", linewidth = .3, color = c2),
    axis.title = element_text(family = "ubuntu", size = 18, color = c4),
    axis.text = element_text(family = "victor", size = 15, color = "black"),
    axis.text.y = element_text(vjust = 0))

# guardo
ggsave(
  plot = g,
  filename = "2024/s04/viz.png",
  width = 30,
  height = 30,
  units = "cm")

# abro
browseURL("2024/s04/viz.png")
