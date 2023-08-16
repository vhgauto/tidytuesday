
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# colores, Nord
c1 <- "#AD8CAE"
c2 <- "#EDDAEB"
c3 <- "#222B4C"
c4 <- "white"
c5 <- "black"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# números, porcentajes, cantidades
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# eje horizontal, temporadas
font_add_google(name = "Tilt Prism", family = "tilt", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 33. *spam* dataset en {{kernlab}}</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-15/readme.md")

spam <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

# me interesan los elementos relativos al dinero: money, dollar, n000
# cómo se diferencian los e-mails que son o NO son spam

# tiene que tener los elementos: porcentajes > 0
d <- spam |> 
  select(dollar, money, n000, yesno) |> 
  pivot_longer(cols = -yesno, names_to = "param", values_to = "valor") |> 
  filter(valor != 0)

# figura ------------------------------------------------------------------

# líneas de referencia horizontales
lineas_tbl <- tibble(
  y = c(120, 60, 80, 40, 80, 40),
  param = c("dollar", "dollar", "money", "money", "n000", "n000")
)

# cantidad de datos, para agregar en las esquinas
d_cantidad <- d |> 
  count(yesno, param) |> 
  inner_join(
    slice_max(
      lineas_tbl, 
      order_by = y, 
      n = 3), 
    by = join_by(param))

# cantidad de e-mails que SÍ son spam
d_yes <- d_cantidad |> 
  filter(yesno == "y") |> 
  mutate(label = glue("n = {n}"))

# cantidad de e-mails que NO son spam
d_no <- d_cantidad |> 
  filter(yesno == "n") |> 
  mutate(label = glue("n = {n}"))

# etiquetas para los strips
etiq <- c(
  dollar = "**$**",
  money = "**money**",
  n000 = "**000**"
)

# colores para representar SÍ/NO es spam
yesno_colores <- c(SÍ = alpha("white", .9), NO = alpha("black", .9))

# título y subtítulo
# palabra SPAM en blanco y negro
spam_blanco <- "<b style='color:white;'>SPAM</b>"
spam_negro <- "<b style='color:black;'>SPAM</b>"

mi_title <- glue("{spam_blanco}{spam_negro}{spam_blanco}{spam_negro}")

mi_subt <- glue(
  "¿Qué caracteriza al **spam**? Por lo visto, hablar de plata. 
  Al analizar {nrow(d)} e-mails, se
  contabilizó el porcentaje de términos relativos al dinero: 
  el signo **$**, la palabra **money** y tres ceros **000**.
  Se indican las cantidades (n) de e-mails en cada panel.")

# figura
g <- d |> 
  mutate(yesno = if_else(yesno == "y", "SÍ", "NO")) |> 
  mutate(yesno = factor(yesno, levels = c("SÍ", "NO"))) |>
  ggplot(aes(valor, fill = yesno, color = yesno)) +
  # horizontales
  geom_hline(
    data = lineas_tbl, aes(yintercept = y), 
    linetype = 3, color = c2) +
  # cantidad, eje vertical, línea horizontal
  geom_text(
    data = lineas_tbl, aes(x = 0, y = y, label = y), inherit.aes = FALSE,
    hjust = 0, vjust = 0, nudge_y = 1, family = "victor", color = c2, size = 3) +
  # línea de frecuencia
  geom_freqpoly(
    binwidth = .075, alpha = .9) +
  # cantidades, n, YES
  geom_text(
    data = d_yes, aes(2, y, label = label), inherit.aes = FALSE,
    hjust = 0, vjust = 0, color = yesno_colores["SÍ"], nudge_y = +1,
    family = "victor", size = 4, fontface = "bold") +
  # cantidades, n, NO
  geom_text(
    data = d_no, aes(2, y, label = label), inherit.aes = FALSE,
    hjust = 0, vjust = 1, color = yesno_colores["NO"], nudge_y = -1,
    family = "victor", size = 4, fontface = "bold") +
  # facetas
  facet_wrap(vars(param), scales = "free", labeller = as_labeller(etiq)) +
  # escalas
  scale_x_log10(
    breaks = c(.01, .1, 1, 10),
    labels = c("0,01%", "0,1%", "1%", "10%")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(
    values = yesno_colores, 
    labels = c("<b style='color:white;'>SÍ</b>", "<b style='color:black;'>NO</b>")) +
  coord_cartesian(clip = "off") +
  labs(
    title = mi_title,
    subtitle = mi_subt,
    x = "Porcentaje del total de caracteres del e-mail",
    y = "Cantidad",
    color = "¿Es spam?    ",
    caption = mi_caption) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 4))
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(
      fill = c1, color = c2, linewidth = 3),
    plot.margin = margin(15, 20, 10, 15),
    plot.title = element_markdown(size = 76, family = "tilt"),
    plot.subtitle = element_textbox_simple(
      family = "ubuntu", size = 14, color = c3),
    plot.caption = element_markdown(
      hjust = 1, color = c2, margin = margin(10, 0, 0, 0)),
    axis.text.x = element_text(color = c2, family = "victor"),
    axis.text.y = element_blank(),
    axis.title.x = element_markdown(
      color = c3, margin = margin(10, 0, 5, 0), family = "ubuntu", size = 17),
    axis.title.y = element_markdown(color = c3, family = "ubuntu", size = 17),
    axis.ticks.x = element_line(color = c2),
    panel.spacing.x = unit(1, "cm"),
    panel.grid = element_blank(),
    strip.text = element_markdown(
      color = c3, family = "ubuntu", size = 15,
      margin = margin(5, 0, 30, 0)),
    legend.position = "top",
    legend.margin = margin(10, 0, 5, 0),
    legend.title = element_text(family = "ubuntu", color = c3, size = 14),
    legend.text = element_markdown(family = "ubuntu", size = 12)
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_33/viz.png",
  width = 30,
  height = 18.64,
  units = "cm")

# abro
browseURL("2023/semana_33/viz.png")
