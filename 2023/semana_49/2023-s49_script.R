
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(patchwork)
library(ggflags) # https://github.com/jimjam-slam/ggflags
library(ggh4x)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "grey80"
c2 <- "grey60"
c3 <- "black"
c4 <- "white"
c5 <- "grey30"
c6 <- "grey90"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# números, fechas, ranking
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# números, fechas, ranking
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 49. ",
  "Our World in Data</span>")
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

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-05/readme.md")

life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv') |> 
  janitor::clean_names()

# me interesa ver la evolución de otros países comparados con Argentina,
# en dos períodos, 1950 y 2020

# rango de edades en 1950 y 2020, para Argentina
rango <- life_expectancy |> 
  filter(code == "ARG" & (year == 1950 | year == 2020)) |> 
  pull(life_expectancy)

# factor para aplicar a las edades de Argentina, y obtener otros países con 
# espectativas de vida cercanas, en 1950 y 2020
factor <- .01

# valores de edad en 1950
min_inf <- rango[1]*(1-factor)
min_sup <- rango[1]*(1+factor)

life_expectancy |> 
  filter(year == 1950 & between(life_expectancy, min_inf, min_sup)) |> 
  drop_na()

# países a comparar en 1950
p_1950 <- c("Argentina", "Bulgaria", "Malta", "Spain", "Slovakia")

# valores de edad en 2020
max_inf <- rango[2]*(1-factor)
max_sup <- rango[2]*(1+factor)

life_expectancy |> 
  filter(year == 2020 & between(life_expectancy, max_inf, max_sup)) |> 
  drop_na() |> 
  print(n = 30)

# países a comparar en 2020
p_2020 <- c("Argentina", "Turkey", "Hungary", "Malaysia", "Saudi Arabia")

# tibble para 1950
tbl_1950 <- life_expectancy |> 
  filter(year >= 1950 & entity %in% p_1950) |> 
  mutate(estado = "inicio")

# tibble para 2020
tbl_2020 <- life_expectancy |> 
  filter(year >= 1950 & entity %in% p_2020) |> 
  mutate(estado = "final")

# combino y selecciono décadas
d <- bind_rows(tbl_1950, tbl_2020) |> 
  mutate(estado = fct(estado, levels = c("inicio", "final"))) |> 
  filter(year %% 10 == 0)

# figura ------------------------------------------------------------------

# ubicación de las banderas
d_bandera <- d |> 
  filter(
    (year == 2020 & estado == "inicio") | (year == 1960 & estado == "final"))

# países involucrados
paises <- sort(unique(d_bandera$entity))

# traducción de los nombres de los países 
paises_trad <- c(
  "Argentina", "Bulgaria", "Hungría", "Malasia", "Malta", "Arabia Saudita",
  "Eslovaquia", "España", "Turquía")|> str_c("    ")

# obtengo el código ISO2C de los países, para poder usar {ggflags}
iso2 <- countrycode::codelist |> 
  select(contains("iso")) |> 
  filter(iso.name.en %in% paises) |> 
  pull(iso2c) |> 
  str_to_lower()

# vector con nombres
names(iso2) <- paises

# agrego los códigos de país y muevo a la derecha las banderas del 1er panel
d_bandera_tbl <- d_bandera |> 
  mutate(bandera = iso2[entity]) |> 
  mutate(year = if_else(estado == "inicio", year+4, year))

# ejes verticales
eje_vertical <- list(
  scale_y_continuous(
    limits = c(60, 85), expand = c(0, 0), breaks = seq(60, 90, 5)),
  scale_y_continuous(
    limits = c(40, 80), expand = c(0, 0), breaks = seq(40, 80, 5)))

# rango de edades, Argentina 1950-2020
rango_label <- gt::vec_fmt_number(
  rango, decimals = 1, sep_mark = "", dec_mark = ",")

# descripciones
desc_tbl <- tibble(
  x = c(1975, 2020),
  y = c(rango[1], 62),
  label = c(
    glue(
      "Cuatro países que en **1950**<br>",
      "tenían casi la misma esperanza<br>",
      "de vida que **Argentina**,<br>",
      "alrededor de {rango_label[1]} años."),
    glue(
      "Cuatro países con<br>",
      "similar esperanza de<br>",
      "vida que **Argentina** en<br>",
      "**2020**, aproximadamente<br>",
      "{rango_label[2]} años.")),
  estado = c("inicio", "final")) |> 
  mutate(estado = fct(estado, levels = c("inicio", "final"))) |> 
  mutate(hjust = c(0, 1), vjust = c(0, 1)) |> 
  mutate(xend = c(1953, 2020), yend = c(rango[1], 75))

# edades en 1875 y 1975
exp_100 <- life_expectancy |> 
  filter(code == "ARG" & (year == 1875 | year == 1975)) |> 
  arrange(life_expectancy) |> 
  pull(life_expectancy) |> 
  gt::vec_fmt_number(decimals = 1, sep_mark = "", dec_mark = ",")

# subtítulo
mi_sub <- glue(
  "En 1875, la espectativa de vida al nacer en **Argentina** ",
  "era de {exp_100[1]} años.<br>",
  "100 años después, en 1975, se duplicaba alcanzando los ",
  "{exp_100[2]} años.<br>")

# figura
g <- ggplot(d, aes(year, life_expectancy, color = entity, fill = entity)) +
  # puntos y líneas de expectativa de vida
  geom_line(show.legend = FALSE, linewidth = 2, alpha = 1) +
  geom_point(size = 2, shape = 23, show.legend = FALSE, stroke = 1,alpha = 1) +
  geom_point(size = 1.7, show.legend = FALSE, color = c1, shape = 18) +
  # contorno de las banderas
  geom_point(
    data = d_bandera_tbl, aes(year, life_expectancy), size = 14, 
    show.legend = FALSE) +
  # banderas
  geom_flag(data = d_bandera_tbl, aes(country = bandera), size = 12) +
  # aclaraciones
  geom_richtext(
    data = desc_tbl, aes(x, y, label = label, hjust = hjust, vjust = vjust), 
    inherit.aes = FALSE, fill = c2, label.color = NA, family = "ubuntu",
    color = c4, size = 5) +
  # flechas
  geom_curve(
    data = desc_tbl, aes(x, y, xend = xend, yend = yend), inherit.aes = FALSE,
    curvature = .1, arrow = arrow(angle = 20,length = unit(.3, "line")),
    color = c2, arrow.fill = c2) +
  # punto en la esquina de las aclaraciones
  geom_point(
    data = desc_tbl, aes(x, y), inherit.aes = FALSE, size = 2, color = c2) +
  facet_wrap(vars(estado), nrow = 1, scales = "free") +
  facetted_pos_scales(y = eje_vertical) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_country(labels = paises_trad, breaks = iso2) +
  MetBrewer::scale_color_met_d(name = "Redon") +
  MetBrewer::scale_fill_met_d(name = "Redon") +
  coord_cartesian(clip = "off") +
  labs(
    country = NULL, x = NULL, y = NULL, caption = mi_caption, 
    subtitle = mi_sub) +
  guides(country = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(
    plot.margin = margin(6.5, 50, 6.5, 40),
    plot.background = element_rect(
      fill = c1, color = c5, linewidth = 3),
    plot.subtitle = element_markdown(
      size = 22, family = "ubuntu", lineheight = unit(1.3, "line")),
    plot.caption.position = "plot",
    plot.caption = element_markdown(color = c4, family = "ubuntu", size = 12),
    panel.background = element_blank(),
    panel.spacing.x = unit(2,"line"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = c6, linewidth = .3),
    aspect.ratio = 2,
    legend.position = "bottom",
    legend.background = element_rect(fill = c5, color = NA),
    legend.key = element_blank(),
    legend.text = element_text(family = "ubuntu", color = c4, size = 10),
    strip.text = element_blank(),
    axis.text.y = element_text(family = "victor", size = 12, color = c5),
    axis.text.x = element_text(family = "bebas", size = 20, color = c5),
    axis.ticks.x = element_blank())

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_49/viz.png",
  width = 30,
  height = 31.5,
  units = "cm")

# abro
browseURL("2023/semana_49/viz.png")
