
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# colores, Nord, afternoon praire
c1 <- "#009B9F"
c2 <- "#C75DAA"
c3 <- "#7DC5C7"
c4 <- "#DEA9CC"
c5 <- "grey90"
c6 <- "grey30"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# cantidad, eje vertical
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Playfair Display SC", family = "playfair")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 35.  U.S. Copyright Office Fair Use Index</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-29/readme.md")

fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')

# máxima cantidad de categorías por observación
# sirve p/definir la cantidad de columnas en las que voy a separar 'categories'
fair_use_cases |> 
  select(categories, fair_use_found) |> 
  mutate(cantidad = str_count(categories, ";")) |> 
  slice_max(cantidad, n = 1, with_ties = FALSE) |> 
  pull(cantidad)
# son 4, así que son 4+1 = 5 categorías

# 10 categorías más frecuentes
top_cat <- fair_use_cases |> 
  select(categories, fair_use_found) |> 
  separate_wider_delim(
    cols = categories,
    delim = "; ", 
    names = glue("a{1:5}"), 
    too_few = "align_start") |> 
  pivot_longer(
    cols = starts_with("a"),
    names_to = "n",
    values_to = "cat"
  ) |> 
  select(-n) |> 
  drop_na(cat) |> 
  mutate(cat = str_to_lower(cat)) |> 
  count(cat, sort = TRUE) |> 
  slice_max(order_by = n, n = 10) |> 
  pull(cat)

# traducción
top_cat_trad <- c(
  "texto", "film/audiovisual", "educación/becas/investigación", "fotografía",
  "internet/digitalización", "revisión/comentario", "reporte de noticias",
  "parodia/sátira", "pintura/dibujo/gráfico", "música"
)

names(top_cat_trad) <- top_cat

# resolución VERDADERO/FALSO por las 10 categorías más frecuentes
d <- fair_use_cases |> 
  select(categories, fair_use_found) |> 
  separate_wider_delim(
    cols = categories,
    delim = "; ", 
    names = glue("a{1:5}"), 
    too_few = "align_start") |> 
  pivot_longer(
    cols = starts_with("a"),
    names_to = "n",
    values_to = "cat"
  ) |> 
  select(-n) |> 
  drop_na(cat) |> 
  mutate(cat = str_to_lower(cat)) |> 
  filter(cat %in% top_cat) |> 
  count(cat, fair_use_found) |> 
  mutate(n = if_else(fair_use_found == FALSE, -n, n)) |> 
  pivot_wider(names_from = fair_use_found, values_from = n) |> 
  rename(falso = `FALSE`, verdadero = `TRUE`) |> 
  mutate(cat = top_cat_trad[cat]) |> 
  mutate(cat = str_to_sentence(cat)) |> 
  mutate(cat = fct_reorder(cat, verdadero))

# totales
d_tot_v <- sum(d$verdadero)
d_tot_f <- abs(sum(d$falso))
d_tot <- d_tot_v + d_tot_f

# figura ------------------------------------------------------------------

# título y subtítulo
g_tit <- "legítimo <span style='font-size:40pt'>v</span> no legítimo"

g_sub <- glue("El derecho de autor permite el uso de obras protegidas en caso de 
**uso legítimo**. Sin embargo,<br>muchas veces eso lleva a conflictos que se resuelven
en la Justicia. Se muestran las 10 categorías<br>más frecuentes de {d_tot} casos en EEUU,
y si el veredicto fue de uso <span style='color:{c2};'>**legítimo**</span> ({d_tot_v}) o 
<span style='color:{c1};'>**no legítimo**</span> ({d_tot_f}).")

# tamaño de los puntos
t_grande <- 7
t_pequeño <- 6

# figura
g <- ggplot(d, aes(x = 0, y = cat, yend = cat)) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = .1, linetype = "8f") +
  geom_segment(aes(xend = falso), color = c6) +
  geom_segment(aes(xend = verdadero), color = c6) +
  geom_point(aes(x = falso), size = t_grande, color = c5) +
  geom_point(aes(x = verdadero), size = t_grande, color = c5) +
  geom_point(aes(x = falso, color = "f"), size = t_pequeño) +
  geom_point(aes(x = verdadero, color = "v"), size = t_pequeño) +
  scale_x_continuous(
    breaks = seq(-50, 50, 25),
    limits = c(-55, 50),
    labels = abs(seq(-50, 50, 25))) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_color_manual(values = c(c1, c2)) +
  coord_cartesian(ylim = c(.75, 10.25)) +
  labs(
    x = "Cantidad de casos",
    y = NULL,
    color = NULL,
    title = g_tit,
    subtitle = g_sub,
    caption = mi_caption) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.background = element_rect(fill = "grey20", color = c5, linewidth = 3),
    plot.margin = margin(19, 19, 10, 19),
    plot.title.position = "plot",
    plot.title = element_markdown(
      size = 55, family = "playfair", color = "white", hjust = .5),
    plot.subtitle = element_markdown(
      color = c5, family = "ubuntu", size = 15, margin = margin(5, 70, 25, 70),
      hjust = .5, lineheight = 1.25),
    plot.caption = element_markdown(
      color = c4, family = "ubuntu", size = 12, margin = margin(10, 0, 5, 0)),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = .05, color = "grey40"),
    panel.grid.minor.x = element_line(linewidth = .05, color = "grey30"),
    axis.text.y = element_text(color = c3, family = "ubuntu", size = 15),
    axis.text.x = element_text(color = c4, family = "victor", size = 12),
    axis.title.x = element_text(
      family = "ubuntu", color = c4, size = 20, margin = margin(15, 0, 0, 0))
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_35/viz.png",
  width = 30,
  height = 30,
  units = "cm")

# abro
browseURL("2023/semana_35/viz.png")
