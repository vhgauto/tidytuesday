
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(fontawesome)
library(gender)
library(showtext)
library(glue)
library(ggtext)

# fuente ------------------------------------------------------------------

# colores
# scales::show_col(tayloRswift::swift_palettes$lover)
c1 <- "#88C0D0"
c2 <- "#81A1C1"
c3 <- "#5E81AC"
c4 <- "grey90"
# c4 <- "#FFD1D7"
# c5 <- "grey10"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# algoritmos, eje vertical
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# eje horizontal, años
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = FALSE)
# título
font_add_google(name = "Sarabun", family = "sarabun")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")
font_add("fa-regular", "icon/Font Awesome 6 Free-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 45. ",
  "MIT Election Data and Science Lab, ",
  "**Harvard Dataverse**</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-11-07/readme.md")

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')

# me interesa el género de los candidatos y la proporción en el tiempo

# función para obtener el 1er nombre, y si es una sola letra, voy al segundo
get_first_name <- function(name) {
  name_parts <- str_split(name, " ")[[1]]
  if (nchar(name_parts[1]) == 1) {
    return(name_parts[2])
  } else {
    return(name_parts[1])
  }
}

# primer nombre de cada candidato, agrupado por año
# se agrupan, por año, los mismos nombres
d <- house |> 
  filter(!writein) |> 
  select(year, candidate) |> 
  mutate(primer = map(.x = candidate, ~ get_first_name(name = .x))) |> 
  unnest(primer) |> 
  count(primer, .by = year) |> 
  arrange(.by) |> 
  rename(year = .by)

d_ipums <- d |> 
  mutate(genero = map(
    .x = primer,
    \(x) gender(x, countries = "United States", method = "ipums")$gender))

d_ssa <- d |> 
  mutate(genero = map(
    .x = primer,
    \(x) gender(x, countries = "United States", method = "ssa")$gender))

d_napp <- d |> 
  mutate(genero = map(
    .x = primer,
    \(x) gender(x, method = "napp")$gender))

# d |> 
#   rename(year = .by) |> 
#   mutate(genero = as.character(genero)) |> 
#   filter(genero != "logical(0)") |> 
#   mutate(g = genero == "male") |> 
#   arrange(year) |> 
#   reframe(p = sum(g*n)/sum(n), .by = year) |>
#   write_tsv("2023/semana_43/d3.tsv")

# a1 <- read_tsv("2023/semana_43/d.tsv") # ipums
# a2 <- read_tsv("2023/semana_43/d1.tsv") # ssa
# # d2 <- read_tsv("2023/semana_43/d2.tsv") # kantrowitz
# a3 <- read_tsv("2023/semana_43/d3.tsv") # napp
# 
# e <- bind_rows(
#   a1 |> mutate(tipo = "ipums"),
#   a2 |> mutate(tipo = "ssa"),
#   # d2 |> mutate(tipo = "kantrowitz"),
#   a3 |> mutate(tipo = "napp")) |> 
#   mutate(p = 1 - p)
# 
# ggplot(e, aes(year, p, color = tipo)) +
#   geom_line(show.legend = FALSE) +
#   # geom_text(aes(label = "★"), size = 6.5, color = "grey30") +
#   geom_text(aes(label = "\uf005"), family = "fa-regular", size = 1.5, color = "grey30") +
#   geom_text(aes(label = "\uf005"), family = "fa-regular", size = 1)
  # scale_x_continuous(limits = c(1975, 2025), expand = c(0, 0))


# ff <- bind_rows(
#   d_ipums |> 
#     # rename(year = .by) |>
#     mutate(genero = as.character(genero)) |>
#     filter(genero != "logical(0)") |>
#     mutate(g = genero == "male") |>
#     arrange(year) |>
#     reframe(p = sum(g*n)/sum(n), .by = year) |> 
#     mutate(tipo = "ipums"),
#   d_ssa |> 
#     # rename(year = .by) |>
#     mutate(genero = as.character(genero)) |>
#     filter(genero != "logical(0)") |>
#     mutate(g = genero == "male") |>
#     arrange(year) |>
#     reframe(p = sum(g*n)/sum(n), .by = year) |> 
#     mutate(tipo = "ssa"),
#   d_napp |> 
#     # rename(year = .by) |>
#     mutate(genero = as.character(genero)) |>
#     filter(genero != "logical(0)") |>
#     mutate(g = genero == "male") |>
#     arrange(year) |>
#     reframe(p = sum(g*n)/sum(n), .by = year) |> 
#     mutate(tipo = "napp"),
# ) |> mutate(p = 1 - p)
# 
# ggplot(ff, aes(year, p, color = tipo)) +
#   geom_line(show.legend = FALSE) +
#   # geom_text(aes(label = "★"), size = 6.5, color = "grey30") +
#   geom_text(aes(label = "\uf005"), family = "fa-regular", size = 1.5, color = "grey30") +
#   geom_text(aes(label = "\uf005"), family = "fa-regular", size = 1)
# scale_x_continuous(limits = c(1975, 2025), expand = c(0, 0))





e <- bind_rows(
  d_ipums |> mutate(tipo = "ipums"), 
  d_napp |> mutate(tipo = "napp"), 
  d_ssa |> mutate(tipo = "ssa")) |> 
  mutate(genero = as.character(genero)) |>
  filter(genero != "logical(0)") |>
  mutate(g = genero == "male") |>
  reframe(p = sum(g*n)/sum(n), .by = c(year, tipo)) |> 
  mutate(p = 1 - p)

e |> 
  write_tsv("2023/semana_45/datos.tsv")

e <- read_tsv("2023/semana_45/datos.tsv")

algoritmo_label <- e |> 
  filter(year == max(e$year))

p <- expand.grid(
  x = seq(1980, 2020, 10),
  y = seq(.1, .3, .05)) |> 
  as_tibble()

g <- ggplot(e, aes(year, p, color = tipo)) +
  geom_line(alpha = .4, linewidth = 1.5, show.legend = FALSE) +
  geom_text(aes(label = "\uf005"), family = "fa-solids", size = 4) +
  geom_point(size = 1, color = "white", shape = 20) +
  geom_text(
    data = algoritmo_label, aes(year, p, label = str_to_upper(tipo)),
    hjust = 0, nudge_x = .6, family = "victor", size = 6) +
  annotate(geom = "point", x = p$x, y = p$y, color = "#306489", shape = 18) +
  annotate(
    geom = "text", x = 1976, y = .31, hjust = 0, vjust = 0, label = glue(
      "La participación femenina en el Congreso de\n EEUU crece, pero aún ",
      "queda camino por recorrer"), family = "sarabun", size = 10, color = c4) +
  scale_y_continuous(
    breaks = seq(.05, .35, .05), limits = c(.05, .35), expand = c(0, .01),
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  scale_color_manual(values = c(c1, c2, c3)) +
  coord_cartesian(clip = "off") +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    plot.background = element_rect(fill = "#222B4C", color = "pink",linewidth = 3),
    plot.caption = element_markdown(
      color = c4, size = 11, family = "ubuntu", margin = margin(15, 0, 10, 0)),
    plot.caption.position = "plot",
    panel.grid.major = element_line(
      color = "#306489", linetype = "8f", linewidth = .2),
    axis.text = element_text(color = c4),
    axis.text.x = element_text(family = "bebas", size = 35),
    axis.text.y = element_text(family = "victor", size = 15, vjust = 0),
    legend.position = "none"
  ); ggsave(
    plot = g,
    filename = "2023/semana_45/viz.png",
    width = 30,
    height = 30,
    units = "cm"); browseURL("2023/semana_45/viz.png")


# figura ------------------------------------------------------------------

