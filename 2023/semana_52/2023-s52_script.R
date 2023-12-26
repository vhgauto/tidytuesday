
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#E1C59A"
c2 <- "#E59A52"
c3 <- "#F7BD6E"
c4 <- "#094568"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# calificación IMDB
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-regular", "icon/Font Awesome 6 Free-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 52. ",
  "Historical Trends in R Package Structure and Interdependency on CRAN,<br>",
  "**Mark Padgham** & **Noam Ross**</span>")
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

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-26/readme.md")

cran_20221122 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-26/cran_20221122.csv')

# me interesa saber los días y horarios en los que NO hay paquetes subidos a 
# CRAN, los momentos en el año en los que los desarrolladores descansan (?)

d <- cran_20221122 |>
  select(date) |>
  mutate(
    mes = month(date),
    dia = day(date),
    hora = hour(date)) |>
  mutate(fecha = ymd(glue("2020-{mes}-{dia}"))) |>
  count(fecha, hora) |>
  mutate(
    mes = format(fecha, "%B") |> str_to_upper(),
    dia = day(fecha)) |>
  mutate(mes = fct_reorder(mes, month(fecha))) |>
  mutate(rango = cut_width(n, width = 2))

# d_max <- d |>
#   slice_max(order_by = n, by = mes, n = 1)
# 
# g <- ggplot(d, aes(hora, dia, fill = rango)) +
#   geom_raster(color = NA, linewidth = .5) +
#   # geom_point(data = d_max, shape = 0, size = 5, color = "red", stroke = 1) +
#   facet_wrap(vars(mes), nrow = 3, scales = "free") +
#   scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, 1)) +
#   scale_y_continuous(
#     expand = c(0, 0), limits = c(.5, 31.5), breaks = seq(1, 31, 1)) +
#   tidyterra::scale_fill_whitebox_d(palette = "bl_yl_rd") +
#   # tidyterra::scale_fill_wiki_d() +
#   coord_cartesian(clip = "off") +
#   theme_void() +
#   theme(
#     plot.background = element_rect(fill = "black"),
#     panel.grid = element_blank(),
#     panel.background = element_rect(fill = "black"),
#     axis.text.x = element_text(
#       color = "white", family = "victor", hjust = .5, size = 8),
#     axis.text.y = element_text(
#       color = "white", family = "victor", hjust = 1, size = 10),
#     legend.text = element_text(color = "white"),
#     strip.text = element_text(color = "white", size = 15, family = "ubuntu")
#   ); ggsave(
#     plot = g,
#     filename = "2023/semana_52/viz.png",
#     width = 30,
#     height = 30,
#     units = "cm"
#   ); browseURL("2023/semana_52/viz.png")

d_na <- d |> 
  complete(mes, dia, hora) |> 
  filter(is.na(n)) |> 
  mutate(n = if_else(is.na(n), 1, 0)) |> 
  mutate(fecha = ymd(glue("2020-{mes}-{dia}"))) |> 
  drop_na(fecha)

mi_subtitle <- glue(
  "Días y horas con nula actividad en **CRAN**, el repositorio central de ",
  "<b style='font-family:victor;'>R</b>")

g2 <- ggplot(d_na, aes(hora, dia, fill = n)) +
  geom_tile(color = "#E05E35", linewidth = .5) +
  facet_wrap(vars(mes), nrow = 3, scales = "free") +
  scale_x_continuous(
    expand = c(0, 0), breaks = seq(0, 24, 4), 
    sec.axis = dup_axis(guide = guide_axis(position = "top"))) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(.5, 31.5), breaks = c(1, seq(5, 30, 5)),
    sec.axis = dup_axis()) +
  scale_fill_gradient(low = "#AF141F", high = "#AF141F") +
  labs(
    x = "Hora <span>→</span>", y = "↑\nDía", subtitle = mi_subtitle,
    caption = mi_caption) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#2E141E"),
    plot.margin = margin(rep(10, 4)),
    plot.subtitle = element_markdown(color = "#F7BD6E", family = "ubuntu"),
    plot.caption = element_markdown(
      color = "#EF9352", family = "ubuntu", size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.spacing = unit(.1, "line"),
    panel.ontop = FALSE,
    legend.position = "none",
    axis.title.x.bottom = element_markdown(
      hjust = 0, color = "#EF9352", size = 20, family = "serif", 
      margin = margin(t = 10)),
    axis.title.y.left = element_text(
      vjust = 0, color = "#EF9352", size = 20, family = "serif",
      margin = margin(r = 10)),
    axis.line = element_line(color = "#E05E35", linewidth = .5),
    axis.text = element_text(
      color = "#FFE98C", family = "victor", hjust = .5, size = 9),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    strip.text = element_text(color = "#FFE98C", size = 25, family = "ubuntu"),
    strip.placement = "outside"
  ); ggsave(
    plot = g2,
    filename = "2023/semana_52/viz2.png",
    width = 30,
    height = 32,
    units = "cm"
  ); browseURL("2023/semana_52/viz2.png")


