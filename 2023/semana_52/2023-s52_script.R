
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#E05E35"
c2 <- "#AF141F"
c3 <- "#F7BD6E"
c4 <- "#EF9352"
c5 <- "#FFE98C"
c6 <- "#2E141E"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# horas, días
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# títulos de eje, título
font_add_google(name = "Source Serif 4", family = "source", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

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
  mutate(mes = fct_reorder(mes, month(fecha)))

d_na <- d |> 
  complete(mes, dia, hora) |> 
  filter(is.na(n)) |> 
  mutate(n = if_else(is.na(n), 1, 0)) |> 
  mutate(fecha = ymd(glue("2020-{mes}-{dia}"))) |> 
  drop_na(fecha)

# figura ------------------------------------------------------------------

# título y subtítulo
mi_title <- glue("El descanso de <span style='color:white'>CRAN</span>")

mi_subtitle <- glue(
  "Días y horas con actividad nula en **CRAN**, el repositorio central de ",
  "<b style='font-family:victor;'>R</b>. Durante la madrugada<br>",
  "y a fin de año son los momentos en los que los ",
  "desarrolladores no publican sus productos.")

# función que agrega ceros delante de los números
f_ceros <- function(x) {
  if (nchar(x) == 1) {
    y <- glue("0{x}")
  } else {y <- glue("{x}")}
  
  return(as.character(y))
}

# valores de ejes
rango_x <- seq(0, 24, 3)
rango_y <- c(1, seq(3, 30, 3))

# figura
g <- ggplot(d_na, aes(hora, dia, fill = n)) +
  geom_tile(color = c1, linewidth = .5) +
  facet_wrap(vars(mes), nrow = 3, scales = "free") +
  scale_x_continuous(
    expand = c(0, 0), breaks = rango_x, 
    labels = list_c(map(rango_x, f_ceros)),
    sec.axis = dup_axis(guide = guide_axis(position = "top"))) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(.5, 31.5), breaks = rango_y,
    labels = list_c(map(rango_y, f_ceros)),
    sec.axis = dup_axis()) +
  scale_fill_gradient(low = c2, high = c2) +
  labs(
    x = "Hora <span style='font-size:30pt'>→</span>", 
    y = "<span style='font-size:30pt'>↑</span><br>Día", 
    title = mi_title,
    subtitle = mi_subtitle,
    caption = mi_caption) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = c6, color = scales::muted(c2, l = 2), linewidth = 3),
    plot.margin = margin(rep(10, 4)),
    plot.title = element_markdown(
      size = 75, family = "source", color = c5, hjust = .5),
    plot.subtitle = element_markdown(
      color = c3, family = "ubuntu", margin = margin(b = 15), size = 18, 
      hjust = .5),
    plot.caption = element_markdown(
      color = c4, family = "ubuntu", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(0, "line"),
    panel.spacing.y = unit(.3, "line"),
    panel.ontop = FALSE,
    legend.position = "none",
    axis.title.x.bottom = element_markdown(
      hjust = 0, color = c4, size = 20, family = "source", 
      margin = margin(t = 10)),
    axis.title.y.left = element_markdown(
      vjust = 0, color = c4, size = 20, family = "source",
      margin = margin(r = 10)),
    axis.line = element_line(color = c1, linewidth = .5),
    axis.text.x.bottom = element_text(
      color = c5, family = "victor", hjust = .5, size = 11, 
      margin = margin(t = 3)),
    axis.text.y.left = element_text(
      color = c5, family = "victor", hjust = .5, size = 11, 
      margin = margin(r = 3, l = 3)),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    strip.text = element_text(
      color = c5, size = 25, family = "ubuntu", margin = margin(b = 5),
      hjust = 0),
    strip.placement = "outside")

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_52/viz.png",
  width = 30,
  height = 37,
  units = "cm")

# abro
browseURL("2023/semana_52/viz.png")
