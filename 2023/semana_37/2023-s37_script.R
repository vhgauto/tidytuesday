
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# colores, RColorBrewer, Set1
c1 <- "grey10"
c2 <- "#EE9569"
c3 <- "#75AEE0"
c4 <- "#CAB2D6"
c5 <- "#FDBF6F"
c6 <- "grey90"
c7 <- "grey40"

# texto gral, categorías (eje vertical)
font_add_google(name = "Ubuntu", family = "ubuntu")
# horas, porcentajes
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Taviraj", family = "taviraj")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:victor;'>{{<b>tidytuesdayR</b>}}</span> semana 37. The Human Chronome Project</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-09-12/readme.md")

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv')

# me interesan una selección de categorías, y comparo Argentina con el mundo

# traducción de las categorías
subcat_trad <- c(
  `Food preparation` = "Preparación de comida",
  `Sleep & bedrest` = "Dormir y descanso",
  `Physical child care` = "Cuidado de niños",
  `Health care` = "Cuidado de la salud",
  `Hygiene & grooming` = "Higiene y aseo",
  `Schooling & research` = "Escuela e investigación",
  `Religious practice` = "Práctica religiosa",
  `Social` = "Actividades sociales",
  `Active recreation` = "Actividades recreativas",
  `Meals` = "Comidas"
)

# categorías de interés, en inglés
subcat_interes <- names(subcat_trad)

# Argentina
arg <- all_countries |> 
  filter(country_iso3 == "ARG") |> 
  rename_with(.cols = everything(), .fn = str_to_lower) |> 
  rename(hr = hoursperdaycombined) |> 
  filter(subcategory %in% subcat_interes) |> 
  mutate(categ = subcat_trad[subcategory]) |> 
  select(categ, hr)

# mundo
mundo <- global_human_day |> 
  rename_with(.cols = everything(), .fn = str_to_lower) |> 
  rename(hr = hoursperday) |> 
  filter(subcategory %in% subcat_interes) |> 
  mutate(categ = subcat_trad[subcategory]) |> 
  select(categ, hr)

# calculo la diferencia entre horas por categorías, Argentina y el mundo
# agrego el porcentaje del día que representa c/categoría
d1 <- bind_rows(
  arg |> mutate(grupo = "Argentina"),
  mundo |> mutate(grupo = "Mundo")) |> 
  pivot_wider(names_from = grupo, values_from = hr) |> 
  mutate(porcent = Argentina/24) |> 
  mutate(porcent = gt::vec_fmt_percent(porcent, decimals = 1, dec_mark = ",", sep_mark = "")) |> 
  mutate(label = glue("<span style='font-size:30px'>{porcent}</span>")) |> 
  rowwise() |> 
  mutate(x = max(c(Argentina, Mundo))) |> 
  mutate(categ = str_wrap(categ, width = 15))
  
# transformo a tabla larga para graficar
d2 <- d1 |> 
  pivot_longer(cols = c(Argentina, Mundo), names_to = "grupo", values_to = "hm") |> 
  mutate(grupo = fct(grupo, levels = c("Mundo", "Argentina"))) |> 
  mutate(categ = str_wrap(categ, width = 15)) |> 
  mutate(categ = fct_reorder(categ, hm))

# figura ------------------------------------------------------------------

# título y subtítulo
mi_tit <- "UN DÍA ARGENTINO"
mi_sub <- glue(
  "Más de un tercio del día la pasamos<br>descansando y durmiendo.<br><br>
  Se comparan las horas dedicadas<br>
  por <b style='color:{c3}'>argentinos</b> y el <b style='color:{c2}'>promedio
  mundial</b><br>en 10 actividades cotidianas seleccionadas.<br><br>
  Se agregan los porcentajes que cada<br>
  actividad nos ocupa en el día.")

# figura
g <- ggplot(d2, aes(hm, categ, color = grupo, fill = grupo)) +
  geom_col(
    position = position_dodge(width = .9), show.legend = FALSE, width = .1) +
  geom_point(
    position = position_dodge(width = .9), show.legend = FALSE, size = 4) +
  geom_richtext(
    data = d1, aes(x+.25, categ, label = label), inherit.aes = FALSE,
    family = "victor", color = c6,
    label.color = NA, fill = NA, hjust = 0, position = position_dodge(width = .9)) +
  annotate(
    geom = "richtext", x = 3.25, y = 2, hjust = 0, vjust = 0, label = mi_sub,
    label.color = c7, label.margin = unit(rep(2, 4), "lines"), fill = c1, 
    color = c6, size = 7.5, family = "ubuntu") +
  scale_x_continuous(
    breaks = seq(1, 10, 1),
    expand = c(0, 0),
    limits = c(0, 10),
    labels = scales::label_number(suffix = "H")) +
  scale_color_manual(values = c(c2, c3)) +
  scale_fill_manual(values = c(c2, c3)) +
  coord_cartesian(clip = "off") +
  labs(
    title = mi_tit,
    caption = mi_caption,
    x = "Horas diarias dedicadas a la actividad", y = NULL) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(8.25, 40, 8.25, 20),
    plot.background = element_rect(fill = c1, color = c6, linewidth = 3),
    plot.title = element_text(
      family = "taviraj", size = 70, color = "white", hjust = .5,
      margin = margin(15, 0, 5, 0)),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = c6, family = "ubuntu", hjust = 1, size = 10, 
      margin = margin(10, 0, 0, 0)),
    panel.background = element_rect(fill = c1, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = .1, linetype = "8f", color = c7),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      family = "ubuntu", color = c6, size = 20, margin = margin(10, 0, 0, 0)),
    axis.text.y = element_text(
      size = 20, family = "sans", color = c6, margin = margin(0, 10, 0, 0)),
    axis.text.x = element_text(size = 15, family = "victor", color = c6)
  )

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_37/viz.png",
  width = 30,
  height = 29,
  units = "cm")

# abro
browseURL("2023/semana_37/viz.png")
