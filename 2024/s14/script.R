
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(ggsvg)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores de la bandera panafricana
c1 <- "#AC3E48"
c2 <- "#000000"
c3 <- "#00863D"
# fondo
c4 <- "white"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf")

# íconos
font_add(
  family = "jet", 
  regular = "fuente/JetBrainsMonoNLNerdFontMono-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {14}. ",
  "<b>W. E. B. Du Bois</b>, 1900 Paris Exposition.</span>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 14)
dubois <- tuesdata$dubois_week10

# me interesa representar a cada personas, dividido por profesión, en un waffle,
# con íconos individuales

# traducción de las ocupaciones
trabajos <- pull(dubois, Occupation)
trabajos_trad <- c(
  "profesores", "ministros", "gobierno", "negocios", "otros", "amas de casa")

names(trabajos_trad) <- trabajos

# cantidad de personas por profesión, siendo 330 el total
d <- dubois |> 
  mutate(n = round(Percentage, 1)/100*330) |> 
  mutate(n2 = round(n, 1)) |> 
  mutate(n3 = round(n2)) |> 
  mutate(s = sum(n3)) |> 
  select(Occupation, n = n3) |> 
  mutate(ocupacion = trabajos_trad[Occupation]) |> 
  mutate(ocupacion = fct_reorder(ocupacion, n))

# figura ------------------------------------------------------------------

# función para crear las coordenadas (x,y) y generar un waffle
f_waffle <- function(prof) {
  
  # cantidad de emojis
  d_n <- d |> 
    filter(ocupacion == prof) |> 
    pull(n)
  
  # eje vertical y horizontal incompleto
  y_enteros <- d_n %/% ancho
  x_extra <- d_n %% ancho
  
  # si es múltiplo del ancho, agrega o no una nueva línea
  if (x_extra == 0) {
    tbl <- expand.grid(x = 1:ancho, y = 1:y_enteros) |> 
      tibble() |> 
      mutate(profesion = prof)
  } else {
    tbl <- expand.grid(x = 1:ancho, y = 1:y_enteros) |> 
      tibble() |> 
      add_row(x = 1:x_extra, y = y_enteros+1) |> 
      mutate(profesion = prof)
  }
  
  return(tbl)
  
}

# función que cambia el color de cada letra
f_label <- function(x) {
  p_l <- x |> 
    str_split("") |> 
    unlist()
  
  prof_label <- tibble(p = p_l) |> 
    mutate(color = rep(c(c1, c2, c3), length.out = length(p_l))) |> 
    mutate(label = glue("<span style='color:{color}'>{p}</span>")) |> 
    pull(label) |> 
    str_flatten()
  
  return(prof_label)
}

# cantidad de íconos, horizontal
ancho <- 7

# creo tibble para figura waffle, con colores/rellenos aleatorios
e <- map(trabajos_trad, f_waffle) |> 
  list_rbind() |> 
  mutate(n = n(), .by = profesion) |> 
  mutate(profesion = fct_reorder(profesion, -n)) |> 
  mutate(color = sample(c(c1, c2, c3), size = n(), replace = TRUE)) |> 
  mutate(
    fill = case_match(
      color,
      c1 ~ c2,
      c2 ~ c3,
      c3 ~ c1
    )
  )

# ícono de persona
persona_icon <- glue("<span style='font-family:jet;'>&#xf064d;</span>")

# profesiones con letras de cada color
e_label <- e |> 
  filter(y == max(y) & x == 1, .by = profesion) |> 
  mutate(y = y+1) |>
  mutate(label = map(toupper(profesion), \(x) f_label(x))) |> 
  unnest(label)

# browseURL("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2024/challenge10/original-plate-37.jpg")

# leyenda para indicar que cada ícono es una persona
d_legend <- tibble(
  profesion = fct("amas de casa"),
  y = 20,
  x = 2,
  label = glue("{persona_icon} = 1 persona")
)

# subtítulo
mi_subtitle <- tibble(
  profesion = fct("otros"),
  y = 22,
  x = 1,
  label = glue(
    "<span style='color:{c1}'>",
    "Ocupaciones de 330 personas negras graduadas<br>",
    "de la <b>Universidad de Atlanta</b> en 1900, EE.UU.",
    "</span>",
    "<br><br>",
    
    "<span style='color:{c2}'>",
    "Recreación de la lámina 37, de <b>W.E.B Du Bois</b>,<br>",
    "para la Exposición de París.",
    "</span>",
    "<br><br>",
    
    "<span style='color:{c3}'>",
    "La presente paleta de colores<br>",
    "corresponde a la <b>bandera panafricana</b>:",
    "</span>",
    )
)

# bandera panafricana, de Wikipedia, .svg
bandera_url <- "https://upload.wikimedia.org/wikipedia/commons/a/ab/Flag_of_the_UNIA.svg"
bandera_txt <- paste(readLines(bandera_url), collapse = "\n")

# verifico que la bandera se vea correctamente
grid::grid.draw( svg_to_rasterGrob(bandera_txt) )

mi_bandera <- tibble(
  profesion = fct("otros"),
  y = 16,
  x = 6
)

# figura
g <- ggplot(e, aes(x, y)) +
  # cuadros
  geom_tile(aes(fill = fill), color = c4, linewidth = 1) +
  # contorno blanco
  geom_richtext(
    color = c4, label = persona_icon, fill = NA, label.color = NA, 
    family = "jet", size = 3.3, lineheight = unit(2, "line"), nudge_y = -.05, 
    show.legend = FALSE) +
  # ícono de personas
  geom_richtext(
    aes(color = color),
    label = persona_icon, fill = NA, label.color = NA, family = "jet", size = 3,
    lineheight = unit(2, "line"), nudge_y = -.05, show.legend = FALSE) +
  # profesiones
  geom_richtext(
    data = e_label, aes(.5, y, label = label), hjust = 0, family = "ubuntu",
    size = 6, vjust = 0, fill = NA, label.color = NA, inherit.aes = FALSE) +
  # leyenda
  geom_richtext(
    data = d_legend, aes(x, y, label = label), fill = NA, label.color = NA,
    family = "jet", size = 4, color = c1, hjust = 0
  ) +
  # subtítulo
  geom_richtext(
    data = mi_subtitle, aes(x, y, label = label), fill = NA, label.color = NA,
    family = "ubuntu", size = 6, color = c1, hjust = 0
  ) +
  # bandera
  geom_point_svg(
    data = mi_bandera, aes(x, y), svg = bandera_txt, size = 40, hjust = 0, 
    vjust = 1) + 
  facet_wrap(vars(profesion), nrow = 1) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_equal(clip = "off") +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c4, color = c1, linewidth = 3),
    plot.caption = element_markdown(
      color = c1, family = "ubuntu", size = 10, lineheight = unit(1.1, "line"),
      margin = margin(b = 5, r = 5)),
    panel.spacing.x = unit(.3, "line"),
    strip.text = element_blank()
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s14/viz.png",
  width = 30,
  height = 20.715,
  units = "cm")

# abro
browseURL("2024/s14/viz.png")
