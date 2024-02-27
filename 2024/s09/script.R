
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#4F53B7"
c2 <- "#DDDDEE"
c3 <- "#6B6100"
c4 <- "#E0DDC9"
c5 <- "#C75DAA"
c6 <- "#F3DEEE"

# fuente: ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf")

# fuente: victor
font_add(
  family = "victor", 
  regular = "fuente/VictorMono-ExtraLight.ttf",
  bold = "fuente/VictorMono-VariableFont_wght.ttf",
  italic = "fuente/VictorMono-ExtraLightItalic.ttf")

# fuente: abril
font_add_google(
  name = "Abril Fatface",
  family = "abril"
)

# fuente: bebas
font_add(
  family = "bebas",
  regular = "fuente/BebasNeue-Regular.ttf"
)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {9}. ",
  "Artículo *February 29*, Wikipedia.</span>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:fa-brands;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 9)
births <- tuesdata$births
deaths <- tuesdata$deaths

# me interesa un listado de 29 nacimientos/muertes, señalando el caso de 
# James Milne Wilson, que nació y murió un 29/2
jmw <- inner_join(deaths, births, by = join_by(person)) |> 
  pull(person)

jmw_n <- births |> 
  filter(person == jmw) |> 
  pull(year_birth)

jmw_m <- births |> 
  filter(person == jmw) |> 
  pull(year_death)

# para asegurar la reproducibilidad
set.seed(2024)

# cantidad de personas y desplazamiento horizontal
n_nombres <- 28
nud <- .2

# nacimientos
n_28 <- births |> 
  select(persona = person) |> 
  filter(persona != jmw) |> 
  slice_sample(n = n_nombres) |> 
  mutate(tipo = "nacimiento")

n_jmw <- births |> 
  filter(person == jmw) |> 
  select(persona = person) |> 
  mutate(tipo = "nacimiento")

d_nac <- bind_rows(n_28, n_jmw) |> 
  slice_sample(n = n_nombres+1) |> 
  mutate(x = rep_len(c(1-nud, 1+nud), n_nombres + 1))

# muertes
m_28 <- deaths |> 
  select(persona = person) |> 
  filter(persona != jmw) |> 
  slice_sample(n = n_nombres) |> 
  mutate(tipo = "muerte")

m_jmw <- deaths |> 
  filter(person == jmw) |> 
  select(persona = person) |> 
  mutate(tipo = "muerte")

d_mue <- bind_rows(m_28, m_jmw) |> 
  slice_sample(n = n_nombres+1) |> 
  mutate(x = rep_len(c(3-nud, 3+nud), n_nombres + 1))

# combino datos
d <- bind_rows(d_nac, d_mue) |> 
  mutate(y = row_number(), .by = tipo) |> 
  mutate(nudge_x = rep_len(c(.2, -.2), n_nombres + 1), .by = tipo) |> 
  mutate(persona = str_wrap(persona, 15)) |> 
  mutate(persona = str_replace_all(persona, fixed("\n"), "<br>")) |> 
  mutate(tipo = factor(tipo, levels = c("nacimiento", "muerte")))

# figura ------------------------------------------------------------------

# subtítulo
mi_subtitle <- glue(
  "29 personas que <b style='color:{c1}'>nacieron</b> ó ",
  "<b style='color:{c3}'>murieron</b> el **29 de febrero**.<br>",
  "Es curioso el caso de <b style='color:{c5}'>{jmw}</b> ",
  "({jmw_n}-{jmw_m}), Primer<br>Ministro de Tasmania, ",
  "muerto el día de su cumpleaños."
)

# figura
g <- ggplot(d, aes(x, y, label = persona, color = tipo)) +
  # personas
  geom_richtext(
    size = 6, family = "bebas", fill = NA, label.color = NA,
    show.legend = FALSE) +
  # JMW
  geom_richtext(
    data = filter(d, str_detect(persona, "Milne")), aes(fill = tipo),
    family = "bebas", size = 6, show.legend = FALSE, label.color = NA,
    label.padding = unit(c(5, 5, 5, 5), "mm"), label.r = unit(0, "lines")) +
  # 29
  annotate(
    geom = "text", x = 2, y = (n_nombres+1)/2, label = "2\n9", fontface = "bold",
    size = 150, family = "abril", lineheight = unit(1, "line"), 
    color = c5) +
  annotate(
    geom = "text", x = 2, y = (n_nombres+1)/2, label = "Febrero", size = 20,
    fontface = "bold", family = "abril", lineheight = unit(1, "line"), 
    color = c5) +
  scale_color_manual(
    values = c(c1, c3)) +
  scale_fill_manual(
    values = c(c2, c4)) +
  coord_cartesian(
    clip = "off", xlim = c(.5, 3.5), expand = FALSE) +
  labs(subtitle = mi_subtitle, caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(color = c5, fill = c6, linewidth = 3),
    plot.margin = margin(t = 25, r = 28.7, b = 25, l = 28.7),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 20, margin = margin(b = 50), hjust = .5,
      lineheight = unit(1.3, "line")),
    plot.caption = element_markdown(
      family = "ubuntu", size = 14, color = c1, margin = margin(t = 60))
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s09/viz.png",
  width = 30,
  height = 37,
  units = "cm")

# abro
browseURL("2024/s09/viz.png")
