
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(ggbump)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
col <- MetBrewer::met.brewer(name = "Isfahan2", n = 5)
c1 <- "grey20"
c2 <- "grey40"

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

# fuente: Bebas Neue
font_add(
  family = "bebas", 
  regular = "fuente/BebasNeue-Regular.ttf")

# fuente: Marvel
font_add_google(
  name = "Marvel",
  family = "marvel"
)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-regular", "icon/Font Awesome 6 Free-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{col[1]};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {12}. ",
  "Mutant Moneyball: A Data Driven Ultimate X-Men, <b>Anderson Evans</b></span>")
autor <- glue("<span style='color:{col[1]};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:fa-brands;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{col[1]};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 12)
mutant <- tuesdata$mutant_moneyball

# me interesa la evolución en la popularidad (en precio promedio de los issues)
# de los 10 personajes más frecuentes

# 10 más frecuentes
pop <- mutant |> 
  slice_max(TotalIssues, n = 10) |> 
  pull(Member) |> 
  sort()

# acomodo los nombres y el ranking por década
d <- mutant |> 
  filter(Member %in% pop) |> 
  janitor::clean_names() |> 
  select(member, starts_with("pp") & ends_with("ebay")) |> 
  mutate(member = str_replace_all(member, "([A-Z])", " \\1")) |> 
  mutate(member = str_to_title(member)) |> 
  mutate(
    across(
      .cols = -member,
      .fns = ~ parse_number(.x)
    )
  ) |>
  pivot_longer(
    cols = -member,
    names_to = "decada",
    values_to = "precio"
  ) |> 
  mutate(decada = str_remove(decada, "ppi") |> str_remove("s_ebay")) |> 
  mutate(decada = as.numeric(decada)) |> 
  mutate(r = rank(precio, ties.method = "first"), .by  = decada) |> 
  mutate(member = str_wrap(member, 6))

# arreglo el orden de los nombres y tipo de línea
d_rank <- d |> 
  filter(decada == 60) |> 
  mutate(member = fct_reorder(member, r)) |> 
  arrange(desc(member)) |> 
  mutate(linetype = if_else(as.numeric(member) > 5, "1", "2")) |> 
  select(member, pos = r, linetype)

# nombres en la década de los 60 y 90
d_lim <- d |> 
  filter(decada == 60 | decada == 90) |> 
  mutate(hjust = if_else(decada == 60, 1, 0)) |> 
  mutate(decada = if_else(decada == 60, decada-.5, decada+.5))

# combino los datos
e <- inner_join(d, d_rank, by = join_by(member)) |> 
  mutate(member = fct_reorder(member, pos))

# figura ------------------------------------------------------------------

# subtítulo y aclaración
mi_subtitle <- glue(
  "Ranking de los personajes más populares del cómic ",
  "<b style='color:{col[2]}'>X-MEN</b>,<br>",
  "por precio<b style='color:{col[4]};'>*</b> promedio por publicación en 4 ",
  "décadas"
)

eje_x <- glue("<b style='color:{col[4]};'>*</b>basado en ventas en <i>eBay</i>")

# figura
g <- ggplot(e, aes(decada, r, color = member, linetype = linetype)) +
  geom_bump(linewidth = 1.3) +
  geom_point(size = 3, shape = 23, fill = c1, stroke = 1) +
  geom_text(
    data = d_lim, aes(decada, r, label = member, hjust = hjust, color = member),
    size = 8, family = "marvel", lineheight = unit(.8, "line"),
    inherit.aes = FALSE) +
  scale_x_continuous(
    labels = \(x) glue("'{x}")) +
  scale_y_continuous(
    breaks = 1:10, labels = glue("#{10:1}")) +
  scale_color_manual(values = c(col, col)) +
  scale_linetype_manual(values = c("solid", "83")) +
  labs(
    subtitle = mi_subtitle, 
    caption = mi_caption, 
    x = eje_x) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(l = 30, r = 80),
    plot.background = element_rect(fill = c1, color = col[1], linewidth = 3),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      color = "white", size = 22, hjust = .5, family = "ubuntu",
      margin = margin(t = 10, b = 15)),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, color = col[4],
      margin = margin(t = 20, r = -70, b = 10), lineheight = unit(1.2, "line")),
    legend.position = "none",
    text = element_text(size = 5),
    axis.title.x = element_markdown(
      hjust = 0, color = c2, family = "ubuntu", size = 17, 
      margin = margin(t = 5, l = -80)),
    axis.text.x = element_text(
      family = "bebas", size = 50, color = c2, margin = margin(t = 10)),
    axis.text.y = element_text(
      margin = margin(r = 90), size = 20, family = "victor", color = c2,
      vjust = 1)
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s12/viz.png",
  width = 30,
  height = 28.3,
  units = "cm")

# abro
browseURL("2024/s12/viz.png")
