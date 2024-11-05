
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "#F04C44"
c2 <- "#110C3A"
c3 <- "#76BE72"
c4 <- "#FFF5F5"
c5 <- "grey40"

# fuente: Ubuntu
font_add(
  family = "ubuntu",
  regular = "fuente/Ubuntu-Regular.ttf",
  bold = "fuente/Ubuntu-Bold.ttf",
  italic = "fuente/Ubuntu-Italic.ttf"
)

# monoespacio & íconos
font_add(
  family = "jet",
  regular = "fuente/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

font_add_google(
  name = "Bebas Neue",
  family = "bebas"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
paper <- str_wrap("Regime types and regime change: A new dataset on democracy, coups, and political institutions", 50) |> 
  str_replace_all("\\n", "<br>")
paper_autor <- "Bjørnskov & Rode"
fuente <- glue(
  "Datos: <span style='color:{c1};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {45}, ",
  "<b>{paper}</b>, {paper_autor}.</span>"
)
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 45)
democracy_data <- tuesdata$democracy_data

# porcentaje anual de democracias, monarquías y presidenciales

d <- democracy_data |> 
  select(year, is_democracy, is_monarchy, is_presidential) |> 
  drop_na() |> 
  mutate(
    across(
      .cols = everything(),
      .fns = \(x) mean(x)
    ),
    .by = year
  ) |> 
  pivot_longer(
    cols = starts_with("is"),
    names_to = "estado",
    values_to = "prop"
  ) |> 
  distinct() |> 
  mutate(
    estado_tran = case_match(
      estado,
      "is_democracy" ~ "Democracia",
      "is_monarchy" ~ "Monarquía",
      "is_presidential" ~ "Presidencial"
    )
  )

# etiquetas
d_2005 <- filter(d, year == 2005) |> 
  mutate(
    vjust = c(1.2, 1.6, 1.4)
  )

# figura ------------------------------------------------------------------

mi_subtitulo <- glue(
  "En los últimos 70 años, mientras las<br>",
  "<b style='color: {c1}'>democracias</b> están en aumento,<br>",
  "las <b style='color: {c3}'>presidencias</b> y las ",
  "<b style='color: {c2}'>monarquías</b><br>",
  "se estabilizaron."
)

g <- ggplot(d, aes(year, prop, color = estado_tran)) +
  geom_line(
    linewidth = 2.7, show.legend = FALSE, lineend = "round"
  ) +
  geom_line(
    aes(group = estado_tran), color = c4, linetype = "44",
    linewidth = .4, show.legend = FALSE, lineend = "round"
  ) +
  geom_label(
    data = d_2005, aes(label = estado_tran, fill = estado_tran, vjust = vjust),
    show.legend = FALSE, size = 8, fontface = "bold", color = c4, hjust = 0,
    label.r = unit(0, "mm")
  ) +
  annotate(
    geom = "richtext", x = I(.01), y = I(.99), label = mi_subtitulo, size = 8,
    family = "ubuntu", fill = c4, label.color = c5, hjust = 0, vjust = 1,
    color = c5, label.padding = unit(.4, "lines"), label.r = unit(0, "mm")
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(.18, .65),
    expand = c(0, 0),
    breaks = seq(.1, .7, .1),
    labels = scales::label_percent()
  ) +
  scale_color_manual(
    values = c(c1, c2, c3)
  ) +
  scale_fill_manual(
    values = c(c1, c2, c3)
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, caption = mi_caption) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(r = 35, l = 10, b = 10, t = 8),
    plot.background = element_rect(fill = c4, color = c3, linewidth = 3),
    plot.caption = element_markdown(
      family = "ubuntu", size = 14, color = c2, lineheight = unit(1.1, "line"),
      margin = margin(t = 25)
    ),
    panel.grid.major = element_line(
      color = c5, linetype = 1,linewidth = .2
    ),
    panel.grid.minor = element_line(
      color = c5, linetype = 2, linewidth = .1
    ),
    axis.text.x = element_text(family = "bebas", size = 30, color = c2),
    axis.text.y = element_text(family = "jet", size = 20, color = c2)
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s45/viz.png",
  width = 30,
  height = 31,
  units = "cm"
)

# abro
browseURL(glue("{getwd()}/2024/s45/viz.png"))
