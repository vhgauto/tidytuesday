
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
cc <- MetBrewer::met.brewer(name = "Tam")
c1 <- "white"
c2 <- "#9F5690"
c3 <- "#E50712"

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

# Tangerine
font_add_google(
  name = "Tangerine",
  family = "tangerine"
)

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{cc[1]};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {51}, ",
  "<b>Dungeons and Dragons Free Rules (2024 edition)</b>.</span>"
)
autor <- glue("<span style='color:{cc[1]};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
icon_bsky <- glue("<span style='font-family:jet;'>&#xe28e;</span>")
usuario <- glue("<span style='color:{cc[1]};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {icon_bsky} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 51)
spells <- tuesdata$spells

# me interesa el conteo de hechizos por nivel y escuela

# traducciones de las escuelas
escuelas <- unique(spells$school)
escuelas_trad <- c(
  "Evocación", "Abjuración", "Transmutación", "Encantamiento", "Necromancia",
  "Adivinación", "Ilusionismo", "Conjuración"
)
escuelas_trad <- set_names(escuelas_trad, escuelas)

# íconos
sombrero <- glue("<span style='font-family:jet; color:{cc[2]}'>&#xf1477;</span>")
varita <- glue("<span style='font-family:jet; color:{cc[2]}'>&#xf1844;</span>")

# cantidad de hechizos por escuela
d_n <- count(spells, school) |> 
  mutate(school = escuelas_trad[school]) |> 
  mutate(school = glue("{sombrero} {school} {varita}")) |> 
  mutate(school = fct_reorder(school, n)) |> 
  mutate(label = glue("n={n}")) |> 
  rename("n_top" = n)

# cantidad de hechizos por escuela y nivel
d <- spells |> 
  select(name, level, school) |> 
  reframe(
    n = n(),
    .by = c(level, school)
  ) |> 
  arrange(level) |> 
  mutate(level = factor(level)) |>
  mutate(
    es_max = n == max(n),
    .by = c(school)
  ) |> 
  mutate(school = escuelas_trad[school]) |> 
  mutate(school = glue("{sombrero} {school} {varita}")) |> 
  mutate(school = factor(school)) |> 
  
  inner_join(d_n, by = join_by(school)) |> 
  
  mutate(school = fct_reorder(school, n_top)) |> 
  mutate(school = fct_rev(school))

# figura ------------------------------------------------------------------

# logo de D&D y subtítulo
logo <- "https://upload.wikimedia.org/wikipedia/en/thumb/8/8e/Dungeons_%26_Dragons_5th_Edition_logo.svg/1024px-Dungeons_%26_Dragons_5th_Edition_logo.svg.png"
logo_label <- glue("<img src='{logo}' width=100>")

mi_subtitulo <- glue(
  "En <b style='color:{cc[8]}'>Dungeons & Dragons</b>, hay más del doble de 
  hechizos de <b>transmutación</b> que <b>ilusionismos</b>. El nivel 1 es 
  el más frecuente de los <b>encantamientos</b>.<br>Se destacan en cada escuela 
  el nivel de hechizo más <b style='color:{cc[1]}'>frecuente</b>."
)

# figura
g <- ggplot(d, aes(n, level, fill = es_max)) +
  geom_col(show.legend = FALSE) +
  geom_label(
    data = d_n, aes(Inf, Inf, label = label), hjust = .99, vjust = .99, size = 3,
    family = "jet", color = cc[1], inherit.aes = FALSE, show.legend = FALSE,
    fill = c2, label.size = unit(0, "pt"), label.r = unit(0, "pt"),
    label.padding = unit(.5, "lines")
  ) +
  scale_x_continuous(
    breaks = seq(0, 16, 2),
    expand = c(0, 0),
    limits = c(0, 15)
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    limits = as.character(0:9)
  ) +
  scale_fill_manual(
    values = c(cc[7], cc[1])
  ) +
  facet_wrap(vars(school), nrow = 2, scales = "free") +
  coord_cartesian(clip = "off") +
  labs(
    x = "# de hechizos",
    y = "Nivel del hechizo",
    subtitle = mi_subtitulo,
    caption = mi_caption,
    tag = logo_label
  ) +
  theme_minimal(base_size = 9) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(10.3, 10, 10, 10),
    plot.title.position = "plot",
    plot.background = element_rect(fill = c2, color = cc[5], linewidth = 3),
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 12, color = c1, hjust = .5, lineheight = 1.1
    ),
    plot.caption = element_markdown(
      family = "ubuntu", size = 9, color = c1, margin = margin(t = 15),
      lineheight = 1
    ),
    plot.tag.location = "plot",
    plot.tag.position = c(.08, .04),
    plot.tag = element_markdown(),
    panel.spacing = unit(15, "pt"),
    panel.background = element_rect(fill = cc[8], color = NA),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      color = cc[6], linewidth = .1, linetype = "66"
    ),
    axis.text = element_text(color = c1, family = "jet"),
    axis.title = element_text(color = c1, family = "ubuntu", size = 14),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    strip.text = element_markdown(
      color = c1, family = "tangerine", hjust = 0, size = 22,
      face = "bold", margin = margin(b = 0)
    )
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s51/viz.png",
  width = 30,
  height = 19.3,
  units = "cm"
)

# abro
browseURL(paste0(getwd(), "/2024/s51/viz.png"))
