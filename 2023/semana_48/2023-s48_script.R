
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# colores
cc <- scico::scico(palette = "devon", n = 10)
c1 <- cc[3]
c2 <- cc[4]
c3 <- cc[5]
c4 <- cc[6]
c5 <- cc[1]
c6 <- cc[10]
c7 <- cc[9]

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# números, fechas, ranking
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'><span style='font-family:mono;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana 48. ",
  "<b style='font-family:mono;'>{{datardis}}</b>, ",
  "**Jonathan Kitt**</span>")
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

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-11-28/readme.md")

drwho_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')
drwho_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

# me interesa el rating que tiene los guionistas que más capítulos escribieron

# top 4 guinistas con más capítulos
escritores_top <- count(drwho_writers, writer, sort = TRUE) |> 
  slice(1:4) |> 
  pull(writer)

# calculo la proporción de capítulos escritos por los guionistas top

# guionistas que más capítulos escribieron
n_top_gionistas <- drwho_writers |> 
  filter(writer %in% escritores_top) |> 
  distinct(story_number) |> 
  nrow()

# total de capítulos
n_tot <- drwho_episodes |> 
  distinct(story_number) |> 
  nrow()

rel <- round(n_top_gionistas/n_tot*100)

# agrego las imágenes de los guionistas y acomodo respecto del número de 
# capítulos
d <- inner_join(
  drwho_episodes, drwho_writers, by = join_by(story_number)) |> 
  select(rating, writer) |> 
  filter(writer %in% escritores_top) |> 
  mutate(writer = fct_reorder(writer, rating)) |> 
  mutate(orden = as.numeric(writer)) |> 
  mutate(foto = glue("2023/semana_48/{writer}.jpg")) |> 
  mutate(label = glue("**{writer}**<br><img src='{foto}' width='120'>")) |> 
  mutate(label = fct_reorder(label, rating))

# figura ------------------------------------------------------------------

# logo de Doctor Who
logo <- "2023/semana_48/logo.png"
logo_label <- glue("<img src='{logo}' width='400'>")

# subtítulo
mi_sub <- glue(
  "Rating (en millones de espectadores, **Reino Unido**)<br>",
  "de los cuatro **guionistas** con mayor participación.<br>",
  "En total escribieron el **{rel}**% de los episodios.")

# figura
g <- ggplot(d, aes(writer, rating)) +
  # puntos
  geom_dotplot(
    aes(color = writer, fill = writer), stroke = 1,
    binaxis = "y", stackdir = "center", binwidth = .5, show.legend = FALSE) +
  # fotos de los escritores
  geom_richtext(
    aes(y = 74, label = label), vjust = 1, show.legend = FALSE, size = 6,
    fill = NA, label.color = NA, color = c6, family = "ubuntu") +
  # logo Doctor Who
  annotate(
    geom = "richtext", x = .5, y = 89, hjust = .26, vjust = 0, 
    label = logo_label, fill = NA, label.color = NA) +
  # línea horizontal en la mediana
  stat_summary(
    aes(color = writer), geom = "crossbar", fun = median, width = .6, 
    show.legend = FALSE) +
  # valor de la mediana
  stat_summary(aes(x = orden-.3, y = stage(rating, after_stat = median+.3),
    label = after_stat(str_replace(median, "\\.", ","))), geom = "text",
    size = 6, color = c6, hjust = 0, show.legend = FALSE,
    fun.data = ~ round(data.frame(median = median(.x)), 1), family = "victor") +
  scale_y_continuous(
    limits = c(68, 91), expand = c(0, 0), breaks = seq(75, 90, 5)) +
  scale_fill_manual(values = alpha(c(c1, c2, c3, c4), .3)) +
  scale_color_manual(values = c(c1, c2, c3, c4)) +
  labs(subtitle = mi_sub, caption = mi_caption) +
  coord_cartesian(clip = "off", xlim = c(.5, 4.5), expand = FALSE) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(100, 22, 0, 22),
    plot.background = element_rect(fill = c5, color = c1, linewidth = 3),
    plot.subtitle = element_markdown(
      family = "ubuntu", size = 20, color = c7, margin = margin(0, 0, 20, 250)),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, margin = margin(20, 5, 5, 0), color = c6),
    panel.grid.major.y = element_line(color = c1, linetype = 2),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      color = c2, family = "victor", vjust = 0, margin = margin(0, 20, 0, 0),
      size = 13))

# guardo
ggsave(
  plot = g,
  filename = "2023/semana_48/viz.png",
  width = 30,
  height = 35,
  units = "cm")

# abro
browseURL("2023/semana_48/viz.png")

