
# paquetes ----------------------------------------------------------------

# browseURL("https://twitter.com/nrennie35/status/1663457969898680320")

library(tidyverse)
library(rvest)
library(ggrepel)
library(glue)
library(ggtext)
library(showtext)

" Signac"
c1 <- "#f7f4f9"
c2 <- "grey10"
c3 <- "#381a61"
c4 <- "#fed9a6"
c5 <- "grey70"
c6 <- "#7c4b73"

# años, eje vertical
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = TRUE)
# título
font_add_google(name = "Libre Baskerville", family = "libre", db_cache = FALSE)
# resto del texto
font_add_google(name = "Ubuntu", family = "ubuntu", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:mono;'>{{<b>tidytuesdayR</b>}}</span> semana 22, Wikipedia</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")


# datos -------------------------------------------------------------------



# browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-30/readme.md")

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')



jeanne <- centenarians |> 
  slice_max(order_by = age, n = 1) |> 
  mutate(y = 0, yend = 1) |> 
  select(name, birth_date, death_date) |> 
  pivot_longer(cols = -name, names_to = "estado", values_to = "fecha") |> 
  mutate(etq = format(fecha, "%d/%m/%Y")) |> 
  mutate(vjust = if_else(estado == "birth_date", 1, 0))

link <- "https://es.wikipedia.org/wiki/Anexo%3APresidentes_de_la_Naci%C3%B3n_Argentina"

wi <- link %>%
  read_html() |> 
  html_element("table") %>%
  html_table()

pr <- wi |> 
  janitor::clean_names() |> 
  select(-1, -2) |> 
  select(presi = presidente_de_la_nacion_3, inicio = inicio_del_gobierno, fin = fin_del_gobierno) |> 
  filter(!str_detect(presi, "inexistente")) |> 
  separate(col = presi, into = c("presi", NA), sep = "\\(") |> 
  separate(col = inicio, into = c("inicio", NA), sep = "\\[") |> 
  separate(col = fin, into = c("fin", NA), sep = "\\[") |> 
  filter(presi != "Alberto Fernández") |> 
  mutate(across(.cols = c(inicio, fin), .fns = dmy)) 

pr2 <- pr |> 
  filter(between(inicio, min(jeanne$fecha), max(jeanne$fecha))) |> 
  distinct() |> 
  mutate(fila = row_number()) |> 
  mutate()

pr2_izq <- pr2 |> 
  filter(fila %% 2 == 0)

pr2_der <- pr2 |> 
  filter(fila %% 2 != 0)


g <- ggplot() +
  # horizontales c/25 años
  geom_hline(
    yintercept = seq(ymd(18750101), ymd(20000101), "25 year"),
    color = c5, linewidth = .25, linetype = "ff") +
  # vertical central
  geom_segment(
    aes(x = 0, xend = 0, y = ymd("1875-02-21"), yend = ymd("1997-08-04")),
    color = c4, linewidth = 3) +
  # nacimiento/fallecimiento
  geom_richtext(
    data = jeanne, aes(x = 0, y = fecha, label = etq, vjust = vjust),
    color = c3, fill = c4, size = 11, label.padding = unit(.4, "line"), 
    label.color = NA, family = "ubuntu") +
  # presidentes
  geom_text_repel(
    data = pr2_der, aes(x = 0, y = inicio, label = presi), 
    xlim = c(.1, 2), size = 8, seed = 2023, family = "ubuntu", color = c3) +
  geom_text_repel(
    data = pr2_izq, aes(x = 0, y = inicio, label = presi), 
    xlim = c(-2, -.1), size = 8, seed = 2023, family = "ubuntu", color = c3) +
  # puntos presidentes sobre vertical central
  geom_point(data = pr2, aes(x = 0, y = inicio), color = "#ec7014", size = 2) +
  # ejes
  scale_x_continuous(limits = c(-.7, .7), expand = c(0, 0)) +
  scale_y_date(
    breaks = seq(ymd(18750101), ymd(20000101), "25 year"), 
    expand = c(0, 0), limits = c(ymd(18700101), ymd(20000101)),
    date_labels = "%Y",) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, 
       title = glue("Jeanne Calment vivió 122 años"),
       subtitle = glue(
         "**Jeanne Calment** es considerada la persona más longeva de la historia. 
         Nació en Francia en **1875** y falleció en **1997**. Para dar 
         dimensión a su extensa vida, se muestran los **{nrow(pr2)}** 
         presidentes argentinos (democráticos y de facto) que tuvimos durante este período."),
       caption = mi_caption) +
  # tema
  theme(
    aspect.ratio = 1.95,
    # plot.margin = margin(6.8, 10, 6.8, 10),
    # plot.margin = margin(5, 17.35, 5, 17.35),
    plot.margin = margin(5, 29.5, 5, 29.5),
    plot.background = element_rect(
      fill = c1, color = c6, linewidth = 3),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 52, color = c3, family = "libre", margin = margin(5, 0, 5, 0)),
    plot.subtitle = element_textbox_simple(
      color = c2, size = 22, margin = margin(5, 0, 35, 7), family = "ubuntu"),
    plot.caption = element_markdown(
      hjust = .3, size = 15, color = c2, family = "ubuntu"),
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(color = c5, family = "bebas", size = 40),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

# guardo
ggsave(filename = "2023/semana_22/viz.png",
       plot = g,
       width = 30,
       height = 58,
       units = "cm",
       dpi = 300)

# abro
browseURL("2023/semana_22/viz.png")






















