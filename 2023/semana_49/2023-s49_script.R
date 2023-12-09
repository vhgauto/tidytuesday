
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(patchwork)
library(ggflags)
library(tidyverse)

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
  "{{<b>tidytuesdayR</b>}}</span> semana 49. ",
  "Our World in Data, ",
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

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-05/readme.md")

life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv')
# life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_different_ages.csv')
# life_expectancy_female_male <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_female_male.csv')

vida <- life_expectancy |> 
  janitor::clean_names()

# vida |> 
#   filter(code == "ARG") |> 
#   ggplot(aes(year, life_expectancy)) +
#   geom_line() +
#   theme_linedraw() +
#   theme(
#     aspect.ratio = 1
#   )


rango <- vida |> 
  filter(code == "ARG" & (year == 1950 | year == 2020)) |> 
  pull(life_expectancy)

factor <- .01

min_inf <- rango[1]*(1-factor)
min_sup <- rango[1]*(1+factor)

# vida |> 
#   filter(year == 1950 & between(life_expectancy, min_inf, min_sup)) |> 
#   drop_na()

p_1950 <- c("Argentina", "Bulgaria", "Malta", "Spain", "Slovakia")

# g1 <- vida |> 
#   filter(year >= 1950 & entity %in% p_1950) |> 
#   ggplot(aes(year, life_expectancy, color = entity)) +
#   geom_point(size = 1, alpha = .6) +
#   geom_smooth(se = FALSE, method = loess, formula = y ~ x) +
#   theme_linedraw() +
#   theme(
#     aspect.ratio = 1
#   )

max_inf <- rango[2]*(1-factor)
max_sup <- rango[2]*(1+factor)

# vida |> 
#   filter(year == 2020 & between(life_expectancy, max_inf, max_sup)) |> 
#   drop_na() |> 
#   arrange(life_expectancy) |> 
#   print(n = Inf)

p_2020 <- c("Argentina", "Turkey", "Hungary", "Malaysia", "Saudi Arabia")


# g2 <- vida |> 
#   filter(year >= 1950 & entity %in% p_2020) |> 
#   ggplot(aes(year, life_expectancy, color = entity)) +
#   geom_point(size = 1, alpha = .6) +
#   geom_smooth(se = FALSE, method = loess, formula = y ~ x) +
#   theme_linedraw() +
#   theme(
#     aspect.ratio = 1
#   )

# g1+g2

r1 <- vida |> 
  filter(year >= 1950 & entity %in% p_1950) |> 
  mutate(estado = "inicio")

r2 <- vida |> 
  filter(year >= 1950 & entity %in% p_2020) |> 
  mutate(estado = "final")

r <- bind_rows(r1, r2) |> 
  mutate(estado = fct(estado, levels = c("inicio", "final")))

d <- r |> 
  filter(year %% 10 == 0)

d_bandera <- d |> 
  filter(
    (year == 2020 & estado == "inicio") | (year == 1950 & estado == "final"))

paises <- sort(unique(d_bandera$entity))

iso2 <- countrycode::codelist |> 
  select(contains("iso")) |> 
  filter(iso.name.en %in% paises) |> 
  pull(iso2c) |> 
  str_to_lower()

names(iso2) <- paises

d_bandera2 <- d_bandera |> 
  mutate(bandera = iso2[entity])




ggplot(d, aes(year, life_expectancy, color = entity)) +
  geom_point(size = 1, alpha = .6) +
  geom_line() +
  geom_flag(data = d_bandera2, aes(country = bandera), size = 6) +
  facet_wrap(vars(estado), nrow = 1, scales = "free") +
  scale_country() +
  # scale_size(range = c(0, 15)) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )

browseURL("https://github.com/jimjam-slam/ggflags")


#

life_expectancy |> 
  janitor::clean_names() |> 
  filter(code %in% c("ARG", "OWID_WRL")) |> 
  filter(year >= 1923) |> 
  filter(year %% 10 == 0) |> 
  ggplot(aes(year, life_expectancy, color = entity)) +
  geom_point(alpha = .5) +
  geom_line() +
  # geom_smooth(
  #   formula = y ~ x, method = loess, se = FALSE, linewidth = .5) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )

life_expectancy |> 
  janitor::clean_names() |> 
  filter(year == 2021) |> 
  slice_min(order_by = life_expectancy, n = 1)

life_expectancy |> 
  janitor::clean_names() |> 
  filter(year == 2021) |> 
  slice_max(order_by = life_expectancy, n = 1)

cc <- c("TCD", "MCO")

d <- life_expectancy |> 
  janitor::clean_names() |> 
  filter(code %in% c("ARG", "OWID_WRL", cc)) |> 
  filter(year >= 1950) |>
  filter(year %% 10 == 0) |>
  mutate(entity = fct_reorder(entity, -life_expectancy))

d_dif <- d |> 
  mutate(
    diferencia = max(life_expectancy) - min(life_expectancy),
    .by = entity) |> 
  filter(year == 2020) |> 
  mutate(diferencia = gt::vec_fmt_number(
    diferencia, decimals = 1, sep_mark = "", dec_mark = ",")) |> 
  mutate(diferencia = glue("+{diferencia}"))

ggplot(d, aes(year, life_expectancy, color = entity)) +
  geom_point(alpha = .5) +
  geom_text(
    data = d_dif, aes(label = diferencia), hjust = 0, nudge_x = .5,
    show.legend = FALSE) +
  geom_smooth(
    formula = y ~ x, method = loess, se = FALSE, linewidth = .5) +
  coord_cartesian(clip = "off") +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )

life_expectancy |> 
  janitor::clean_names() |> 
  filter(code == "ARG") |> 
  # filter(year >= 1923) |> 
  filter(year %% 10 == 0) |> 
  ggplot(aes(year, life_expectancy, color = entity)) +
  geom_point(alpha = .5) +
  geom_label(
    aes(label = format(life_expectancy, digits = 3)), hjust = 0, vjust = 1) +
  geom_smooth(
    formula = y ~ x, method = loess, se = FALSE, linewidth = .5) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )


life_expectancy_different_ages |> 
  janitor::clean_names() |> 
  filter(code %in% c("ARG", "OWID_WRL")) |> 
  pivot_longer(
    cols = starts_with("life_"), 
    names_to = "edad",
    values_to = "expectativa") |> 
  drop_na(expectativa) |> 
  mutate(id = glue("{entity}_{edad}")) |> 
  filter(year >= 1950) |> 
  filter(year %% 10 == 0) |> 
  ggplot(aes(year, expectativa, color = edad, group = id)) +
  geom_point(alpha = .3) +
  geom_smooth(
    formula = y ~ x, method = loess, se = FALSE, linewidth = .5) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )

life_expectancy_female_male |> 
  janitor::clean_names() |> 
  filter(code %in% c("ARG", "OWID_WRL")) |> 
  filter(year %% 10 == 0) |> 
  ggplot(aes(year, life_expectancy_diff_fm, color = code)) +
  geom_point(alpha = .3) +
  geom_smooth(
    formula = y ~ x, method = loess, se = FALSE, linewidth = .5) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )



# guardo
ggsave(
  plot = g,
  filename = "2023/semana_49/viz.png",
  width = 30,
  height = 35,
  units = "cm")

# abro
browseURL("2023/semana_49/viz.png")
