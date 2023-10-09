
# paquetes ----------------------------------------------------------------

library(tidyverse)

# fuente ------------------------------------------------------------------

# colores

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# wage, eje vertical
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# años, eje horizontal
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = FALSE)
# título
font_add_google(name = "Taviraj", family = "taviraj")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solids", "icon/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'><span style='font-family:victor;'>{{<b>tidytuesdayR</b>}}</span> semana 36. Union Membership and Coverage Database. B. Hirsch, D. Macpherson, W. Even</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-03/readme.md")

grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')

glimpse(grants)
glimpse(grant_opportunity_details)

grant_opportunity_details |> 
  select(
    original_closing_date_for_applications, 
    current_closing_date_for_applications,
    starts_with("category")) |> 
  select(-category_explanation) |> 
  drop_na(
    original_closing_date_for_applications, 
    current_closing_date_for_applications) |> 
  pivot_longer(
    cols = starts_with("category"), names_to = "cat", values_to = "valor") |> 
  filter(valor) |> 
  rename(
    original = original_closing_date_for_applications,
    current = current_closing_date_for_applications) |> 
  mutate(cat = str_remove(cat, "category_")) |> 
  ggplot(aes(original, current, color = cat)) +
  geom_point()
