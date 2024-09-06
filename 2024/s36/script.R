
# paquetes ----------------------------------------------------------------

library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
pal <- c("#77053C", "#9C1052", "#D08BAA")
c1 <- "#272F18"
c2 <- "white"
c3 <- "gold"

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

#  IBM Plex Sans
font_add_google(name = "IBM Plex Sans", family = "ibm")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <span style='color:{pal[3]};'><span style='font-family:jet;'>",
  "{{<b>tidytuesdayR</b>}}</span> semana {36}, ",
  "2024 Stack Overflow Annual Developer Survey, <b>Havisha Khurana</b>.</span>"
)
autor <- glue("<span style='color:{pal[3]};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
icon_circulo <- glue(
  "<span style='font-family:jet; color: {c3}'>&#xf111;</span>")
icon_so <- glue("<span style='font-family:jet'>&#xe710;</span>")
usuario <- glue("<span style='color:{pal[3]};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}"
)

# datos -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, 36)
qname <- tuesdata$qname_levels_single_response_crosswalk
survey_questions <- tuesdata$stackoverflow_survey_questions
single_response <- tuesdata$stackoverflow_survey_single_response

# me interesa la opinión de los Argentinos sobre IA

# preguntas y las respuestas asociadas
preg_level <- inner_join(
  qname,
  survey_questions,
  by = join_by(qname)
) |> 
  select(question, everything())

# etiqueta de las preguntas
preg <- c("ai_select", "ai_acc", "ai_complex", "ai_threat")

# respuestas Argentinas
arg_resp <- single_response |> 
  filter(country == "Argentina") |> 
  select(any_of(preg)) |> 
  pivot_longer(
    cols = everything(),
    names_to = "qname",
    values_to = "level"
  )

# cantidad de respuestas
d_arg <- inner_join(
  arg_resp,
  preg_level,
  by = join_by(qname, level)
) |> 
  select(-level) |> 
  count(qname, label, question) |> 
  mutate(tot = sum(n), .by = qname) |> 
  mutate(prop = round(n/tot*100, 1)) |> 
  mutate(qname = fct(qname, levels = preg)) |>
  mutate(question = fct_reorder(question, as.numeric(qname))) |>
  mutate(region = "Argentina")

# orden de las respuestas
orden_ai_select <- c(
  "No, but I plan to soon",
  "No, and I don't plan to",
  "Yes"
)
orden_ai_select <- glue("1_____{orden_ai_select}")

orden_ai_acc <- c(
  "Highly trust",
  "Somewhat trust",
  "Neither trust nor distrust",
  "Somewhat distrust",
  "Highly distrust"
)
orden_ai_acc <- glue("2_____{orden_ai_acc}")

orden_ai_complex <- c(
  "Very poor at handling complex tasks",
  "Bad at handling complex tasks",
  "Neither good or bad at handling complex tasks",
  "Good, but not great at handling complex tasks",
  "Very well at handling complex tasks"
)
orden_ai_complex <- glue("3_____{orden_ai_complex}")

orden_ai_threat <- c(
  "Yes",
  "I'm not sure",
  "No"
)
orden_ai_threat <- glue("4_____{orden_ai_threat}")

# acomodo todas las respuestas
ordenes <- c(orden_ai_select, orden_ai_acc, orden_ai_complex, orden_ai_threat)
ordenes_eje <- str_remove(ordenes, "._____")
names(ordenes_eje) <- ordenes

# traducciones de las respuestas
ordenes_trad <- c(
  "No, pero planeo hacerlo pronto",
  "No, y no planeo hacerlo",
  "Sí",
  "Confío plenamente",
  "Algo confío",
  "No confío ni desconfío",
  "Algo desconfío",
  "Desconfío plenamente",
  "Muy malo operando tareas complejas",
  "Mal operando tareas complejas",
  "Ni bien ni mal operando tareas complejas",
  "Bien operando tareas complejas",
  "Muy bien operando tareas complejas",
  "Sí",
  "No sé",
  "No"
) |> str_wrap(20)
names(ordenes_trad) <- ordenes

# agrego los órdenes a las respuestas
orden_arg <- d_arg |> 
  arrange(qname, label) |> 
  mutate(orden = glue("{as.numeric(qname)}_____{label}")) |> 
  mutate(orden = factor(orden, ordenes)) |> 
  arrange(ordenes) |> 
  mutate(label = fct_inorder(label))

# combino todas las respuestas
d <- inner_join(orden_arg, d_arg) |> 
  arrange(orden) |> 
  mutate(label = fct_reorder(label, as.numeric(orden)))

# figura ------------------------------------------------------------------

# traducción de las preguntas
question_trad <- c(
  "¿Usás <b>IA</b> en tu trabajo?",
  "En tu trabajo, ¿cuánta presición creés que<br>tienen las respuestas generadas por <b>IA</b>?",
  "Las <b>IA</b>, ¿qué tan bien manejan las<br>tareas complejas?",
  "En tu trabajo, ¿creés que la <b>IA</b> es<br>una amenaza?")
question_trad <- glue("{icon_circulo} {question_trad}")
names(question_trad) <- sort(unique(d$question))

# subtítulo
mi_subtitulo <- glue(
  "Qué opinan los usuarios ",
  "<b style='color: {c3}; font-family: ibm;'>ARGENTINOS</b> de ",
  "stack<b>overflow</b> acerca<br>de la Inteligencia Artificial."
)

# figura
g <- ggplot(d, aes(prop, orden)) +
  geom_col(show.legend = TRUE, fill = c1) +
  geom_richtext(
    aes(label = glue("{prop}%")), hjust = .15, position = position_nudge(y = 0),
    label.r = unit(0, "mm"), label.size = unit(1, "mm"), color = c2,
    label.color = c1, fill = pal[1], family = "jet", ) +
  facet_wrap(
    vars(question), ncol = 2, scales = "free",
    labeller = as_labeller(question_trad)) +
  scale_x_continuous(
    limits = c(0, NA), labels = scales::label_number(suffix = "%")
  ) +
  scale_y_discrete(labels = ordenes_trad) +
  coord_cartesian(clip = "off", xlim = c(0, NA)) +
  labs(
    title = icon_so, subtitle = mi_subtitulo, x = NULL, caption = mi_caption
  ) +
  theme_void() +
  theme(
    aspect.ratio = 1.1,
    plot.margin = margin(t = 15, r = 130, l = 16.4, b = 5),
    plot.background = element_rect(
      fill = pal[1], color = pal[2], linewidth = 3
    ),
    plot.title.position = "plot",
    plot.title = element_markdown(
      size = 60, color = c3, margin = margin(l = 20)
    ),
    plot.subtitle = element_markdown(
      family = "ubuntu", color = c2, size = 20,
      margin = margin(l = 120, t = -50)
    ),
    plot.caption = element_markdown(
      family = "ubuntu", color = c2, size = 12,
      margin = margin(t = 24, b = 5, r = -110)
    ),
    panel.spacing.x = unit(4, "line"),
    panel.spacing.y = unit(2, "line"),
    panel.background = element_rect(fill = pal[2], color = NA),
    panel.grid.major.x = element_line(
      colour = pal[3], linetype = "FF", linewidth = .1
    ),
    axis.text = element_text(color = c2),
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      family = "ubuntu", hjust = 1, size = 13, margin = margin(r = 5)
    ),
    axis.text.x = element_text(
      family = "jet", margin = margin(t = 5), hjust = 0, color = pal[3]
    ),
    strip.background = element_blank(),
    strip.text = element_markdown(
      family = "ibm", color = c2, hjust = 0, size = 15,
      margin = margin(b = 5)
    )
  )

# guardo
ggsave(
  plot = g,
  filename = "2024/s36/viz.png",
  width = 30,
  height = 24,
  units = "cm")

# abro
browseURL(glue("{getwd()}/2024/s36/viz.png"))

