
# browseURL("https://nrennie.rbind.io/blog/script-templates-r/")

# mensaje -----------------------------------------------------------------

mensaje <- function(x) {
  cat(
    crayon::bgBlack(
      crayon::white(
        glue::glue(
          "\n\n--- {x} ---\n\n\n")
      )
    )
  )
}

# paquetes ----------------------------------------------------------------

paquetes <- function() {
  purrr::map(
    c("glue", "ggtext", "showtext", "tidyverse"),
    ~suppressPackageStartupMessages(
      library(
        package = .x,
        character.only = TRUE,
        warn.conflicts = FALSE,
        quietly = TRUE,
        verbose = FALSE
      )
    )
  )
}

paquetes()

# nueva semana ------------------------------------------------------------

# función que crea una nueva carpeta con un script para el procesamiento
# de los datos de {tidytuesday} de la semana de interés

nueva_semana <- function(semana_numero, año = 2025) {
  
  semana_numero <<- semana_numero
  
  # nombre de la carpeta a crear
  if (semana_numero <= 9) {
    semana_carpeta <- glue::glue("{año}/s0{semana_numero}")
  } else {
    semana_carpeta <- glue::glue("{año}/s{semana_numero}")
  }
  
  # archivo .R
  new_file <- file.path(semana_carpeta, "script.R")
  
  # verifico que la carpeta de la semana no exista
  semanas_ok <- list.files(glue::glue("{año}")) |> 
    stringr::str_remove("s") |> 
    as.numeric()
  
  if (length(semanas_ok) != 0 & mean(semanas_ok == semana_numero) != 0) {
    
    mensaje("Semana ya creada")
    
    system(glue::glue("open {new_file}"))
    
    stop()
  }
  
  # verifico que el número de semana sea correcto
  if (semana_numero %% as.integer(semana_numero) != 0) {
    
    stop(
      mensaje("Número de semana en formato incorrecto")
    )
  }
  
  # creo directorio
  dir.create(semana_carpeta, recursive = TRUE)
  
  if (!file.exists(new_file)) {
    file.create(new_file)
    
    # leo el contenido de la plantilla
    r_txt <- readLines("_plantilla.R")
    
    # remplazo el año, nombre de carpeta y semana
    r_txt <- gsub(
      pattern = "año",
      replacement = año,
      x = r_txt
    )
    
    r_txt <- gsub(
      pattern = "semana_carpeta",
      replacement = semana_carpeta,
      x = r_txt
    )
    
    r_txt <- gsub(
      pattern = "semana_numero",
      replacement = semana_numero,
      x = r_txt
    )
    
    # creo el nuevo script
    writeLines(r_txt, con = new_file)
    
    mensaje(glue::glue("Script creado para semana {semana_numero}"))
    
  }
  
  system(glue::glue("open {paste0(getwd(), '/', new_file)}"))
  
  source(new_file)
    
}

# caption -----------------------------------------------------------------

caption <- function(fuente1, fuente2 = NULL, col, week) {
  
  if (is.null(fuente2)) {
    fuente <- glue(
      "Datos: <span style='color:{col};'><span style='font-family:jet;'>",
      "{{<b>tidytuesdayR</b>}}</span> semana {week}, ",
      "<b>{fuente1}</b>.</span>"
    )
  } else {
    fuente <- glue(
      "Datos: <span style='color:{col};'><span style='font-family:jet;'>",
      "{{<b>tidytuesdayR</b>}}</span> semana {week}, ",
      "<b>{fuente1}</b>, {fuente2}.</span>"
    )
  }
  
  autor <- glue("<span style='color:{col};'>**Víctor Gauto**</span>")
  icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
  icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
  icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
  icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
  icon_bsky <- glue("<span style='font-family:jet;'>&#xe28e;</span>")
  usuario <- glue("<span style='color:{col};'>**vhgauto**</span>")
  sep <- glue("**|**")
  
  mi_caption <- glue(
    "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
    "{icon_mastodon} {icon_bsky} {usuario}"
  )
  
  return(mi_caption)
}

mensaje("Paquetes y funciones cargadas")

mensaje(
  glue::glue(
    "Usar {crayon::bold('nueva_semana()')} para iniciar el procesamiento"
  )
)
