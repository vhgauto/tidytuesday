
# browseURL("https://nrennie.rbind.io/blog/script-templates-r/")

# paquetes ----------------------------------------------------------------

library(glue)
library(stringr)
library(crayon)

# función que crea una nueva carpeta con un script para el procesamiento
# de los datos de tidytuesday de la semana de interés

nueva_semana <- function(semana_numero, año = 2024) {
  
  # nombre de la carpeta a crear
  if (semana_numero <= 9) {
    semana_carpeta <- glue("{año}/s0{semana_numero}")
  } else {
    semana_carpeta <- glue("{año}/s{semana_numero}")
  }
  
  # archivo .R
  new_file <- file.path(semana_carpeta, "script.R")
  
  # verifico que la carpeta de la semana no exista
  semanas_ok <- list.files(glue("{año}")) |> 
    str_remove("s") |> 
    as.numeric()
  
  if (mean(semanas_ok == semana_numero) != 0) {
    
    
    cat(red("\n\n\nSemana ya creada\n\n\n"))
    
    system(glue("open {new_file}"))
    
    stop()
  }
  
  # verifico que el número de semana sea correcto
  if (semana_numero %% as.integer(semana_numero) != 0) {
    
    stop(glue(red("\n\n\nNúmero de semana en formato incorrecto\n\n\n")))
  }
  
  # creo directorio
  dir.create(semana_carpeta, recursive = TRUE)
  
  glue(blue(bold("\n\nNueva carpeta creada\n\n")))
  
  if (!file.exists(new_file)) {
    file.create(new_file)
    
    # copy lines to .R file
    r_txt <- readLines("plantilla.R")
    
    # replace placeholder text with variables
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
    
    # write to new file
    writeLines(r_txt, con = new_file)
    
    cat(blue(bold("\n\nArchivo R creado para semana {semana_numero}\n\n")))
    
  }
  
  system(glue("open {new_file}"))
  
  source(new_file)
    
}

cat(bgRed(
  white(
    '\n\nUsar función ' %+%
      bold('nueva_semana()') %+%
      ' para iniciar el procesamiento.\n\n'
  )
))
