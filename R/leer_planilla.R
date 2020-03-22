#' Leer planilla de priorización
#'
#' Lee la planilla de priorización en formato excel y devuelve una
#' lista con tibbles para cada área de FONACIDE.
#'
#' @param path ruta al archivo a leer
#' @param inicio_encabezado expresión regular para detectar el inicio del encabezado
#' @param inicio_datos expresión regular para detectar el inicio de los datos
#'
#' @return Una lista de tibbles con las planillas para cada área
#' @export
#' @encoding UTF-8
#'
leer_planilla <- function(path,
                          inicio_encabezado = "Prioridad",
                          inicio_datos = "^1$") {
  cat("---------------------------------------------\n")
  cat("Leyendo el archivo:", basename(path), "\n")
  
  hojas <- readxl::excel_sheets(path)
  output <- vector("list", length(hojas))
  names(output) <- hojas
  
  cat("La planilla tiene", length(hojas), "hojas.\n")
  for (sheet in hojas) {
    # Se lee la hoja completa
    df <- readxl::read_excel(
      path = path,
      sheet = sheet,
      col_types = "text",
      col_names = FALSE,
      na = "-",
      .name_repair = ~ make.names(sample(letters, length(.), replace = TRUE), unique = TRUE) # Se generan nombres aleatorios para evitar mensajes en consola
    )
    cat(which(sheet == hojas), ") ", sheet, "\n \t", sep = "")
    # Se descartan posibles columnas sin datos
    # df <- df %>% dplyr::select_if(~ is.na(.) %>% mean() < 0.9)
    
    
    # Buscar inicio de encabezado y datos ---------------------------
    # inicio_encabezado <- "Prioridad"
    # inicio_datos <- "^1$"
    inicio <- integer()
    j <- 1
    inicio_regex <- stringr::str_c(inicio_encabezado, inicio_datos, sep = "|")
    while (!length(inicio) == 2) {
      inicio <- stringr::str_which(df[[j]], inicio_regex)
      j <- j + 1
      if (j == 6) {
        stop("No se encontró el inicio de la planilla")
        break()
      }
    }
    
    # Obtener nombre de columnas ---------------------------
    nombres <- leer_encabezado(df, inicio)
    nombres <- purrr::map_chr(nombres, ~ stringr::str_replace(., ".*stablecimiento.*|.*local.*", "codigo_establecimiento"))
    nombres <- purrr::map_chr(nombres, ~ stringr::str_replace(., ".*digo de Instituc.*", "codigo_institucion"))
    cat("Nombres de columnas obtenidos.\n \t")
    
    # Leer los datos sin el encabezado ----------------------------------
    planilla <- dplyr::slice(df, seq(from = inicio[[2]], to = nrow(df)))

    # Se descartan posibles filas sin datos, como explicaciones al final de la
    # planilla que comienzan con números y estan en la primera columna.
    planilla <- planilla %>% dplyr::filter(stringr::str_detect(.[[1]], "^\\d+$"))

    # Nombrar correctamente las columnas ----------------------------------
    planilla <- planilla %>% dplyr::rename_all(~nombres)
    planilla <- planilla %>% dplyr::mutate_all(~ readr::parse_guess(.))
    output[[sheet]] <- planilla
    cat("Hoja leída correctamente.\n")
  }

  # Nombrar correctamente la lista con el área FONACIDE correspondiente (para planillas de priorización)
  areas_fonacide <- stringr::str_replace_all(
    hojas,
    c(
      ".*ula.*" = "Aulas",
      ".*anitari.*" = "Sanitarios",
      ".*spacio.*" = "Otros Espacios",
      ".*quipam.*" = "Equipamientos",
      ".*obiliario.*" = "Equipamientos",
      ".*limentac.*|.*scola.*" = "Alimentación escolar"
    )
  )
  names(output) <- areas_fonacide
  return(output)
}

# Falta ver como cada hojas toma los valores NA (- capaz)
