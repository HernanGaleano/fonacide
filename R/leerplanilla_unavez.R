#' Leer planilla de priorización
#'
#' @param path
#'
#' @return Una lista de tibbles con las planillas para cada área
#' @export
#'
#' @examples
leer_planilla <- function(path, 
                          inicio_encabezado = "Prioridad",
                          inicio_datos = "^1$") {
  cat("------- Leyendo el archivo", basename(path),"\n")
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
      .name_repair = ~ make.names(base::sample(letters, length(.), replace = TRUE), unique = TRUE) # Se generan nombres aleatorios para evitar mensajes en consola
    )
    cat("Leyendo hoja:", sheet, "\t")
    # Se descartan posibles columnas sin datos
    #df <- df %>% dplyr::select_if(~ is.na(.) %>% mean() < 0.9)

    # Busca donde comienza el encabezado (Prioridad) y donde comienzan los datos (1)
    inicio <- integer()
    j <- 1

    # inicio_encabezado <- "Prioridad"
    # inicio_datos <- "^1$"
    inicio_regex <- stringr::str_c(inicio_encabezado, inicio_datos, sep = "|")
    while (!length(inicio) == 2) {
      inicio <- stringr::str_which(df[[j]], inicio_regex)
      j <- j + 1
      if (j == 6) {
        stop("No se encontró el inicio de la planilla")
        break()
      }
    }
    
    nombres <- leer_encabezado(df, inicio)
    nombres <- purrr::map_chr(nombres, ~ stringr::str_replace(., ".*stablecimiento.*|.*local.*", "codigo_establecimiento"))
    nombres <- purrr::map_chr(nombres, ~ stringr::str_replace(., ".*digo de Instituc.*", "codigo_institucion"))
    cat("Nombres obtenidos.\t")
    ## Leer los datos sin el encabezado ----------------------------------
    planilla <- dplyr::slice(df, seq(from = inicio[[2]], to = nrow(df)))

    # Se descartan posibles filas sin datos, como explicaciones al final de la
    # planilla que comienzan con números y estan en la primera columna.
    planilla <- planilla %>% dplyr::filter(stringr::str_detect(.[[1]], "^\\d+$"))

    ## Nombrar correctamente las columnas ----------------------------------
    planilla <- planilla %>% dplyr::rename_all( ~ nombres)
    planilla <- planilla %>% dplyr::mutate_all( ~ readr::parse_guess(.)) 
    output[[sheet]] <- planilla
    cat("Hoja leída correctamente.\n")
  }

  # Nombrar correctamente la lista con el area FONACIDE correspondiente (para planillas de priorización)
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
