#' Lee el encabezado de varias filas
#' 
#' \code{leer_encabezado()} devuleve un string vector con los nombres de las columnas.
#' Concatena los nombres con las filas inferiores en caso que el encabezado tenga varias
#' filas. Se utiliza principalmente para leer los encabezados de las planillas de 
#' priorización.
#'
#' @param df un tibble del cual leer los encabezados
#' @param inicio un numeric vector de tamaño 2 que contiene la posición en `df` donde
#' empiezan el encabezado (\code{inicio[[1]]}) y los datos (\code{inicio[[1]]}).
#'
#' @return un string vector con los nombres de las columnas.
#' @export
#' @encoding UTF-8
#'
leer_encabezado <- function(df, inicio) {
  # Extrae el encabezado del df
  encabezado <- dplyr::slice(df, seq(from = inicio[[1]], to = inicio[[2]] - 1))

  # Rellena las celdas vacías resultantes de las celdas combinadas para poder
  # concatenar después
  m <- dim(encabezado)[[1]] # número de filas
  n <- dim(encabezado)[[2]] # número de columnas
  for (i in m:2) {
    for (j in 1:n) {
      if (!is.na(encabezado[[i, j]])) {
        if (is.na(encabezado[[i - 1, j]])) {
          if (is.na(encabezado[[i, j - 1]])) {
            encabezado[[i - 1, j]] <- encabezado[[i, j]]
            encabezado[[i, j]] <- NA
          } else {
            encabezado[[i - 1, j]] <- encabezado[[i - 1, j - 1]]
          }
        }
      }
    }
  }
  
  # En algunas planillas como "Franco-2014-Otros Espacios" todavía existan celdas vacias
  # en la primera fila de encabezado después del código anterior. Se verifica y 
  # rellenan estas celdas.
  existe_NA <- is.na(encabezado[1, ]) %>% mean()
  if (existe_NA != 0) {
    for (j in 1:n) {
      if (is.na(encabezado[[1, j]])) {
        encabezado[[1, j]] <- paste0(encabezado[[1, j - 1]], j)
      }
    }
  }
  # Concatenación del encabezado para nombres de columnas
  nombres <-
    purrr::map_chr(encabezado, ~ na.omit(.) %>%
      stringr::str_c(collapse = " - ")) %>%
    unname()

  # Verificar nombres duplicados, concatenar con el de la izquierda
  duplicados <- duplicated(nombres)
  nombres <- purrr::map_chr(
    seq_along(nombres),
    ~ ifelse(duplicados[[.]],
      stringr::str_c(nombres[[. - 1]],
        nombres[[.]],
        sep = " - "
      ),
      nombres[[.]]
    )
  )

  # Cambia el enconding a latin1 para evitar problemas al cargar el tibble en
  # una lista
  nombres <- enc2native(nombres)

  return(nombres)
}
