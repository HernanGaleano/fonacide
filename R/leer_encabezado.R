#' Read multi row heading
#'
#' \code{read_headings} returns a string vector with concatenated names of a
#' multi row heading.
#' 
#' @param df a tibble to read the headings from
#' @param inicio a numeric vector length 2 that contains the row number where the
#'   heading (position 1) and the data (position 2) start.
#'
#' @return a string vector that contains the concatenated columns names in the heading.
#' @export
#'
#'
leer_encabezado <- function(df, inicio) {
  ## Leer nombre de columnas ----------------------------------
  ### Subset del nombre de las columnas
  encabezado <- dplyr::slice(df, seq(from = inicio[[1]], to = inicio[[2]] - 1))

  ### Rellena las celdas vacías que quedan de las celdas combinadas
  m <- dim(encabezado)[[1]] # numero de filas
  n <- dim(encabezado)[[2]] # numero de columnas
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
  ### En caso de que todavía existan celdas vacias en la primera fila de encabezado
  ### Para Franco-2014-Otros Espacios
  existe_NA <- is.na(encabezado[1, ]) %>% mean
  if (existe_NA != 0) {
    for (j in 1:n) {
      if (is.na(encabezado[[1, j]])) {
        encabezado[[1, j]] <- paste0(encabezado[[1, j-1]], j)
      }
    }
  }
  
  
  
  
  ### Concatenación del encabezado para nombres de columnas
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
