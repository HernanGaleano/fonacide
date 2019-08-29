#' Leer encabezado, excel con varias filas de encabezado 
#'
#' @param df 
#' @param inicio 
#'
#' @return
#' @export
#'
#' @examples

leer_encabezado <- function(df, inicio) {
  ## Leer nombre de columnas ----------------------------------
  ### Subset del nombre de las columnas
  encabezado <- slice(df, seq(from = inicio[[1]], to = inicio[[2]]-1))
  
  ### Rellena las celdas combinadas
  m <- dim(encabezado)[[1]] #numero de filas
  n <- dim(encabezado)[[2]] #numero de columnas
  for (i in m:2) {
    for (j in 1:n) {
      if (!is.na(encabezado[[i, j]])) {
        if (is.na(encabezado[[i - 1, j]])) {
          if (is.na(encabezado[[i, j - 1]])) {
            encabezado[[i - 1, j]] <- encabezado[[i, j]]
            encabezado[[i, j]] <- NA#encabezado[[i, j - 1]] #llenar con NA
          } else{
            encabezado[[i - 1, j]] <- encabezado[[i - 1, j - 1]]
          }
        }
      }
    }
  }
  ### Concatenación del encabezado para nombres de columnas
  nombres <- map_chr(encabezado, ~ na.omit(.) %>% str_c(collapse = " - ")) %>% unname()
  
  #Verificar nombres duplicados, concatenar con el de la izquierda
  duplicados <- duplicated(nombres)
  nombres <- map_chr(seq_along(nombres), ~ ifelse(duplicados[[.]], str_c(nombres[[.-1]], nombres[[.]], sep = " - "),nombres[[.]]))
  
  #Cambia el enconding a latin1 para evitar problemas al cargar el tibble en una lista
  nombres <- enc2native(nombres)
  
  return(nombres)
}