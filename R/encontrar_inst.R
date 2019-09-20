#' Title
#'
#' Dado un string con el nombre de institución (como está en los contratos
#' probablmente), extrae las partes (esc_col, num, nombre_esc) del nombre para
#' luego buscar en matriculacioens.
#' 
#'
#' @param santa 
#'
#' @return
#' @export
#'
#' @examples
extraer_nombre_institucion <- function(santa) {
  aa <- str_split(santa, " ")
  numero <- ""
  nombre_esc <- ""
  
  num <- str_detect(aa[[1]],"(\\d{2,5}|\\d{1,2}\\.\\d{2,5})")
  N <- str_detect(aa[[1]],"N(°|º)")
  esc_col <- str_detect(aa[[1]], regex("Escuela|b.sica|colegio", ignore_case = TRUE))
  nombre_esc <- num|N|esc_col
  
  esc_col <- aa[[1]][esc_col]
  esc_col <- str_c(esc_col, collapse = " ")
  num <- aa[[1]][num]
  nombre_esc <- aa[[1]][!nombre_esc]
  nombre_esc <- str_c(nombre_esc, collapse = " ")
  
  if (length(num) == 0) {
    num <- ""
  }
  return(list(esc_col, num, nombre_esc))
}

#a <- map(contratos$`Institución beneficiada (contratos)`[1:10], ~ extraer_nombre_institucion(.))

#institucion <- "medalla"
#institucion <- bus[[1]]
