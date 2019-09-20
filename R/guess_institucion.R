#' Guess institución
#'
#' A partir de la lista(escuela/colegio, num, nombre) (resultado de la función
#' "encontrar_inst" da la institución resultado de la planilla matriculación
#'
#' @param lista 
#'
#' @return
#' @export
#'
#' @examples
guess_institucion <- function(lista) {
  #lista es la lista resultado de extraer_nombre (escuela/colegio, num, nombre)
  esc_col <- lista[[1]]
  num <- lista[[2]]
  nombre <- lista[[3]]
  
  #hacer la conversión si las variables no están vacias
  nombre <- stri_trans_general(nombre, "Latin-ASCII")
  esc_col <- stri_trans_general(esc_col, "Latin-ASCII")
  resultado <- matriAlto %>%
    select(`Nombre de la Institución`, codigo_establecimiento, `Cód. Institución`, Localidad)
  
  resultado <- matriAlto %>%
    dplyr::filter(
      stringr::str_detect(
        nombre_institucion,
        regex(nombre, ignore_case = TRUE)
      )
    )
  
  #Cambiar: Si hay más de uno pero si no está vacío, si está vacio agarrar el matriAlto
  if (nrow(resultado) != 1) { #Buscar por número de escuela
    #Que este sea el nuevo si tiene menos filas pero no está vacío
    bus <- resultado %>%
      dplyr::filter(
        stringr::str_detect(
          nombre_institucion,
          regex(num, ignore_case = TRUE)
        )
      )
    if (nrow(bus) != 0) {
      resultado <- bus
    }
  } 
  
  if (nrow(resultado) != 1) {
    #Que este sea el nuevo si tiene menos filas pero no está vacío
    bus <- resultado %>%
      dplyr::filter(
        stringr::str_detect(
          nombre_institucion,
          regex(esc_col, ignore_case = TRUE)
        )
      )
    if (nrow(bus) != 0) {
      resultado <- bus
    }
  }   
  # if (nrow(resultado) != 1){
  #   #output
  #   output <- list(3)
  # }
  
  output <- list(resultado$codigo_establecimiento, resultado$`Cód. Institución`, resultado$`Nombre de la Institución`)
  #return(output)    
}
