#' Get priorización
#'
#' Teniendo el tibble prio_tb con datos de priorización de todos los años y
#' áreas, da la posición en el listado priorización según el código de
#' establecimiento, área y año.
#'
#' @param co_establecimiento 
#' @param area 
#' @param a 
#'
#' @return
#' @export
#'
#' @examples
#Hasta el 2017 existen codigo de institución en las planillas de priorización
#A partir del 2016 no se repiten los codigos de establecimiento parece
get_prio_estb <- function(co_establecimiento, año) {
  var_cod_establecimiento <- "codigo_establecimiento" #Variable con el nomber de la columna en el tibble
  areas <- c("Aulas", "Sanitarios", "Otros Espacios", "Equipamientos")
  area <- areas[[1]]
  tb_priorizacion <- fonacide::priorizacion_Franco[[año]]
  
  
  
  #Esto funcionaría para 2018 y 2019
  #Falta considerar cuando no se encuentra la institución
  prio_area <- tb_priorizacion[[area]]
  
  busqueda <- prio_area %>%
    dplyr::filter(!!as.name(var_cod_establecimiento) == co_establecimiento)
  posicion <- busqueda %>% min()
  
  #Cambiar el tipo de columna en la funcion leer priorizacion
  return(busqueda)
  #return(as.integer(fila[[1]]))
}


#Ejemplos
#get_prio_estb(1001004, "Aulas")
#get_prio_estb(1002008, "2019")


# 
# 
# Function encontrar_priori(codigo_institucion, area) {
#   df=tibble[[area]]
#   fila=Encontrar fila de codigo_institucion en tible
#   Return(tibble[[fila]])
#   
#   Ver como trata valores que no se encuentran
#   Supongo que es NA
# }
# 
# priori_todos años(codigo_establecimiento, area) {
#   
#   for años en tibble{
#     out[año] = encontrar_priori(codigo_establecimiento, area, año)
#     
#   }
   