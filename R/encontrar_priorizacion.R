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


#Ejemplos
#get_prio_estb(1001004, "Aulas")
#get_prio_estb(1002008, "2015")


# Para obtener los nombres de columnas de priorizacion_franco --------------------------------
#Para un años
#purrr::map(areas, ~ stringr::str_extract_all(names(priorizacion_Franco[["2014"]][[.]]),".*ombre.*", simplify = TRUE) %>% purrr::keep(. != ""))
#Para todos los años con nombres 
#purrr::map(años, function(x) purrr::map(areas, ~ stringr::str_extract_all(names(priorizacion_Franco[[x]][[.]]),".*ombre.*", simplify = TRUE) %>% purrr::keep(. != "")) %>% purrr::set_names(areas)) %>% purrr::set_names(años) 

### Capaz hacer que el codigo de institucion sea opcional
get_prio<- function(año, co_establecimiento, co_institucion) {
  areas <- c("Aulas", "Sanitarios", "Otros Espacios", "Equipamientos")
  posiciones <- c(-1, -1, -1, -1)
  posiciones <- purrr::set_names(posiciones, areas)
  
  if (is.na(co_establecimiento) | is.na(co_institucion)) { #Mejorar esto
    return(posiciones)
  }
    
  año <- as.character(año)
  var_cod_establecimiento <- "codigo_establecimiento" #Variable con el nombre de la columna en el tibble
  var_cod_institucion <- "codigo_institucion" #Variable con el nombre de la columna en el tibble

  #año tiene que ser string
  tb_priorizacion <- fonacide::priorizacion_Franco[[año]]

  
  cantidad_resultados <- c(0, 0, 0, 0)
  cantidad_resultados <- purrr::set_names(cantidad_resultados, stringr::str_c("cant_", areas))
  
  #Esto funcionaría para 2014 al 2017
  #Falta considerar cuando no se encuentra la institución
  for (area in areas) {
    if (año %in% c(2018,2018)) { #Esto es para evitar que busque por codigo de institución en los años donde no figura en la planilla (Mejorar implementación)
      num_filas = 0
    } else{
      busqueda <- tb_priorizacion[[area]]%>%
        dplyr::filter(!!as.name(var_cod_institucion) == co_institucion)
      num_filas <- busqueda[1] %>% nrow()  
    }
    if (num_filas == 0) {
      busqueda <- tb_priorizacion[[area]] %>%
        dplyr::filter(!!as.name(var_cod_establecimiento) == co_establecimiento)
      num_filas <- busqueda[1] %>% nrow()  
    }
    if (num_filas != 0) {
      posiciones[[area]] <- busqueda[1] %>% min
      cantidad_resultados[[stringr::str_c("cant_", area)]] <- num_filas
    }
  }
  #nombre_institucion <- busqueda %>% 
  #  dplyr::filter(`Nº de Prioridad (1)` == posicion) %>% .$`Nombre de la Institución`
  
  #return(list("posiciones"=posiciones, "cantidad_resultados"=cantidad_resultados))
  #return(c(posiciones, cantidad_resultados))
  return(posiciones)
}
#get_prio(2017, 6750, 1002017)
#git add -A && git commit -m 'staging all files'


get_prio_list<- function(a_list) {
  print(a_list)
  año <- a_list[[1]]
  co_establecimiento <- a_list[[2]] 
  co_institucion <- a_list[[3]]
  cat(año, co_establecimiento, co_institucion, "/n")
  
  año <- as.character(año)
  var_cod_establecimiento <- "codigo_establecimiento" #Variable con el nombre de la columna en el tibble
  var_cod_institucion <- "codigo_institucion" #Variable con el nombre de la columna en el tibble
  areas <- c("Aulas", "Sanitarios", "Otros Espacios", "Equipamientos")
  #año tiene que ser string
  tb_priorizacion <- fonacide::priorizacion_Franco[[año]]
  posiciones <- c(-1, -1, -1, -1)
  posiciones <- purrr::set_names(posiciones, areas)
  
  cantidad_resultados <- c(0, 0, 0, 0)
  cantidad_resultados <- purrr::set_names(cantidad_resultados, stringr::str_c("cant_", areas))
  
  #Esto funcionaría para 2014 al 2017
  #Falta considerar cuando no se encuentra la institución
  for (area in areas) {
    if (año %in% c(2018,2018)) { #Esto es para evitar que busque por codigo de institución en los años donde no figura en la planilla (Mejorar implementación)
      num_filas = 0
    } else{
      busqueda <- tb_priorizacion[[area]]%>%
        dplyr::filter(!!as.name(var_cod_institucion) == co_institucion)
      num_filas <- busqueda[1] %>% nrow()  
    }
    if (num_filas == 0) {
      busqueda <- tb_priorizacion[[area]] %>%
        dplyr::filter(!!as.name(var_cod_establecimiento) == co_establecimiento)
      num_filas <- busqueda[1] %>% nrow()  
    }
    if (num_filas != 0) {
      posiciones[[area]] <- busqueda[1] %>% min
      cantidad_resultados[[stringr::str_c("cant_", area)]] <- num_filas
    }
  }
  #nombre_institucion <- busqueda %>% 
  #  dplyr::filter(`Nº de Prioridad (1)` == posicion) %>% .$`Nombre de la Institución`
  
  #return(list("posiciones"=posiciones, "cantidad_resultados"=cantidad_resultados))
  #return(c(posiciones, cantidad_resultados))
  return(posiciones)
}
