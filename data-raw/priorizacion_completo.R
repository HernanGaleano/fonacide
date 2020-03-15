

archivos <- list.files("data-raw/planillas_priorizacion/", pattern = "rds", full.names = TRUE)
nombres_municipios <- list.files("data-raw/planillas_priorizacion/", pattern = "rds") %>% 
                      stringr::str_remove(".rds")

priorizacion_fonacide <- purrr::map(archivos, readr::read_rds) %>% 
                         purrr::set_names(nombres_municipios)

usethis::use_data(priorizacion_fonacide, name = priorizacion_fonacide, overwrite = TRUE)
