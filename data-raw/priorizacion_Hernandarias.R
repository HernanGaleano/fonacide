
municipio <- "Hernandarias"
folder_path <- paste0("data-raw/planillas_priorizacion/", municipio)
files <- list.files(folder_path, full.names = TRUE)

#Nombres(años) para la lista
anhos <- sub(pattern = "\\..*$", replacement = "", basename(files))

priorizacion_Hernandarias <- purrr::map(files, ~ leer_planilla(.))
priorizacion_Hernandarias <- purrr::set_names(priorizacion_Hernandarias, anhos)

nombre <- paste0("data-raw/planillas_priorizacion/", municipio, ".rds")

readr::write_rds(x = priorizacion_Hernandarias, path = nombre)
