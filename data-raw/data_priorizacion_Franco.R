
municipio <- "Presidente Franco"
folder_path <- paste0("data-raw/planillas_priorizacion/", municipio)
files <- list.files(folder_path, full.names = TRUE)

#Nombres(años) para la lista
anhos <- sub(pattern = "\\..*$", replacement = "", basename(files))

priorizacion_Franco <- purrr::map(files, ~ leer_planilla(.))
priorizacion_Franco <- purrr::set_names(priorizacion_Franco, anhos)

nombre <- paste0("data-raw/planillas_priorizacion/", municipio, ".rds")

readr::write_rds(x = priorizacion_Franco, path = nombre)
