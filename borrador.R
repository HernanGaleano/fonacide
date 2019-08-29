#Códigos útiles

# table(test$anio, test$fechaCorte)


# > test %>% 
#   +     filter(anio==2012, grepl(pattern="altos", tolower(descripcionEntidad))) %>% 
#   +     group_by(mes) %>% 
#   +     summarise(sum(montoTransferido))


# completo %>% 
#   filter(anio==2019) %>% 
#   group_by(descripcionNivel) %>% 
#   summarise(sum(montoTransferido))