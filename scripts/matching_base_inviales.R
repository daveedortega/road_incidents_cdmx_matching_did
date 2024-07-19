## Nuevo Script para Revisar si hay el mismo polígono se puede intersectar varias veces
# DAOA 
# 13/06/2024


# Preparar Espacio --------------------------------------------------------
dev.off()
pacman::p_load(tidyverse, scales, sf, beepr)
# rm(list = ls())

# Cargar Datos ------------------------------------------------------------

# Incidentes Viales
inviales_15 <- read_csv("input/incidentes_viales/inViales_2014_2015.csv") 
inviales_18 <- read_csv("input/incidentes_viales/inViales_2016_2018.csv")
inviales_21 <- read_csv("input/incidentes_viales/inViales_2019_2021.csv")
# Actualizado hasta julio 2023
inviales_22 <- read_csv("input/incidentes_viales/inViales_2022_2023_7.csv") # Además dice alcaldía no colonia
inviales_22 <- inviales_22 %>% rename(delegacion_inicio=alcaldia_inicio,delegacion_cierre=alcaldia_cierre) %>% 
  select(!c(colonia))
# Juntar Incidentes Viales en una sola BdD
inviales_18_22 <- rbind(inviales_15,inviales_18,inviales_21,inviales_22)
rm(inviales_15,inviales_18,inviales_21,inviales_22) 

# Generar Bases Inviales --------------------------------------------------

# Seleccionar Variables necesarias
inviales_18_22 <- inviales_18_22 %>% mutate(ano_mes = format(fecha_cierre,format = "%Y-%m")) %>% 
  select(ano_mes,tipo_incidente_c4,incidente_c4,latitud,longitud,tipo_entrada)

# Convertir a punto geográfico

inviales_18_22 <- inviales_18_22 %>% na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

## Separar base de incidentes viales en 25 categorías
test_17_split <- inviales_18_22 %>% mutate(incidente_tipificado = paste(incidente_c4,tipo_incidente_c4)) %>% # Conjuntamos incidente y tipo
  select(ano_mes,incidente_tipificado,geometry) %>% group_by(incidente_tipificado) %>% group_split() # Se generan 25 categorías

# Genera un split de 25 delitos categorizados, seleccionarmos los más importantes

atropellado_lesionado <- test_17_split[[6]] # ------------------ 254,717 Atropellados Lesionados
choque_cl_accidente <- test_17_split[[7]] # -------------------- 482,412 Choques con Lesionados
choque_sl_accidente <- test_17_split[[11]] # -------------------- 1,033,593 Choques sin Lesionados
moto_accidente <- test_17_split[[18]] # --------------------------- 114,258 Accidentes Motociclistas

# Split by dates 

choque_sl_accidente <- choque_sl_accidente %>% group_split(ano_mes)
choque_cl_accidente <- choque_cl_accidente %>% group_split(ano_mes)
atropellado_lesionado <- atropellado_lesionado %>% group_split(ano_mes)
moto_accidente <- moto_accidente %>% group_split(ano_mes)

# Añadimos total
total_incidentes <- inviales_18_22 %>% group_by(ano_mes) %>% group_split()
rm(test_17_split)

# Funcion que itera sobre lista de incidentes para crear splits por mes
source("iterated_intersection.R")
# Secuencia de Fechas
fechas_secuencia <- seq(as.Date("2013-12-01"),as.Date("2023-07-01"),by = "month")
# Secuencia de colonias
test <- data.frame(id = 1:length(matched_colonias$geometry))
# Transformamos CRS
matched_colonias <- st_transform(matched_colonias,st_crs(choque_sl_accidente[[1]]))
matched_colonias <- matched_colonias %>% st_make_valid()

# Intersectamos 
choque_sl_accidente_final<- iterated_intersection(choque_sl_accidente,test,matched_colonias)
choque_cl_accidente_final <- iterated_intersection(choque_cl_accidente,test,matched_colonias)
atropellado_lesionado_final<- iterated_intersection(atropellado_lesionado,test,matched_colonias)
moto_accidente_final <-  iterated_intersection(moto_accidente,test,matched_colonias)
total_intersected<- iterated_intersection(total_incidentes,test,matched_colonias)

# pegamos IDs únicos
choque_sl_accidente_final <- cbind(ID = matched_colonias$ID,choque_sl_accidente_final) %>% select(!id)
choque_cl_accidente_final <- cbind(ID = matched_colonias$ID,choque_cl_accidente_final) %>% select(!id)
atropellado_lesionado_final <- cbind(ID = matched_colonias$ID,atropellado_lesionado_final) %>% select(!id)
moto_accidente_final <- cbind(ID = matched_colonias$ID,moto_accidente_final) %>% select(!id)
total_intersected <- cbind(ID = matched_colonias$ID,total_intersected) %>% select(!id)

# Pivot
choque_sl_accidente_final <- choque_sl_accidente_final %>% pivot_longer(!ID,names_to = "incidente_fecha",values_to = "incidentes") 
choque_cl_accidente_final <- choque_cl_accidente_final %>% pivot_longer(!ID,names_to = "incidente_fecha",values_to = "incidentes") 
atropellado_lesionado_final <- atropellado_lesionado_final %>% pivot_longer(!ID,names_to = "incidente_fecha",values_to = "incidentes") 
moto_accidente_final <- moto_accidente_final %>% pivot_longer(!ID,names_to = "incidente_fecha",values_to = "incidentes") 
total_intersected <- total_intersected %>% pivot_longer(!ID,names_to = "incidente_fecha",values_to = "incidentes") 

# borramos lista
# str_remove(choque_sl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")#Remove everything starting with list(c( and until a whitespace (\s)
choque_sl_accidente_final$incidente_fecha <- str_remove(choque_sl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
choque_cl_accidente_final$incidente_fecha <- str_remove(choque_cl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
atropellado_lesionado_final$incidente_fecha <- str_remove(atropellado_lesionado_final$incidente_fecha, "list\\(c\\(.*\\s")
moto_accidente_final$incidente_fecha <- str_remove(moto_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")

# separamos las fechas en otra columna
choque_sl_accidente_final <- choque_sl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")# Look around for separator
choque_cl_accidente_final <- choque_cl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")
atropellado_lesionado_final <- atropellado_lesionado_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Lesionado )")
moto_accidente_final <- moto_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")
# New sep
total_intersected <- total_intersected %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

# mutamos para agregar Total
total_intersected <- total_intersected %>% mutate(incidente = "Total") %>% group_by(ID,incidente,ano_mes) %>% 
  summarise(incidentes = sum(incidentes))

# Quitamos unused whitespace
choque_sl_accidente_final$incidente <- choque_sl_accidente_final$incidente %>% str_squish()
choque_cl_accidente_final$incidente <- choque_cl_accidente_final$incidente %>% str_squish()
atropellado_lesionado_final$incidente <- atropellado_lesionado_final$incidente %>% str_squish()
moto_accidente_final$incidente <- moto_accidente_final$incidente %>% str_squish()

# Hacemos fechas fechas y ordenamos
choque_sl_accidente_final <- choque_sl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,ID,incidente,incidencia = incidentes) 
choque_cl_accidente_final <- choque_cl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,ID,incidente,incidencia = incidentes) 
atropellado_lesionado_final <- atropellado_lesionado_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,ID,incidente,incidencia = incidentes) 
moto_accidente_final <- moto_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,ID,incidente,incidencia = incidentes) 
total_intersected <- total_intersected %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,ID,incidente,incidencia = incidentes) 

# En creamos base final

# Sequence of dates
dates_complete <- fechas_secuencia %>% rep(length(matched_colonias$ID)) 
# id of colonias
colonias_key <- matched_colonias$ID %>% rep(length(fechas_secuencia)) %>% sort() 
# complete df
dummy_df <- tibble(dates_complete,ID = colonias_key)
complete_df <- dummy_df %>% left_join(matched_colonias,by = "ID")

# Pegamos a Base Original
complete_choque_sl_accidente <- complete_df %>% left_join(choque_sl_accidente_final, by = c("ID","dates_complete"))
complete_choque_cl_accidente <- complete_df %>% left_join(choque_cl_accidente_final, by = c("ID","dates_complete"))
complete_atropellado_lesionado <- complete_df %>% left_join(atropellado_lesionado_final, by = c("ID","dates_complete"))
complete_moto_accidente <- complete_df %>% left_join(moto_accidente_final, by = c("ID","dates_complete"))
complete_total<- complete_df %>% left_join(total_intersected, by = c("ID","dates_complete"))

# Suma de Incidentes

complete_suma_invi <- complete_df %>% 
  left_join(choque_sl_accidente_final, by = c("ID","dates_complete")) %>% 
  left_join(choque_cl_accidente_final, by = c("ID","dates_complete")) %>% 
  left_join(atropellado_lesionado_final, by = c("ID","dates_complete")) %>% 
  left_join(moto_accidente_final, by = c("ID","dates_complete")) %>% 
  mutate(incidencia = ifelse(is.na(incidencia.x),0, incidencia.x)+ifelse(is.na(incidencia.y),0, incidencia.y)+
           ifelse(is.na(incidencia.y.y),0, incidencia.y.y)+ifelse(is.na(incidencia.x.x),0, incidencia.x.x)) %>% 
  mutate(incidente = "Suma")


# Sanity Checks
complete_choque_sl_accidente$incidencia %>% sum(na.rm = T)
choque_sl_accidente_final$incidencia %>% sum()

complete_choque_cl_accidente$incidencia %>% sum(na.rm = T)
choque_cl_accidente_final$incidencia %>% sum()

complete_atropellado_lesionado$incidencia %>% sum(na.rm = T)
atropellado_lesionado_final$incidencia %>% sum()

complete_moto_accidente$incidencia %>% sum(na.rm = T)
moto_accidente_final$incidencia %>% sum()

complete_suma_invi$incidencia %>% sum(na.rm = T)
sum(moto_accidente_final$incidencia,atropellado_lesionado_final$incidencia, choque_cl_accidente_final$incidencia, choque_sl_accidente_final$incidencia)

total_intersected$incidencia %>% sum(na.rm = T)
complete_total$incidencia %>% sum(na.rm = T)

# Final DBs ---------------------------------------------------------------


complete_choque_sl_accidente <- complete_choque_sl_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11") ,1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_choque_cl_accidente <- complete_choque_cl_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,
         cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_atropellado_lesionado <- complete_atropellado_lesionado %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_moto_accidente <- complete_moto_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

## Creamos una que sea la suma de las anteriores con lo que ya tenemos hasta aquí ----
complete_suma_invi <- complete_suma_invi %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

## Total de incidentes viales ----
complete_total <- complete_total %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03") & dates_complete< as.Date("2023-01-30"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,ID,COLONIA, match,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,
         cb_1b,cb_2b,tr_eb,tr_9b,l_1b,l_12b,
         inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)


# Clean Space
rm(list = setdiff(ls(),c('complete_total', 'complete_suma_invi', 'complete_moto_accidente', 
                         'complete_atropellado_lesionado', 'complete_choque_cl_accidente', 
                         'complete_choque_sl_accidente', 'complete_delitos', 'complete_suma', 'complete_complemento', 
                         'complete_plagios', 'complete_violfam', 'complete_lesiones', 
                         'complete_danios', 'complete_sexual', 'complete_asesinatos', 
                         'complete_robos')))
print("...DONE...")
beepr::beep(sound = 1)

# Revision of new matches
complete_total %>% count(ID) %>% count(n)# unique id
complete_total %>% count(COLONIA) %>% filter(n>116) %>% arrange(desc(n))# higher matching in names (duplicates)
complete_total %>% filter(COLONIA == 'Santa Maria Tomatlan') %>% # Plots
  ggplot(aes(dates_complete, incidencia))+
  geom_line()+
  facet_wrap(~ID)

# Save --------------------------------------------------------------------

save(complete_atropellado_lesionado, file = './output/baes_generadas/matches/atropellado_lesionado.Rdata')
save(complete_choque_cl_accidente, file = './output/baes_generadas/matches/choque_con_lesionados.Rdata')
save(complete_choque_sl_accidente, file = './output/baes_generadas/matches/choque_sin_lesionados.Rdata')
save(complete_moto_accidente, file = './output/baes_generadas/matches/accidentes_moto.Rdata')
save(complete_suma_invi, file = './output/baes_generadas/matches/suma_incidentes_viales.Rdata')
save(complete_total, file = './output/baes_generadas/matches/complete_incidentes.Rdata')


