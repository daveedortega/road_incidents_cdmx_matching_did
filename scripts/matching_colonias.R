## Creation of a Synthetic Control Group
## David A. Ortega - 165229
# 13/06/2023

## Preparar Espacio ----
dev.off()
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
options(scipen = 9999)

## Cargar Mapas ----

colonias_cdmx <- read_sf("~/Desktop/Mapas/colonias_indicemarginacionurbana_2020/cdmx_imu_2020/cdmx_imu2020.shp")
colonias_cdmx <- colonias_cdmx %>% st_make_valid() # Hacemos válidos los polígonos inválidos

# Mapas de líneas Y buffers:
#CB L1
cablebus_1b <- read_sf("~/Desktop/Mapas/cb_l1_e/cb_l1_b500.shp")
cablebus_1 <- read_sf("~/Desktop/Mapas/cb_l1_e/cb_l1_l.shp")
#CB L2
cablebus_2b <- read_sf("~/Desktop/Mapas/cb_l2_shp/cb_l2_l.shp")
cablebus_2 <- read_sf("~/Desktop/Mapas/cb_l2_shp/cb_l2_b500.shp")
#TR L9
trole_l9b <- read_sf("~/Desktop/Mapas/trolebus/trolebus_l9_b500.shp")
trole_l9 <- read_sf("~/Desktop/Mapas/trolebus/t2.shp") %>% filter(LINEA == 9) %>% st_transform(crs = st_crs(cablebus_2b))
#TR E
trole_elevadob <- read_sf("~/Desktop/Mapas/tr_e_shp/tr_e_b500_v.shp")
trole_elevado <- read_sf("~/Desktop/Mapas/tr_e_shp/tr_e_l.shp")
# L12
l12b <- read_sf("~/Desktop/Mapas/STC_shp/stc_l12_b500.shp") # Toda la línea
el12b <- read_sf("~/Desktop/Mapas/STC_shp/stc_l12_eb500_feb.shp")
l12bfeb <- read_sf("~/Desktop/Mapas/STC_shp/l12b_cerrada_postenero.shp")%>% st_transform(crs = st_crs(cablebus_1)) # Buffer que continua cerrado post enero
l12<- read_sf("~/Desktop/Mapas/STC_shp/stc_l.shp") %>% filter(LINEA == "12") # Toda la línea
# L1
l1b <-  read_sf("~/Desktop/Mapas/STC_shp/stc_l1_ec500m.shp") # Estaciones Cerradas
l1 <- read_sf("~/Desktop/Mapas/STC_shp/stc_l.shp") %>% filter(LINEA == "1")

# Intersectamos con mapa de colonias para saber cuáles intersectan ----
# Revisamos cuales son 
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(cablebus_1)) # Cambiamos mapa a mismo CRS de otros, ahora todos tienen el mismo
colonias_cdmx <- colonias_cdmx %>% st_make_valid()

cb_1 <- lengths(st_intersects(colonias_cdmx,cablebus_1))
cb_1b <- lengths(st_intersects(colonias_cdmx,cablebus_1b))

cb_2 <- lengths(st_intersects(colonias_cdmx,cablebus_2))
cb_2b <- lengths(st_intersects(colonias_cdmx,cablebus_2b))

tr_e <- lengths(st_intersects(colonias_cdmx,trole_elevado))
tr_eb <- lengths(st_intersects(colonias_cdmx,trole_elevadob))

tr_9 <- lengths(st_intersects(colonias_cdmx,trole_l9))
tr_9b <- lengths(st_intersects(colonias_cdmx,trole_l9b))

l_1 <- lengths(st_intersects(colonias_cdmx,l1))
l_1b <- lengths(st_intersects(colonias_cdmx,l1b))

l_12 <- lengths(st_intersects(colonias_cdmx,l12))
l_12b <- lengths(st_intersects(colonias_cdmx,l12b))
l12bfeb<- lengths(st_intersects(colonias_cdmx,l12bfeb))

# Homologamos a 0,1, para presencia o no del tratamiento ----

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1,cb_2,tr_9,tr_e,l_1,l_12,cb_1b,cb_2b,tr_9b,tr_eb,l_1b,l_12b) %>% mutate(cb_1 = ifelse(cb_1>0,1,0)) %>% 
  mutate(cb_2 = ifelse(cb_2>0,1,0)) %>% mutate(tr_e = ifelse(tr_e>0,1,0)) %>% 
  mutate(l_12 = ifelse(l_12>0,1,0)) %>% mutate(l_1 = ifelse(l_1>0,1,0))%>% mutate(tr_9 = ifelse(tr_9>0,1,0)) %>% 
  mutate(cb_1b = ifelse(cb_1b>0,1,0)) %>% mutate(cb_2b = ifelse(cb_2b>0,1,0)) %>% mutate(tr_9b = ifelse(tr_9b>0,1,0)) %>% 
  mutate(tr_eb = ifelse(tr_eb>0,1,0)) %>% mutate(l_1b = ifelse(l_1b>0,1,0)) %>% mutate(l_12b = ifelse(l_12b>0,1,0)) %>% 
  mutate(l12bfeb = ifelse(l12bfeb>0,1,0))

rm(l_1,l_12,cb_1,cb_2,tr_e,tr_9,l_1b,cb_1b,cb_2b,tr_9b,tr_eb,l_12b) # Eliminamos lo que no necesitamos

rm(cablebus_1, cablebus_1b, cablebus_2, cablebus_2b, el12b, l_alt, l1, l12, l12b, l1b, l12bfeb, # Eliminamos mapas
   trole_elevado, trole_elevadob, trole_l9, trole_l9b)

# Mapas que queremos Balancear ----------------------------------------------

# Línea cualquiera
l_alt <- read_sf("~/Desktop/Mapas/STC_shp/stc_l.shp") %>% filter(!LINEA %in% c("1", "12"))
l_alt <- st_transform(l_alt, crs = st_crs(colonias_cdmx))
l_alt %>% select(LINEA, geometry) %>% plot()

ggplot(data = l_alt) + 
  geom_sf(data = colonias_cdmx, aes(), linewidth = 0.1)+
  geom_sf(aes(color = LINEA)) + 
  theme_minimal()+
  labs(title = 'Lineas de Metro', 
       color = 'Linea:')  

# Lineas de Metrobus
l_metrobus <- read_sf("~/Desktop/Mapas/metrobus/lineas_metrobus.shp")
l_metrobus <- l_metrobus %>% st_transform(., crs = st_crs(colonias_cdmx))
l_metrobus <- st_transform(l_metrobus, crs = st_crs(colonias_cdmx))

ggplot(data = l_metrobus) + 
  geom_sf(data = colonias_cdmx, aes(), linewidth = 0.1)+
  geom_sf(aes(color = Name)) + 
  theme_minimal()+
  labs(title = 'Lineas de Metrobus', 
       color = 'Linea:')
  

# Vialidades primarias
vialidades_primarias <- read_sf("~/Desktop/Mapas/vialidades_primarias_cdmx/vialidades_primarias_cdmx.shp")
vialidades_primarias <- st_transform(vialidades_primarias, crs = st_crs(colonias_cdmx))

ggplot(data = vialidades_primarias) + 
  geom_sf(data = colonias_cdmx, aes(), linewidth = 0.1)+
  geom_sf(aes(color = tipo_vi)) + 
  theme_minimal()+
  labs(title = 'Vialidades Primarias en CDMX', 
       color = 'Tipo de Vialidad:')

# Concesionadas
concesionadas <- read_sf("~/Desktop/Mapas/concesionado_shp/CConcesionado_lineas.shp")
concesionadas <- st_transform(concesionadas, crs = st_crs(colonias_cdmx))

ggplot(data = concesionadas) +
  geom_sf(data = colonias_cdmx, aes(), linewidth = 0.1)+
  geom_sf(aes(color = CORREDOR)) +  
  theme_minimal() +
  labs(title = "Corredores concesionados en CDMX",
       color = "Corredor Concesionario:") + 
  guides(color = guide_legend(ncol = 2))  # Set legend to two columns

# Todo
ggplot(data = concesionadas) +
  geom_sf(data = colonias_cdmx, aes(), linewidth = 0.1)+
  geom_sf(aes(color = 'Rutas Concesionadas')) +  
  geom_sf(data = vialidades_primarias, aes(color = 'Vialidades Primarias'))+
  geom_sf(data = l_metrobus, aes(color = 'Líneas de Metrobus'))+
  geom_sf(data = l_alt, aes(color = 'Líneas de Metro'))+
  theme_minimal() +
  labs(title = "All Overlaps",
       color = "Overlaps:")

# New Intersections -------------------------------------------------------

i_concesionados <- lengths(st_intersects(colonias_cdmx,concesionadas))
i_metrobus <- lengths(st_intersects(colonias_cdmx,l_metrobus))
i_metro_otras <- lengths(st_intersects(colonias_cdmx, l_alt))
i_vialidades <- lengths(st_intersects(colonias_cdmx, vialidades_primarias))

# Paste 
colonias_cdmx <- colonias_cdmx %>% cbind(i_concesionados, i_metrobus, i_metro_otras, i_vialidades)

# Testing
# ggplot(data = colonias_cdmx) + 
#   geom_sf(aes(fill = factor(i_concesionados))) +
#   geom_sf(aes(fill = factor(i_metrobus)))+
#   geom_sf(aes(fill = factor(i_metro_otras)))+
#   geom_sf(aes(fill = factor(i_vialidades)))

# Balance -----------------------------------------------------------------

colonias_cdmx %>% glimpse()

colonias_cdmx <- colonias_cdmx %>% mutate(starts = ifelse(cb_1b + cb_2b + tr_9b + tr_eb>0,1,0), 
                         stops = ifelse(l_1b + l_12b>0,1,0), 
                         treated = ifelse(cb_1b + cb_2b + tr_9b + tr_eb + l_1b + l_12b>0, 1, 0), 
                         each_thing = case_when(cb_1b + cb_2b + tr_9b + tr_eb>0 ~ 'Starts', 
                                                l_1b + l_12b>0 ~'Stops', 
                                                T ~ 'Control'), 
                         treatments = case_when(cb_1b + cb_2b + tr_9b + tr_eb>0 ~ 1, 
                                                l_1b + l_12b>0 ~2, 
                                                T ~ 0)
                         )


pacman::p_load(RCT)

RCT::balance_table(data = colonias_cdmx %>% as.data.frame() %>% select(treatments, IM_2020, POBTOT, i_concesionados, i_metrobus, i_metro_otras, i_vialidades), 
                   treatment = 'treatments') %>% 
  rename(covariate = variables1, 
         control_mean = Media_control1, 
         starts_mean = Media_trat1, 
         stops_mean = Media_trat2, 
         p_value_control_vs_starts = p_value1, 
         p_value_control_vs_stops = p_value2)

colonias_cdmx %>% group_by(each_thing) %>% 
  summarise(mean(IM_2020), 
            mean(POBTOT), 
            mean(i_concesionados), 
            mean(i_metrobus), 
            mean(i_metro_otras), 
            mean(i_vialidades), 
            n()
            ) %>% rename(group = each_thing)%>% kableExtra::kable(format = 'pipe')

colonias_cdmx %>% as.data.frame()

# LM Incidente - Covariates for matching ----------------------------------

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

# Punto geográfico
inviales_18_22 <- inviales_18_22 %>% na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

# Revisamos cuales son 
inviales_18_22 <- st_transform(inviales_18_22,st_crs(colonias_cdmx)) # Cambiamos mapa a mismo CRS de otros, ahora todos tienen el mismo

total_inviales <- lengths(st_intersects(colonias_cdmx,inviales_18_22))

colonias_cdmx$inviales <- total_inviales 

dummy_lm <- lm(inviales ~ IM_2020 + POBTOT + i_concesionados + i_metrobus + i_metro_otras + i_vialidades, 
   data = colonias_cdmx)

summary_dummy <- dummy_lm %>% summary() 

summary_dummy$coefficients %>% clipr::write_clip()

# Matching using matchit & Mahalanobis ------------------------------------

# Load Matching
pacman::p_load(MatchIt)

# Make Polygon into df
colonias_df <- colonias_cdmx %>% as.data.frame()

# Complete Match
match_out <- MatchIt::matchit(data = colonias_df, 
                 formula = treated ~ IM_2020 + POBTOT + i_concesionados + i_metrobus + i_metro_otras + i_vialidades, 
                 distance = 'mahalanobis', # One of many
                 method = 'nearest',
                 replace = T)

match_out %>% summary()

# Match for STOPS

match_stops <- MatchIt::matchit(data = colonias_df %>% filter(starts==0), 
                              formula = treated ~ IM_2020 + POBTOT + i_concesionados + i_metrobus + i_metro_otras + i_vialidades, 
                              distance = 'mahalanobis', # One of many
                              method = 'nearest',
                              replace = T)

match_stops %>% summary()

  # Match for STARTS

match_starts <- MatchIt::matchit(data = colonias_df %>% filter(stops==0), 
                              formula = treated ~ IM_2020 + POBTOT + i_concesionados + i_metrobus + i_metro_otras + i_vialidades, 
                              distance = 'mahalanobis', # One of many
                              method = 'nearest',
                              replace = T)

# Fractional matching happens b.c. some are matched more than once, in this case 20 colonias are matched more than once, 3 are matched 3 times
match_starts %>% summary()

match_out$match.matrix %>% as.data.frame() %>% count(V1) %>% filter(n>1) %>% arrange(n) %>% count(n)



# New DB for DiD ----------------------------------------------------------

# Make DF
colonias_df <- colonias_df %>% mutate(id = row_number()) 

# Convert the matrix to a data frame to left join them
matches_start <- as.data.frame(match_starts$match.matrix) %>%  rownames_to_column(var = "row_number") %>% rename(id = V1) %>% 
  count(id) %>% arrange(n) %>% mutate(id = as.numeric(id))
matches_stop <- as.data.frame(match_stops$match.matrix) %>%  rownames_to_column(var = "row_number")%>% rename(id = V1) %>% 
  count(id) %>% arrange(n)%>% mutate(id = as.numeric(id))


# Starts df
match_starts %>% summary()

# 165 obs so good
control_starts <- colonias_cdmx %>% mutate(id = row_number())  %>% inner_join(matches_start) %>% 
  uncount(n) # duplicates rows according to a column, in this case n 

colonias_starts <- colonias_cdmx %>% filter(starts == 1) %>% rbind(control_starts %>% select(-id)) %>% mutate(match = 'start')

# Stops df 
match_stops %>% summary()

# 190 obs
control_stops <- colonias_cdmx %>% mutate(id = row_number())  %>% inner_join(matches_stop) %>% 
  uncount(n) 

colonias_stops <- colonias_cdmx %>% filter(stops == 1) %>% rbind(control_stops %>% select(-id)) %>% mutate(match = 'stops')


rm(list = setdiff(ls(), c('colonias_starts', 'colonias_stops')))

# Single DF to be intersected

matched_colonias <- rbind(colonias_starts, colonias_stops)
# Done
matched_colonias <- matched_colonias %>% mutate(ID = 1:nrow(matched_colonias))

rm(list = setdiff(ls(), c('matched_colonias')))
beepr::beep()
