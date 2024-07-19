## Matching DiD
# DAOA 
# 13/06/2024

# Preparar Espacio --------------------------------------------------------

dev.off()
rm(list = ls())
pacman::p_load(tidyverse, scales, did, beepr)

# Cargar Bases ------------------------------------------------------------

load(file = './output/baes_generadas/matches/atropellado_lesionado.Rdata')
load(file = './output/baes_generadas/matches/choque_con_lesionados.Rdata')
load(file = './output/baes_generadas/matches/choque_sin_lesionados.Rdata')
load(file = './output/baes_generadas/matches/accidentes_moto.Rdata')
load(file = './output/baes_generadas/matches/suma_incidentes_viales.Rdata')
load(file = './output/baes_generadas/matches/complete_incidentes.Rdata')

# DiD ---------------------------------------------------------------------

# Numeric
# Needed for the package
complete_total$dates_complete %>% min()
complete_total$dates_complete %>% max()

# Remove 2013
dummy_dates <- data.frame(dates_complete = seq.Date(as.Date("2014-01-01"),as.Date("2023-08-01"), 
                                                    by = '1 month'),date_numeric = 1:length(unique(complete_total$dates_complete)))


# Setting Parameters and splitting DB  -------------------------------------------------

# Starts:
# CB L1: 91
# CB L2: 92
# TR L9: 85
# TR L10: 106

# Stops: 
# L1: 103
# L12: 89
# L12 Reopening of 1st strech: 109
# Reopening of 2nd Strech: 116 (Last obs for now)


# Complete DiD base -------------------------------------------------------

# Todos los incidentes

base_did_inviales <- complete_total %>% 
  left_join(dummy_dates) %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, total_inviales = incidencia, start, stop, match) 

# Quitamos observación de Diciembre 2013
base_did_inviales <- base_did_inviales %>% filter(!is.na(date_numeric))

# atropellados

atropellados_did <- complete_atropellado_lesionado %>% 
  left_join(dummy_dates) %>%
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, atropellados = incidencia) 

# choque con lesionados

choquecl_did <- complete_choque_cl_accidente %>% 
  left_join(dummy_dates) %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, choque_cl = incidencia) 

# chocque sin lesionados

choquesl_did <- complete_choque_sl_accidente %>% 
  left_join(dummy_dates) %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, choque_sl = incidencia) 

# accidentes moto

accidentes_moto_did <- complete_moto_accidente %>% 
  left_join(dummy_dates) %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, accidente_moto = incidencia) 

# Total

total_accidentes_viales <- complete_total %>% 
  left_join(dummy_dates) %>% 
  mutate(start =  cb_1b * 91 + cb_2b * 92 + tr_eb * 106 + tr_9 * 85) %>% 
  mutate(start = ifelse(start == 198, 92,start)) %>% 
  mutate(stop = l_12b * 89 + l_1b * 103) %>% 
  select(ID, date_numeric, total_incidentes = incidencia) 

# pegamos otros choques a la base completa

base_did_inviales <- base_did_inviales %>% 
  left_join(atropellados_did, by = c('ID', 'date_numeric')) %>% 
  left_join(choquecl_did, by = c('ID', 'date_numeric')) %>% 
  left_join(choquesl_did, by = c('ID', 'date_numeric')) %>% 
  left_join(accidentes_moto_did, by = c('ID', 'date_numeric')) %>%   # late registration
  left_join(total_accidentes_viales, by = c('ID', 'date_numeric'))

# Tests -------------------------------------------------------------------

# testing aggregation

base_did_inviales %>% filter(start>0) %>% count(ID) %>% count(n)
base_did_inviales %>% filter(match=='start') %>% count(ID) %>% count(n)

base_did_inviales %>% filter(stop>0) %>% count(ID) %>% count(n)
base_did_inviales %>% filter(match=='stops') %>% count(ID) %>% count(n)

# Separate ----------------------------------------------------------------

# Filter Start

did_inviales_start <- base_did_inviales %>% filter(match == 'start')

# Filter Stop

did_inviales_stop <- base_did_inviales %>% filter(match == 'stops', 
                                                  date_numeric <= 108 # Enero 2023
                                                  ) 

# Save --------------------------------------------------------------------

# save(did_inviales_start, file = './output/baes_generadas/matches/did_db_start.Rdata')
# save(did_inviales_stop, file = './output/baes_generadas/matches/did_db_stop.Rdata')

# Load --------------------------------------------------------------------
rm(list = ls())
load(file = './output/baes_generadas/matches/did_db_start.Rdata')
load(file = './output/baes_generadas/matches/did_db_stop.Rdata')

# DiD Start -----------------------------------------------------------------

# Complete
did_total_start <- att_gt(
  yname = "total_incidentes",
  tname = "date_numeric",
  idname = "ID",
  gname = "start",
  data = did_inviales_start
)

# Choque con Lesionados
did_choquecl_start <- att_gt(
  yname = "choque_cl",
  tname = "date_numeric",
  idname = "ID",
  gname = "start",
  data = did_inviales_start
)

# Choque sin Lesionados

did_choquesl_start <- att_gt(
  yname = "choque_sl",
  tname = "date_numeric",
  idname = "ID",
  gname = "start",
  data = did_inviales_start
)

#Atropellados
did_atropellados_start <- att_gt(
  yname = "atropellados",
  tname = "date_numeric",
  idname = "ID",
  gname = "start",
  data = did_inviales_start
)

# Moto
# Stops being NA in T = 37

did_accidente_moto_start <- att_gt(
  yname = "accidente_moto",
  tname = "date_numeric",
  idname = "ID",
  gname = "start",
  data = did_inviales_start %>% filter(date_numeric >= 37)
)


# Aggregate Effects -------------------------------------------------------


# Simple ATT --------------------------------------------------------------

# Simple: ATT
# Creates a weighted average of all group-time average treatment effects with weights proportional to group size
set.seed(99)
agg_start_complete <- aggte(did_total_start, type = 'simple', na.rm = T)
agg_start_choquecl <- aggte(did_choquecl_start, type = 'simple', na.rm = T)
agg_start_choquesl <- aggte(did_choquesl_start, type = 'simple', na.rm = T)
agg_start_atropellados <- aggte(did_atropellados_start, type = 'simple', na.rm = T)
agg_start_moto <- aggte(did_accidente_moto_start, type = 'simple', na.rm = T)


# ATT starts

agg_start_complete
agg_start_choquecl
agg_start_choquesl
agg_start_atropellados
agg_start_moto


# Dynamic ATT(t) - Event Study ---------------------------------------------------

# Dynamic, ATT(t): best agregation
# computes average effects across different lengths of exposure to the treatment and is similar to an "event study"

# Complete
agg_start_complete <- aggte(did_total_start, type = 'dynamic', na.rm = T)
summary(agg_start_complete)
ggdid(agg_start_complete, title = "Dynamic Effect of the start group over All Incidents", xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_complete.png",
#        plot = ggdid(agg_start_complete, title = "Dynamic Effect of the start group over All Incidents"), width = 14,height = 6)

# Choques CL
agg_start_choquecl <- aggte(did_choquecl_start, type = 'dynamic', na.rm = T)
summary(agg_start_choquecl)
ggdid(agg_start_choquecl, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_choque_cl.png",
#        plot = ggdid(agg_start_choquecl, title = "Dynamic Effect of the start group over Choques CL"), width = 14,height = 6)

# Choques SL
agg_start_choquesl <- aggte(did_choquesl_start, type = 'dynamic', na.rm = T)
summary(agg_start_choquesl)
ggdid(agg_start_choquesl, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_choque_sl.png",
#        plot = ggdid(agg_start_choquecl, title = "Dynamic Effect of the start group over Choques SL"), width = 14,height = 6)

# Atropellados
agg_start_atropellados <- aggte(did_atropellados_start, type = 'dynamic', na.rm = T)
summary(agg_start_atropellados)
ggdid(agg_start_atropellados, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_atropellados.png",
#        plot = ggdid(agg_start_atropellados, title = "Dynamic Effect of the start group over Atropellados"), width = 14,height = 6)


# Moto
agg_start_moto <- aggte(did_accidente_moto_start, type = 'dynamic', na.rm = T)
summary(agg_start_moto)
ggdid(agg_start_moto, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_moto.png",
#        plot = ggdid(agg_start_moto, title = "Dynamic Effect of the start group over Moto"), width = 14,height = 6)


# Group Effect - ATT(g) ---------------------------------------------------

# Group Effect (ATT(g))
# (this is the default option and computes average treatment effects across 
# different groups; here the overall effect averages the effect across different group

# Complete
agg_start_complete <- aggte(did_total_start, type = 'group', na.rm = T)
summary(agg_start_complete)
ggdid(agg_start_complete)

# ggsave(filename = "output/did_plots/inviales/group_complete.png",
#        plot = ggdid(agg_start_complete, title = "Group Effect over All Incidents"), width = 4,height = 4)

# Choques con Lesionado
agg_start_choquecl <- aggte(did_choquecl_start, type = 'group', na.rm = T)
summary(agg_start_choquecl)
ggdid(agg_start_choquecl)

# ggsave(filename = "output/did_plots/inviales/group_choques_cl.png",
#        plot = ggdid(agg_start_choquecl, title = "Group Effect over Choques CL"), width = 4,height = 4)

# Choques sin Lesionados
agg_start_choquesl <- aggte(did_choquesl_start, type = 'group', na.rm = T)
summary(agg_start_choquesl)
ggdid(agg_start_choquesl)

# ggsave(filename = "output/did_plots/inviales/group_choques_sl.png",
#        plot = ggdid(agg_start_choquesl, title = "Group Effect over Choques SL"), width = 4,height = 4)

# Atropellados
agg_start_atropellados <- aggte(did_atropellados_start, type = 'group', na.rm = T)
summary(agg_start_atropellados)
ggdid(agg_start_atropellados)

# ggsave(filename = "output/did_plots/inviales/group_atropellados.png",
#        plot = ggdid(agg_start_atropellados, title = "Group Effect over Atropellados"), width = 4,height = 4)

# Accidentes en Moto
agg_start_moto <- aggte(did_accidente_moto_start, type = 'group', na.rm = T)
summary(agg_start_moto)
ggdid(agg_start_moto)

# ggsave(filename = "output/did_plots/inviales/group_moto.png",
#        plot = ggdid(agg_start_moto, title = "Group Effect over Atropellados"), width = 4,height = 4)

# Calendar (Cumulative Effects) -------------------------------------------

# Calendar Effect
# this computes average treatment effects across different time periods; 
# here the overall effect averages the effect across each time period

# Complete
agg_start_complete <- aggte(did_total_start, type = 'calendar', na.rm = T)
summary(agg_start_complete)
ggdid(agg_start_complete)

# Choques CL
agg_start_choquecl <- aggte(did_choquecl_start, type = 'calendar', na.rm = T)
summary(agg_start_choquecl)
ggdid(agg_start_choquecl)

# Choques SL
agg_start_choquesl <- aggte(did_choquesl_start, type = 'calendar', na.rm = T)
summary(agg_start_choquesl)
ggdid(agg_start_choquesl)

# Atropellados
agg_start_atropellados <- aggte(did_atropellados_start, type = 'calendar', na.rm = T)
summary(agg_start_atropellados)
ggdid(agg_start_atropellados)

# Moto
agg_start_moto <- aggte(did_accidente_moto_start, type = 'calendar', na.rm = T)
summary(agg_start_moto)
ggdid(agg_start_moto)

# DiD STOP -----------------------------------------------------------------

# Complete
did_total_stop <- att_gt(
  yname = "total_incidentes",
  tname = "date_numeric",
  idname = "ID",
  gname = "stop",
  data = did_inviales_stop
)

# Choque con Lesionados
did_choquecl_stop <- att_gt(
  yname = "choque_cl",
  tname = "date_numeric",
  idname = "ID",
  gname = "stop",
  data = did_inviales_stop
)

# Choque sin Lesionados

did_choquesl_stop <- att_gt(
  yname = "choque_sl",
  tname = "date_numeric",
  idname = "ID",
  gname = "stop",
  data = did_inviales_stop
)

#Atropellados
did_atropellados_stop <- att_gt(
  yname = "atropellados",
  tname = "date_numeric",
  idname = "ID",
  gname = "stop",
  data = did_inviales_stop
)

# Moto
# Stops being NA in T = 37

did_accidente_moto_stop <- att_gt(
  yname = "accidente_moto",
  tname = "date_numeric",
  idname = "ID",
  gname = "stop",
  data = did_inviales_stop %>% filter(date_numeric >= 37)
)


# Aggregate Effects -------------------------------------------------------


# Simple ATT --------------------------------------------------------------

# Simple: ATT
# Creates a weighted average of all group-time average treatment effects with weights proportional to group size
set.seed(99)
agg_stop_complete <- aggte(did_total_stop, type = 'simple', na.rm = T)
agg_stop_choquecl <- aggte(did_choquecl_stop, type = 'simple', na.rm = T)
agg_stop_choquesl <- aggte(did_choquesl_stop, type = 'simple', na.rm = T)
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'simple', na.rm = T)
agg_stop_moto <- aggte(did_accidente_moto_stop, type = 'simple', na.rm = T)


# ATT stops

agg_stop_complete
agg_stop_choquecl
agg_stop_choquesl
agg_stop_atropellados
agg_stop_moto


# Dynamic ATT(t) - Event Study ---------------------------------------------------

# Dynamic, ATT(t): best agregation
# computes average effects across different lengths of exposure to the treatment and is similar to an "event study"

# Complete
agg_stop_complete <- aggte(did_total_stop, type = 'dynamic', na.rm = T)
summary(agg_stop_complete)
ggdid(agg_stop_complete, title = "Dynamic Effect of the stop group over All Incidents", xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_complete.png",
#        plot = ggdid(agg_stop_complete, title = "Dynamic Effect of the stop group over All Incidents"), width = 14,height = 6)

# Choques CL
agg_stop_choquecl <- aggte(did_choquecl_stop, type = 'dynamic', na.rm = T)
summary(agg_stop_choquecl)
ggdid(agg_stop_choquecl, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_choque_cl.png",
#        plot = ggdid(agg_stop_choquecl, title = "Dynamic Effect of the stop group over Choques CL"), width = 14,height = 6)

# Choques SL
agg_stop_choquesl <- aggte(did_choquesl_stop, type = 'dynamic', na.rm = T)
summary(agg_stop_choquesl)
ggdid(agg_stop_choquesl, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_choque_sl.png",
#        plot = ggdid(agg_stop_choquecl, title = "Dynamic Effect of the stop group over Choques SL"), width = 14,height = 6)

# Atropellados
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'dynamic', na.rm = T)
summary(agg_stop_atropellados)
ggdid(agg_stop_atropellados, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_atropellados.png",
#        plot = ggdid(agg_stop_atropellados, title = "Dynamic Effect of the stop group over Atropellados"), width = 14,height = 6)


# Moto
agg_stop_moto <- aggte(did_accidente_moto_stop, type = 'dynamic', na.rm = T)
summary(agg_stop_moto)
ggdid(agg_stop_moto, xgap = 5)

# ggsave(filename = "output/did_plots/inviales/dynamic_moto.png",
#        plot = ggdid(agg_stop_moto, title = "Dynamic Effect of the stop group over Moto"), width = 14,height = 6)


# Group Effect - ATT(g) ---------------------------------------------------

# Group Effect (ATT(g))
# (this is the default option and computes average treatment effects across 
# different groups; here the overall effect averages the effect across different group

# Complete
agg_stop_complete <- aggte(did_total_stop, type = 'group', na.rm = T)
summary(agg_stop_complete)
ggdid(agg_stop_complete)

# ggsave(filename = "output/did_plots/inviales/group_complete.png",
#        plot = ggdid(agg_stop_complete, title = "Group Effect over All Incidents"), width = 4,height = 4)

# Choques con Lesionado
agg_stop_choquecl <- aggte(did_choquecl_stop, type = 'group', na.rm = T)
summary(agg_stop_choquecl)
ggdid(agg_stop_choquecl)

# ggsave(filename = "output/did_plots/inviales/group_choques_cl.png",
#        plot = ggdid(agg_stop_choquecl, title = "Group Effect over Choques CL"), width = 4,height = 4)

# Choques sin Lesionados
agg_stop_choquesl <- aggte(did_choquesl_stop, type = 'group', na.rm = T)
summary(agg_stop_choquesl)
ggdid(agg_stop_choquesl)

# ggsave(filename = "output/did_plots/inviales/group_choques_sl.png",
#        plot = ggdid(agg_stop_choquesl, title = "Group Effect over Choques SL"), width = 4,height = 4)

# Atropellados
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'group', na.rm = T)
summary(agg_stop_atropellados)
ggdid(agg_stop_atropellados)

# ggsave(filename = "output/did_plots/inviales/group_atropellados.png",
#        plot = ggdid(agg_stop_atropellados, title = "Group Effect over Atropellados"), width = 4,height = 4)

# Accidentes en Moto
agg_stop_moto <- aggte(did_accidente_moto_stop, type = 'group', na.rm = T)
summary(agg_stop_moto)
ggdid(agg_stop_moto)

# ggsave(filename = "output/did_plots/inviales/group_moto.png",
#        plot = ggdid(agg_stop_moto, title = "Group Effect over Atropellados"), width = 4,height = 4)

# Calendar (Cumulative Effects) -------------------------------------------

# Calendar Effect
# this computes average treatment effects across different time periods; 
# here the overall effect averages the effect across each time period

# Complete
agg_stop_complete <- aggte(did_total_stop, type = 'calendar', na.rm = T)
summary(agg_stop_complete)
ggdid(agg_stop_complete)

# Choques CL
agg_stop_choquecl <- aggte(did_choquecl_stop, type = 'calendar', na.rm = T)
summary(agg_stop_choquecl)
ggdid(agg_stop_choquecl)

# Choques SL
agg_stop_choquesl <- aggte(did_choquesl_stop, type = 'calendar', na.rm = T)
summary(agg_stop_choquesl)
ggdid(agg_stop_choquesl)

# Atropellados
agg_stop_atropellados <- aggte(did_atropellados_stop, type = 'calendar', na.rm = T)
summary(agg_stop_atropellados)
ggdid(agg_stop_atropellados)

# Moto
agg_stop_moto <- aggte(did_accidente_moto_stop, type = 'calendar', na.rm = T)
summary(agg_stop_moto)
ggdid(agg_stop_moto)





