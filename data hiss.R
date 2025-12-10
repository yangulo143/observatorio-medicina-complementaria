if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rio, flextable, officer,  scales, ggExtra, writexl, janitor, corrr, ggpubr, ggExtra, gtsummary, tableone, broom, epiDisplay, car )

data_hiss<- import("TRAMA_MAC_2019-2023.csv")
tabyl(data_hiss$ano)

sapply(data_hiss, class)
colnames(data_hiss)

# Vectores de códigos de interés
codigos_interes <- c("97810", "97811", "97813", "97814", "U0903", "90861", "90880", "90849", "90857",
                     "U0900", "U0902", "U0904", "U0907", "U0908", "U0901", "U0905", "U0906",
                     "97022", "98925", "98926", "98927", "98928", "98929", "98940", "98941",
                     "98942", "98943", "97124", "Z5102", "U0080", "U900", "U902", "U904", "U907",
                     "U908", "U901", "U905", "U906", "U903", "U080")

cod_servsa_interes <- c(301902, 301901, 301903, 301904, 301905, 301906)

# Filtrado de la base
data_hiss_mec <- data_hiss %>%
  filter(codigo %in% codigos_interes | cod_servsa %in% cod_servsa_interes)

tabyl(data_hiss_mec$ano)

atendidos_2019 <- data_hiss_mec %>%
  filter(!is.na(ID_paciente), 
         ano==2019) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

atendidos_2020 <- data_hiss_mec %>%
  filter(!is.na(ID_paciente), 
         ano==2020) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))


atendidos_2021 <- data_hiss_mec %>%
  filter(!is.na(ID_paciente), 
         ano==2021) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

atendidos_2022 <- data_hiss_mec %>%
  filter(!is.na(ID_paciente), 
         ano==2022) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

atendidos_2023 <- data_hiss_mec %>%
  filter(!is.na(ID_paciente), 
         ano==2023) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))


# Filtramos NA y agrupamos por año
#atendidos_por_ano <- data_hiss_mec %>%
  #filter(!is.na(ID_paciente), !is.na(ano)) %>%
  #group_by(ano) %>%
  #group_split()

# Si quieres nombrarlas por año:
#names(atendidos_por_ano) <- data_hiss_mec %>%
  #filter(!is.na(ID_paciente), !is.na(ano)) %>%
  #distinct(ano) %>%
  #arrange(ano) %>%
  #pull(ano)


#atendidos<- tabyl(data_hiss$id_cita) %>%
  #mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  #arrange(desc(n))

atenciones<- data_hiss_mec %>%
  dplyr::select(id_cita, ano, renaes, Ubigeo_res:tip_edad, codigo, ID_paciente, Profesion)

renaes<-import("USLRC20250627020203_xp.xls")
colnames(renaes)

renaes<-renaes%>%
  rename(renaes=`Código Único`, 
         Nombre=`Nombre del establecimiento`) %>%
  dplyr::select(Institución, renaes, Nombre, Tipo:UBIGEO)
renaes_modificada <- renaes %>%
  mutate(renaes = as.integer(renaes))


codigos<- tabyl(atenciones_completa$codigo) %>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

export(codigos, "codigos.xlsx")

tabyl(data_hiss$cod_servsa)

tabyl(atenciones_completa$Departamento)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

######2024########

atenciones_2024<- import("TRAMA_MAC_2024_2025.csv")
atenciones_2024 <- atenciones_2024 %>%
  filter(codigo %in% codigos_interes | cod_servsa %in% cod_servsa_interes)

tabyl(atenciones_2024$ano)

atenciones_2024<- atenciones_2024 %>%
  dplyr::select(id_cita, ano, renaes, Ubigeo_res:tip_edad, codigo, ID_paciente, Profesion)


atendidos_2024 <- atenciones_2024 %>%
  filter(!is.na(ID_paciente), 
         ano==2024) %>%
  tabyl(ID_paciente) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

atenciones_2024_completa <- atenciones_2024 %>%
  left_join(renaes_modificada, by = "renaes")%>%
  filter(ano==2024)

tabyl(atenciones_2024_completa$Departamento)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

NA_ubigeo<- atenciones_2024_completa %>%
  filter(is.na(Departamento)) %>%
  tabyl(renaes)



base_atenciones_completas <- bind_rows(atenciones_completa, atenciones_2024_completa)

tabyl(base_atenciones_completas$Departamento)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

codigos2<- tabyl(base_atenciones_completas$codigo) %>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

export(codigos2, "codigos2.xlsx")

codigos_prof<- base_atenciones_completas%>%
  filter(!is.na(codigo), !is.na(Profesion)) %>%
  tabyl(codigo, Profesion)

export(codigos_prof, "codigos_prof.xlsx")  

tabyl(base_atenciones_completas$Profesion)


################ Mejorando proceso de selección###########

# Filtrar los id_cita que tengan al menos un código de medicina complementaria


codigos_interes <- c("97810", "97811", "97813", "97814", "U0903", "90861", "90880", "90849", "90857",
                     "U0900", "U0902", "U0904", "U0907", "U0908", "U0901", "U0905", "U0906", "U0907",
                     "97022", "98925", "98926", "98927", "98928", "98929", "98940", "98941",
                     "98942", "98943", "97124", "Z5102", "U0080", "U900", "U902", "U904", "U907",
                     "U908", "U901", "U905", "U906", "U903", "U080")

id_filtrados <- data_hiss %>%
  filter(codigo %in% codigos_interes) %>%
  distinct(id_cita)

# Conservar todas las filas con esos id_cita
data_filtrada <- data_hiss %>%
  filter(id_cita %in% id_filtrados$id_cita)

library(dplyr)

frecuencia_citas <- data_filtrada %>%
  count(id_cita, name = "frecuencia") %>%
  arrange(desc(frecuencia))

data_hiss_mec <- data_hiss %>%
  filter(codigo %in% codigos_interes)

frecuencia_citas2 <- data_hiss_mec %>%
  count(id_cita, name = "frecuencia") %>%
  arrange(desc(frecuencia))

tabyl(data_hiss_mec$codigo)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))



data_hiss_mec_unica <- data_hiss_mec %>%
  distinct(id_cita, codigo, .keep_all = TRUE)

tabyl(data_hiss_mec_unica$codigo)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

frecuencia_citas2 <- data_hiss_mec_unica %>%
  count(id_cita, name = "frecuencia") %>%
  arrange(desc(frecuencia))

data_hiss_mec_unica<- data_hiss_mec_unica %>%
  mutate(terapia=case_when(
    codigo %in% c("97810", "97811") ~ "Acupuntura",
    codigo %in% c("97813", "97814") ~ "Electroacupuntura",
    codigo %in% c("U903") ~ "Terapia Mente-cuerpo (en general)",
    codigo %in% c("90861") ~ "Terapia de relajación",
    codigo %in% c("90880") ~ "Sesión de hipnoterapia",
    codigo %in% c("90849") ~ "Sesión de psicoterapia de grupo que incluye múltiples grupos familiares",
    codigo %in% c("90857") ~ "Psicoterapia interactiva de grupo",
    codigo %in% c("U900") ~ "Terapia Neural",
    codigo %in% c("U902") ~ "Reflexologia",
    codigo %in% c("U904") ~ "Homeopatia",
    codigo %in% c("U907") ~ "Terapia Floral",
    codigo %in% c("U908") ~ "Bioenergética",
    codigo %in% c("U906") ~ "Trofoterapia",
    codigo %in% c("U905") ~ "Fitoterapia",
    codigo %in% c("97022") ~ "Hidroterapia",
    codigo %in% c("98925", "98926", "98927", "98928", "98929") ~ "Osteopatía",
    codigo %in% c("98940", "98941", "98942", "98943") ~ "Quiropraxia",
    codigo %in% c("97124") ~ "Masoterapia",
    codigo %in% c("Z5102", "U901") ~ "Laserterapia",
    codigo %in% c("U080") ~ "Consejeria en Medicina Complementaria y Alternativa",
  ))

tabyl(data_hiss_mec_unica$terapia)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

tabyl(data_hiss_mec_unica$cod_servsa)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

ubigeos<- import("ubgeos.xlsx")
sapply(ubigeos, class)

data_hiss_mec_unica<- data_hiss_mec_unica%>%
  mutate(ubigeo = Ubigeo_res)

ubigeos<- ubigeos%>%
  mutate(ubigeo = as.integer(ubigeo))


atenciones_completa <- data_hiss_mec_unica %>%
  left_join(ubigeos, by = "ubigeo")

atenciones_completa <- atenciones_completa %>%
  left_join(renaes_modificada, by = "renaes")


colnames(atenciones_completa)

tabyl(atenciones_completa$tip_edad)


##### evaluar registros NA

library(naniar)
gg_miss_var(atenciones_completa)


NA_ubigeo<- atenciones_completa %>%
  filter(is.na(Departamento)) %>%
  tabyl(renaes)

###Armar variables para el dashboard



###### busqueda maria auxiliadora

atenciones_ma <- atenciones_completa %>%
  filter(renaes==5987)

tabyl (atenciones_ma$ano)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

tabyl (atenciones_ma$codigo)%>%
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))

##### Preprocesamiento de códigos

library(tidyverse)

data_hiss2<- import("TRAMA_MAC_2024_2025.csv")

base_total <- bind_rows(data_hiss, data_hiss2)

# Diccionario de corrección de códigos
correcciones <- c("U0900" = "U900",
                  "U0901" = "U901",
                  "U0902" = "U902",
                  "U0903" = "U903",
                  "U0904" = "U904",
                  "U0905" = "U905",
                  "U0906" = "U906",
                  "U0907" = "U907",
                  "U0908" = "U908", 
                  "U0080" = "U080")
# Aplicar las correcciones

data_hiss_c <- base_total %>%
  mutate(codigo = ifelse(codigo %in% names(correcciones),
                         correcciones[codigo],
                         codigo))

# 1. Generar la tabla de frecuencias ordenada
resumen_codigos <- data_hiss_c %>%
  group_by(codigo) %>%
  summarise(
    frecuencia = n(),  # Conteo absoluto
    # Opcional: ver si el código se usa más como definitivo, presuntivo, etc.
    tipo_mas_comun = names(which.max(table(diagnost))) 
  ) %>%
  mutate(
    porcentaje = (frecuencia / sum(frecuencia)) * 100,
    acumulado = cumsum(porcentaje)
  ) %>%
  arrange(desc(frecuencia)) # Ordenar de mayor a menor

export(resumen_codigos, "resumen_codigos.xlsx")
# 2. Imprimir cuántos códigos únicos existen en total
cat("Total de códigos únicos encontrados:", nrow(resumen_codigos), "\n")

# 3. Ver los 20 códigos más frecuentes
head(resumen_codigos, 20)

# Opcional: Exportar a CSV para revisar manualmente si no tienes el diccionario a la mano
# write.csv(resumen_codigos, "listado_codigos_frecuencia.csv", row.names = FALSE)



