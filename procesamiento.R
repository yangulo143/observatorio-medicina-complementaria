library(tidyverse)
library(lubridate)
library(rio) 
# Para importar datos

# 1. Cargar Datos (Simulación de carga)
# df <- import("ruta/a/tu/base_hiss.csv")

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rio, flextable, officer,  scales, ggExtra, writexl, janitor, corrr, ggpubr, ggExtra, gtsummary, tableone, broom, epiDisplay, car )

#data_hiss<- import("TRAMA_MAC_2019-2023.csv")


# 2. Definir Códigos de Medicina Complementaria
# IMPORTANTE: Debes tener el listado de códigos que el MINSA considera "Medicina Complementaria"
# Ejemplo hipotético:
codigos_med_comp_indiv <- c("99901", "99902") # Reemplazar con códigos reales CPT/CIE
codigos_med_comp_grup  <- c("C0001", "C0002") # Talleres, sesiones educativas

# 3. Limpieza y Transformación
data_hiss_c<-data_hiss_c%>%
  filter(!(ano == 2025 & mes == 6))

df_clean <- data_hiss_c %>%
  mutate(
    # Conversión de fecha
    fecha = make_date(ano, mes, dia),
    
    # Estandarización de Edad a Años
    edad_anios = case_when(
      tip_edad == "A" ~ as.numeric(edad),
      tip_edad == "M" ~ as.numeric(edad)/12,
      tip_edad == "D" ~ as.numeric(edad)/365,
      TRUE ~ NA_real_
    ),
    
    # Grupos Etarios (Ciclos de vida MINSA)
    ciclo_vida = case_when(
      edad_anios < 5 ~ "Niño",
      edad_anios >= 5 & edad_anios < 12 ~ "Niño",
      edad_anios >= 12 & edad_anios < 18 ~ "Adolescente",
      edad_anios >= 18 & edad_anios < 60 ~ "Adulto",
      edad_anios >= 60 ~ "Adulto Mayor"
    ),
    
  )

# 1. Creamos una columna temporal 'es_grupal' en la data original

library(tidyverse)

# 1. Definir la lógica de "vacío" de forma robusta
# Usamos trimws() para quitar espacios y comparamos con "" O revisamos si es NA
df_clean <- df_clean %>%
  mutate(
    es_grupal = (is.na(tip_edad)      | trimws(tip_edad) == "") & 
      (is.na(sexo)      | trimws(sexo) == "") & 
      (is.na(Establec)  | trimws(Establec) == "") & 
      (is.na(Servicio)  | trimws(Servicio) == "")
  )

tabyl(df_clean$es_grupal)

# 2. Generar df_grupales (Los que cumplieron la condición)
df_grupales <- df_clean %>%
  filter(es_grupal == TRUE) %>%
  dplyr::select(-es_grupal)

# 3. Generar df_individuales (El resto)
df_individuales <- df_clean %>%
  filter(!es_grupal) %>% # El símbolo '!' invierte la selección (NO es grupal)
  dplyr::select(-es_grupal)

# --- VERIFICACIÓN ---
cat("Total registros:", nrow(df_clean), "\n")
cat("Grupales detectados:", nrow(df_grupales), "\n")
cat("Individuales detectados:", nrow(df_individuales), "\n")

df_grupales<- df_grupales%>%
  mutate(labconf_num = as.numeric(replace_na(labconf, 0)))


# GUARDAR EN .RDS (Para lectura rápida en Quarto)
saveRDS(df_grupales, "data/df_grupales.rds")
saveRDS(df_individuales, "data/df_individuales.rds")

colSums(is.na(df_individuales))

sum(df_individuales$Ubigeo_res == 0, na.rm = TRUE)
sapply(df_individuales, class)


##Tablas para mapas

library(tidyverse)
library(stringr)

# 1. TABLA MAESTRA DE COORDENADAS DEPARTAMENTALES (PERÚ)
# Fuente: Coordenadas estándar de capitales regionales para visualización
library(tidyverse)
library(stringr)

df_individuales <- readRDS("data/df_individuales.rds")

# Actualizamos la tabla para incluir la distinción
coordenadas_geo <- tribble(
  ~COD_GEO, ~REGION,             ~lat,      ~lng,
  "01",     "AMAZONAS",          -6.2317,   -77.8690,
  "02",     "ANCASH",            -9.5278,   -77.5278,
  "03",     "APURIMAC",          -13.6339,  -72.8814,
  "04",     "AREQUIPA",          -16.3988,  -71.5350,
  "05",     "AYACUCHO",          -13.1588,  -74.2239,
  "06",     "CAJAMARCA",         -7.1638,   -78.5003,
  "07",     "CALLAO",            -12.0566,  -77.1181,
  "08",     "CUSCO",             -13.5226,  -71.9673,
  "09",     "HUANCAVELICA",      -12.7861,  -74.9769,
  "10",     "HUANUCO",           -9.9306,   -76.2422,
  "11",     "ICA",               -14.0678,  -75.7286,
  "12",     "JUNIN",             -11.1581,  -75.9930,
  "13",     "LA LIBERTAD",       -8.1160,   -79.0300,
  "14",     "LAMBAYEQUE",        -6.7714,   -79.8409,
  "15",     "LIMA REGIÓN",       -11.1066,  -77.6050, # Centrado en Huacho
  "1501",   "LIMA METROPOLITANA",-12.0464,  -77.0428, # Plaza Mayor / Centro
  "16",     "LORETO",            -3.7491,   -73.2538,
  "17",     "MADRE DE DIOS",     -12.5933,  -69.1891,
  "18",     "MOQUEGUA",          -17.1983,  -70.9357,
  "19",     "PASCO",             -10.6675,  -76.2561,
  "20",     "PIURA",             -5.1945,   -80.6328,
  "21",     "PUNO",              -15.8422,  -70.0199,
  "22",     "SAN MARTIN",        -6.0342,   -76.9717,
  "23",     "TACNA",             -18.0146,  -70.2536,
  "24",     "TUMBES",            -3.5715,   -80.4593,
  "25",     "UCAYALI",           -8.3791,   -74.5539
)

df_individuales_geo <- df_individuales %>%
  mutate(
    # 1. Estandarizar a texto de 6 dígitos
    ubigeo_str = str_pad(as.character(Ubigeo_res), width = 6, side = "left", pad = "0"),
    
    # 2. Crear la llave de enlace (COD_GEO)
    COD_GEO = case_when(
      # CASO 1: Sin Información (El ubigeo es 0 o 000000)
      Ubigeo_res == 0 | ubigeo_str == "000000" ~ "SIN_INFO",
      
      # CASO 2: Lima Metropolitana
      substr(ubigeo_str, 1, 4) == "1501" ~ "1501",
      
      # CASO 3: Lima Región
      substr(ubigeo_str, 1, 2) == "15"   ~ "15",
      
      # CASO 4: Resto del país
      TRUE                               ~ substr(ubigeo_str, 1, 2)
    )
  ) %>%
  # 3. Unir con la tabla maestra de coordenadas
  left_join(coordenadas_geo, by = "COD_GEO") %>%
  
  # 4. LIMPIEZA FINAL (Importante)
  # Si es "SIN_INFO", el join devolvió NA en REGION, lat y lng.
  # Aquí ponemos un nombre bonito para las tablas.
  mutate(
    REGION = ifelse(COD_GEO == "SIN_INFO", "SIN INFORMACIÓN", REGION)
  )
# Verificación rápida
tabyl(df_individuales_geo$COD_GEO)
tabyl(df_individuales_geo$REGION)

saveRDS(df_individuales_geo, "data/df_individuales_geo.rds")

tabyl(df_individuales_geo$Fi)%>%
  arrange(desc(n))%>%
  adorn_totals("row")%>%
  mutate(pct = scales::percent(n/sum(n)))
