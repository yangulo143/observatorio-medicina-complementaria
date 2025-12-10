if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, zoo, rio, flextable, officer, readxl, scales, ggExtra, writexl, janitor, corrr, ggpubr, ggExtra, gtsummary, tableone, broom, epiDisplay, car )



# 1. LEER SOLO LOS ENCABEZADOS (Las primeras 2 filas)
# ---------------------------------------------------
ruta_archivo <- "atenciones por cpms.xlsx" # <--- CAMBIA ESTO POR TU NOMBRE DE ARCHIVO

headers <- readxl::read_excel(ruta_archivo, col_names = FALSE, n_max = 2)

# Transponemos para trabajar más fácil: Fila 1 (Años) y Fila 2 (CPMS)
header_t <- t(headers) %>% as.data.frame()
colnames(header_t) <- c("anio_raw", "cpms_raw")

# 2. LIMPIEZA DE ENCABEZADOS
# ---------------------------------------------------
# Rellenamos los años hacia abajo (locf = last observation carried forward)
# Esto convierte: [2018, NA, NA, 2019] -> [2018, 2018, 2018, 2019]
nombres_cols <- header_t %>%
  mutate(
    anio_relleno = na.locf(anio_raw, na.rm = FALSE), # Rellenar años
    
    # Creamos un nombre único combinando Año + CPMS
    # Ej: "2018_97124", "2019_97810"
    # Si es la primera columna (Diagnóstico), le ponemos "DIAGNOSTICO"
    nuevo_nombre = case_when(
      row_number() == 1 ~ "DIAGNOSTICO",
      TRUE ~ paste0(anio_relleno, "_", cpms_raw)
    )
  ) %>%
  pull(nuevo_nombre) # Extraemos el vector de nombres

# 3. LEER LOS DATOS REALES Y APLICAR NOMBRES
# ---------------------------------------------------
# Leemos saltando las 2 filas de encabezado sucias
datos_raw <- read_excel(ruta_archivo, skip = 2, col_names = FALSE)

# Asignamos nuestros nombres limpios
colnames(datos_raw) <- nombres_cols

# 4. TRANSFORMACIÓN FINAL (PIVOT)
# ---------------------------------------------------
datos_tidy <- datos_raw %>%
  # Quitamos las columnas de "Total" que vienen en el Excel (calcularemos las nuestras)
  dplyr::select(-contains("Total"), -contains("NA")) %>% 
  
  # Convertimos de Ancho a Largo
  pivot_longer(
    cols = -DIAGNOSTICO,     # Todo menos la columna de diagnósticos
    names_to = "temp_key",   # Columna temporal
    values_to = "cantidad"   # Los valores numéricos
  ) %>%
  
  # Separamos "2018_97124" en "2018" y "97124"
  separate(temp_key, into = c("anio", "cpms_codigo"), sep = "_") %>%
  filter(DIAGNOSTICO != "Total general") %>%
  
  # Limpieza final de tipos de datos
  mutate(
    anio = as.integer(anio),
    cantidad = as.numeric(cantidad),
    # Reemplazar NAs por 0 si es necesario para sumar
    cantidad = replace_na(cantidad, 0) 
  )

# ¡LISTO! Visualizamos
head(datos_tidy)

export(datos_tidy, "datos_tidy_cpms.xlsx")

# 1. LEER ENCABEZADOS (Ajustado para detectar EDAD)
# ---------------------------------------------------
ruta_archivo <- "atenciones por grupo etario.xlsx" # <--- ACTUALIZA LA RUTA

# Leemos solo las 2 primeras filas para descifrar la estructura
headers <- read_excel(ruta_archivo, col_names = FALSE, n_max = 2)

# Transponemos
header_t <- t(headers) %>% as.data.frame()
colnames(header_t) <- c("anio_raw", "cpms_raw")

# 2. LIMPIEZA DE NOMBRES DE COLUMNA
# ---------------------------------------------------
nombres_cols <- header_t %>%
  mutate(
    # Rellenamos los años hacia abajo (2019, NA, NA -> 2019, 2019, 2019)
    anio_relleno = na.locf(anio_raw, na.rm = FALSE),
    
    # LÓGICA ACTUALIZADA:
    # Si es la primera fila/columna, forzamos el nombre "EDAD"
    # Para el resto, combinamos "Año_CPMS"
    nuevo_nombre = case_when(
      row_number() == 1 ~ "EDAD", 
      TRUE ~ paste0(anio_relleno, "_", cpms_raw)
    )
  ) %>%
  pull(nuevo_nombre)

# 3. CARGA Y TRANSFORMACIÓN
# ---------------------------------------------------
datos_edad_tidy <- read_excel(ruta_archivo, skip = 2, col_names = FALSE) %>%
  # Asignamos los nombres limpios
  set_names(nombres_cols) %>%
  
  # Quitamos columnas basura (Totales calculados en Excel o vacías)
  dplyr::select(-contains("Total"), -contains("NA")) %>%
  
  # Pivotamos (De Ancho a Largo) - Ahora preservando la columna EDAD
  pivot_longer(
    cols = -EDAD, 
    names_to = "temp_key",
    values_to = "cantidad"
  ) %>%
  
  # Separamos Año y CPMS
  separate(temp_key, into = c("anio", "cpms_codigo"), sep = "_") %>%
  
  # --- FILTRO DE SEGURIDAD ---
  # Eliminamos filas donde la edad sea texto de resumen (ej. "Total general")
  filter(!str_detect(as.character(EDAD), "Total|General|Resumen")) %>%
  
  # Ajuste de tipos de datos
  mutate(
    anio = as.integer(anio),
    
    # OJO CON LA EDAD: 
    # A veces Excel pone "Menor de 1 año" o números. 
    # Si son solo números, descomenta la siguiente línea:
    # EDAD = as.integer(EDAD), 
    
    cantidad = replace_na(as.numeric(cantidad), 0)
  )

# Vista previa
glimpse(datos_edad_tidy)

export(datos_edad_tidy, "datos_tidy_edad.xlsx")

# 1. LEER LA ESTRUCTURA DE ENCABEZADOS
# ---------------------------------------------------
ruta_archivo <- "atenciones por region.xlsx" # <--- CAMBIA ESTO

# Leemos las dos filas de cabecera
headers <- read_excel(ruta_archivo, col_names = FALSE, n_max = 2)

# Transponemos y rellenamos los años vacíos
nombres_cols <- t(headers) %>% 
  as.data.frame() %>%
  mutate(
    V1 = na.locf(V1, na.rm = FALSE), # Rellenar años (2018, NA -> 2018, 2018)
    
    # Creamos el nombre compuesto
    nuevo_nombre = case_when(
      # Detectamos la primera columna (sea "Región", "Region", o vacía) y la forzamos a "REGION"
      row_number() == 1 ~ "REGION", 
      TRUE ~ paste0(V1, "_", V2)
    )
  ) %>%
  pull(nuevo_nombre)

# 2. CARGA DE DATOS Y LIMPIEZA
# ---------------------------------------------------
datos_region_tidy <- read_excel(ruta_archivo, skip = 2, col_names = FALSE) %>%
  set_names(nombres_cols) %>%
  
  # Eliminamos columnas de totales de Excel si existen
  dplyr::select(-contains("Total"), -contains("NA")) %>%
  
  # Pivotar: De Ancho a Largo
  pivot_longer(
    cols = -REGION, 
    names_to = "temp_key",
    values_to = "cantidad"
  ) %>%
  
  # Separar Año y CPMS
  separate(temp_key, into = c("anio", "cpms_codigo"), sep = "_") %>%
  
  # --- LIMPIEZA FINAL ---
  filter(!str_detect(REGION, "Total|General")) %>% # Quitar fila de totales
  mutate(
    anio = as.integer(anio),
    cantidad = replace_na(as.numeric(cantidad), 0), 
    # ESTANDARIZACIÓN DE TEXTO (Importante para mapas futuros)
    # Convierte "ANCASH " -> "ANCASH" y quita espacios extra
    REGION = str_trim(str_to_upper(REGION)) 
  )

# ¡Listo para usar!
head(datos_region_tidy)
export(datos_region_tidy, "datos_tidy_region.xlsx")


