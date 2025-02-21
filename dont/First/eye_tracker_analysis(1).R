################################################
# Laboratorio de Cognición Corporizada
# -----------------------------------------
#
# Análisis de seguimiento ocular (eyetracking)
# ----------------------------------------------
# Kexxu Eye
#
# 2024
################################################


# LabCoCo
# Labcoco*7283

# Distancia de la mirada del centro de la pantalla, de -1 a 1
# El video es 1280x720 pixeles para Kexxu Eye v1
# pupil_rel_ps_x 
# pupil_rel_ps_y


# Para leer los datos en formato .json
install.packages("jsonlite")
library(jsonlite)

# Para transformar / analizar datos
library(dplyr)
library(tidyr)

# Gráficas
library(ggplot2)
# install.packages("viridis")
library(viridis)

# Qutiar notación científica
options(scipen=999)

# Paquete interesante para eyetracking - actualizar R
# install.packages("eyetackingR")
# library(eyetrackingR)


# Seleccionar y leer el archivo
path <- file.choose()


# Para convertir las posiciones del ojo en coordinadas en la pantalla se usa la siguiente función

read_kexxu_eye_data <- function(path) {
  # Instalar y cargar jsonlite si aún no está instalado/cargado
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
    library(jsonlite)
  }
  
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  # Inicializar una lista para almacenar las coordenadas
  locs <- list()
  
  # Iterar sobre cada línea
  for (line in lines) {
    parts <- strsplit(line, " ")[[1]] # Dividir la línea actual (line) en sus componentes, usando el espacio (" ") como delimitador.
   # if (grepl("/eyetracking$", parts[1])) { # Verificar si el primer elemento de parts termina con la cadena "/eyetracking", la función grepl devuelve TRUE si lo encuentra
      # Convertir la cadena JSON en una lista
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- js$pupil_rel_pos_x
      ry <- js$pupil_rel_pos_y
      x <- 640 - as.integer((rx) * 640)
      y <- 360 + as.integer((ry) * 360)
      # Extraer el timestamp
      timestamp <- as.numeric(js$timestamp_ms)
      locs[[length(locs) + 1]] <- c(x, y, timestamp) # Cada conjunto de datos x, y, timestamp se agrega al final de la lista
   # }
  }
  # Convertir la lista en un dataframe para facilitar su manejo
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("x", "y", "timestamp_ms")
  return(locs_df)
}

# Se guarda la información en la variable locs_df
locs_df <- read_kexxu_eye_data(path)

# Se convierte en data.frame
locs_df <- as.data.frame(locs_df)

# Resumen estadístico de las coordenadas x e y, y timestamp
summary(locs_df)  

# Posición de la pupila en la pantalla
plot(locs_df$x, locs_df$y, main = "Posición de la Pupila en el Monitor",
     xlab = "Coordenada X", ylab = "Coordenada Y", pch = 19)

# Tendencia de la coordenada X a lo largo del tiempo
plot(locs_df$timestamp, locs_df$x, type = "l", col = "blue", 
     main = "Tendencia de la Coordenada X a lo Largo del Tiempo", xlab = "Tiempo", ylab = "Coordenada X")

# Para la coordenada Y
plot(locs_df$timestamp, locs_df$y, type = "l", col = "red",
     main = "Tendencia de la Coordenada Y a lo Largo del Tiempo", xlab = "Tiempo", ylab = "Coordenada Y")


# Convertir las columnas x y y a numérico
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))


# Densidad de puntos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +  # Puntos semitransparentes
  geom_density_2d() +  # Añade contornos de densidad
  theme_minimal() +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")

# Usar geom_bin2d para crear un heatmap - mapa de densidad para visualizar la distribución y 
# densidad de conjuntos de datos en dos dimensiones - se divide el espacio en rectángulos "bins"
# se cuenta el número de casos (puntos) en cada rectángulo y se colorean en función de los conteos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_bin2d(bins = 50) +  # Ajustar el número de bins según la resolución deseada
  scale_fill_gradient(low = "blue", high = "red") +  # Colores para baja y alta densidad
  #coord_fixed() +  # Mantiene la relación 1:1 de los ejes
  labs(title = "Heatmap de Densidad de Miradas",
       x = "Coordenada X",
       y = "Coordenada Y",
       fill = "Conteo") +
  theme_minimal()


# Definir una lista de ROIs como rectángulos [x_min, y_min, x_max, y_max]
rois <- list(
  roi1 = c(x_min = 396, y_min = -763, x_max = 650, y_max = 1023),
  roi2 = c(x_min = 651, y_min = -763, x_max = 900, y_max = 1023)
)


# Función para verificar si un punto está dentro de una ROI
is_point_in_roi <- function(x, y, roi) {
  x >= roi['x_min'] && x <= roi['x_max'] && y >= roi['y_min'] && y <= roi['y_max']
}

# Añadir una nueva columna al dataframe para la ROI de cada punto
locs_df$roi <- NA  # Inicializar con NA

for (roi_name in names(rois)) {
  roi <- rois[[roi_name]]
  for (i in 1:nrow(locs_df)) {
    if (is_point_in_roi(locs_df$x[i], locs_df$y[i], roi)) {
      locs_df$roi[i] <- roi_name
    }
  }
}

# Contar puntos por ROI
table(locs_df$roi)

# Visualizar las roi mediante la columna roi que indica la región de interés
ggplot(locs_df, aes(x = x, y = y, color = roi)) +
  geom_point(alpha = 0.5) +  # Usamos semi-transparencia para ver mejor la densidad
  theme_minimal() +
  labs(title = "Distribución de Puntos por Región de Interés",
       x = "Coordenada X", y = "Coordenada Y",
       color = "Región de Interés") +
  scale_color_brewer(palette = "Set1")  # Escoger una paleta de colores para diferenciar los ROIs


# Convertir a segundos desde el inicio 0
locs_df <- locs_df %>%
  mutate(time_seconds = (timestamp_ms - min(timestamp_ms)) / 1000)  

# Por si se requiere
# Calcular el número de observaciones (como proxy del tiempo de mirada) por ROI
tiempo_mirada <- aggregate(time_seconds ~ roi, data = locs_df, FUN = length)
tiempo_mirada

# Por si se requiere
# Extraer los tiempos de mirada para ROI1 y ROI2 desde el inicio del experimento
tiempo_mirada_roi1 <- locs_df$time_seconds[locs_df$roi == "roi1"]
tiempo_mirada_roi1
tiempo_mirada_roi2 <- locs_df$time_seconds[locs_df$roi == "roi2"]
tiempo_mirada_roi2


# Calcular la Tendencia Promedio de las Coordenadas X Y por ROI
locs_df_trend <- locs_df %>%
  group_by(roi, time_seconds) %>%
  summarise(x_mean = mean(x, na.rm = TRUE), y_mean = mean(y, na.rm = TRUE))  # Ajustar por cualquier NA

locs_df_trend

# La elección de usar el promedio de x-y en intervalos regulares ayuda a suavizar la variabilidad 
# y resaltar las tendencias generales. 
# Variaciones significativas en las tendencias entre diferentes ROIs  podría indicar diferencias 
# en el comportamiento de mirada o en el interés visual a lo largo del tiempo.

# Gráfico de líneas - cómo varía la coordenada X promedio a lo largo del tiempo para cada ROI
ggplot(locs_df_trend, aes(x = time_seconds, y = x_mean, color = roi)) +
  geom_line() +  # Dibuja la línea de tendencia
  theme_minimal() +
  labs(title = "Tendencia de la Coordenada X por ROI",
       x = "Tiempo (segundos)",
       y = "Coordenada X Promedio",
       color = "Región de Interés")

# Gráfico de líneas - cómo varía la coordenada Y promedio a lo largo del tiempo para cada ROI
ggplot(locs_df_trend, aes(x = time_seconds, y = y_mean, color = roi)) +
  geom_line() +  # Dibuja la línea de tendencia
  theme_minimal() +
  labs(title = "Tendencia de la Coordenada Y por ROI",
       x = "Tiempo (segundos)",
       y = "Coordenada Y Promedio",
       color = "Región de Interés")


# Transformar de formato ancho a largo
# Donde cada fila representa una observación única de x_mean o y_mean, 
# indicado por la nueva columna coordinate.
locs_df_long <- locs_df_trend %>%
  pivot_longer(cols = c(x_mean, y_mean), names_to = "coordinate", values_to = "mean_value")

# Comparar directamente las tendencias de x_mean y y_mean a lo largo del tiempo en un único gráfico, 
# manteniendo la distinción entre las dos coordenadas y entre diferentes ROIs.
ggplot(locs_df_long, aes(x = time_seconds, y = mean_value, color = roi)) +
  geom_line(aes(linetype = coordinate)) +  # Diferenciar por tipo de línea
  geom_point(aes(shape = coordinate)) +  # Diferenciar por forma
  theme_minimal() +
  labs(title = "Tendencias de Coordenadas X e Y por ROI a lo largo del tiempo",
       x = "Tiempo (segundos)",
       y = "Valor Medio",
       color = "Región de Interés",
       linetype = "Coordenada",
       shape = "Coordenada") +
  scale_shape_manual(values = c(16, 17)) +  # Personaliza las formas si es necesario
  scale_linetype_manual(values = c("solid", "dashed"))  # Personaliza los tipos de línea

ggplot(locs_df, aes(x = x, y = y)) +
  geom_path(alpha = 0.7, color = "blue") +  # Dibujar trayectorias
  theme_minimal() +
  labs(title = "Trayectorias de las Coordenadas del Ojo",
       x = "Coordenada X",
       y = "Coordenada Y")

