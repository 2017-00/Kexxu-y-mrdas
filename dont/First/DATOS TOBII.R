# Instalar y cargar las bibliotecas necesarias
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)

library(dplyr)
library(tidyr)
library(ggplot2)
if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}
library(viridis)

# Quitar notación científica
options(scipen=999)

# Función para leer y modificar datos del eye tracker
read_kexxu_eye_data <- function(path) {
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  
  # Inicializar una lista para almacenar las coordenadas
  locs <- list()
  
  # Iterar sobre cada línea
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Intentar convertir la línea JSON en un objeto list
    js <- tryCatch(jsonlite::fromJSON(line), error = function(e) {
      message("Error al analizar JSON en la línea ", i, ": ", e)
      return(NULL)
    })
    
    # Si el objeto JSON es NULL (error al analizar), pasar a la siguiente línea
    if (is.null(js)) {
      next
    }
    
    # Comprobar si el tipo es "gaze" y si hay datos de pupilas
    if (js$type == "gaze" && !is.null(js$data$eyeright$pupildiameter)) {
      # Extraer coordenadas de origen de la mirada del ojo derecho
      rx <- js$data$eyeright$gazeorigin[1]
      ry <- js$data$eyeright$gazeorigin[2]
      
      if ((720 + as.integer(ry * 720)) > -9500){
        # Convertir las coordenadas a valores enteros
        x <- 640 - as.integer(rx * 640)
        # xprom <- xprom + x
        y <- 360 + as.integer(ry * 360)
        #  yprom <- yprom + y
      
        # Extraer el timestamp
        timestamp <- as.numeric(js$timestamp)
      
        # Agregar los datos x, y, timestamp a la lista
        locs[[length(locs) + 1]] <- c(x, y, timestamp)
      }
      else{
        next
      }
    }
  }
  
  # Convertir la lista en un dataframe para facilitar su manejo
  locs_df <- as.data.frame(do.call(rbind, locs))
  colnames(locs_df) <- c("x", "y", "timestamp")
  
  return(locs_df)
}

# Seleccionar el archivo
path <- file.choose()

# Leer los datos del archivo JSON
locs_df <- read_kexxu_eye_data(path)

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

ggplot(locs_df, aes(x = x, y = y)) +
  geom_path(alpha = 0.7, color = "blue") +  # Dibujar trayectorias
  theme_minimal() +
  labs(title = "Trayectorias de las Coordenadas del Ojo",
       x = "Coordenada X",
       y = "Coordenada Y")

