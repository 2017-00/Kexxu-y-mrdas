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

# Seleccionar el archivo
path <- file.choose()

# Función para leer y modificar datos del eye tracker
read_tobii_eye_data <- function(path) {
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  
  # Inicializar una lista para almacenar las coordenadas
  locs <- list()
  
  # Definimos los puntos de referencia
  puntos <- data.frame(
    x = c(857, 936, 1023, 857, 935, 1025, 861, 944, 1023),
    y = c(1080-490, 1080-474, 1080-475, 1080-541, 1080-559, 1080-530, 1080-618, 1080-599, 1080-609)
  )
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
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
      rx <- js$data$gaze2d[1]
      ry <- js$data$gaze2d[2]
      
      # Convertir las coordenadas a valores enteros
      x <- as.integer((rx) * 1920)
      y <- 1080 - as.integer((ry) * 1080)
      
      # Extraer el timestamp
      timestamp <- as.numeric(js$timestamp)
      
      # Calcular distancias a todos los puntos de referencia y diferencias en x y y
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)*(1920/) 
      dif_y <- (y - puntos$y)*(1080/435.5)
      p_x <- puntos$x
      p_y <- puntos$y
      
      # Determinar el grupo con la mínima distancia
      group <- which.min(distancias)
      
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      
      # Agregar los datos x, y, timestamp a la lista
      locs[[length(locs) + 1]] <- c(x, y, group_colors[group], min(distancias), dif_x[group], dif_y[group], p_x[group], p_y[group])
    }
  }
  
  # Convertir la lista en un dataframe para facilitar su manejo
  locs_df <- as.data.frame(do.call(rbind, locs))
  colnames(locs_df) <- c("x", "y", "group", "distance", "dif_x", "dif_y", "p_x", "p_y")
  
  return(locs_df)
}

# Leer los datos del archivo JSON
locs_df <- read_tobii_eye_data(path)

# Convertir las columnas x y y a numérico
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))
write.csv(locs_df, file = "C:/Users/ignaz/Downloads/final/final/tobii/catalan/catalan2-1.csv", row.names = FALSE)

# Densidad de puntos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 857, y = 1080 - 490), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 936, y = 1080 - 474), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1023, y = 1080 - 475), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 857, y = 1080 - 541), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 935, y = 1080 - 559), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1025, y = 1080 - 530), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 861, y = 1080 - 618), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 944, y = 1080 - 599), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1023, y = 1080 - 609), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
