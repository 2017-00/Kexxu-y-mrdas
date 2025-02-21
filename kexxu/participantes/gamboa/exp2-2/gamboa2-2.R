library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
options(scipen=999)

# Seleccionar el archivo
path <- file.choose()

# Función para leer y procesar los datos del eye-tracker
read_kexxu_eye_data <- function(path) {
  lines <- readLines(path)
  locs <- list()
  
  # Definir los puntos de referencia
  puntos <- data.frame(
    x = c(603, 485, 728, 469, 731, 601, 592, 473, 728),
    y = c(720-313, 720-402, 720-398, 720-214, 720-203, 720-391, 720-196, 720-297, 720-295)
  )
  
  # Función para calcular la distancia entre dos puntos
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  # Procesar cada línea del archivo
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]]
    if (grepl("/eyetracking$", parts[1])) {
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- js$pupil_rel_pos_x
      ry <- js$pupil_rel_pos_y
      x <- 640 - as.integer(rx * 640)
      y <- 360 - as.integer(ry * 360)
      timestamp <- as.numeric(js$timestamp_ms)
      
      # Calcular distancias a todos los puntos de referencia y diferencias en x y y
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)#*(1920/522) 
      dif_y <- (y - puntos$y)#*(1080/294)
      p_x <- puntos$x
      p_y <- puntos$y
      
      # Determinar el grupo con la mínima distancia
      group <- which.min(distancias)
      
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      
      locs[[length(locs) + 1]] <- c(x, y, group_colors[group], min(distancias), dif_x[group], dif_y[group], p_x[group], p_y[group])
    }
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("x", "y", "group", "distance", "dif_x", "dif_y", "p_x", "p_y")
  
  return(as.data.frame(locs_df))
}

# Leer los datos del eye-tracker
locs_df <- read_kexxu_eye_data(path)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
locs_df$dif_x <- as.numeric(as.character(locs_df$dif_x))
locs_df$dif_y <- as.numeric(as.character(locs_df$dif_y))

# Filtrar cualquier fila con valores NA
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))

# Escribir los datos procesados en un archivo CSV
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/final/1134/gamboa2-2.csv", row.names = FALSE)

# Graficar los datos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 603, y = 720-313), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 485, y = 720-402), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 728, y = 720-398), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 469, y = 720-214), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 731, y = 720-203), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 601, y = 720-391), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 592, y = 720-196), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 473, y = 720-297), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 728, y = 720-295), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")

  
