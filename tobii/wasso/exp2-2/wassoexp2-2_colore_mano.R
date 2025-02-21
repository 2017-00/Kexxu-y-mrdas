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
options(scipen=999)

path <- file.choose()

read_tobii_eye_data <- function(path) {
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  
  # Inicializar una lista para almacenar las coordenadas
  locs <- list()
  
  # Definimos los puntos de referencia
  puntos <- data.frame(
    x = c(711, 952, 1187, 730, 949, 1187, 736, 966, 1178),
    y = c(1080-299, 1080-299, 1080-289, 1080-489, 1080-482, 1080-462, 1080-656, 1080-636, 1080-623)
  )
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  # Iterar sobre cada línea
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    js <- tryCatch(jsonlite::fromJSON(line), error = function(e) {
      message("Error al analizar JSON en la línea ", i, ": ", e)
      return(NULL)
    })
    
    if (is.null(js)) {
      next
    }
    
    if (js$type == "gaze" && !is.null(js$data$eyeright$pupildiameter)) {
      rx <- js$data$gaze2d[1]
      ry <- js$data$gaze2d[2]
      
      x <- as.integer((rx) * 1920)
      y <- 1080 - as.integer((ry) * 1080)
      
      timestamp <- as.numeric(js$timestamp)
      
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)*(1920/994) 
      dif_y <- (y - puntos$y)*(1080/556)
      p_x <- puntos$x
      p_y <- puntos$y
      
      group <- which.min(distancias)
      
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      
        locs[[length(locs) + 1]] <- c(x, y, group_colors[group], min(distancias), dif_x[group], dif_y[group], p_x[group], p_y[group])
    }
  }
  
  locs_df <- as.data.frame(do.call(rbind, locs))
  colnames(locs_df) <- c("x", "y", "group", "distance", "dif_x", "dif_y", "p_x", "p_y")
  
  return(locs_df)
}

locs_df <- read_tobii_eye_data(path)

locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/tobii/wasso/exp2-2/wassoexp2-2.csv", row.names = FALSE)

# Densidad de puntos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 711, y = 1080 - 299), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 952, y = 1080 - 299), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1187, y = 1080 - 289), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 730, y = 1080 - 489), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 966, y = 1080 - 482), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1187, y = 1080 - 462), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 736, y = 1080 - 656), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 966, y = 1080 - 636), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 1178, y = 1080 - 623), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
