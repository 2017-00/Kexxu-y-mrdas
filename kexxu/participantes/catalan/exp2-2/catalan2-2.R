library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
options(scipen=999)

path <- file.choose()

read_kexxu_eye_data <- function(path) {
  lines <- readLines(path)
  locs <- list()
  
  # Definimos los puntos de referencia
  puntos <- data.frame(
    x = c(592, 493, 704, 484, 706, 601, 594, 492, 704),
    y = c(720-273, 720-361, 720-346, 720-201, 720-196, 720-365, 720-208, 720-301, 720-287)
  )
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]]
    if (grepl("/eyetracking$", parts[1])) {
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- js$pupil_rel_pos_x
      ry <- js$pupil_rel_pos_y
      x <- 640 - as.integer((rx) * 640)
      y <- 360 -  as.integer((ry) * 360)
      timestamp <- as.numeric(js$timestamp_ms)
      
      # Calcular distancias a todos los puntos de referencia y diferencias en x y y
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)*(1920/440) 
      dif_y <- (y - puntos$y)*(1080/245)
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

locs_df <- read_kexxu_eye_data(path)
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/final/1134/catalan2-2.csv", row.names = FALSE)

# Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 592, y = 720-273), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 493, y = 720-361), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 704, y = 720-346), color = "black", shape = 8, size = 4) +  
  geom_point(aes(x = 484, y = 720-201), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 706, y = 720-196), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 601, y = 720-365), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 594, y = 720-208), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 492, y = 720-301), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 704, y = 720-287), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")

