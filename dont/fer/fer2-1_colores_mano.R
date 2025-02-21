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
    x = c(646, 600, 698, 598, 695, 599, 642, 594, 687),
    y = c(720-231, 720-263, 720-252, 720-181, 720-174, 720-251, 720-172, 720-213, 720-209)
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
      dif_x <- (x - puntos$x)*(1920/395) 
      dif_y <- (y - puntos$y)*(1080/220)
      p_x <- puntos$x
      p_y <- puntos$y
      
      # Determinar el grupo con la mínima distancia
      group <- which.min(distancias)
      
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      
      if ((y>400)&&(x<800)){
        locs[[length(locs) + 1]] <- c(x, y, group_colors[group], min(distancias), dif_x[group], dif_y[group], p_x[group], p_y[group])
      }
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
write.csv(locs_df, file = "C:/Users/ignaz/Downloads/final/final/kexxu/fer/fer2-1.csv", row.names = FALSE)


# Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 646, y = 720-231), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 600, y = 720-263), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 698, y = 720-252), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 598, y = 720-181), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 695, y = 720-174), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 599, y = 720-251), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 642, y = 720-172), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 594, y = 720-213), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 687, y = 720-209), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
