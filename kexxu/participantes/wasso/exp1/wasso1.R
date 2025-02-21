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
    x = c(602, 759),
    y = c(720-286, 720-275)
  )
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]]
    if (grepl("/eyetracking$", parts[1])) {
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- (js$pupil_rel_pos_x)/26.9895
      ry <- (js$pupil_rel_pos_y)/26.9895
      x <- (640 - as.integer((rx) * 640))/26.9895
      y <- (360 -  as.integer((ry) * 360))/26.9895
      timestamp <- as.numeric(js$timestamp_ms)
      
      # Calcular distancias a todos los puntos de referencia y diferencias en x y y
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)*(1920/511)
      dif_y <- (y - puntos$y)*(1080/248)
      p_x <- puntos$x
      p_y <- puntos$y
      
      # Determinar el grupo con la mínima distancia
      group <- which.min(distancias)
      
      group_colors <- c("purple", "orange")
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
#write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/wasso/exp1/wasso1.csv", row.names = FALSE)

# Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(color = group), alpha = 1) +
  #geom_path(aes(group = 1), size = .5) +
  theme_minimal() +
  geom_point(aes(x = 602/26.9895, y = (720-286)/26.9895), color = "black", shape = 3, size = 1.5) +
  geom_point(aes(x = 759/26.9895, y = (720-275)/26.9895), color = "black", shape = 16, size = 1.5) +
  theme(legend.position = "none") +
  labs(x = "Coordenada X",
       y = "Coordenada Y") 
