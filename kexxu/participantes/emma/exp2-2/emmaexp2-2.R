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
    x = c(544, 655, 757, 557, 623, 755, 539, 653, 736),
    y = c(720-196, 720-171, 720-180, 720-244, 720-300, 720-249, 720-367, 720-317, 720-347)
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
      dif_x <- (x - puntos$x)*(1920/406) 
      dif_y <- (y - puntos$y)*(1080/224)
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
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/emma/emmaexp2-2.csv", row.names = FALSE)


  # Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 544, y = 720-196), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 655, y = 720-171), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 757, y = 720-180), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 557, y = 720-244), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 623, y = 720-300), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 755, y = 720-249), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 539, y = 720-367), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 653, y = 720-317), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 736, y = 720-347), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
