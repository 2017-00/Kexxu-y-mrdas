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
    x = c(631, 597, 690, 604, 693, 603, 649, 609, 713),
    y = c(720-262, 720-290, 720-281, 720-248, 720-226, 720-286, 720-212, 720-254, 720-245)
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
      dif_x <- (x - puntos$x)*(1920/385) 
      dif_y <- (y - puntos$y)*(1080/217)
      p_x <- puntos$x
      p_y <- puntos$y
      
      # Determinar el grupo con la mínima distancia
      group <- which.min(distancias)
      
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      
      if ((x>500)&&(y>300)){
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
write.csv(locs_df, file = "C:/Users/ignaz/Downloads/final/final/kexxu/carlos/carlos2-1.csv", row.names = FALSE)


# Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 637, y = 720-262), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 597, y = 720-290), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 690, y = 720-281), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 604, y = 720-248), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 693, y = 720-226), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 603, y = 720-286), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 649, y = 720-212), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 609, y = 720-254), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 713, y = 720-245), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
