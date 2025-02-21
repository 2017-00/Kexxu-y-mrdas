# Cargar las librerías necesarias
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(tcltk)

options(scipen=999)

# Seleccionar múltiples archivos
paths <- tk_choose.files()

# Función para leer y procesar los datos del eye-tracker
read_kexxu_eye_data <- function(path) {
  lines <- read.csv(path, header = TRUE, sep = ",")
  
  locs <- list()
  
  for (i in 1:nrow(lines)) {
    line <- lines[i, ]
    x_ref <- line$p_x
    y_ref <- line$p_y
    group <- line$group
    
    # Correcciones de las coordenadas de referencia
    if (group == "magenta"){
      x_ref <- 960
      y_ref <- 540
    }
    if (group == "pink"){
      x_ref <- 263.13
      y_ref <- 13.5
    }
    if (group == "orange"){
      x_ref <- 1656.87
      y_ref <- 13.5
    }
    if (group == "blue"){
      x_ref <- 263.13
      y_ref <- 1066.5
    }
    if (group == "red"){
      x_ref <- 1656.87
      y_ref <- 1066.5
    }
    if (group == "purple"){
      x_ref <- 960
      y_ref <- 13.5
    }
    if (group == "yellow"){
      x_ref <- 960
      y_ref <- 1066.5
    }
    if (group == "green"){
      x_ref <- 263.13
      y_ref <- 540
    }
    if (group == "brown"){
      x_ref <- 1656.87
      y_ref <- 540
    }
    
    x_dif <- x_ref + line$dif_x
    y_dif <- y_ref + line$dif_y
 #   group <- "red"
    
    locs[[length(locs) + 1]] <- c(group, x_dif, y_dif)
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo", "x", "y")
  
  return(as.data.frame(locs_df))
}

# Procesar y combinar los datos de todos los archivos seleccionados
all_data <- lapply(paths, read_kexxu_eye_data)
locs_df <- bind_rows(all_data)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(locs_df$x)
locs_df$y <- as.numeric(locs_df$y)
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/final/1134/final2-2_kexxu_csv.csv", row.names = FALSE)

# Graficar los datos
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 263.13, y = 1066.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 1066.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1656.87, y = 1066.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 263.13, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1656.87, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 263.13, y = 13.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 13.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1656.87, y = 13.5), color = "black", shape = 3, size = 4) +
  geom_point(aes(color = grupo), alpha = 0.5, size = .5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")+
  scale_x_continuous(breaks = seq(0, 2000, by = 200)) +
  scale_y_continuous(breaks = seq(0, 1200, by = 100))
