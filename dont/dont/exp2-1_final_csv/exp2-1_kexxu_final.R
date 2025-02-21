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
      x_ref <- 611.565
      y_ref <- 191.565
    }
    if (group == "orange"){
      x_ref <- 1308.435
      y_ref <- 191.565
    }
    if (group == "blue"){
      x_ref <- 611.565
      y_ref <- 888.435
    }
    if (group == "red"){
      x_ref <- 1308.435
      y_ref <- 888.435
    }
    if (group == "purple"){
      x_ref <- 960
      y_ref <- 191.565
    }
    if (group == "yellow"){
      x_ref <- 960
      y_ref <- 888.435
    }
    if (group == "green"){
      x_ref <- 611.565
      y_ref <- 540
    }
    if (group == "brown"){
      x_ref <- 1308.435
      y_ref <- 540
    }
    
    x_dif <- x_ref + line$dif_x
    y_dif <- y_ref + line$dif_y
    group <- "red"
    
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
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/exp2-1_final_csv/final2-1_colores_mano.csv", row.names = FALSE)

# Graficar los datos
ggplot(locs_df, aes(x = x, y = y)) + #475819263
  geom_point(aes(x = 611.565, y = 888.435), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 888.435), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1308.435, y = 888.435), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 611.565, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1308.435, y = 540), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 611.565, y = 191.565), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 960, y = 191.565), color = "black", shape = 3, size = 4) +
  geom_point(aes(x = 1308.435, y = 191.565), color = "black", shape = 3, size = 4) +
  geom_point(aes(color = grupo), alpha = 1, size = .5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")

