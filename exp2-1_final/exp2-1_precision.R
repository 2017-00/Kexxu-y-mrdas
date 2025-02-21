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
  x_prom_total <- 0
  y_prom_total <- 0
  
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
    
    # Acumular las diferencias
    x_prom_total <- abs(x_ref - x_dif)
    y_prom_total <- abs(y_ref - y_dif)
    
    # Elevar al cuadrado las diferencias 
    x_prom_c <- x_prom_total^2
    y_prom_c <- y_prom_total^2
    
    locs[[length(locs) + 1]] <- c(group, x_dif, y_dif, x_prom_total, y_prom_total, x_prom_c, y_prom_c)
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo", "x", "y", "x_prom", "y_prom", "x_root", "y_root")
  
  return(as.data.frame(locs_df))
}

# Procesar y combinar los datos de todos los archivos seleccionados
all_data <- lapply(paths, read_kexxu_eye_data)
locs_df <- bind_rows(all_data)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(locs_df$x)
locs_df$y <- as.numeric(locs_df$y)
locs_df$x_prom <- as.numeric(locs_df$x_prom)
locs_df$y_prom <- as.numeric(locs_df$y_prom)
locs_df$x_root <- as.numeric(locs_df$x_root)
locs_df$y_root <- as.numeric(locs_df$y_root)

# Calcular los promedios generales de las diferencias
x_prom <- mean(locs_df$x_prom) / 26.98
y_prom <- mean(locs_df$y_prom) / 26.98

# Calcular error cuadratico medio
x_root <- sqrt(mean(locs_df$x_root)) / 26.98
y_root <- sqrt(mean(locs_df$y_root)) / 26.98

# Imprimir los resultados de promedio (grados)
print(paste("Precision en eje X:", x_prom))
print(paste("Precision en eje Y:", y_prom))

# Imprimir los resultados de error cuadratico (grados)
print(paste("Exactitud en eje X:", x_root))
print(paste("Exactitud en eje Y:", y_root))

