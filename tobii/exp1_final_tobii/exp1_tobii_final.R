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
read_tobii_eye_data <- function(path) {
  lines <- read.csv(path, header = TRUE, sep = ",")
  
  locs <- list()
  
  for (i in 1:nrow(lines)) {
    line <- lines[i, ]
    x_ref <- line$p_x
    y_ref <- line$p_y
    group <- line$group
    dif <- line$distance
    
    # Correcciones de las coordenadas de referencia
    if (group == "blue"){
      x_ref <- 35.581
      y_ref <- 20.014
    }
    if (group == "yellow"){
      x_ref <- 54.181
      y_ref <- 20.014
    }
    
    x_dif <- x_ref + (line$dif_x)/26.98
    y_dif <- y_ref + (line$dif_y)/26.98
    
    if (length(locs) > 0) {
      last_loc <- locs[[length(locs)]]
      if ((last_loc$grupo == "blue") && (group == "yellow") && (x_dif - last_loc$x > 10)) {
        locs[[length(locs) + 1]] <- list(grupo = group, x = x_dif, y = y_dif)
      }
      else if ((last_loc$grupo == "yellow") && (group == "blue") && (last_loc$x - x_dif > 10)) {
        locs[[length(locs) + 1]] <- list(grupo = group, x = x_dif, y = y_dif)
      }
      else if ((last_loc$grupo == "yellow") && (group == "yellow") && (last_loc$x < x_dif) && ((y_dif-last_loc$y)^2 < 3)) {
        locs[[length(locs) + 1]] <- list(grupo = group, x = x_dif, y = y_dif)
      }
      else if ((last_loc$grupo == "blue") && (group == "blue") && (last_loc$x > x_dif) && ((last_loc$y-y_dif)^2 < 3)) {
        locs[[length(locs) + 1]] <- list(grupo = group, x = x_dif, y = y_dif)
      } 
    }
    else {
      locs[[length(locs) + 1]] <- list(grupo = group, x = x_dif, y = y_dif, dif)
    }
  }
  
  locs_df <- do.call(rbind, lapply(locs, as.data.frame, stringsAsFactors = FALSE))
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo", "x", "y", "dif")
  return(locs_df)
}

# Procesar y combinar los datos de todos los archivos seleccionados
all_data <- lapply(paths, read_tobii_eye_data)
locs_df <- bind_rows(all_data)


# Convertir columnas a numéricas
locs_df <- locs_df %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(c(x, y), as.numeric))
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/tobii/exp1_final_tobii/wasso1.csv", row.names = FALSE)

# Graficar los datos
ggplot(locs_df, aes(x = x, y = y)) + #475819263
  geom_path(aes(group = 1), color = "blue", size = .3) +
  geom_point(aes(x = 35, y = 20.014), color = "black", shape = 3, size = 6) +
  geom_point(aes(x = 53.6, y = 20.014), color = "black", shape = 16, size = 6) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Coordenada X",
       y = "Coordenada Y") +
  scale_x_continuous(breaks = seq(0, 75, by = 10)) +
  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
  expand_limits(x = c(25, 70), y = c(10, 40))  

