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
read_exp2_1_eye_data <- function(path) {
  lines <- read.csv(path, header = TRUE, sep = ",")
  
  locs <- list()
  
  for (i in 1:nrow(lines)) {
    line <- lines[i, ]
    x_ref <- line$x
    y_ref <- line$y
    group <- line$grupo
    
    locs[[length(locs) + 1]] <- c(group, x_ref, y_ref)
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo", "x", "y")
  
  return(as.data.frame(locs_df))
}

# Procesar y combinar los datos de todos los archivos seleccionados
all_data <- lapply(paths, read_exp2_1_eye_data)
locs_df <- bind_rows(all_data)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(locs_df$x)
locs_df$y <- as.numeric(locs_df$y)

# Graficar los datos
ggplot(locs_df, aes(x = x, y = y)) + #475819263
  geom_point(aes(color = grupo), alpha = .5, size = .5) +
  geom_point(aes(x = 611.565, y = 888.435), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 960, y = 888.435), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 1308.435, y = 888.435), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 611.565, y = 540), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 960, y = 540), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 1308.435, y = 540), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 611.565, y = 191.565), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 960, y = 191.565), color = "black", alpha = 0.75, size = 1.5) +
  geom_point(aes(x = 1308.435, y = 191.565), color = "black", alpha = 0.75, size = 1.5) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Display x coordinate (pixels)",
       y = "Display y coordinate (pixels)")+
  scale_x_continuous(breaks = seq(0, 2000, by = 200)) +
  scale_y_continuous(breaks = seq(-300, 1200, by = 150)) +
  expand_limits(x = c(0, 1900))  
