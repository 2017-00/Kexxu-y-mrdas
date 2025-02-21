# Cargar las librerías necesarias
library(circular)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(tcltk)

options(scipen=999)

# Seleccionar múltiples archivos
paths <- tk_choose.files()

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
    angulo <- atan2(line$dif_y, line$dif_x) * (180 / pi)
    if (angulo < 0) {
      angulo <- angulo + 360
    }
    
    #group <- "blue"
    
    locs[[length(locs) + 1]] <- c(group, x_dif, y_dif, dif, line$dif_x, line$dif_y, angulo)
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo", "x", "y", "dif_euc", "dif_x", "dif_y", "ang")
  
  return(as.data.frame(locs_df))
}

all_data <- lapply(paths, read_tobii_eye_data)
locs_df <- bind_rows(all_data)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(locs_df$x)
locs_df$y <- as.numeric(locs_df$y)
locs_df$dif_euc <- as.numeric(locs_df$dif_euc)
locs_df$dif_x <- as.numeric(locs_df$dif_x)
locs_df$dif_y <- as.numeric(locs_df$dif_y)
locs_df$ang <- as.numeric(locs_df$ang)
write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/exp2-1_final/csv/final2-1_tobii.csv", row.names = FALSE)

Var_Ang <- var(locs_df$ang)
Des_Est <- sqrt(Var_Ang)

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

hist(locs_df$ang,
     breaks = 100,
     main = "Histograma de diferencias de distancias",
     xlab = "Valores",
     ylab = "Frecuencia")

# Crear listas para almacenar los ángulos por color###########
angulos_magenta <- list()
angulos_pink <- list()
angulos_orange <- list()
angulos_blue <- list()
angulos_red <- list()
angulos_purple <- list()
angulos_yellow <- list()
angulos_green <- list()
angulos_brown <- list()

# Filtrar y asignar ángulos para cada grupo de color
for (i in 1:nrow(locs_df)) {
  line <- locs_df[i, ]
  
  if (line$grupo == "magenta") {
    angulos_magenta <- c(angulos_magenta, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "pink") {
    angulos_pink <- c(angulos_pink, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "orange") {
    angulos_orange <- c(angulos_orange, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "blue") {
    angulos_blue <- c(angulos_blue, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "red") {
    angulos_red <- c(angulos_red, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "purple") {
    angulos_purple <- c(angulos_purple, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "yellow") {
    angulos_yellow <- c(angulos_yellow, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "green") {
    angulos_green <- c(angulos_green, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
  if (line$grupo == "brown") {
    angulos_brown <- c(angulos_brown, circular(line$ang, units = "degrees", modulo = "2pi"))
  }
}

angulos_magenta <- circular(unlist(angulos_magenta), units = "degrees", modulo = "2pi")
angulos_pink <- circular(unlist(angulos_pink), units = "degrees", modulo = "2pi")
angulos_orange <- circular(unlist(angulos_orange), units = "degrees", modulo = "2pi")
angulos_blue <- circular(unlist(angulos_blue), units = "degrees", modulo = "2pi")
angulos_red <- circular(unlist(angulos_red), units = "degrees", modulo = "2pi")
angulos_purple <- circular(unlist(angulos_purple), units = "degrees", modulo = "2pi")
angulos_yellow <- circular(unlist(angulos_yellow), units = "degrees", modulo = "2pi")
angulos_green <- circular(unlist(angulos_green), units = "degrees", modulo = "2pi")
angulos_brown <- circular(unlist(angulos_brown), units = "degrees", modulo = "2pi")

par(mfrow = c(1, 3))
plot(angulos_blue, stack = TRUE, bins = 100, col = "blue", ylim = c(-4, 4))
plot(angulos_yellow, stack = TRUE, bins = 100, col = "yellow", ylim = c(-4, 4))
plot(angulos_red, stack = TRUE, bins = 100, col = "red", ylim = c(-4, 4))

plot(angulos_green, stack = TRUE, bins = 100, col = "green", ylim = c(-4, 4))
plot(angulos_magenta, stack = TRUE, bins = 100, col = "magenta", ylim = c(-4, 4))
plot(angulos_brown, stack = TRUE, bins = 100, col = "brown", ylim = c(-4, 4))


plot(angulos_pink, stack = TRUE, bins = 100, col = "pink", ylim = c(-4, 4))
plot(angulos_purple, stack = TRUE, bins = 100, col = "purple", ylim = c(-4, 4))
plot(angulos_orange, stack = TRUE, bins = 100, col = "orange", ylim = c(-4, 4))

par(mfrow = c(1, 1))


