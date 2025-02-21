#Pos las librerias :D
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
options(scipen=999)
#Seleccionamos el archivo json que se va a analizar 
path <- file.choose()

read_kexxu_eye_data <- function(path) {
  #Se leen las lineas de todo el json y se inicializa una lista para guardar lo que necesitamos
  lines <- readLines(path)
  locs <- list()
  
  #Se introducen las coordenadas de los puntos del experimento (sacados a manita)
  puntos <- data.frame(
    x = c(569, 484, 688, 473, 682, 582, 571, 475, 680),
    y = c(720-268, 720-342, 720-318, 720-178, 720-168, 720-337, 720-183, 720-265, 720-253)
  )
  #Funcion para calcular la distancia euclidiana, mas adelante se implementa
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  #Se analizan de una en una las lineas que se leyeron (lines)
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]] #Se separa en partes por cada espacio vacio " "
    if (grepl("/eyetracking$", parts[1])) {
      #Se excluyen las primeras dos partes de parts y une lo restante separados por " "
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      #pupil_rel_pos_x esta contenido en la variable js, lo extraemos y lo guardamos
      #en una variable independiente. Lo mismo para las coords de y
      rx <- js$pupil_rel_pos_x 
      ry <- js$pupil_rel_pos_y
      #Se hacen conversiones para transformar las coords que nos da el json de entre 0 y 1
      #para que estèn en rangos de la resolucion de Kexxu (1080x720)
      x <- 640 - as.integer((rx) * 640)
      y <- 360 -  as.integer((ry) * 360)
      timestamp <- as.numeric(js$timestamp_ms)
      
      #Se calcula la distancia euclidiana del la coord i que se esta analizando
      #con respecto a los nueve puntos del experimento que guardamos antes en el df puntos
      distancias <- apply(puntos, 1, function(p) distancia(p[1], p[2], x, y))
      dif_x <- (x - puntos$x)*(1920/419) ##Se divide entre esto porque sacamos a mano tambien la escala a la que 
      dif_y <- (y - puntos$y)*(1080/233) ## corresponde la pantalla de la pc del experimento
      p_x <- puntos$x
      p_y <- puntos$y
      
      #Determina el indice del grupo al que pertenece con la mínima distancia
      group <- which.min(distancias)
      #Cambiamos el indice numerico a un color, 1 = blue, 2 = yellow .... :)
      group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")
      #Guardamos todas las variables que nos interesan en la lista que habiamos inicializamos
      locs[[length(locs) + 1]] <- c(x, y, group_colors[group], min(distancias), dif_x[group], dif_y[group], p_x[group], p_y[group])
    }
  }
  #se combinan todas las filas de la lista locs
  locs_df <- do.call(rbind, locs)
  #Agregamos nombre a cada columna
  colnames(locs_df) <- c("x", "y", "group", "distance", "dif_x", "dif_y", "p_x", "p_y")
  #Se devuelve locs_df como un df
  return(as.data.frame(locs_df))
}

#Aplicamos toda la funcion anterior
locs_df <- read_kexxu_eye_data(path)
#Se convierten los valores a numericos
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
#Se filtra para sacar coords que no sean validas
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))
#Guardamos los resultados en la direccion que queramos
#write.csv(locs_df, file = "/home/luiszamora/Documentos/Eyetracker/kexxu/wasso/exp2-2/wasso2-2.csv", row.names = FALSE)


# Graficar
ggplot(locs_df, aes(x = x, y = y)) +
  #Se establecen los puntos que extrajimos a manita
  geom_point(aes(x = 569, y = 720-268), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 484, y = 720-342), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 688, y = 720-318), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 473, y = 720-178), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 682, y = 720-168), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 582, y = 720-337), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 571, y = 720-183), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 475, y = 720-265), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 680, y = 720-253), color = "black", shape = 8, size = 4) +
  #Se plotean todos los puntos de locs_df por colores de grupo
  geom_point(aes(color = group), alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
