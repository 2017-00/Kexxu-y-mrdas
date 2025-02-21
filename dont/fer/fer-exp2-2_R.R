library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
options(scipen=999)

path <- file.choose()

read_kexxu_eye_data <- function(path) {
  # Instalar y cargar jsonlite si aún no está instalado/cargado
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
    library(jsonlite)
  }
  
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  
  # Inicializar una lista para almacenar las coordenadas
  locs <- list()
  
  # Iterar sobre cada línea
  for (i in seq_along(lines))
  {
 # for (line in lines)
    
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]] # Dividir la línea actual (line) en sus componentes, usando el espacio (" ") como delimitador.
    if (grepl("/eyetracking$", parts[1])) { # Verificar si el primer elemento de parts termina con la cadena "/eyetracking", la función grepl devuelve TRUE si lo encuentra
      # Convertir la cadena JSON en una lista
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- js$pupil_rel_pos_x
      ry <- js$pupil_rel_pos_y
      x <- 640 - as.integer((rx) * 640) 
      y <- 720 - (360 -  as.integer((ry) * 360))
      # Extraer el timestamp
      timestamp <- as.numeric(js$timestamp_ms)
      #    if((y < 300)&&(x < 800)) { # (x > 300) && 
   #   if((i<100)){
    #    locs[[length(locs) + 1]] <- c(x, y, timestamp) # Cada conjunto de datos x, y, timestamp se agrega al final de la lista
     # }
      if((i>260)&&(i<400)){
        locs[[length(locs) + 1]] <- c(x, y, timestamp) # Cada conjunto de datos x, y, timestamp se agrega al final de la lista
      }
      
      #   }
    }
  }
  
  
  # Convertir la lista en un dataframe para facilitar su manejo
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("x", "y", "timestamp_ms")
  
  
  return(locs_df)
}



# Se guarda la información en la variable locs_df
locs_df <- read_kexxu_eye_data(path)

# Se convierte en data.frame
locs_df <- as.data.frame(locs_df)

# Convertir las columnas x y y a numérico
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
######################################

# Densidad de puntos CHIDAAAA
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 569, y = 268), color = "green", size = 11.88) +
  geom_point(aes(x = 484, y = 342), color = "green", size = 11.88) +
  geom_point(aes(x = 688, y = 318), color = "green", size = 11.88) +
  geom_point(aes(x = 473, y = 178), color = "green", size = 11.88) +
  geom_point(aes(x = 682, y = 168), color = "green", size = 11.88) +
  geom_point(aes(x = 582, y = 337), color = "green", size = 11.88) +
  geom_point(aes(x = 571, y = 183), color = "green", size = 11.88) +
  geom_point(aes(x = 475, y = 265), color = "green", size = 11.88) +
  geom_point(aes(x = 680, y = 253), color = "green", size = 11.88) +
  geom_point(alpha = 0.5) +  # Puntos semitransparentes
  geom_density_2d() +  # Añade contornos de densidad
  theme_minimal() +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y") 

ggplot(locs_df, aes(x = x, y = y)) +
  #  geom_point(aes(x = 585, y = 233), color = "green", size = 11.88) +
  #  geom_point(aes(x = 709, y = 233), color = "green", size = 11.88) +
  geom_path(alpha = 0.7, color = "blue") +  # Puntos semitransparentes
  theme_minimal() +
  labs(title = "Trayectorias",
       x = "Coordenada X",
       y = "Coordenada Y")
