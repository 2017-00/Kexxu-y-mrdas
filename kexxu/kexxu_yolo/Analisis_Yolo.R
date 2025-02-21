library(jsonlite)
library(ggplot2)
library(dplyr)
###En este còdigo primero se inserta el js del analisis con yolo y despues el crudo del mismo exp
############## Leer datos YOLO
path <- file.choose()
data_yolo <- fromJSON(path)

# Procesar predicciones de YOLO
predicciones <- do.call(rbind, lapply(data_yolo$YOLOv5, function(x) {
  x <- unlist(x)
  data.frame(
    x_min = as.numeric(x[1]),
    y_min = as.numeric(x[2]),
    x_max = as.numeric(x[3]),
    y_max = as.numeric(x[4]),
    confidence = as.numeric(x[5]),
    class = as.numeric(x[6]),
    x_center = (as.numeric(x[1]) + as.numeric(x[3])) / 2,
    y_center = (as.numeric(x[2]) + as.numeric(x[4])) / 2
  )
}))

# Calcular promedios de coordenadas para cada clase
clases <- c(0:8)
mean <- predicciones %>%
  filter(class %in% clases) %>%
  group_by(class) %>%
  summarise(
    x_center_promedio = mean(x_center, na.rm = TRUE),
    y_center_promedio = 720 - mean(y_center, na.rm = TRUE)
  )
options(digits = 10)

############## Función para leer datos de eyetracking
read_kexxu_yolo_data <- function(path) {
  lines <- readLines(path)
  locs <- list()
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]] 
    
    if (grepl("/eyetracking$", parts[1])) {
      js <- tryCatch(
        jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " ")),
        error = function(e) NULL
      )
      if (!is.null(js)) {
        rx <- js$pupil_rel_pos_x
        ry <- js$pupil_rel_pos_y
        x <- 640 - as.integer(rx * 640)
        y <- 360 - as.integer(ry * 360)
        
        distancias <- apply(mean, 1, function(p) distancia(p[2], p[3], x, y))
        group <- which.min(distancias)
        group_colors <- c("blue", "yellow", "red", "green", "magenta", "brown", "pink", "purple", "orange")

        if (!is.na(group) && length(distancias) > 0 && (x < 800) && (y > 250)) {
          dif_x <- (x - mean$x_center_promedio[group]) * (1920 / 419)
          dif_y <- (y - mean$y_center_promedio[group]) * (1080 / 233)
          locs[[length(locs) + 1]] <- c(
            x = x,
            y = y,
            group = group_colors[group],
            distance = min(distancias),
            dif_x = dif_x,
            dif_y = dif_y,
            p_x = mean$x_center_promedio[group],
            p_y = mean$y_center_promedio[group]
          )
        }
      }
    }
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("x", "y", "group", "distance", "dif_x", "dif_y", "p_x", "p_y")
  return(as.data.frame(locs_df))
}

############### Leer datos del mismo experimento, pero en crudo
path <- file.choose()
locs_df <- read_kexxu_yolo_data(path)

# Convertir coordenadas a numéricas y filtrar datos inválidos
locs_df <- locs_df %>%
  mutate(
    x = as.numeric(as.character(x)),
    y = as.numeric(as.character(y))
  ) %>%
  filter(!is.na(x) & !is.na(y))

# Visualización
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(color = group), alpha = 0.7) +
  geom_point(data = mean, aes(x = x_center_promedio, y = y_center_promedio), color = "black", shape = 8, size = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "YOLO: Densidad de la Posición de la Pupila",
    x = "Coordenada X",
    y = "Coordenada Y"
  )

#Predicciones finales que nos interesan
mean
