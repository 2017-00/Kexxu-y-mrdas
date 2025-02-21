library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(tcltk)

options(scipen=999)

paths <- tk_choose.files()

read_tobii_eye_data <- function(path) {
  lines <- read.csv(path, header = TRUE, sep = ",")
  
  locs <- list()
  
  for (i in 1:nrow(lines)) {
    line <- lines[i, ]
    x_ref <- line$p_x
    y_ref <- line$p_y
    group <- line$group
    dif_t <- line$distance
    dif_x <- line$dif_x
    dif_y <- line$dif_y
    
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
    #group <- "blue"
    
    locs[[length(locs) + 1]] <- c(group,x_ref, y_ref, x_dif, y_dif, dif_t, dif_x, dif_y)
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("grupo","x_r", "y_r", "x", "y", "dif", "dif_x", "dif_y")
  
  return(as.data.frame(locs_df))
}

all_data <- lapply(paths, read_tobii_eye_data)
locs_df <- bind_rows(all_data)

# Convertir columnas a numéricas
locs_df$x <- as.numeric(locs_df$x)
locs_df$y <- as.numeric(locs_df$y)
locs_df$dif <- as.numeric(locs_df$dif)
locs_df$dif_x <- as.numeric(locs_df$dif_x)
locs_df$dif_y <- as.numeric(locs_df$dif_y)
locs_df$x_r <- as.numeric(locs_df$x_r)
locs_df$y_r <- as.numeric(locs_df$y_r)
write.csv(locs_df, file = "/home/luis-ignacio-zamora/Documents/mudanza/Documentos/Eyetracker/PlotsProba/Tobii/exp2-2/exp2-2_tobii.csv", row.names = FALSE)

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
  geom_point(aes(color = grupo), alpha = 1, size = .5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")

summary(locs_df)
#####PARA ESTADISTICA
hist(locs_df$dif,
     breaks = 100,
     main = "Histograma de diferencias de distancias",
     xlab = "Valores",
     ylab = "Frecuencia")

plot(locs_df$dif_x,
     type = "p",
     col = locs_df$grupo,
     main = "Diferencias de Distancias en eje X",
     xlab = "Índice",  
     ylab = "Valores de diferencia",  
     pch = 16,
     cex = 0.7)
modelo_regresion <- lm(locs_df$dif_x ~ seq_along(locs_df$dif_x))
abline(modelo_regresion, col = "red", lwd = 2)

plot(locs_df$dif_y,
     type = "p",
     col = locs_df$grupo,
     main = "Diferencias de Distancias en eje Y",
     xlab = "Índice",  
     ylab = "Valores de diferencia",  
     pch = 16,
     cex = 0.7) 
modelo_regresion <- lm(locs_df$dif_y ~ seq_along(locs_df$dif_y))
abline(modelo_regresion, col = "red", lwd = 2)

plot(locs_df$dif,
     type = "p",
     col = locs_df$grupo,
     main = "Diferencias de Distancias",
     xlab = "Índice",  
     ylab = "Valores de diferencia",  
     pch = 16,
     cex = 0.7) 
modelo_regresion <- lm(locs_df$dif ~ seq_along(locs_df$dif))
abline(modelo_regresion, col = "red", lwd = 2)


varianza <- var(locs_df$dif)
varianza_x <- var(locs_df$dif_x)
varianza_y <- var(locs_df$dif_y)

des_est <- sqrt(varianza)
des_est_x <- sqrt(varianza_x)
des_est_Y <- sqrt(varianza_y)

correlacion_Spe_x <- cor(locs_df$x, locs_df$x_r, method = "spearman")
correlacion_Pear_x <- cor(locs_df$x, locs_df$x_r)

correlacion_Spe_y <- cor(locs_df$y, locs_df$x_r, method = "spearman")
correlacion_Pear_y <- cor(locs_df$y, locs_df$x_r)








plot(locs_df$x, locs_df$x_r,
     main = "Gráfica de Dispersión (X)",
     xlab = "Eyetracker X",
     ylab = "Coordenada Real X",
     pch = 16,  
     col = "blue") 
#abline(lm(real_coords$x ~ tracker_coords$x), col = "red", lwd = 2)


