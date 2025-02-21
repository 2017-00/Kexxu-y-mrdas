library(jsonlite)
library(ggplot2)
library(dplyr)2

path <- file.choose()
data <- fromJSON(path)

predicciones <- do.call(rbind, lapply(data$YOLOv5, function(x) {
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

mean <- predicciones %>%
  group_by(class) %>%
  summarise(
    x_center_promedio = mean(x_center, na.rm = TRUE),
    y_center_promedio = mean(y_center, na.rm = TRUE)
  )
options(digits = 10)
print(mean)

# Crear el data frame con los datos de coordenadas
data <- data.frame(
  x_center_promedio = c(646, 450, 537, 550, 644, 443, 649, 458, 561),
  y_center_promedio = c(720-323, 720-342, 720-253, 720-401, 720-241, 720-256, 720-387, 720-399, 720-335)
)

# Crear el gráfico
ggplot(data, aes(x = x_center_promedio, y = y_center_promedio)) +
  geom_point(color = "black", shape = 8, size = 4) +   # Puntos de color negro
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Densidad de la Posición de la Pupila",
    x = "Coordenada X",
    y = "Coordenada Y"
  )

# Datos para el sujeto 1 (LIZ)
suj1 <- data.frame(
  x = c(646, 450, 537, 550, 644, 443, 649, 458, 561),
  y = c(720 - 323, 720 - 342, 720 - 253, 720 - 401, 720 - 241, 720 - 256, 720 - 387, 720 - 399, 720 - 335)
)

# Datos para el sujeto 2 (CAT)
suj2 <- data.frame(
  x = c(700, 488, 594, 596, 703, 486, 702, 489, 592),
  y = c(720 - 288, 720 - 302, 720 - 210, 720 - 368, 720 - 204, 720 - 205, 720 - 348, 720 - 357, 720 - 275)
)

# Datos para el sujeto 3 (EMA)
suj3 <- data.frame(
  x = c(694, 558, 657, 653, 757, 556, 741, 543, 626),
  y = c(720 - 249, 720 - 245, 720 - 175, 720 - 317, 720 - 180, 720 - 191, 720 - 350, 720 - 361, 720 - 293)
)

# Datos para el sujeto 4 (GAMBOA)
suj4 <- data.frame(
  x = c(723, 487, 593, 600, 727, 467, 724, 485, 596),
  y = c(720 - 292, 720 - 302, 720 - 197, 720 - 383, 720 - 207, 720 - 210, 720 - 394, 720 - 397, 720 - 293)
)

# Datos para el sujeto 5 (GUASSO)
suj5 <- data.frame(
  x = c(732, 494, 628, 629, 819, 549, 728, 519, 632),
  y = c(720 - 284, 720 - 304, 720 - 204, 720 - 368, 720 - 175, 720 - 209, 720 - 367, 720 - 377, 720 - 296)
)

# Combinar todos los sujetos en un solo data frame
data_combined <- rbind(
  data.frame(suj1, subject = "SUJ1"),
  data.frame(suj2, subject = "SUJ2"),
  data.frame(suj3, subject = "SUJ2"),
  data.frame(suj4, subject = "SUJ3"),
  data.frame(suj5, subject = "SUJ4")
)

# Crear el gráfico
ggplot(data_combined, aes(x = x, y = y, color = subject)) +
  geom_point(size = 3) +                               # Puntos para cada sujeto
  theme_minimal() +                                   # Tema minimalista
  labs(
    title = "Posición de la Pupila por Sujeto",
    x = "Coordenada X",
    y = "Coordenada Y",
    color = "Sujeto"
  ) +
  scale_color_manual(values = c("SUJ1" = "red", "SUJ2" = "blue", "SUJ3" = "green", "GSUJ4" = "purple", "SUJ5" = "orange")) +  # Colores personalizados
  theme(legend.position = "right")                    # Ubicación de la leyenda






