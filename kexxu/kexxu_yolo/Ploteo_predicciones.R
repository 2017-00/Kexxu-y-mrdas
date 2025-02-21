
###El dataframe suj# son las predicciones que arroja Yolo
###El dataframe puntos# son las coordenadas que se obtuvieorn a mano del exp 2.2
###SUJETO 1
suj1 <- data.frame(
  x = c(646, 450, 537, 550, 644, 443, 649, 458, 561),
  y = c(720-323, 720-342, 720-253, 720-401, 720-241, 720-256, 720-387, 720-399, 720-335)
)
puntos1 <- data.frame(
  x = c(545, 461, 651, 441, 643, 551, 538, 448, 647),
  y = c(720-317, 720-399, 720-388, 720-256, 720-239, 720-399, 720-254, 720-341, 720-324)
)
###SUEJTO 2
suj2 <- data.frame(
  x = c(700, 488, 594, 596, 703, 486, 702, 489, 592),
  y = c(720 - 288, 720 - 302, 720 - 210, 720 - 368, 720 - 204, 720 - 205, 720 - 348, 720 - 357, 720 - 275)
)
puntos2 <- data.frame(
  x = c(592, 493, 704, 484, 706, 601, 594, 492, 704),
  y = c(720 - 273, 720 - 361, 720 - 346, 720 - 201, 720 - 196, 720 - 365, 720 - 208, 720 - 301, 720 - 287)
)
###SUJETO 3
suj3 <- data.frame(
  x = c(694, 558, 657, 653, 757, 556, 741, 543, 626),
  y = c(720 - 249, 720 - 245, 720 - 175, 720 - 317, 720 - 180, 720 - 191, 720 - 350, 720 - 361, 720 - 293)
)
puntos3 <- data.frame(
  x = c(544, 655, 757, 557, 623, 755, 539, 653, 736),
  y = c(720 - 196, 720 - 171, 720 - 180, 720 - 244, 720 - 300, 720 - 249, 720 - 367, 720 - 317, 720 - 347)
)
###SUJETO 4
suj4 <- data.frame(
  x = c(723, 487, 593, 600, 727, 467, 724, 485, 596),
  y = c(720 - 292, 720 - 302, 720 - 197, 720 - 383, 720 - 207, 720 - 210, 720 - 394, 720 - 397, 720 - 293)
)
puntos4 <- data.frame(
  x = c(603, 485, 728, 469, 731, 601, 592, 473, 728),
  y = c(720 - 313, 720 - 402, 720 - 398, 720 - 214, 720 - 203, 720 - 391, 720 - 196, 720 - 297, 720 - 295)
)
###SUJETO 5
suj5 <- data.frame(
  x = c(732, 494, 628, 629, 819, 549, 728, 519, 632),
  y = c(720 - 284, 720 - 304, 720 - 204, 720 - 368, 720 - 175, 720 - 209, 720 - 367, 720 - 377, 720 - 296)
)
puntos5 <- data.frame(
  x = c(569, 484, 688, 473, 682, 582, 571, 475, 680),
  y = c(720 - 268, 720 - 342, 720 - 318, 720 - 178, 720 - 168, 720 - 337, 720 - 183, 720 - 265, 720 - 253)
)
###PUNTOS DE REFERENCIA
resp <- data.frame(
  x = c(263.13, 960, 1656.87, 263.13, 960, 1656.87, 263.13, 960, 1656.87),
  y = c(1066.5, 1066.5, 1066.5, 540, 540, 540, 13.5, 13.5, 13.5)
)
#######DATAFRAMES DIFERENCIA POR PARTICIPANTE
dif_suj1 <- data.frame(
  x_diff = puntos1$x - suj1$x + resp$x,
  y_diff = puntos1$y - suj1$y + resp$y,
  x_est = abs(puntos1$x - suj1$x),
  y_est = abs(puntos1$y - suj1$y)
)
dif_suj2 <- data.frame(
  x_diff = puntos2$x - suj2$x + resp$x,
  y_diff = puntos2$y - suj2$y + resp$y,
  x_est = abs(puntos2$x - suj2$x),
  y_est = abs(puntos2$y - suj2$y)
)
dif_suj3 <- data.frame(
  x_diff = puntos3$x - suj3$x + resp$x,
  y_diff = puntos3$y - suj3$y + resp$y,
  x_est = abs(puntos3$x - suj3$x),
  y_est = abs(puntos3$y - suj3$y)
)
dif_suj4 <- data.frame(
  x_diff = puntos4$x - suj4$x + resp$x,
  y_diff = puntos4$y - suj4$y + resp$y,
  x_est = abs(puntos4$x - suj4$x),
  y_est = abs(puntos4$y - suj4$y)
)
dif_suj5 <- data.frame(
  x_diff = puntos5$x - suj5$x + resp$x,
  y_diff = puntos5$y - suj5$y + resp$y,
  x_est = abs(puntos5$x - suj5$x),
  y_est = abs(puntos5$y - suj5$y)
)

# Instala y carga los paquetes necesarios
# install.packages("ggplot2")
library(ggplot2)

# Combina los data frames de diferencias en uno solo, añadiendo una columna 'grupo' para identificar cada conjunto de datos
dif_suj1$grupo <- "Suj1"
dif_suj2$grupo <- "Suj2"
dif_suj3$grupo <- "Suj3"
dif_suj4$grupo <- "Suj4"
dif_suj5$grupo <- "Suj5"

#Promedios de diferencia de distancia
mean(dif_suj1$x_est)/26.9895
mean(dif_suj1$y_est)/26.9895
sqrt(mean(dif_suj1$x_est^2))/26.9895
sqrt(mean(dif_suj1$y_est^2))/26.9895

mean(dif_suj2$x_est)/26.9895
mean(dif_suj2$y_est)/26.9895
sqrt(mean(dif_suj2$x_est^2))/26.9895
sqrt(mean(dif_suj2$y_est^2))/26.9895

mean(dif_suj3$x_est)/26.9895
mean(dif_suj3$y_est)/26.9895
sqrt(mean(dif_suj3$x_est^2))/26.9895
sqrt(mean(dif_suj3$y_est^2))/26.9895

mean(dif_suj4$x_est)/26.9895
mean(dif_suj4$y_est)/26.9895
sqrt(mean(dif_suj4$x_est^2))/26.9895
sqrt(mean(dif_suj4$y_est^2))/26.9895

mean(dif_suj5$x_est)/26.9895
mean(dif_suj5$y_est)/26.9895
sqrt(mean(dif_suj5$x_est^2))/26.9895
sqrt(mean(dif_suj5$y_est^2))/26.9895

# Combina todos los data frames
dif_total <- rbind(
  dif_suj1,
  dif_suj2,
  dif_suj3,
  dif_suj4,
  dif_suj5
)

dif_total_df <- data.frame(
  rbind(
    dif_suj1,
    dif_suj2,
    dif_suj3,
    dif_suj4,
    dif_suj5
  )
)

#Estadisticas totales
mean(dif_total_df$x_est)/26.9895
mean(dif_total_df$y_est)/26.9895
sqrt(mean(dif_total_df$x_est^2))/26.9895
sqrt(mean(dif_total_df$y_est^2))/26.9895

# Crea el gráfico
ggplot() +
  # Puntos de referencia en negro
  geom_point(aes(x = 263.13, y = 1066.5), color = "black", size = 2) +
  geom_point(aes(x = 960, y = 1066.5), color = "black", size = 2) +
  geom_point(aes(x = 1656.87, y = 1066.5), color = "black", size = 2) +
  geom_point(aes(x = 263.13, y = 540), color = "black", size = 2) +
  geom_point(aes(x = 960, y = 540), color = "black", size = 2) +
  geom_point(aes(x = 1656.87, y = 540), color = "black", size = 2) +
  geom_point(aes(x = 263.13, y = 13.5), color = "black", size = 2) +
  geom_point(aes(x = 960, y = 13.5), color = "black", size = 2) +
  geom_point(aes(x = 1656.87, y = 13.5), color = "black", size = 2) +
  
  # Agrega los puntos de los data frames de diferencias con transparencia
  geom_point(data = dif_total, aes(x = x_diff, y = y_diff, color = grupo), alpha = 0.5, size = 2) +
  
  # Personalización del gráfico
  theme_minimal() +
  labs(
    x = "Coordenada X",
    y = "Coordenada Y"
  ) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())+
  expand_limits(x = c(0, 2000), y = c(0, 1200))  

