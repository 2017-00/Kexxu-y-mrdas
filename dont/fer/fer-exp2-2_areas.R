library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
options(scipen=999)

path <- file.choose()

read_kexxu_eye_data <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
    library(jsonlite)
  }
  
  lines <- readLines(path)
  locs <- list()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    parts <- strsplit(line, " ")[[1]]
    if (grepl("/eyetracking$", parts[1])) {
      js <- jsonlite::fromJSON(paste(parts[-c(1:2)], collapse = " "))
      rx <- js$pupil_rel_pos_x
      ry <- js$pupil_rel_pos_y
      x <- 640 - as.integer((rx) * 640)
      y <- 360 -  as.integer((ry) * 360)
      timestamp <- as.numeric(js$timestamp_ms)
      #punto1
      if((i > 0) && (i < 121)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto2
      if((i > 120) && (i < 261)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto3
      if((i > 260) && (i < 381)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto4
      if((i > 380) && (i < 501)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto5
      if((i > 500) && (i < 621)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto6
      if((i > 620) && (i < 761)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto7
      if((i > 760) && (i < 881)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto8
      if((i > 880) && (i < 1021)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto9/1020/1181
      if((i > 1020) && (i < 1181)){
        group <- "red" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
    }
  }
  
  locs_df <- do.call(rbind, locs)
  colnames(locs_df) <- c("x", "y", "timestamp_ms", "group")
  
  return(as.data.frame(locs_df))
}

locs_df <- read_kexxu_eye_data(path)
locs_df$x <- as.numeric(as.character(locs_df$x))
locs_df$y <- as.numeric(as.character(locs_df$y))
locs_df <- locs_df %>% filter(!is.na(x) & !is.na(y))

######################################
ggplot(locs_df, aes(x = x, y = y)) +
  geom_point(aes(x = 569, y = 720-268), color = "green", size = 11.88) +
  geom_point(aes(x = 484, y = 720-342), color = "green", size = 11.88) +
  geom_point(aes(x = 688, y = 720-318), color = "green", size = 11.88) +
  geom_point(aes(x = 473, y = 720-178), color = "green", size = 11.88) +
  geom_point(aes(x = 682, y = 720-168), color = "green", size = 11.88) +
  geom_point(aes(x = 582, y = 720-337), color = "green", size = 11.88) +
  geom_point(aes(x = 571, y = 720-183), color = "green", size = 11.88) +
  geom_point(aes(x = 475, y = 720-265), color = "green", size = 11.88) +
  geom_point(aes(x = 680, y = 720-253), color = "green", size = 11.88) +
  geom_point(aes(color = group), alpha = 1) +
 # geom_density_2d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
    


