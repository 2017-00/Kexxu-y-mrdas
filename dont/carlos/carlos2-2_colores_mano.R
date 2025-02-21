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
      if ((x < 800) && (y > 250)){
      #punto1
      if((i > 0) && (i < 141)){
        group <- "blue" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto2
      if((i > 140) && (i < 281)){
        group <- "black" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto3
      if((i > 280) && (i < 421)){
        group <- "yellow" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto4
      if((i > 420) && (i < 561)){
        group <- "orange" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto5
      if((i > 560) && (i < 701)){
        group <- "violet" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto6
      if((i > 700) && (i < 821)){
        group <- "magenta" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto7
      if((i > 820) && (i < 961)){
        group <- "grey" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto8
      if((i > 960) && (i < 1101)){
        group <- "purple" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
      #punto9/1020/1181
      if((i > 1100) && (i < 1181)){
        group <- "red" 
        locs[[length(locs) + 1]] <- c(x, y, timestamp, group)
      }
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
  geom_point(aes(x = 627, y = 720-284), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 540, y = 720-353), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 724, y = 720-346), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 527, y = 720-218), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 720, y = 720-208), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 629, y = 720-356), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 623, y = 720-209), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 534, y = 720-290), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = 723, y = 720-281), color = "black", shape = 8, size = 4) +
  geom_point(aes(color = group), alpha = 1) +
  # geom_density_2d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Densidad de la Posición de la Pupila",
       x = "Coordenada X",
       y = "Coordenada Y")
