# Seleccionar el archivo
path <- file.choose()
# Función para leer y modificar datos del eye tracker, eliminando líneas no deseadas
clean_tobii_eye_data <- function(path) {
  # Leer todas las líneas del archivo
  lines <- readLines(path)
  
  # Inicializar un vector para almacenar las líneas válidas
  valid_lines <- c()
  
  distancia <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  # Iterar sobre cada línea
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Intentar convertir la línea JSON en un objeto list
    js <- tryCatch(jsonlite::fromJSON(line), error = function(e) {
      message("Error al analizar JSON en la línea ", i, ": ", e)
      return(NULL)
    })
    
    # Si el objeto JSON es NULL (error al analizar), pasar a la siguiente línea
    if (is.null(js)) {
      next
    }
    
    # Comprobar si el tipo es "gaze" y si hay datos de pupilas
    if (js$type == "gaze" && !is.null(js$data$eyeright$pupildiameter)) {
      # Extraer coordenadas de origen de la mirada del ojo derecho
      rx <- js$data$gaze2d[1]
      ry <- js$data$gaze2d[2]
      
      # Convertir las coordenadas a valores enteros
      x <- as.integer((rx) * 1920)
      y <- 1080 - as.integer((ry) * 1080)
      
      # Si las coordenadas cumplen con el criterio, guardar la línea
      if ((x > 700)&&(y<1000)) {
        valid_lines <- c(valid_lines, line)
      }
    }
  }
  
  # Sobrescribir el archivo original con las líneas filtradas
  writeLines(valid_lines, path)
  
  message("Archivo limpiado y sobrescrito con las líneas válidas.")
}

# Ejecución de la función con el archivo seleccionado
clean_tobii_eye_data(path)

