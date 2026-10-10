library(tibble)
library(dplyr)
library(ggplot2)
library(stats)
library(patchwork)
#------------------Grafica de la serie-----------------------------#

graficar_serie <- function(datos, titulo) {
  
  #Graficos la series, en esta funcion recibe dos parametros datos y titulo, donde 
  #datos viene con dos tributos fuente y observacion que se brindan en la funcion de leer_seri
  
  
  # Sacamos la informacion de fuente y unidad
  fuente <- attr(datos, "fuente")
  unidad <- attr(datos, "unidad")
  
  # Observaciones
  n <- nrow(datos)
  
  # Creamos el grafico 
  ggplot(datos, aes(x = fecha, y = y)) +
    geom_line() +
    
    labs(
      title = titulo,
      x = "Fecha",
      y = paste(unidad, sep = ""),
      caption = paste(
        "Fuente:", fuente,
        "| Observaciones:", n
      )
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(
        face = "bold"
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      plot.caption = element_text(
        hjust = 0
      )
    )
}

#--------------------Grafico correlograma-----------------------------#

correlograma <- function(datos,m = NULL){
  
  # Extraemos la serie numérica
  datos <- datos$y
  
  
  T <- length(datos) #Numero de observaciones
  
  #Valida si m es null o diferente 
  if(is.null(m)){
    m <- min(floor(T/4),24)
  }
  
  #-----------Funcion ACF Manual------------------#
  ACF <- function(datos, h) {
    
    T <- length(datos)
    promedio <- sum(datos) / T
    denominador <- sum((datos - promedio)^2)
    
    producto <- c()
    for (i in 1:T) {
      resultado <- datos[i] - promedio
      producto <- c(producto, resultado)
    }
    
    valor <- c()
    for (i in 1:(T - h)) {
      resultado <- producto[i + h] * producto[i]
      valor <- c(valor, resultado)
    }
    
    acf <- sum(valor) / denominador
    
    return(acf)
  }
  
  # Calculamos la ACF manual desde 1 hasta m
  acf_manual <- c()
  
  for (h in 1:m) {
    resultado <- ACF(datos, h)
    acf_manual <- c(acf_manual, resultado)
  }
  
  # ACF calculada por R
  acf_r <- acf(datos, lag.max = m, plot = FALSE)$acf[-1]
  
  # Diferencias absolutas entre la ACF manual y la de R
  diferencias <- abs(acf_manual - acf_r)
  
  # Maxima diferencia
  diferencia_maxima <- max(diferencias)
  
  #-----------Calculamos la PACF con la funcion de R---------------#
  pacf_r <- pacf(datos, lag.max = m, plot = FALSE)$acf
  
  # Creamos los datos para graficar la ACF
  datos_acf <- data.frame(
    rezago = 1:m,
    valor = acf_manual
  )
  
  # Creamos los datos para graficar la PACF
  datos_pacf <- data.frame(
    rezago = 1:m,
    valor = as.numeric(pacf_r)
  )
  
  # Calculamos la banda de confianza del 95 %
  banda <- qnorm((1 + 0.95) / 2) / sqrt(T)
  
  # Funcion para crear los graficos ACF y PACF
  graficar_correlacion <- function(datos, titulo, etiqueta_y) {
    
    ggplot(datos, aes(x = rezago, y = valor)) +
      
      # Barras verticales
      geom_segment(
        aes(xend = rezago, yend = 0)
      ) +
      
      # Linea central
      geom_hline(yintercept = 0) +
      
      # Banda superior
      geom_hline(
        yintercept = banda,
        linetype = "dashed"
      ) +
      
      # Banda inferior
      geom_hline(
        yintercept = -banda,
        linetype = "dashed"
      ) +
      
      labs(
        title = titulo,
        x = "Rezago",
        y = etiqueta_y
      ) +
      
      theme_minimal()
  }
  
  # Grafico de la ACF
  grafico_acf <- graficar_correlacion(
    datos_acf,
    "Función de Autocorrelación (ACF)",
    "ACF"
  )
  
  # Grafico de la PACF
  grafico_pacf <- graficar_correlacion(
    datos_pacf,
    "Función de Autocorrelación Parcial (PACF)",
    "PACF"
  )
  
  # Unimos los graficos
  panel <- grafico_acf / grafico_pacf
  
  return(list(acf =acf_manual, panel = panel,diferencia_maxima= diferencia_maxima))
  
}

#----------------- Grafico de rezagos -----------------------------#
grafico_rezagos <- function(y, k, unidad = "unidad", nombre = "serie") {
  
  # Ver varios graficos 
  par(mfrow = c(1, length(k)),
      mar = c(4, 2, 2, 1),
      oma = c(4, 4, 4, 1))
  
  # Repetir el procedimiento para cada rezago
  for (i in k) {
    
    # Número de observaciones
    n <- length(y)
    
    # Construir los vectores pareados
    y_t <- y[(i + 1):n]
    y_lag <- y[1:(n - i)]
    
    # Calcular las medias
    media_y <- mean(y_t)
    media_lag <- mean(y_lag)
    
    # Calcular la pendiente
    b <- sum((y_lag - media_lag) * (y_t - media_y)) /
      sum((y_lag - media_lag)^2)
    
    # Calcular el intercepto
    a <- media_y - b * media_lag
    
    # Graficar
    plot(
      y_lag, y_t,
      main = paste("k =", i),
      xlab = "",
      ylab = "",
      pch = 16
    )
    
    # Construir los valores de la recta
    x_recta <- c(min(y_lag), max(y_lag))
    y_recta <- a + b * x_recta
    
    # Graficar la recta 
    lines(x_recta, y_recta, lwd = 2)
  }
  
  # Titulo
  mtext(
    nombre,
    side = 3,
    outer = TRUE,
    line = 1,
    cex = 1.2,
    font = 2
  )
  
  # Eje X 
  mtext(
    paste("y[t-k] (", unidad, ")", sep = ""),
    side = 1,
    outer = TRUE,
    line = 2,
    cex = 1
  )
  
  # Eje Y 
  mtext(
    paste("y[t] (", unidad, ")", sep = ""),
    side = 2,
    outer = TRUE,
    line = 2,
    cex = 1
  )
  
  # Volver a la configuración normal
  par(mfrow = c(1, 1))
}
#---------------------Grafico Estacional------------------------------#

grafico_estacional <- function(y, s, unidad, titulo, nombre_estacion = "Mes (1 = enero)") {
  t <- seq_along(y)
  d <- data.frame(
    ciclo    = factor(((t - 1) %/% s) + 1),   # año 1, 2, ..., 14
    estacion = ((t - 1) %% s) + 1,            # mes 1..12
    y        = y
  )
  ggplot2::ggplot(d, ggplot2::aes(x = estacion, y = y,
                                  group = ciclo, colour = ciclo)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = 1:s) +
    ggplot2::labs(title = titulo, x = nombre_estacion, y = unidad, colour = "Año",
                  caption = paste("Observaciones:", length(y))) +
    ggplot2::theme_minimal()
}


#---------------------Grafico de descomposicion------------------------#
grafico_descomposicion <- function(dec, y, unidad, nombre_estacion = "Mes (1 = enero)") {
  t <- seq_along(y)
  s <- length(dec$indices)
  
  p1 <- ggplot2::ggplot(data.frame(t = t, y = y, tend = dec$tendencia)) +
    ggplot2::geom_line(ggplot2::aes(t, y), colour = "grey60") +
    ggplot2::geom_line(ggplot2::aes(t, tend), colour = "red", na.rm = TRUE) +
    ggplot2::labs(title = "Serie (gris) y tendencia (rojo)",
                  x = "Tiempo (meses)", y = unidad) +
    ggplot2::theme_minimal()
  
  p2 <- ggplot2::ggplot(data.frame(e = 1:s, i = dec$indices),
                        ggplot2::aes(e, i)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_x_continuous(breaks = 1:s) +
    ggplot2::labs(title = paste("Índices estacionales (", dec$tipo, ")", sep = ""),
                  x = nombre_estacion, y = unidad) +
    ggplot2::theme_minimal()
  
  p3 <- ggplot2::ggplot(data.frame(t = t, r = dec$residuo),
                        ggplot2::aes(t, r)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuo", x = "Tiempo (meses)", y = unidad,
                  caption = paste("Observaciones:", length(y))) +
    ggplot2::theme_minimal()
  
  patchwork::wrap_plots(p1, p2, p3, ncol = 1)
}
