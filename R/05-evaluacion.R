

# prueba jarque 

jarque_bera <- function(e) {
  
  # e = errores
  
  N  <- length(e)
  me <- mean(e)
  
  # Momentos centrados
  m2 <- mean((e - me)^2)
  m3 <- mean((e - me)^3)
  m4 <- mean((e - me)^4)
  
  # Asimetria
  A  <- m3 / m2^(3/2)
  #  kurtosis
  K  <- m4 / m2^2
  
  # Estadistico JB
  JB   <- (N / 6) * (A^2 + (K - 3)^2 / 4)
  
  # Grados de libertad
  df   <- 2
  
  # Valor critico al 5%
  cv   <- qchisq(0.95, df)
  
  # Valor p
  pval <- 1 - pchisq(JB, df)
  
  tibble::tibble(
    asimetria    = round(A,    4),
    curtosis     = round(K,    4),
    estadistico  = round(JB,   4),
    df           = df,
    valor_critico_5pct = round(cv, 4),
    p_value      = round(pval, 4),
    rechazo_H0   = pval < 0.05
  )
}


durbin_watson <- function(e) {
  
  # errores
  
  n   <- length(e)
  
  # Diferencias consecutivas
                              # sumas al cudrados    
  num <- sum(diff(e)^2)       # suma de las diferencias entre las e_t
  den <- sum(e^2)             # suma de los e_t 
  
  d   <- num / den
  
  # Valor critico aproximado (region de no rechazo: 1.5 < d < 2.5)
  # Limites de Savin-White para referencia general (n grande)
  dl  <- 1.5
  du  <- 2.5
  
  tibble::tibble(
    estadistico        = round(d, 4),
    df                 = NA_real_,
    valor_critico_5pct = NA_real_,   # depende de n y k; requiere tablas
    p_value            = NA_real_,   # distribucion no estandar
    interpretacion     = dplyr::case_when(
      d < dl  ~ "Autocorrelacion positiva",
      d > (4 - dl) ~ "Autocorrelacion negativa",
      d >= du & d <= (4 - du) ~ "Sin autocorrelacion",
      TRUE    ~ "Region de indecision"
    )
  )
}



optimizar <- function(y, metodo, rejilla) {
  
  # funsion MSE
  calc_mse <- function(params) {
    
    # Extraer parametros segun metodo
    res <- switch(metodo,
                  
                  "mm" = {
                    k <- params[1]
                    ajustar_mm(y, k)
                  },
                  
                  "dmm" = {
                    k <- params[1]
                    ajustar_dmm(y, k)
                  },
                  
                  "ses" = {
                    alpha <- params[1]
                    ajustar_ses(y, alpha)
                  },
                  
                  "holt" = {
                    alpha <- params[1]
                    beta  <- params[2]
                    ajustar_holt(y, alpha, beta)
                  },
                  
                  stop("metodo no reconocido: use 'mm', 'dmm', 'ses' o 'holt'")
    )
    
    # MSE: promedio de residuos^2 
    residuos <- res$residuales
    mean(residuos^2, na.rm = TRUE) #ignorando NA
  }
  
  # Calcular MSE para cada fila de la rejilla
  rejilla$mse <- apply(rejilla, 1, calc_mse)
  
  # Fila con menor MSE
  optimo <- rejilla[which.min(rejilla$mse), ] # which.min = saca el indice donde esta el menor mse en rejilla
  
  list(
    rejilla = rejilla,
    optimo  = optimo
  )
}


origen_movil <- function(datos, n_origenes) {  #datos = vector numerico
  
  n <- length(datos)
  primer_origen <- n - n_origenes - 1
  
  resultados <- vector("list", n_origenes)
  
  for (i in seq_len(n_origenes)) {
    
    origen_actual <- primer_origen + i
    
    resultados[[i]] <- list(
      origen      = origen_actual,
      entrenamiento = datos[1:origen_actual],
      prueba      = datos[origen_actual + 1]
    )
  }
  
  return(resultados)
}
