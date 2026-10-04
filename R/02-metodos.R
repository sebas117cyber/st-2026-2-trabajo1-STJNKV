#-------------- Media Simple --------------------#
ajustar_media <- function(y){
  
  # Validacion 
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  
  # longitud
  T <- length(y)
  
  # vector de estimados
  yhat <- rep(NA_real_, T)#Misma longitud, cuando no le ingresamos valor x lo pone como NA
  
  # Calentamiento
  media <- y[1]
  yhat[2] <- media
  
  # Actualizacion recursiva de la media
  for(t in 2:T){
    
    media <- media + (y[t] - media) / t
    
    if(t < T){
      yhat[t + 1] <- media
    }
  }
  
  # extramuestrales
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    # todos los pronosticos finales son la media 
    rep(media, h)
  }
  
  # parametros
  parametros <- list(
    media_final = media
  )
  
  return(list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}

#-----------Media Movil--------------#
ajustar_mm <- function(y, k){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  stopifnot(k >= 2)
  stopifnot(k <= length(y))
  
  T <- length(y)
  
  # Pronostico
  y_hat <- rep(NA_real_, T)
  
  # Primera ventana
  suma <- sum(y[1:k])
  
  # Pronostico dentro de la muestra
  if(k < T){
    
    y_hat[k + 1] <- suma / k
    
    for(t in (k + 1):(T - 1)){
      
      suma <- suma - y[t - k] + y[t]
      
      y_hat[t + 1] <- suma / k
    }
    
    # Actualizamos a la última ventana observada
    suma <- suma - y[T - k] + y[T]
  }
  
  # Pronosticos extramuestrales
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      h >= 1,
      h == as.integer(h)
    )
    
    media_final <- suma / k
    
    rep(media_final, h)
  }
  
  # Parametros
  parametros <- list(
    k = k,
    media_final = suma / k
  )
  
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}


#-------------- Doble Media Móvil --------------------#
ajustar_dmm <- function(y, k){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  stopifnot(k >= 2)
  stopifnot(k <= length(y))
  
  T <- length(y)
  
  # Ventana
  stopifnot(2 * k - 1 <= T)
  
  # Vectores
  y_hat <- rep(NA_real_, T)
  MM <- rep(NA_real_, T)
  DMM <- rep(NA_real_, T)
  E_hat <- rep(NA_real_, T)
  B_hat <- rep(NA_real_, T)
  
  # Primera media movil
  suma_mm <- sum(y[1:k])
  MM[k] <- suma_mm / k
  
  # segunda media movil 
  suma_dmm <- MM[k]
  cantidad_mm <- 1
  
  if(k < T){
    
    for(t in (k + 1):T){
      
      # Actualizamos la primera media 
      suma_mm <- suma_mm - y[t - k] + y[t]
      MM[t] <- suma_mm / k
      
      # Actualizamos cantidad de medias mmoviles
      cantidad_mm <- cantidad_mm + 1
      suma_dmm <- suma_dmm + MM[t]
      
      # Actualizamos el valor k 
      if(cantidad_mm > k){
        suma_dmm <- suma_dmm - MM[t - k]
      }
      
      # Podemos calcular DMM cuando tenemos k valores de MM
      if(cantidad_mm >= k){
        
        DMM[t] <- suma_dmm / k
        
        # Nivel estimado
        E_hat[t] <- 2 * MM[t] - DMM[t]
        
        # Tendencia estimada
        B_hat[t] <- (2 / (k - 1)) * (MM[t] - DMM[t])
        
        # Pronostico
        if(t < T){
          y_hat[t + 1] <- E_hat[t] + B_hat[t]
        }
      }
    }
  }
  
  # Estado final
  E_final <- E_hat[T]
  B_final <- B_hat[T]
  
  # extramuestrales
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    # Y_hat
    E_final + B_final * seq_len(h)
  }
  
  # Parametros
  parametros <- list(
    k = k,
    MM_final = MM[T],
    DMM_final = DMM[T],
    E_final = E_final,
    B_final = B_final
  )
  
  # Resultado
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros,
    MM = MM,
    DMM = DMM,
    E_hat = E_hat,
    B_hat = B_hat
  ))
}

#-------------- Suavizamiento Exponencial Simple --------------------#

ajustar_ses <- function(y, alpha){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  stopifnot(length(alpha) == 1)
  stopifnot(alpha > 0, alpha < 1)
  
  # Cantidad de observaciones
  T <- length(y)
  
  # Vector de pronósticos
  y_hat <- rep(NA_real_, T)
  
  # Calentamiento
  y_hat[2] <- y[1]
  
  # Suavizamiento exponencial simple
  for(t in 2:(T - 1)){
    
    # Error 
    error <- y[t] - y_hat[t]
    
    # Correccion de error
    y_hat[t + 1] <- y_hat[t] + alpha * error
  }
  
  # extramuestrales
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(y_hat[T], h)
  }
  
  # parametros
  parametros <- list(
    alpha = alpha,
    nivel_final = y_hat[T]
  )
  
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}

#-------------- Holt lineal --------------------#

ajustar_holt <- function(y, alpha, beta){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  stopifnot(length(alpha) == 1)
  stopifnot(length(beta) == 1)
  stopifnot(alpha > 0, alpha < 1)
  stopifnot(beta > 0, beta < 1)
  
  # Total de observaciones
  T <- length(y)
  
  # Pronostico
  y_hat <- rep(NA_real_, T)
  
  # Trayectorias del nivel y pendiente 
  L <- rep(NA_real_, T)
  T_hat <- rep(NA_real_, T)
  
  # Calentamiento
  L[1] <- y[1]
  T_hat[1] <- 0
  
  # Primer pronostico
  y_hat[2] <- L[1] + T_hat[1]
  
  # Actualizacion nivel y pendiente 
  for(t in 2:T){
    
    # Error 
    error <- y[t] - y_hat[t]
    
    # L correccion de error
    L[t] <- L[t - 1] + T_hat[t - 1] + alpha * error
    
    # T correccion de error
    T_hat[t] <- T_hat[t - 1] + alpha * beta * error
    
    #Validamos la igualdad 
    L_ponderado <- alpha * y[t] +
      (1 - alpha) * (L[t - 1] + T_hat[t - 1])
    
    T_ponderado <- beta * (L[t] - L[t - 1]) +
      (1 - beta) * T_hat[t - 1]
    
    stopifnot(
      isTRUE(all.equal(L[t], L_ponderado)),
      isTRUE(all.equal(T_hat[t], T_ponderado))
    )
    
    # Pronostico del siguiente periodo
    if(t < T){
      y_hat[t + 1] <- L[t] + T_hat[t]
    }
  }
  
  # Pronosticos extramuestrales
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    L[T] + T_hat[T] * seq_len(h)
  }
  
  # Parametros
  parametros <- list(
    alpha = alpha,
    beta = beta,
    nivel_final = L[T],
    tendencia_final = T_hat[T]
  )
  
  # Resultado
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros,
    nivel = L,
    tendencia = T_hat
  ))
}

#-------------- Tendencia Lineal ---------------#

ajustar_tendencia_lineal <- function(y){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 3)
  stopifnot(!anyNA(y))
  
  # Total de observaciones
  T <- length(y)
  
  # P. muestrales
  
  
  y_hat <- rep(NA_real_, T)
  
  for(t in 3:T){
    
    # Informacion disponible hasta t-1
    y_entrenamiento <- y[1:(t - 1)]
    
    # Tiempo correspondiente a esa información
    tiempo_entrenamiento <- 1:(t - 1)
    
    # Matriz X
    X_prev <- cbind(
      1,
      tiempo_entrenamiento
    )
    
    # Ecuaciones normales
    beta_prev <- solve(
      crossprod(X_prev),
      crossprod(X_prev, y_entrenamiento)
    )
    
    # Pronostico de Y_t
    y_hat[t] <- beta_prev[1] + beta_prev[2] * t
  }
  
  
  # Ajuste final con toda la serie
  
  
  tiempo <- 1:T
  
  X <- cbind(
    1,
    tiempo
  )
  
  # beta 
  beta <- solve(
    crossprod(X),
    crossprod(X, y)
  )
  
  beta_0 <- beta[1]
  beta_1 <- beta[2]
  
  # Valores ajustados
  ajustados <- as.vector(X %*% beta)
  
  # Residuos
  residuos <- y - ajustados
  
  # #parámetros
  p <- ncol(X)
  
  
  # Medidas del ajuste
  
  
  # Suma de cuadrados de residuos
  SSE <- sum(residuos^2)
  
  # Suma de cuadrados total
  SST <- sum((y - mean(y))^2)
  
  # R cuadrado
  R2 <- 1 - SSE / SST
  
  # Sigma^2 estimada
  sigma2 <- SSE / (T - p)
  
  # Durbin-Watson
  DW <- sum(diff(residuos)^2) / SSE
  
  # 4. Error estándar ordinario
  
  XtX_inv <- solve(crossprod(X))
  
  var_ordinaria <- sigma2 * XtX_inv
  
  se_ordinario <- sqrt(diag(var_ordinaria))
  
  #  Error estándar robusto HAC Bartlett
  
  # Número de rezagos 
  L <- floor(4 * (T / 100)^(2 / 9))
  
  # No puede superar T-1
  L <- min(L, T - 1)
  
  # X multiplicada fila por fila por los residuos
  Xe <- X * residuos
  
  # Gamma_0
  S <- crossprod(Xe)
  
  # Gammas para h = 1,...,L
  if(L >= 1){
    
    for(h in 1:L){
      
      # Peso de Bartlett
      w <- 1 - h / (L + 1)
      
      # Productos cruzados desplazados
      gamma_h <- crossprod(
        Xe[(h + 1):T, , drop = FALSE],
        Xe[1:(T - h), , drop = FALSE]
      )
      
      # HAC
      S <- S + w * (gamma_h + t(gamma_h))
    }
  }
  
  # Matriz de varianza robusta
  var_robusta <- XtX_inv %*% S %*% XtX_inv
  
  se_robusto <- sqrt(diag(var_robusta))
  
  
  # Estadísticos t y valores p robustos
  
  t_robusto <- beta / se_robusto
  
  p_robusto <- 2 * pt(
    -abs(t_robusto),
    df = T - p
  )
  
  # Tabla de coeficientes-
  
  coeficientes <- data.frame(
    termino = c("beta_0", "beta_1"),
    estimacion = as.numeric(beta),
    se_ordinario = se_ordinario,
    se_robusto = se_robusto,
    t_robusto = t_robusto,
    p_valor = p_robusto
  )
  
  # Pronósticos extramuestrales
  
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    tiempos_futuros <- T + seq_len(h)
    
    X_futuro <- cbind(
      1,
      tiempos_futuros
    )
    
    as.vector(X_futuro %*% beta)
  }
  
  # 9. Parámetros
  
  parametros <- list(
    coeficientes = coeficientes,
    beta_0 = beta_0,
    beta_1 = beta_1,
    R2 = R2,
    sigma2 = sigma2,
    durbin_watson = DW,
    rezagos_bartlett = L
  )
  
  # 10. Resultado
  
  
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}

#-------------- Tendencia Cuadratica---------------#
ajustar_tendencia_cuadratica <- function(y){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 3)
  stopifnot(!anyNA(y))
  # Total de observaciones
  T <- length(y)
  
  #Pronostico muestral
  
  y_hat <- rep(NA_real_, T)
  
  for(t in 4:T){
    
    # Informacion disponible hasta t-1
    y_entrenamiento <- y[1:(t - 1)]
    tiempo <- 1:(t - 1)
    
    # Matriz de diseño
    X <- cbind(
      1,
      tiempo,
      tiempo^2
    )
    
    # Ecuaciones normales
    beta <- solve(
      crossprod(X),
      crossprod(X, y_entrenamiento)
    )
    
    # Pronostico de Y_t
    y_hat[t] <-
      beta[1] +
      beta[2] * t +
      beta[3] * t^2
  }
  
  
  # Ajuste final
  
  
  tiempo <- 1:T
  
  X <- cbind(
    1,
    tiempo,
    tiempo^2
  )
  
  # beta 
  beta <- solve(
    crossprod(X),
    crossprod(X, y)
  )
  
  # Coeficientes
  beta_0 <- beta[1]
  beta_1 <- beta[2]
  beta_2 <- beta[3]
  
  
  #Valores ajustados y residuos
  
  ajustados <- as.vector(X %*% beta)
  
  residuos <- y - ajustados
  
  # parametros
  p <- ncol(X)
  
  # R2 y sigma2
  
  SSE <- sum(residuos^2)
  
  SST <- sum((y - mean(y))^2)
  
  R2 <- 1 - SSE / SST
  
  sigma2 <- SSE / (T - p)
  
  #Error estandar 
  
  XtX_inv <- solve(crossprod(X))
  
  se_ordinario <- sqrt(
    diag(sigma2 * XtX_inv)
  )
  
  # Error estándar robusto HAC Bartlett
  
  L <- floor(
    4 * (T / 100)^(2 / 9)
  )
  
  L <- min(L, T - 1)
  
  Xe <- X * residuos
  
  S <- crossprod(Xe)
  
  if(L >= 1){
    
    for(h in 1:L){
      
      peso <- 1 - h / (L + 1)
      
      gamma_h <- crossprod(
        Xe[(h + 1):T, , drop = FALSE],
        Xe[1:(T - h), , drop = FALSE]
      )
      
      S <- S + peso * (
        gamma_h + t(gamma_h)
      )
    }
  }
  
  var_robusta <-
    XtX_inv %*% S %*% XtX_inv
  
  se_robusto <- sqrt(
    diag(var_robusta)
  )
  
  # Estadístico t y valor p
  
  
  estadistico_t <- beta / se_robusto
  
  p_valor <- 2 * pt(
    -abs(estadistico_t),
    df = T - p
  )
  
  # Tabla de coeficientes
  
  
  coeficientes <- data.frame(
    termino = c(
      "beta_0",
      "beta_1",
      "beta_2"
    ),
    estimacion = beta,
    se_ordinario = se_ordinario,
    se_robusto = se_robusto,
    t = estadistico_t,
    p_valor = p_valor
  )
  
  # Durbin-Watson
  
  
  DW <- sum(diff(residuos)^2) / SSE
  
  
  # Pronosticos extramuestrales
  
  
  pronosticar <- function(h){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    futuro <- T + seq_len(h)
    
    beta_0 +
      beta_1 * futuro +
      beta_2 * futuro^2
  }
  
  
  # Parametros
  parametros <- list(
    coeficientes = coeficientes,
    R2 = R2,
    sigma2 = sigma2,
    durbin_watson = DW,
    rezagos_bartlett = L
  )
  
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}

#------------- Tendencia exponencial ---------------#

ajustar_tendencia_exponencial <- function(y){
  
  # Validaciones
  stopifnot(is.numeric(y))
  stopifnot(length(y) >= 2)
  stopifnot(!anyNA(y))
  
  # La serie debe ser positiva
  if(any(y <= 0)){
    stop("La tendencia exponencial requiere que todos los valores de y sean positivos.")
  }
  
  # Cantidad de observaciones
  T <- length(y)
  
  # T.logaritmica
  log_y <- log(y)
  
  # Pronostico muestral
  y_hat <- rep(NA_real_, T)
  
  for(t in 3:T){
    
    y_entrenamiento <- log_y[1:(t - 1)]
    tiempo <- 1:(t - 1)
    
    X_prev <- cbind(
      1,
      tiempo
    )
    
    # Ecuaciones normales
    beta_prev <- solve(
      crossprod(X_prev),
      crossprod(X_prev, y_entrenamiento)
    )
    
    # a y theta
    a_prev <- beta_prev[1]
    theta_prev <- beta_prev[2]
    
    # Pronostico escala
    y_hat[t] <- exp(
      a_prev + theta_prev * t
    )
  }
  
  # Ajuste final 
  tiempo <- 1:T
  
  X <- cbind(
    1,
    tiempo
  )
  
  # beta
  beta <- solve(
    crossprod(X),
    crossprod(X, log_y)
  )
  
  a <- beta[1]
  theta <- beta[2]
  
  # Coeficientes de la forma original
  beta_0 <- exp(a)
  beta_1 <- exp(theta)
  
  # Valores ajustados y residuos
  
  ajustados_log <- as.vector(X %*% beta)
  
  residuos <- log_y - ajustados_log
  
  # Número de parámetros
  p <- ncol(X)
  
  # R2 y sigma2
  
  SSE <- sum(residuos^2)
  
  SST <- sum((log_y - mean(log_y))^2)
  
  R2 <- 1 - SSE / SST
  
  sigma2_ln <- SSE / (T - p)
  
  # Error estándar ordinario
  
  XtX_inv <- solve(crossprod(X))
  
  se_ordinario <- sqrt(
    diag(sigma2_ln * XtX_inv)
  )
  
  # Error estándar robusto HAC Bartlett
  
  
  L <- floor(
    4 * (T / 100)^(2 / 9)
  )
  
  L <- min(L, T - 1)
  
  Xe <- X * residuos
  
  S <- crossprod(Xe)
  
  if(L >= 1){
    
    for(h in 1:L){
      
      peso <- 1 - h / (L + 1)
      
      gamma_h <- crossprod(
        Xe[(h + 1):T, , drop = FALSE],
        Xe[1:(T - h), , drop = FALSE]
      )
      
      S <- S + peso * (
        gamma_h + t(gamma_h)
      )
    }
  }
  
  var_robusta <-
    XtX_inv %*% S %*% XtX_inv
  
  se_robusto <- sqrt(
    diag(var_robusta)
  )
  
  # Estadístico t y valor p
  
  estadistico_t <- beta / se_robusto
  
  p_valor <- 2 * pt(
    -abs(estadistico_t),
    df = T - p
  )
  
  # Tabla de coeficientes
  
  coeficientes <- data.frame(
    termino = c("a", "theta"),
    estimacion = beta,
    se_ordinario = se_ordinario,
    se_robusto = se_robusto,
    t = estadistico_t,
    p_valor = p_valor
  )
  
  # Durbin-Watson
  
  DW <- sum(diff(residuos)^2) / SSE
  
  # Pronosticos extramuestrales
  
  pronosticar <- function(h, corregir_sesgo = FALSE){
    
    stopifnot(
      length(h) == 1,
      is.numeric(h),
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    stopifnot(
      is.logical(corregir_sesgo),
      length(corregir_sesgo) == 1
    )
    
    factor_sesgo <- 1
    
    if(corregir_sesgo){
      factor_sesgo <- exp(sigma2_ln / 2)
    }
    
    futuro <- T + seq_len(h)
    
    exp(
      a + theta * futuro
    ) * factor_sesgo
  }
  
  # Parametros
  parametros <- list(
    coeficientes = coeficientes,
    a = a,
    theta = theta,
    beta_0 = beta_0,
    beta_1 = beta_1,
    R2 = R2,
    sigma2_ln = sigma2_ln,
    durbin_watson = DW,
    rezagos_bartlett = L
  )
  
  # Resultado
  return(list(
    yhat = y_hat,
    pronosticar = pronosticar,
    parametros = parametros
  ))
}


#-------------- Tendencia funcion principal ---------------#
ajustar_tendencia <- function(y, tipo){
  
  stopifnot(is.numeric(y))
  stopifnot(!anyNA(y))
  stopifnot(length(tipo) == 1)
  stopifnot(tipo %in% c("lineal", "cuadratica", "exponencial"))
  
  if (tipo == "lineal") {
    resultado <- ajustar_tendencia_lineal(y)
  } else if (tipo == "cuadratica") {
    resultado <- ajustar_tendencia_cuadratica(y)
  } else if (tipo == "exponencial") {
    resultado <- ajustar_tendencia_exponencial(y)
  } else {
    stop("tipo debe ser 'lineal', 'cuadratica' o 'exponencial'")
  }
  
  return(resultado)
}




