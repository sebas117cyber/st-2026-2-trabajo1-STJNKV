# R/04-metodos.R  --  Equipo STJNKV
# Paquetes permitidos: R base, dplyr, tidyr, purrr, tibble, readr, ggplot2, patchwork
# Convencion: cada metodo devuelve list(yhat, pronosticar, parametros, ...)
#   yhat[t] usa solo y[1:(t-1)]; calentamiento = NA
# Ninguna funcion lee objetos globales.

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------- 1. Media simple
ajustar_media <- function(y) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  
  T_obs <- length(y)
  
  fitted <- numeric(T_obs)
  fitted[1] <- y[1]
  for (t in 2:T_obs) fitted[t] <- fitted[t - 1] + (1 / t) * (y[t] - fitted[t - 1])
  # fitted[t] = media de y[1:t]
  
  yhat <- c(NA_real_, fitted[-T_obs])          # yhat[t] = media de y[1:(t-1)]
  media_final <- fitted[T_obs]
  
  
  forecast_fn <- function(h) {
    rep(media_final, h)
  }
  
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    forecast_fn(h)
  }
  
  list(
    metodo         = "Media simple",
    yhat           = yhat,
    pronosticar    = pronosticar,
    parametros     = list(media_final = media_final),
    ajustados      = fitted,
    residuales     = y - yhat,
    Extra_muestral = forecast_fn,
    T_observado    = T_obs
  )
}

# ---------------------------------------------------------------- 2. Ingenuo
ajustar_ingenuo <- function(y) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  
  T_obs <- length(y)
  yhat  <- c(NA_real_, y[-T_obs])
  
  # Pronostico extramuestral: se mantiene el ultimo valor observado
  # para cualquier horizonte h (misma logica de la funcion original)
  ultimo_valor <- y[T_obs]
  forecast_fn <- function(h) {
    rep(ultimo_valor, h)
  }
  
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    forecast_fn(h)
  }
  
  list(
    metodo         = "Ingenuo",
    yhat           = yhat,
    pronosticar    = pronosticar,
    parametros     = list(),
    residuales     = y - yhat,
    Extra_muestral = forecast_fn,
    T_observado    = T_obs
  )
}

# ---------------------------------------------------------------- 2a. Ingenuo estacional
ajustar_ingenuo_estacional <- function(y, s) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  stopifnot(is.numeric(s), length(s) == 1, s >= 2, length(y) > s)
  
  T_obs <- length(y)
  
  fitted <- rep(NA_real_, T_obs)
  for (t in (s + 1):T_obs) {
    fitted[t] <- y[t - s]
  }
  
  residuo <- y - fitted   # NA en calentamiento (t = 1..s)
  
  # yhat: fitted[t] ya es Yhat_t (info disponible hasta t-1 pues s >= 1)
  yhat <- fitted
  
  # Pronostico extramuestral: repite el ultimo ciclo observado
  forecast_fn <- function(h) {
    m   <- ((h - 1) %% s) + 1
    idx <- T_obs - s + m
    y[idx]
  }
  
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    forecast_fn(1:h)
  }
  
  list(
    metodo         = paste("Ingenuo estacional s =", s),
    yhat           = yhat,
    pronosticar    = pronosticar,
    parametros     = list(s = s),
    s              = s,
    Ajustados      = fitted,
    residuales     = residuo,
    Extra_muestral = forecast_fn,
    T_observado    = T_obs
  )
}


# ---------------------------------------------------------------- 3. Media movil
ajustar_mm <- function(y, k) {
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 3)
  stopifnot(is.numeric(k), length(k) == 1, k >= 2, k < length(y))
  T_obs <- length(y)
  
  yhat <- rep(NA_real_, T_obs)
  for (j in (k + 1):T_obs) yhat[j] <- mean(y[(j - k):(j - 1)])
  
  mm_final <- mean(y[(T_obs - k + 1):T_obs])
  
  list(
    metodo      = paste("Media movil k =", k),
    yhat        = yhat,
    pronosticar = function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      rep(mm_final, h)
    },
    parametros  = list(k = k, MM_final = mm_final),
    residuales  = y - yhat,
    T_observado = T_obs
  )
}

# ---------------------------------------------------------------- 4. SES
ajustar_ses <- function(y, alpha) {
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  T_obs <- length(y)
  
  # fitted[t] = nivel L_t (despues de ver y_t) = pronostico de y_{t+1}
  fitted <- numeric(T_obs)
  fitted[1] <- y[1]
  for (t in 2:T_obs) {
    e_t       <- y[t] - fitted[t - 1]
    fitted[t] <- fitted[t - 1] + alpha * e_t          # correccion de error
  }
  
  # Verificacion: forma de promedio ponderado L_t = a*y_t + (1-a)*L_{t-1}
  fitted_pp <- numeric(T_obs)
  fitted_pp[1] <- y[1]
  for (t in 2:T_obs) fitted_pp[t] <- alpha * y[t] + (1 - alpha) * fitted_pp[t - 1]
  coinciden <- all(abs(fitted - fitted_pp) < 1e-10)
  
  yhat <- c(NA_real_, fitted[-T_obs])                 # yhat[t] = L_{t-1}
  nivel_final <- fitted[T_obs]                        # pronostico de y_{T+h}
  
  list(
    metodo           = paste("SES alpha =", alpha),
    yhat             = yhat,
    pronosticar      = function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      rep(nivel_final, h)
    },
    parametros       = list(alpha = alpha, nivel_final = nivel_final),
    ajustados        = fitted,
    residuales       = y - yhat,
    formas_coinciden = coinciden,
    T_observado      = T_obs
  )
}

# ---------------------------------------------------------------- 5. Doble media movil
ajustar_dmm <- function(y, k) {
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  stopifnot(is.numeric(k), length(k) == 1, k >= 2, length(y) >= 2 * k)
  T_obs <- length(y)
  
  MM    <- rep(NA_real_, T_obs)
  DMM   <- rep(NA_real_, T_obs)
  E_hat <- rep(NA_real_, T_obs)
  b_hat <- rep(NA_real_, T_obs)
  yhat  <- rep(NA_real_, T_obs)
  
  for (t in k:T_obs) MM[t] <- mean(y[(t - k + 1):t])
  for (t in (2 * k - 1):T_obs) {
    DMM[t]   <- mean(MM[(t - k + 1):t])
    E_hat[t] <- 2 * MM[t] - DMM[t]
    b_hat[t] <- (2 / (k - 1)) * (MM[t] - DMM[t])
  }
  for (t in (2 * k):T_obs) yhat[t] <- E_hat[t - 1] + b_hat[t - 1]
  
  list(
    metodo      = paste("Doble media movil k =", k),
    yhat        = yhat,
    pronosticar = function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      E_hat[T_obs] + b_hat[T_obs] * seq_len(h)
    },
    parametros  = list(k = k, E_final = E_hat[T_obs], b_final = b_hat[T_obs]),
    MM = MM, DMM = DMM, E_hat = E_hat, b_hat = b_hat,
    residuales  = y - yhat,
    T_observado = T_obs
  )
}

# ---------------------------------------------------------------- 6. Tendencias (MCO a mano)
# calcular_yhat = TRUE : yhat de un paso (reajusta con y[1:(t-1)]) y tabla de coeficientes.
# calcular_yhat = FALSE: solo ajusta y pronostica (rapido, para el origen movil).
ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"),
                              corregir_sesgo = FALSE, verbose = FALSE,
                              calcular_yhat = TRUE) {
  
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 3)
  
  tipo   <- match.arg(tipo)
  T_obs  <- length(y)
  t_vect <- seq_len(T_obs)
  
  Tabla_modelo <- function(modelo) {
    X            <- modelo$X
    beta         <- as.vector(modelo$beta)
    residuos     <- as.vector(modelo$residuos)
    nombres_coef <- modelo$nombres_coef
    
    n <- nrow(X)
    k <- ncol(X)
    
    # Varianza robusta con nucleo de Bartlett (Newey-West)
    lag    <- floor(4 * (n / 100)^(2 / 9))
    sigma2 <- sum(residuos^2) / (n - k)
    
    Xe <- X * residuos
    S  <- crossprod(Xe)
    for (l in seq_len(lag)) {
      if (l >= n) break
      w       <- 1 - l / (lag + 1)
      Gamma_l <- crossprod(Xe[(l + 1):n, , drop = FALSE],
                           Xe[1:(n - l), , drop = FALSE])
      S       <- S + w * (Gamma_l + t(Gamma_l))
    }
    
    XtX_inv <- solve(crossprod(X))
    V_rob   <- XtX_inv %*% S %*% XtX_inv
    
    se_ord <- sqrt(pmax(diag(sigma2 * XtX_inv), 0))
    se_rob <- sqrt(pmax(diag(V_rob), 0))
    
    t_stat <- beta / se_rob
    p_val  <- 2 * pt(abs(t_stat), df = n - k, lower.tail = FALSE)
    
    y_usado <- as.vector(X %*% beta + residuos)
    ss_tot  <- sum((y_usado - mean(y_usado))^2)
    ss_res  <- sum(residuos^2)
    r2      <- 1 - ss_res / ss_tot
    dw      <- sum(diff(residuos)^2) / ss_res
    
    tabla <- dplyr::tibble(
      coeficiente   = nombres_coef,
      estimacion    = beta,
      se_ordinario  = se_ord,
      se_robusto    = se_rob,
      t_estadistico = t_stat,
      p_valor       = p_val
    )
    list(tabla = tabla, r2 = r2, sigma2 = sigma2, dw = dw)
  }
  
  if (tipo == "lineal") {
    X    <- cbind(1, t_vect)
    beta <- as.vector(solve(crossprod(X), crossprod(X, y)))
    ajustados <- as.vector(X %*% beta)
    residuos  <- y - ajustados
    resul <- list(X = X, beta = beta, residuos = residuos,
                  nombres_coef = c("beta0", "beta1"))
    pronosticar <- function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      t_fut <- (T_obs + 1):(T_obs + h)
      beta[1] + beta[2] * t_fut
    }
    parametros <- list(beta0 = beta[1], beta1 = beta[2])
  }
  
  if (tipo == "cuadratica") {
    X    <- cbind(1, t_vect, t_vect^2)
    beta <- as.vector(solve(crossprod(X), crossprod(X, y)))
    ajustados <- as.vector(X %*% beta)
    residuos  <- y - ajustados
    resul <- list(X = X, beta = beta, residuos = residuos,
                  nombres_coef = c("beta0", "beta1", "beta2"))
    pronosticar <- function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      t_fut <- (T_obs + 1):(T_obs + h)
      beta[1] + beta[2] * t_fut + beta[3] * t_fut^2
    }
    parametros <- list(beta0 = beta[1], beta1 = beta[2], beta2 = beta[3])
  }
  
  if (tipo == "exponencial") {
    if (any(y <= 0)) stop("Hay valores negativos o ceros en la serie")
    trans_y <- log(y)
    X       <- cbind(1, t_vect)
    beta_ln <- as.vector(solve(crossprod(X), crossprod(X, trans_y)))
    
    residuos_trans <- trans_y - as.vector(X %*% beta_ln)
    sigma2_trans   <- sum(residuos_trans^2) / (T_obs - 2)
    
    beta_0or   <- exp(beta_ln[1])
    beta_1or   <- exp(beta_ln[2])
    fact_sesgo <- if (corregir_sesgo) exp(sigma2_trans / 2) else 1
    
    ajustados <- beta_0or * beta_1or^t_vect * fact_sesgo
    residuos  <- y - ajustados
    # La tabla de coeficientes de la exponencial esta en escala log
    resul <- list(X = X, beta = beta_ln, residuos = residuos_trans,
                  nombres_coef = c("beta0 (log)", "beta1 (log)"))
    pronosticar <- function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      t_fut <- (T_obs + 1):(T_obs + h)
      beta_0or * beta_1or^t_fut * fact_sesgo
    }
    parametros <- list(beta0_ln = beta_ln[1], beta1_ln = beta_ln[2],
                       beta0_or = beta_0or,   beta1_or = beta_1or)
  }
  
  res <- NULL
  if (calcular_yhat || verbose) res <- Tabla_modelo(resul)
  if (verbose) {
    print(res$tabla, digits = 4)
    cat(sprintf("\nR2 = %.6f\nsigma2 = %.6f\nDurbin-Watson = %.4f\n",
                res$r2, res$sigma2, res$dw))
  }
  
  # yhat de un paso: yhat[t] reajusta con y[1:(t-1)]
  yhat <- rep(NA_real_, T_obs)
  if (calcular_yhat) {
    p <- ncol(resul$X)
    for (t in (p + 2):T_obs) {
      yhat[t] <- ajustar_tendencia(y[1:(t - 1)], tipo, corregir_sesgo,
                                   verbose = FALSE,
                                   calcular_yhat = FALSE)$pronosticar(1)[1]
    }
  }
  
  invisible(list(
    tipo        = tipo,
    yhat        = yhat,
    pronosticar = pronosticar,
    parametros  = parametros,
    ajustados   = ajustados,   # ajuste dentro de muestra (para 6b)
    residuales  = residuos,    # residuos del ajuste (para 6b)
    tabla       = if (is.null(res)) NULL else res$tabla,
    r2          = if (is.null(res)) NA_real_ else res$r2,
    sigma2      = if (is.null(res)) NA_real_ else res$sigma2,
    dw          = if (is.null(res)) NA_real_ else res$dw
  ))
}

# ---------------------------------------------------------------- 7  Tendencias con estacional

ajustar_tendencia_estacional <- function(y, s) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 2)
  stopifnot(is.numeric(s), length(s) == 1, s >= 2, length(y) >= s + 2)
  
  T_obs  <- length(y)
  t_vect <- seq_len(T_obs)
  
  # estacion de cada observacion: 1,2,...,s,1,2,...,s,...
  estacion <- ((t_vect - 1) %% s) + 1
  
  # matriz de dummies de suma cero (s-1 columnas):
  # D_i,t = 1 si estacion(t) = i ; -1 si estacion(t) = s ; 0 en otro caso
  D <- matrix(0, nrow = T_obs, ncol = s - 1)
  for (i in seq_len(s - 1)) {
    D[estacion == i, i] <- 1
    D[estacion == s, i] <- -1
  }
  
  X    <- cbind(1, t_vect, D)
  beta <- solve(crossprod(X), crossprod(X, y))
  beta <- as.vector(beta)
  
  fitted  <- as.vector(X %*% beta)
  residuo <- y - fitted
  
  beta0 <- beta[1]
  beta1 <- beta[2]
  delta <- beta[3:(s + 1)]            # delta_1,...,delta_{s-1}
  delta_s <- -sum(delta)              # restriccion de suma cero
  efectos_estacionales <- c(delta, delta_s)
  names(efectos_estacionales) <- paste0("estacion_", seq_len(s))
  
  # yhat: regresion usa toda la muestra (sin calentamiento)
  yhat <- fitted
  
  # pronosticar: proyecta tendencia + efecto estacional correspondiente
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    t_fut        <- (T_obs + 1):(T_obs + h)
    estacion_fut <- ((t_fut - 1) %% s) + 1
    efecto_fut   <- efectos_estacionales[estacion_fut]
    beta0 + beta1 * t_fut + efecto_fut
  }
  
  list(
    metodo      = paste("Tendencia con ficticias suma cero, s =", s),
    yhat        = yhat,
    pronosticar = pronosticar,
    parametros  = list(beta0 = beta0, beta1 = beta1,
                       efectos_estacionales = efectos_estacionales),
    s           = s,
    Ajustados   = fitted,
    residuales  = residuo,
    T_observado = T_obs
  )
}



# ---------------------------------------------------------------- 8. Holt
ajustar_holt <- function(y, alpha, beta) {
  stopifnot(!anyNA(y), is.numeric(y), length(y) >= 3)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  stopifnot(is.numeric(beta),  length(beta)  == 1, beta  > 0, beta  < 1)
  T_obs <- length(y)
  
  L      <- numeric(T_obs)   # nivel
  Tend   <- numeric(T_obs)   # pendiente
  fitted <- rep(NA_real_, T_obs)   # fitted[t] = pronostico de y_t con info hasta t-1
  
  # Inicializacion: L1 = y1, T1 = 0
  L[1]    <- y[1]
  Tend[1] <- 0
  
  for (t in 2:T_obs) {
    fitted[t] <- L[t - 1] + Tend[t - 1]
    L[t]      <- alpha * y[t] + (1 - alpha) * fitted[t]
    Tend[t]   <- beta * (L[t] - L[t - 1]) + (1 - beta) * Tend[t - 1]
  }
  
  # Verificacion: forma de correccion de error
  L2 <- numeric(T_obs); Tend2 <- numeric(T_obs)
  L2[1] <- y[1]; Tend2[1] <- 0
  for (t in 2:T_obs) {
    e_t      <- y[t] - (L2[t - 1] + Tend2[t - 1])
    L2[t]    <- L2[t - 1] + Tend2[t - 1] + alpha * e_t
    Tend2[t] <- Tend2[t - 1] + alpha * beta * e_t
  }
  coinciden_L    <- all(abs(L - L2)       < 1e-10)
  coinciden_Tend <- all(abs(Tend - Tend2) < 1e-10)
  
  yhat <- fitted   # ya es el pronostico de un paso de y_t; yhat[1] = NA
  
  list(
    metodo      = paste("Holt alpha =", alpha, "beta =", beta),
    yhat        = yhat,
    pronosticar = function(h) {
      stopifnot(is.numeric(h), length(h) == 1, h >= 1)
      L[T_obs] + Tend[T_obs] * seq_len(h)
    },
    parametros  = list(alpha = alpha, beta = beta,
                       L_final = L[T_obs], T_final = Tend[T_obs]),
    L = L, Tend = Tend,
    residuales  = y - yhat,
    coinciden_L = coinciden_L, coinciden_Tend = coinciden_Tend,
    T_observado = T_obs
  )
}


# ---------------------------------------------------------------- 9. Holt aditivo

ajustar_hw <- function(y, s, alpha, beta, gamma) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y))
  stopifnot(is.numeric(s), length(s) == 1, s >= 2, length(y) >= 2 * s)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  stopifnot(is.numeric(beta),  length(beta)  == 1, beta  > 0, beta  < 1)
  stopifnot(is.numeric(gamma), length(gamma) == 1, gamma > 0, gamma < 1)
  
  T_obs <- length(y)
  
  L      <- rep(NA_real_, T_obs)
  Tr     <- rep(NA_real_, T_obs)
  S      <- rep(NA_real_, T_obs)
  fitted <- rep(NA_real_, T_obs)
  
  # Inicializacion clasica con los dos primeros ciclos
  L[s]  <- mean(y[1:s])
  Tr[s] <- (mean(y[(s + 1):(2 * s)]) - mean(y[1:s])) / s
  S[1:s] <- y[1:s] - L[s]
  S[1:s] <- S[1:s] - mean(S[1:s])     # garantiza suma cero
  
  # Recursion a partir de t = s+1
  for (t in (s + 1):T_obs) {
    Yhat_t    <- L[t - 1] + Tr[t - 1] + S[t - s]
    fitted[t] <- Yhat_t
    L[t]      <- alpha * (y[t] - S[t - s]) + (1 - alpha) * (L[t - 1] + Tr[t - 1])
    Tr[t]     <- beta  * (L[t] - L[t - 1]) + (1 - beta) * Tr[t - 1]
    S[t]      <- gamma * (y[t] - L[t]) + (1 - gamma) * S[t - s]
  }
  
  residuales <- y - fitted
  
  # Pronostico extramuestral: se repite el patron estacional cada s pasos
  forecast_fn <- function(h) {
    m   <- ((h - 1) %% s) + 1
    idx <- T_obs - s + m
    L[T_obs] + Tr[T_obs] * h + S[idx]
  }
  
  # yhat: fitted[t] ya es Yhat_t (NA en calentamiento, t = 1..s)
  yhat <- fitted
  
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    forecast_fn(1:h)
  }
  
  list(
    metodo         = paste("Holt-Winters aditivo s =", s,
                           "alpha =", alpha, "beta =", beta, "gamma =", gamma),
    yhat           = yhat,
    pronosticar    = pronosticar,
    parametros     = list(alpha = alpha, beta = beta, gamma = gamma,
                          L_final = L[T_obs], T_final = Tr[T_obs],
                          S_final = S[(T_obs - s + 1):T_obs]),
    s              = s,
    alpha          = alpha,
    beta           = beta,
    gamma          = gamma,
    L              = L,
    Tr             = Tr,
    S              = S,
    Ajustados      = fitted,
    residuales     = residuales,
    Extra_muestral = forecast_fn,
    T_observado    = T_obs
  )
}


# ---------------------------------------------------------------- 10. Holt multiplicativo

ajustar_hw_mult <- function(y, s, alpha, beta, gamma) {
  
  # Validaciones
  stopifnot(!anyNA(y), is.numeric(y), all(y > 0))
  stopifnot(is.numeric(s), length(s) == 1, s >= 2, length(y) >= 2 * s)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
  stopifnot(is.numeric(beta),  length(beta)  == 1, beta  > 0, beta  < 1)
  stopifnot(is.numeric(gamma), length(gamma) == 1, gamma > 0, gamma < 1)
  
  T_obs <- length(y)
  
  L      <- rep(NA_real_, T_obs)
  Tr     <- rep(NA_real_, T_obs)
  S      <- rep(NA_real_, T_obs)
  fitted <- rep(NA_real_, T_obs)
  
  # Inicializacion clasica con los dos primeros ciclos
  L[s]  <- mean(y[1:s])
  Tr[s] <- (mean(y[(s + 1):(2 * s)]) - mean(y[1:s])) / s
  S[1:s] <- y[1:s] / L[s]
  S[1:s] <- S[1:s] / mean(S[1:s])     # garantiza promedio igual a 1
  
  # Recursion a partir de t = s+1
  for (t in (s + 1):T_obs) {
    Yhat_t    <- (L[t - 1] + Tr[t - 1]) * S[t - s]
    fitted[t] <- Yhat_t
    L[t]      <- alpha * (y[t] / S[t - s]) + (1 - alpha) * (L[t - 1] + Tr[t - 1])
    Tr[t]     <- beta  * (L[t] - L[t - 1]) + (1 - beta) * Tr[t - 1]
    S[t]      <- gamma * (y[t] / L[t]) + (1 - gamma) * S[t - s]
  }
  
  residuales <- y - fitted
  
  # Pronostico extramuestral: se repite el patron estacional cada s pasos
  forecast_fn <- function(h) {
    m   <- ((h - 1) %% s) + 1
    idx <- T_obs - s + m
    (L[T_obs] + Tr[T_obs] * h) * S[idx]
  }
  
  # yhat: fitted[t] ya es Yhat_t (NA en calentamiento, t = 1..s)
  yhat <- fitted
  
  pronosticar <- function(h) {
    stopifnot(is.numeric(h), length(h) == 1, h >= 1)
    forecast_fn(1:h)
  }
  
  list(
    metodo         = paste("Holt-Winters multiplicativo s =", s,
                           "alpha =", alpha, "beta =", beta, "gamma =", gamma),
    yhat           = yhat,
    pronosticar    = pronosticar,
    parametros     = list(alpha = alpha, beta = beta, gamma = gamma,
                          L_final = L[T_obs], T_final = Tr[T_obs],
                          S_final = S[(T_obs - s + 1):T_obs]),
    s              = s,
    alpha          = alpha,
    beta           = beta,
    gamma          = gamma,
    L              = L,
    Tr             = Tr,
    S              = S,
    Ajustados      = fitted,
    residuales     = residuales,
    Extra_muestral = forecast_fn,
    T_observado    = T_obs
  )
}



# ---------------------------------------------------------------- Graficos de MSE
graficar_mse_1d <- function(resultado, nombre_param, titulo) {
  ggplot2::ggplot(resultado$rejilla,
                  ggplot2::aes(x = .data[[nombre_param]], y = mse)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(data = resultado$optimo,
                        ggplot2::aes(x = .data[[nombre_param]], y = mse),
                        color = "red", size = 3) +
    ggplot2::labs(title = titulo, x = nombre_param, y = "MSE") +
    ggplot2::theme_minimal()
}

graficar_mse_holt <- function(resultado, titulo) {
  ggplot2::ggplot(resultado$rejilla,
                  ggplot2::aes(x = alpha, y = beta, fill = mse)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::geom_point(data = resultado$optimo,
                        ggplot2::aes(x = alpha, y = beta),
                        color = "red", size = 4, inherit.aes = FALSE) +
    ggplot2::labs(title = titulo, x = "alpha", y = "beta", fill = "MSE") +
    ggplot2::theme_minimal()
}