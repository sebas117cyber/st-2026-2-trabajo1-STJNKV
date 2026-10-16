

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



#--------------oPTIMIZAR--------------#
optimizar <- function(y, metodo, rejilla, n0 = length(y), s = NULL) {
  stopifnot(n0 <= length(y), is.data.frame(rejilla))
  y <- y[1:n0]                                  # solo tramo de estimacion
  
  ajustar <- function(p) switch(metodo,
                                "mm"      = ajustar_mm(y, p[1]),
                                "dmm"     = ajustar_dmm(y, p[1]),
                                "ses"     = ajustar_ses(y, p[1]),
                                "holt"    = ajustar_holt(y, p[1], p[2]),
                                "hw"      = {
                                  stopifnot(!is.null(s))
                                  ajustar_hw(y, s, p[1], p[2], p[3])
                                },
                                "hw_mult" = {
                                  stopifnot(!is.null(s))
                                  ajustar_hw_mult(y, s, p[1], p[2], p[3])
                                },
                                stop("metodo no reconocido: use 'mm', 'dmm', 'ses', 'holt', 'hw' o 'hw_mult'"))
  
  yh <- lapply(seq_len(nrow(rejilla)),
               function(i) ajustar(unlist(rejilla[i, ], use.names = FALSE))$yhat)
  
  # primera observacion donde TODOS los puntos de la rejilla ya tienen pronostico
  ini <- max(vapply(yh, function(v) which(!is.na(v))[1], numeric(1)))
  
  rejilla$mse <- vapply(yh, function(v) mean((y[ini:n0] - v[ini:n0])^2), numeric(1))
  
  list(rejilla = rejilla,
       optimo  = rejilla[which.min(rejilla$mse), , drop = FALSE],
       inicio_comun = ini)
}

#--------------------Origen Movil -------------------#
origen_movil <- function(y, ajustador, n0, h) {
  stopifnot(is.numeric(y), is.function(ajustador), h >= 1, n0 >= 3, n0 + h <= length(y))
  T_obs    <- length(y)
  origenes <- n0:(T_obs - h)
  
  filas <- lapply(origenes, function(o) {
    mod  <- ajustador(y[1:o])               # solo informacion hasta o
    pron <- mod$pronosticar(h)[h]           # pronostico de y[o + h]
    data.frame(origen = o, objetivo = o + h, y_real = y[o + h], y_pron = pron)
  })
  out <- do.call(rbind, filas)
  out$error <- out$y_real - out$y_pron      # error = real - pronostico
  out
}

# RMSE, MAD y MAPE (en %) a partir de los errores y los valores reales
medidas <- function(error, y_real) {
  stopifnot(length(error) == length(y_real), !anyNA(error))
  data.frame(RMSE = sqrt(mean(error^2)),
             MAD  = mean(abs(error)),
             MAPE = 100 * mean(abs(error / y_real)))
}


#Validar errores 
validar_errores <- function(e, m = min(floor(length(e) / 4), 24), alpha = 0.05) {
  stopifnot(is.numeric(e), !anyNA(e), length(e) >= 8, m >= 1, m < length(e))
  n <- length(e)
  
  # Prueba t de media cero
  t_obs <- mean(e) / (sd(e) / sqrt(n))
  cr_t  <- qt(1 - alpha / 2, n - 1)
  prueba_t <- list(
    nombre = "Prueba t de media cero", H0 = "E[e] = 0", H1 = "E[e] != 0",
    formula = "t = mean(e)/(s/sqrt(n))", distribucion = sprintf("t con %d gl", n - 1),
    region = sprintf("|t| > %.3f", cr_t), obs = t_obs,
    p = 2 * pt(-abs(t_obs), n - 1), rechaza = abs(t_obs) > cr_t)
  
  # Ljung-Box con chi2_m
  r  <- acf_muestral_m(e, m)
  Q  <- n * (n + 2) * sum(r^2 / (n - 1:m))
  cr_q <- qchisq(1 - alpha, m)
  prueba_lb <- list(
    nombre = sprintf("Ljung-Box (m = %d)", m),
    H0 = sprintf("rho_1 = ... = rho_%d = 0", m), H1 = "algun rho_k != 0",
    formula = "Q = n(n+2) sum_k r_k^2/(n-k)", distribucion = sprintf("chi^2 con %d gl", m),
    region = sprintf("Q > %.3f", cr_q), obs = Q,
    p = pchisq(Q, m, lower.tail = FALSE), rechaza = Q > cr_q)
  
  # Jarque-Bera
  ec <- e - mean(e); m2 <- mean(ec^2)
  S  <- mean(ec^3) / m2^1.5
  K  <- mean(ec^4) / m2^2
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  cr_j <- qchisq(1 - alpha, 2)
  prueba_jb <- list(
    nombre = "Jarque-Bera", H0 = "asimetria = 0 y curtosis = 3 (normalidad)",
    H1 = "no se cumple al menos una",
    formula = "JB = (n/6)(S^2 + (K-3)^2/4)", distribucion = "chi^2 con 2 gl",
    region = sprintf("JB > %.3f", cr_j), obs = JB,
    p = pchisq(JB, 2, lower.tail = FALSE), rechaza = JB > cr_j)
  
  # Durbin-Watson como diagnostico (se lee con d ~ 2(1 - r1))
  d  <- sum(diff(e)^2) / sum(e^2)
  dw <- list(d = d, r1 = r[1], aprox_2_1_menos_r1 = 2 * (1 - r[1]), n = n)
  
  list(t = prueba_t, lb = prueba_lb, jb = prueba_jb, dw = dw,
       asimetria = S, curtosis = K)
}

# Tabla por horizonte: todos los metodos sobre los mismos origenes,
# ordenada de menor a mayor RMSE, con RMSE relativo al referente.
tabla_origen_movil <- function(y, ajustadores, n0, h, referente) {
  stopifnot(is.list(ajustadores), referente %in% names(ajustadores))
  filas <- lapply(names(ajustadores), function(nm) {
    om <- origen_movil(y, ajustadores[[nm]], n0, h)
    cbind(data.frame(metodo = nm, n_origenes = nrow(om), stringsAsFactors = FALSE),
          medidas(om$error, om$y_real))
  })
  tab <- do.call(rbind, filas)
  tab$RMSE_rel_referente <- tab$RMSE / tab$RMSE[tab$metodo == referente]
  tab <- tab[order(tab$RMSE), ]
  rownames(tab) <- NULL
  tab
}

