
#---------------f estacional--------------------#
# Prueba F de igualdad de medias por estación (ANOVA de un factor, a mano).
# x: serie (o serie sin tendencia, con NA permitidos); pos: estación de cada dato (1..s).
f_estacional <- function(x, pos, s, nivel = 0.05) {
  if (length(x) != length(pos)) stop("x y pos deben tener la misma longitud")
  ok  <- !is.na(x)
  x   <- x[ok]; pos <- pos[ok]
  N   <- length(x)
  m   <- mean(x)
  mj  <- tapply(x, pos, mean)
  nj  <- tapply(x, pos, length)
  
  SSB <- sum(nj * (mj - m)^2)                       # entre estaciones
  SSW <- sum((x - mj[as.character(pos)])^2)         # dentro de estaciones
  gl1 <- s - 1
  gl2 <- N - s
  F   <- (SSB / gl1) / (SSW / gl2)
  
  list(F = F, gl1 = gl1, gl2 = gl2,
       critico = qf(1 - nivel, gl1, gl2),
       p = pf(F, gl1, gl2, lower.tail = FALSE),
       medias = mj, SSB = SSB, SSW = SSW)
}


#-----------------ACF muestral ---------------------------#

acf_muestral <- function(y, m) {
  if (!is.numeric(y)) stop("y debe ser numérico")
  if (anyNA(y)) stop("y no debe tener NA")
  n <- length(y)
  if (m >= n) stop("m debe ser menor que n")
  yc  <- y - mean(y)
  g0  <- sum(yc^2) / n
  r   <- vapply(0:m, function(k) sum(yc[(k + 1):n] * yc[1:(n - k)]) / n / g0,
                numeric(1))
  names(r) <- 0:m
  r
}

#------------------------Comparar semestre----------------------------#
comparar_semestres <- function(y, tipo = "aditiva") {
  d12 <- descomposicion_clasica(y, 12, tipo)
  d6  <- descomposicion_clasica(y, 6,  tipo)
  idx <- d12$indices
  
  comunes <- !is.na(d12$residuo) & !is.na(d6$residuo)   # mismos datos en ambas
  list(indices_12       = idx,
       dif_semestres    = idx[1:6] - idx[7:12],
       amplitud_indices = max(idx) - min(idx),
       sd_residuo_12    = sd(d12$residuo[comunes]),
       sd_residuo_6     = sd(d6$residuo[comunes]),
       n_comunes        = sum(comunes))
}

#----------------Comparar tipo-------------------#

comparar_tipo <- function(y, s) {
  a <- descomposicion_clasica(y, s, "aditiva")
  m <- descomposicion_clasica(y, s, "multiplicativa")
  c(sd_rel_aditiva        = sd(a$residuo / a$tendencia, na.rm = TRUE),
    sd_rel_multiplicativa = sd(m$residuo - 1, na.rm = TRUE),
    cor_nivel_amplitud    = cor(a$tendencia, abs(y - a$tendencia),
                                use = "complete.obs"))
}


#----------------pacf_dl-------------------#

pacf_dl <- function(r){
  m   <- length(r)
  phi <- matrix(0, nrow = m, ncol = m)
  phi[1,1] <- r[1]
  
  if(m >= 2){
    for(k in 2:m){
      num <- r[k] - sum(phi[k-1, 1:(k-1)] * r[(k-1):1])
      den <- 1    - sum(phi[k-1, 1:(k-1)] * r[1:(k-1)])
      phi[k,k] <- num / den
      
      if(k > 1){
        for(j in 1:(k-1)){
          phi[k,j] <- phi[k-1,j] - phi[k,k] * phi[k-1,k-j]
        }
      }
    }
  }
  diag(phi)   # phi_{11}, phi_{22}, ..., phi_{mm}
}

#----------------ljung box-------------------#

ljung_box <- function(r, T_obs, m, p = 0) {
  
  # r = (acf muestral, valores, errores), T_obs= total de observaciones
  # m = periodo estacional p = parametros a ajustar si es sobre un modelo
  
  # Estadistico Qm
  h     <- 1:m
  Qm    <- T_obs * (T_obs + 2) * sum(r[h]^2 / (T_obs - h))
  
  # Grados de libertad
  df    <- m - p
  
  # Valor critico al 5%
  cv    <- qchisq(0.95, df) # pchisq= da un cuantil especifico de la chicuadrado con df grados de libertas
  
  # Valor p
  pval  <- 1 - pchisq(Qm, df)
  
  #resultado como tabla
  tibble::tibble(
    estadistico  = round(Qm,   4),
    df           = df,
    valor_critico_5pct = round(cv, 4),
    p_value      = pval,
    rechazo_H0   = pval < 0.05
  )
}

# bandas
banda_ruido_blanco <- function(n){
  1.96 / sqrt(n)
}

banda_bartlett <- function(r, n){
  m <- length(r)
  banda <- numeric(m)
  for(h in 1:m){
    suma <- if(h == 1) 0 else sum(r[1:(h-1)]^2)
    banda[h] <- 1.96 * sqrt((1 + 2*suma) / n)
  }
  banda
}

graficar_bandas <- function(r, pacf_vals, n, m, titulo_extra = ""){
  banda_rb   <- banda_ruido_blanco(n)
  banda_bart <- banda_bartlett(r, n)
  
  datos_acf  <- tibble(rezago = 1:m, valor = r,         banda_bart = banda_bart)
  datos_pacf <- tibble(rezago = 1:m, valor = pacf_vals, banda_bart = banda_bart)
  
  g_acf <- ggplot(datos_acf, aes(x = rezago, y = valor)) +
    geom_segment(aes(xend = rezago, yend = 0)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept =  banda_rb, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = -banda_rb, linetype = "dashed", color = "blue") +
    geom_line(aes(y =  banda_bart), color = "red", linetype = "dotted") +
    geom_line(aes(y = -banda_bart), color = "red", linetype = "dotted") +
    labs(title = paste("ACF", titulo_extra), x = "Rezago", y = "ACF") +
    theme_minimal()
  
  g_pacf <- ggplot(datos_pacf, aes(x = rezago, y = valor)) +
    geom_segment(aes(xend = rezago, yend = 0)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept =  banda_rb, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = -banda_rb, linetype = "dashed", color = "blue") +
    labs(title = paste("PACF", titulo_extra), x = "Rezago", y = "PACF") +
    theme_minimal()
  
  g_acf / g_pacf
}

#----------------varianza a largo-------------------#

varianza_largo_plazo <- function(y, L){
  n    <- length(y)
  ybar <- mean(y)
  
  gamma_j <- function(j){
    sum((y[(j+1):n] - ybar) * (y[1:(n-j)] - ybar)) / n
  }
  
  gamma0 <- sum((y - ybar)^2) / n
  
  suma <- 0
  if(L >= 1){
    for(j in 1:L){
      w    <- 1 - j/(L + 1)     # núcleo de Bartlett
      suma <- suma + w * gamma_j(j)
    }
  }
  
  gamma0 + 2*suma
}