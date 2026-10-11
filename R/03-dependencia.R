
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
