# operadores 

# (1-B)^d (1-B^s)^D
diferenciar <- function(y, d, D, s){
  
  vals <- y
  # estacional
  if (d > 0){
    for (i in seq_len(d)){
      t <- length(vals)
      vals <- vals[-1] - vals[-t]
    }
    
  }
  # con secuencias s
  if (D > 0){
    for (i in seq_len(D)){
      t <- length(vals)
      vals <- vals[(s+1):t] - vals[1:(t-s)]
      
    }
    
  }
  vals
}

# funcion de polinomio

polinomio_rezago <- function(coef) {
  poly <- c(1, coef)          # coeficiente independiente = 1
  raices <- polyroot(poly)    # resuelve el polinomio para encontrar raices
  
  tibble::tibble(
    raiz   = raices,
    modulo = Mod(raices)      # modulos de las raizes
  )
}


# elegir_d <- function(y, d_max = 2) {
#   vars <- purrr::map_dbl(0:d_max, ~ var(diferenciar(y, d = .x, D = 0, s = 1)))
#   tibble::tibble(d = 0:d_max, varianza = vars) |>
#     dplyr::mutate(optimo = varianza == min(varianza))
# }
# 
# elegir_D <- function(y, s, D_max = 1) {
#   vars <- purrr::map_dbl(0:D_max, ~ var(diferenciar(y, d = 0, D = .x, s = s)))
#   tibble::tibble(D = 0:D_max, varianza = vars) |>
#     dplyr::mutate(optimo = varianza == min(varianza))
# }




#-----------Media Movil centrada------------------------#

media_movil_centrada <- function(y, s) {
  #Y: Valores de la serie
  #S. Frecuecnia de la serie 
  if (!is.numeric(y)) stop("y debe ser numérico")
  if (length(s) != 1 || s < 2 || s != round(s)) stop("s debe ser un entero >= 2")
  n <- length(y)
  if (n <= s) stop("la serie debe tener más de s observaciones")
  
  w <- if (s %% 2 == 1) rep(1 / s, s) else c(0.5, rep(1, s - 1), 0.5) / s
  k <- (length(w) - 1) / 2          # s=12 -> k=6 ; s=7 -> k=3
  
  tend <- rep(NA_real_, n)
  for (t in (k + 1):(n - k)) {
    tend[t] <- sum(w * y[(t - k):(t + k)])
  }
  tend
}

#--------------Descomposicion clasica-----------------------------#
descomposicion_clasica <- function(y, s, tipo = c("aditiva", "multiplicativa")) {
  tipo <- match.arg(tipo)
  if (!is.numeric(y)) stop("y debe ser numérico")
  n    <- length(y)
  tend <- media_movil_centrada(y, s)                 # NA en los extremos
  desv <- if (tipo == "aditiva") y - tend else y / tend
  pos  <- ((seq_len(n) - 1) %% s) + 1                # estación de cada dato
  
  idx <- vapply(seq_len(s), function(j) mean(desv[pos == j], na.rm = TRUE),
                numeric(1))
  # Normalización: aditiva suma 0, multiplicativa promedia 1
  idx <- if (tipo == "aditiva") idx - mean(idx) else idx / mean(idx)
  
  est   <- idx[pos]
  resid <- if (tipo == "aditiva") y - tend - est else y / (tend * est)
  
  list(tendencia = tend, indices = idx, estacional = est,
       residuo = resid, tipo = tipo, s = s)
}


