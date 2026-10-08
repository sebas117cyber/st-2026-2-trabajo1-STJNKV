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


# verificaciones
# 
# D <- 1
# s <- 4
# set.seed(2024)
# y <- rnorm(100, 2, 3)
# # caso 1
# mia_est   <- diferenciar(y, d = 0, D = D, s = s)
# base_est  <- diff(y, lag = s, differences = D)
# 
# cat("Diferencia estacional (D =", D, ", s =", s, ") coincide:",
#     all.equal(mia_est, base_est), "\n")
# 
# # caso 2
# d <- 1; D <- 1; s <- 4
# mia_comb  <- diferenciar(y, d = d, D = D, s = s)
# base_comb <- diff(diff(y, lag = s, differences = D), differences = d)
# 
# cat("Combinación (d =", d, ", D =", D, ", s =", s, ") coincide:",
#     all.equal(mia_comb, base_comb), "\n")
# 
# # polinomio
# # c(B) = 1 - 0.5B + 0.25B^2
# coef <- c(-0.5, 0.25)
# print(polinomio_rezago(coef))
# 
