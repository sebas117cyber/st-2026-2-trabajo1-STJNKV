# serie 3

serie_3 <- leer_serie("datos/STJNKV-serie-3.csv", "CORPALMAS acueducto", "m³/s")

#----ficha----

ficha_st3 <- crear_ficha(serie_3, "Caudal medio mensual de un río andino", "m³/s")

tabla_ficha3 <- kable(ficha_st3, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 3") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha3, file = "figs/ficha_serie3.png")

remove(tabla_ficha3, ficha_st3)



#---- tabla----

T_3 <- nrow(serie_3)
frecuencia_3 <- attr(serie_3, "frecuencia")
unidad_3     <- attr(serie_3, "unidad")
fuente_3     <- attr(serie_3, "fuente")

# Regla de partición (ajustar según lo pedido en el enunciado)
n0_3 <- floor(0.8 * T_3)          
H_3  <- frecuencia_3              

origenes_h1_3 <- T_3 - n0_3
origenes_H_3  <- T_3 - n0_3 - H_3 + 1

# Fechas que delimitan cada tramo
fecha_inicio_3 <- serie_3$fecha[1]
fecha_corte_3  <- serie_3$fecha[n0_3]
fecha_fin_3    <- serie_3$fecha[T_3]

tabla_serie3 <- tibble(
  Serie              = "Serie 3",
  Unidad             = unidad_3,
  `T`                = T_3,
  n0                 = n0_3,
  H                  = H_3,
  `Orígenes (h=1)`   = origenes_h1_3,
  `Orígenes (H)`     = origenes_H_3,
  `Fecha inicio`     = format(fecha_inicio_3, "%Y-%m-%d"),
  `Fecha corte (n0)` = format(fecha_corte_3, "%Y-%m-%d"),
  `Fecha fin`        = format(fecha_fin_3, "%Y-%m-%d")
)


tabla_Rs3 <- kable(tabla_serie3, booktabs = TRUE, align = "c",
                   caption = "Partición de la Serie 3: $n_0$, $H$, orígenes y fechas de corte") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  row_spec(nrow(tabla_serie3), extra_latex_after = "\\bottomrule")

save_kable(tabla_Rs3, file = "figs/tablaR_serie3.png")

remove(T_3, frecuencia_3, unidad_3, fuente_3, n0_3, H_3, origenes_h1_3, origenes_H_3, 
       fecha_inicio_3, fecha_corte_3, fecha_fin_3, tabla_serie3, tabla_Rs3)



corre_serie3 <- correlograma(serie_3)




# operador 





#---- grafico ----




#---- operacionde rezago ----

op_Rs6_serie3 <- diferenciar(serie_3$y, 1, 1, 6)

op_Rs12_serie3 <- diferenciar(serie_3$y, 1, 1, 12)



#---- coeficientes de polinomio ----

# estacionalidad 6

w_s3  <- op_Rs12_serie3
n_s3  <- length(w_s3)
w_cs3 <- w_s3 - mean(w_s3)        # centrar la serie



gamma0_s3 <- sum(w_cs3 * w_cs3) / n_s3
gamma1_s3 <- sum(w_cs3[1:(n_s3-1)] * w_cs3[2:n_s3]) / n_s3
gamma2_s3 <- sum(w_cs3[1:(n_s3-2)] * w_cs3[3:n_s3]) / n_s3
# sistema lineal series 3

Gamma_s3 <- matrix(c(gamma0_s3, gamma1_s3,
                     gamma1_s3, gamma0_s3), nrow = 2, byrow = TRUE)
g_s3 <- c(gamma1_s3, gamma2_s3)


coef_s3 <- solve(Gamma_s3, g_s3)

# solucion del sistema:

coef_s1 <- solve(Gamma_s1, g_s1)


# estacionalidad 12

