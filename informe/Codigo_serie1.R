# serie 1

serie_1 <- leer_serie("datos/STJNKV-serie-1.csv", "CORPALMAS acueducto", "miles m³")


# ficha tecnica
crear_ficha <- function(serie, nombre = "Serie 1", unidad = attr(serie, "unidad")) {
  
  freq <- attr(serie, "frecuencia")
  if (is.null(freq)) freq <- 1
  
  frecuencia_label <- switch(as.character(freq),
                             "12"  = "Mensual",
                             "4"   = "Trimestral",
                             "1"   = "Anual",
                             "52"  = "Semanal",
                             "365" = "Diaria",
                             as.character(freq))
  
  fecha_ini <- as.character(min(serie$fecha, na.rm = TRUE))
  fecha_fin <- as.character(max(serie$fecha, na.rm = TRUE))
  val       <- serie$y
  
  tibble(
    Característica = c("Nombre", "Unidad", "Frecuencia",
                       "Fecha inicial", "Fecha final",
                       "T (observaciones)", "Mínimo",
                       "Máximo", "Media"),
    Valor = c(as.character(nombre),
              as.character(if (is.null(unidad)) "sin unidad" else unidad),
              frecuencia_label,
              fecha_ini,
              fecha_fin,
              as.character(length(val)),
              as.character(round(min(val, na.rm = TRUE), 2)),
              as.character(round(max(val, na.rm = TRUE), 2)),
              as.character(round(mean(val, na.rm = TRUE), 2)))
  )
}

ficha_st1 <- crear_ficha(serie_1, "consumo mensual de agua potable 2011-2025", "m³")

tabla_ficha1 <- kable(ficha_st1, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 1") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha1, file = "figs/ficha_serie1.png")

remove(tabla_ficha1, ficha_st1)

# tabla

T_1 <- nrow(serie_1)
frecuencia_1 <- attr(serie_1, "frecuencia")
unidad_1     <- attr(serie_1, "unidad")
fuente_1     <- attr(serie_1, "fuente")

# Regla de partición (ajustar según lo pedido en el enunciado)
n0_1 <- floor(0.8 * T_1)          # 80% entrenamiento
H_1  <- frecuencia_1              # horizonte en la frecuencia de la serie

origenes_h1_1 <- T_1 - n0_1
origenes_H_1  <- T_1 - n0_1 - H_1 + 1

# Fechas que delimitan cada tramo
fecha_inicio_1 <- serie_1$fecha[1]
fecha_corte_1  <- serie_1$fecha[n0_1]
fecha_fin_1    <- serie_1$fecha[T_1]

tabla_serie1 <- tibble(
  Serie              = "Serie 1",
  Unidad             = unidad_1,
  `T`                = T_1,
  n0                 = n0_1,
  H                  = H_1,
  `Orígenes (h=1)`   = origenes_h1_1,
  `Orígenes (H)`     = origenes_H_1,
  `Fecha inicio`     = format(fecha_inicio_1, "%Y-%m-%d"),
  `Fecha corte (n0)` = format(fecha_corte_1, "%Y-%m-%d"),
  `Fecha fin`        = format(fecha_fin_1, "%Y-%m-%d")
)


tabla_Rs1 <- kable(tabla_serie1, booktabs = TRUE, align = "c",
      caption = "Partición de la Serie 1: $n_0$, $H$, orígenes y fechas de corte") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  row_spec(nrow(tabla_serie1), extra_latex_after = "\\bottomrule")

save_kable(tabla_Rs1, file = "figs/tablaR_serie1.png")

remove(T_1, frecuencia_1, unidad_1, fuente_1, n0_1, H_1, origenes_h1_1, origenes_H_1, 
       fecha_inicio_1, fecha_corte_1, fecha_fin_1, tabla_serie1, tabla_Rs1)

# grafico

serie1_graf <- graficar_serie(serie_1, "consumo mensual de agua potable 2011-2025")

corre_serie1 <- correlograma(serie_1)

# reza_serie1 <- corre_serie1$acf


# serie con los valores transformados en log
serie_1log <- serie_1 |> 
  mutate(y = log(y))


graficar_serie(serie_1log, "consumo mensual de agua ")

# operaciones de rezago

# elegir_d(serie_1log$y)
# 
# elegir_D(serie_1log$y, s= 12, D_max = 5)

# (1-B) (1-B^12)
op_rezago_serie1 <- diferenciar(serie_1log$y, 1, 0)


serie_1dif <- serie_1log |> 
  rename(valores = y) |> 
  slice(1:(n()-1)) |> 
  mutate(y = op_rezago_serie1)

graficar_serie(serie_1dif, "serie diferenciada")


# polinomio

# autocovarianza serie 1 diferenciada

w_s1 <- op_rezago_serie1

n_s1 <- length(w_s1)

w_cs1 <- w_s1 - mean(w_s1)


gamma0_s1 <- sum(w_cs1 * w_cs1) / n_s1
gamma1_s1 <- sum(w_cs1[1:(n_s1-1)] * w_cs1[2:n_s1]) / n_s1
gamma2_s1 <- sum(w_cs1[1:(n_s1-2)] * w_cs1[3:n_s1]) / n_s1

# sistema lineal series 1

Gamma_s1 <- matrix(c(gamma0_s1, gamma1_s1,
                  gamma1_s1, gamma0_s1), nrow = 2, byrow = TRUE)
g_s1 <- c(gamma1_s1, gamma2_s1)

# solucion del sistema:

coef_s1 <- solve(Gamma_s1, g_s1)

# raizes del sistema
polinomio_rezago(coef_s1)





