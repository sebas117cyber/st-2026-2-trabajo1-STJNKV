# serie 1

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")


serie_1 <- leer_serie("datos/STJNKV-serie-1.csv", "CORPALMAS acueducto", "miles m³")


# ----ficha tecnica-----


ficha_st1 <- crear_ficha(serie_1, "consumo mensual de agua potable 2011-2025", "m³")

tabla_ficha1 <- kable(ficha_st1, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 1") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha1, file = "figs/ficha_serie1.png")

remove(tabla_ficha1, ficha_st1)


#----- tabla------

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




#----graficos---#

grafico_serie1 <- graficar_serie(serie_1, "consumo mensual de agua potable 2011-2025")

# Guardar gráfico
ggsave(
  "figs/grafico_serie1.png",
  plot = grafico_serie1,
  width = 8,
  height = 5,
  dpi = 300
)


corre_serie1 <- correlograma(serie_1)


ggsave(
  "figs/correlograma_serie1.png",
  plot = corre_serie1$panel,
  width = 8,
  height = 5,
  dpi = 300
)


png(
  "figs/rezagos_serie.png",
  width = 2400,
  height = 1500,
  res = 300
)

#grafico de los rezagos 
grafico_rezagos(
  serie_1$y,
  c(1, 12, 24),
  unidad = "miles de m³",
  nombre = "Gráfico de rezagos - Serie 1"
)

dev.off()

# Frecuencia de la serie 1
frecuencia_serie1 <- attr(serie_1, "frecuencia")
y1 <- serie_1$y

# Quitar la tendencia
tend1 <- media_movil_centrada(y1, frecuencia_serie1)
sin_tend1 <- y1 - tend1

# Posición dentro del ciclo estacional
pos1 <- ((seq_along(y1) - 1) %% frecuencia_serie1) + 1

# Prueba de estacionalidad
f1 <- f_estacional(sin_tend1, pos1, frecuencia_serie1)

# Guardar los resultados
resultado_estacionalidad_1 <- list(
  F = f1$F,
  critico = f1$critico,
  p = f1$p,
  gl1 = f1$gl1,
  gl2 = f1$gl2
)

#Grafico de la serie despues de quitar la tendencia 
png(
  filename = "figs/serie1_sin_tendencia.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  sin_tend1,
  type = "l",
  main = "Consumo mensual de agua potable sin tendencia",
  xlab = "Tiempo",
  ylab = "Valor sin tendencia (miles de m³)"
)

abline(h = 0, lty = 2)

dev.off()




# serie con los valores transformados en log
serie_1log <- serie_1 |> 
  mutate(y = log(y))


graficar_serie(serie_1log, "consumo mensual de agua ")

#----operaciones de rezago----

# elegir_d(serie_1log$y)
# 
# elegir_D(serie_1log$y, s= 12, D_max = 5)

# (1-B)
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



graficar_raices(coef_s1)


