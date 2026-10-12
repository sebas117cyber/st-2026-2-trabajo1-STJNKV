# serie 2

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")

serie_2 <- leer_serie("datos/STJNKV-serie-2.csv", "estación de monitoreo del centro de Villa Clara", 
                      "µg/m³")

#----ficha----

ficha_st2 <- crear_ficha(serie_2, "concentracion de PM2.5  2025-02-03 a 2025-09-10")

tabla_ficha2 <- kable(ficha_st2, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 2") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha2, file = "figs/ficha_serie2.png")

remove(ficha_st2, tabla_ficha2)




# --- tabla 2 ----

T_2 <- nrow(serie_2)
frecuencia_2 <- attr(serie_2, "frecuencia")
unidad_2     <- attr(serie_2, "unidad")
fuente_2     <- attr(serie_2, "fuente")

# Regla de partición (ajustar según lo pedido en el enunciado)
n0_2 <- floor(0.8 * T_2)          # 80% entrenamiento
H_2  <- frecuencia_2              # horizonte en la frecuencia de la serie

origenes_h1_2 <- T_2 - n0_2
origenes_H_2  <- T_2 - n0_2 - H_2 + 1

# Fechas que delimitan cada tramo
fecha_inicio_2 <- serie_2$fecha[1]
fecha_corte_2  <- serie_2$fecha[n0_2]
fecha_fin_2    <- serie_2$fecha[T_2]

tabla_serie2 <- tibble(
  Serie              = "Serie 2",
  Unidad             = unidad_2,
  `T`                = T_2,
  n0                 = n0_2,
  H                  = H_2,
  `Orígenes (h=1)`   = origenes_h1_2,
  `Orígenes (H)`     = origenes_H_2,
  `Fecha inicio`     = format(fecha_inicio_2, "%Y-%m-%d"),
  `Fecha corte (n0)` = format(fecha_corte_2, "%Y-%m-%d"),
  `Fecha fin`        = format(fecha_fin_2, "%Y-%m-%d")
)


tabla_Rs2 <- kable(tabla_serie2, booktabs = TRUE, align = "c",
                   caption = "Partición de la Serie 2: $n_0$, $H$, orígenes y fechas de corte") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  row_spec(nrow(tabla_serie2), extra_latex_after = "\\bottomrule")

save_kable(tabla_Rs2, file = "figs/tablaR_serie2.png")

remove(T_2, frecuencia_2, unidad_2, fuente_2, n0_2, H_2, origenes_h1_2, origenes_H_2, 
       fecha_inicio_2, fecha_corte_2, fecha_fin_2, tabla_serie2, tabla_Rs2)



# ----grafico ----



serie2_graf <- graficar_serie(serie_2, "concentracion de PM2.5  2025-02-03 a 2025-09-10")

ggsave(
  "figs/grafico_serie2.png",
  plot = serie2_graf,
  width = 8,
  height = 5,
  dpi = 300
)

corre_serie2 <- correlograma(serie_2)

ggsave(
  "figs/correlograma_serie2.png",
  plot = corre_serie2$panel,
  width = 8,
  height = 5,
  dpi = 300
)

png(
  "figs/rezagos_serie2.png",
  width = 2400,
  height = 1500,
  res = 300
)

grafico_rezagos(
  serie_2$y,
  c(1, 2),
  unidad = "µg/m³",
  nombre = "Concentración diaria de PM2.5 en una estación de monitoreo"
)

dev.off()



#Item b
y2 <- serie_2$y

s2   <- 7
pos2 <- ((seq_along(y2) - 1) %% s2) + 1  # 1 = lunes, ..., 7 = domingo

# prueba F
f2 <- f_estacional(y2, pos2, s2)



r2 <- acf_muestral(y2, 24)
r2[c("7", "14", "21")]
1.96 / sqrt(length(y2))     
round(f2$medias, 2)
max(f2$medias) - min(f2$medias)
sd(y2)


# elegir_d(serie_2$y)
# 
# elegir_D(serie_2$y, s= 12, D_max = 1)


# -----operacion de rezago----
op_rezago_serie2 <- diferenciar(serie_2$y, 1, 0)


# polinomio 

w_s2 <- op_rezago_serie2

n_s2 <- length(w_s2)

w_cs2 <- w_s2 - mean(w_s2)


gamma0_s2 <- sum(w_s2 * w_s2) / n_s2
gamma1_s2 <- sum(w_s2[1:(n_s2-1)] * w_s2[2:n_s2]) / n_s2
gamma2_s2 <- sum(w_s2[1:(n_s2-2)] * w_s2[3:n_s2]) / n_s2

# sistema lineal series 1

Gamma_s2 <- matrix(c(gamma0_s2, gamma1_s2,
                     gamma1_s2, gamma0_s2), nrow = 2, byrow = TRUE)

g_s2 <- c(gamma1_s2, gamma2_s2)

# solucion del sistema:

coef_s2 <- solve(Gamma_s2, g_s2)

# raizes del sistema
polinomio_rezago(coef_s2)

raiz_s2P <- graficar_raices(coef_s2)

ggsave(
  "figs/raiz_s2P.png",
  plot = raiz_s2P,
  width = 8,
  height = 5,
  dpi = 300
)

