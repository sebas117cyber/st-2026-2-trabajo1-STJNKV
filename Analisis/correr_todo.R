
archivos_r <- list.files("R", pattern = "\\.R$", full.names = TRUE)

# 2. Cargar (source) cada uno de los scripts
sapply(archivos_r, source)


source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")
source("R/04-metodos.R")
source("R/05-evaluacion.R")

serie_1 <- leer_serie("datos/STJNKV-serie-1.csv", "CORPALMAS acueducto", "miles m³")



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

#save_kable(tabla_Rs1, file = "figs/tablaR_serie1.png")

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


graficar_serie(serie_1log, "consumo agua mensual log ")

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

w_s1 <- diferenciar(serie_1$y, 1, 0)

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

coef_s1 <- c(-1) # (1-B)

coef_s1 <- solve(Gamma_s1, g_s1)



# raizes del sistema
polinomio_rezago(coef_s1)



raiz_s1P <- graficar_raices(coef_s1)

ggsave(
  "figs/raiz_s1P.png",
  plot = raiz_s1P,
  width = 8,
  height = 5,
  dpi = 300
)

# pronosticos en el qmd

serie_2 <- leer_serie("datos/STJNKV-serie-2.csv", "estación de monitoreo del centro de Villa Clara", 
                      "µg/m³")


# --- tabla 2 ----

T_2 <- nrow(serie_2)
frecuencia_2 <- attr(serie_2, "frecuencia")
unidad_2     <- attr(serie_2, "unidad")
fuente_2     <- attr(serie_2, "fuente")

# Regla de partición (ajustar según lo pedido en el enunciado)
n0_2 <- floor(0.8 * T_2)          # 80% entrenamiento
H_2  <- 7              # horizonte en la frecuencia de la serie

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



r2 <- acf_muestral_m(y2, 24)
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


gamma0_s2 <- sum(w_cs2 * w_cs2) / n_s2
gamma1_s2 <- sum(w_cs2[1:(n_s2-1)] * w_cs2[2:n_s2]) / n_s2
gamma2_s2 <- sum(w_cs2[1:(n_s2-2)] * w_cs2[3:n_s2]) / n_s2

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


serie_3 <- leer_serie("datos/STJNKV-serie-3.csv", "CORPALMAS acueducto", "m³/s")


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





#---- 3 grafico ----


grafico_serie3 <- graficar_serie(serie_3,"Medición del caudal medio mensual")

# Guardar gráfico
ggsave(
  "figs/grafico_serie3.png",
  plot = grafico_serie3,
  width = 8,
  height = 5,
  dpi = 300
)

corre_serie3 <- correlograma(serie_3,NULL)

# Guardar gráfico
ggsave(
  "figs/correlograma_serie3.png",
  plot = corre_serie3$panel,
  width = 8,
  height = 5,
  dpi = 300
)


png(
  "figs/rezagos_serie03.png",
  width = 2400,
  height = 1500,
  res = 300
)

grafico_rezagos(
  serie_3$y,
  c(1, 2, 3, 4, 5, 6, 8, 9, 12 ),
  unidad = "m³/s",
  nombre = "Medición del caudal medio mensual"
)

dev.off()

#Item b 

#Extraemos los valores de la serie y la frecuencia 
y3 <- serie_3$y
s3 <- attr(serie_3, "frecuencia")  

#Asignamos a cada observacion su posicion
pos3 <- ((seq_along(y3) - 1) %% s3) + 1        # 1 = enero, ..., 12 = diciembre
# Evaluar si existen diferencias significativas entre las medias mensuales
f3   <- f_estacional(y3, pos3, s3)

# Separar la serie en tendencia, componente estacional y componente irregular
dec3 <- descomposicion_clasica(y3, 12, "aditiva")

# Comparar el comportamiento de los dos semestres mediante el modelo aditivo
cs <- comparar_semestres(y3, "aditiva")
round(cs$dif_semestres, 3)
cs$amplitud_indices
c(sd_res_12 = cs$sd_residuo_12, sd_res_6 = cs$sd_residuo_6)
cs$n_comunes          

# Asignar a cada observación el año al que pertenece
anio3 <- ((seq_along(y3) - 1) %/% 12) + 1
rango <- tapply(y3, anio3, function(v) max(v) - min(v))
media <- tapply(y3, anio3, mean)

# Comparar los tipos de descomposición para evaluar el comportamiento
comparar_tipo(y3, 12)
cor(media, rango)
round(rango, 1)


# Guardar el gráfico
png("figs/caudal_medio_mensual_por_anio.png",
    width = 1200, height = 800, res = 150)

grafico_estacional(y3, 12, "m³/s", "Caudal medio mensual por año")

dev.off()

#Guardar grafico 
dec3 <- descomposicion_clasica(y3, 12, "aditiva")
g_dec3 <- grafico_descomposicion(dec3, y3, "m³/s")

ggsave(
  filename = "figs/descomposicion_serie3.png",
  plot = g_dec3,
  width = 10,
  height = 8,
  dpi = 300
)





#---- operacionde rezago ----



# op_Rs6_serie3 <- diferenciar(serie_3$y, 0, 1, 6)

op_Rs12_serie3 <- diferenciar(serie_3$y, 0, 1, 12)


##----verificacion de tendencia----

y3 <- serie_3$y
t  <- seq_along(y3)


mitad <- length(y3) %/% 2
var_primera_mitad  <- sum((y3[1:mitad] - mean(y3[1:mitad]))^2) / mitad
var_segunda_mitad  <- sum((y3[(mitad+1):length(y3)] - mean(y3[(mitad+1):length(y3)]))^2) / (length(y3)-mitad)

cat("Varianza 1ra mitad:", var_primera_mitad, "\n")
cat("Varianza 2da mitad:", var_segunda_mitad, "\n")


n <- length(y3)
media_y_t    <- mean(y3[13:n])
media_y_t12  <- mean(y3[1:(n-12)])

cat("Media y_t:", media_y_t, "\n")
cat("Media y_(t-12):", media_y_t12, "\n")
cat("Diferencia de medias:", media_y_t - media_y_t12, "\n")



n  <- length(y3)
mes <- rep(1:12, length.out = n)

var_por_mes <- tapply(y3, mes, function(x) sum((x-mean(x))^2)/length(x))
var_por_mes




#---- coeficientes de polinomio ----



op_Rs12_serie3 <- diferenciar(serie_3$y, 0, 1, 12)

w12_s3  <- serie_3$y
n12_s3  <- length(w12_s3)
w12_cs3 <- w12_s3 - mean(w12_s3)        # centrar la serie



gamma0_s3_12 <- sum(w12_cs3 * w12_cs3) / n12_s3
gamma1_s3_12 <- sum(w12_cs3[1:(n12_s3-12)] * w12_cs3[13:n12_s3]) / n12_s3 # gamma depende de s
gamma2_s3_12 <- sum(w12_cs3[1:(n12_s3-13)] * w12_cs3[14:n12_s3]) / n12_s3


#----sistema lineal series 3----

Gamma_s3_12 <- matrix(c(gamma0_s3_12, gamma1_s3_12,
                        gamma1_s3_12, gamma0_s3_12), nrow = 2, byrow = TRUE)
g_s3_12 <- c(gamma1_s3_12, gamma2_s3_12)

# solucion diferenciada
#coef_s3_12 <- c(rep(0, 11), -1)   

coef_s3_12 <- solve(Gamma_s3_12, g_s3_12)

polinomio_rezago(coef_s3_12)

#----indentidad gamma1 / gamma0 = r1----

r1_s3 <- gamma1_s3_12 / gamma0_s3_12
r1_s3

var_s3 <- 2*gamma0_s3_12 - 2 * gamma1_s3_12

var_s3

var(serie_3$y)


serie3_s12 <- serie_3 |> 
  rename(valores = y) |> 
  slice(1:(n()-12)) |> 
  mutate(y = op_Rs12_serie3)

graficar_serie(serie3_s12, "serie diferenciada s=12")


raiz_s3P <- graficar_raices(coef_s3_12, "Raizes en el plano unitario serie 3")

ggsave(
  "figs/raiz_s3P.png",
  plot = raiz_s3P,
  width = 8,
  height = 5,
  dpi = 300
)

