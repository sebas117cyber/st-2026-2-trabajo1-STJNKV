# serie 3


source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")

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



#---- coeficientes de polinomio ----



op_Rs12_serie3 <- diferenciar(serie_3$y, 0, 1, 12)

w12_s3  <- serie_3$y
n12_s3  <- length(w12_s3)
w12_cs3 <- w12_s3 - mean(w12_s3)        # centrar la serie



gamma0_s3_12 <- sum(w12_cs3 * w12_cs3) / n12_s3
gamma1_s3_12 <- sum(w12_cs3[1:(n12_s3-12)] * w12_cs3[13:n12_s3]) / n12_s3 # gamma depende de s
gamma2_s3_12 <- sum(w12_cs3[1:(n12_s3-13)] * w12_cs3[14:n12_s3]) / n12_s3

#----indentidad gamma1 / gamma0 = r1----

r1_s3 <- gamma1_s3_12 / gamma0_s3_12
r1_s3

var_s3 <- 2*gamma0_s3_12 - 2 * gamma1_s3_12

var_s3

var(op_Rs12_serie3)

# #----verificacion de tendencia----
# 
# y3 <- serie_3$y
# t  <- seq_along(y3)
# 
# 
# mitad <- length(y3) %/% 2
# var_primera_mitad  <- sum((y3[1:mitad] - mean(y3[1:mitad]))^2) / mitad
# var_segunda_mitad  <- sum((y3[(mitad+1):length(y3)] - mean(y3[(mitad+1):length(y3)]))^2) / (length(y3)-mitad)
# 
# cat("Varianza 1ra mitad:", var_primera_mitad, "\n")
# cat("Varianza 2da mitad:", var_segunda_mitad, "\n")
# 
# 
# n <- length(y3)
# media_y_t    <- mean(y3[13:n])
# media_y_t12  <- mean(y3[1:(n-12)])
# 
# cat("Media y_t:", media_y_t, "\n")
# cat("Media y_(t-12):", media_y_t12, "\n")
# cat("Diferencia de medias:", media_y_t - media_y_t12, "\n")
# 
# 
# 
# n  <- length(y3)
# mes <- rep(1:12, length.out = n)
# 
# var_por_mes <- tapply(y3, mes, function(x) sum((x-mean(x))^2)/length(x))
# var_por_mes
# 
# 

#----sistema lineal series 3----

Gamma_s3_12 <- matrix(c(gamma0_s3_12, gamma1_s3_12,
                     gamma1_s3_12, gamma0_s3_12), nrow = 2, byrow = TRUE)
g_s3_12 <- c(gamma1_s3_12, gamma2_s3_12)

# solucion diferenciada
coef_s3_12 <- c(rep(0, 11), -1)   

coef_s3_12 <- solve(Gamma_s3_12, g_s3_12)

polinomio_rezago(coef_s3_12)

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


#----punto 5----


