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



#----indentidad gamma1 / gamma0 = r1----

r1_s3 <- gamma1_s3_12 / gamma0_s3_12
r1_s3

var_s3 <- 2*gamma0_s3_12 - 2 * gamma1_s3_12

var_s3

var(serie_3$y)

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


z <- diff(serie_3$y, lag = 12)

# Estimación de coeficientes autoregresivos en la serie diferenciada
modelo <- arima(serie_3$y, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 0), period = 12))
coef(modelo)





#---- pronostico----




# ==== CHUNK 1: Lectura y parametros (dado por el usuario) ====

y_s3 <- serie_3$y

T3   <- length(y_s3)
n0_3 <- floor(0.8 * T3)
H3   <- 12

print(c(T = T3, n0 = n0_3, H = H3,
        origenes_h3 = length(n0_3:(T3 - 1)), origenes_H = length(n0_3:(T3 - H3))))
print(serie_3$fecha[c(1, n0_3, T3)])

# ==== FIN CHUNK 1 ====

# ==== CHUNK 2: Optimos (dado por el usuario) ====

opts3_hw <- optimizar(y_s3, "hw", expand.grid(alpha = seq(0.05, 0.95, by = 0.05),
                                              beta  = seq(0.05, 0.95, by = 0.05),
                                              gamma = seq(0.05, 0.95, by = 0.05)),
                      n0 = n0_3, s = 12)

opts3_hmulti <- optimizar(y_s3, "hw_mult", expand.grid(alpha = seq(0.05, 0.95, by = 0.05),
                                                       beta  = seq(0.05, 0.95, by = 0.05),
                                                       gamma = seq(0.05, 0.95, by = 0.05)),
                          n0 = n0_3, s = 12)

print(opts3_hw$optimo);     print(opts3_hmulti$optimo)
print(c(HW_adit = opts3_hw$inicio_comun, HW_multi = opts3_hmulti$inicio_comun))

# ==== FIN CHUNK 2 ====

# ==== CHUNK 3: Calculo rapido de los dos referentes "ingenuos" (inspeccion individual) ====

res_ingenuo_s3 <- ajustar_ingenuo(y_s3)
res_ing_est_s3 <- ajustar_ingenuo_estacional(y_s3, 12)

cat("Nivel final Ingenuo simple:    ", res_ingenuo_s3$Extra_muestral(1), "\n")
cat("Nivel final Ingenuo estacional:", res_ing_est_s3$Extra_muestral(1), "\n")

# ==== FIN CHUNK 3 ====



# ==== CHUNK 4: ITEM 6(c) - Metodos aplicados a la serie 3 (dado por el usuario) ====

ajust3 <- list(
  "Ingenuo estacional"     = function(v) ajustar_ingenuo_estacional(v, 12),
  "tendencia ficticias"    = function(v) ajustar_tendencia_estacional(v, 12),
  "Holt aditivo"           = function(v) ajustar_hw(v, 12, opts3_hw$optimo$alpha,
                                                    opts3_hw$optimo$beta,
                                                    opts3_hw$optimo$gamma),
  "ingenuo (referente)"    = ajustar_ingenuo,
  "Holt-multi(contraste)"  = function(v) ajustar_hw_mult(v, 12, opts3_hmulti$optimo$alpha,
                                                         opts3_hmulti$optimo$beta,
                                                         opts3_hmulti$optimo$gamma)
)


# ==== FIN CHUNK 4 ====


# ==== CHUNK 5: ITEM 6(c) - Origen movil h=1 y h=H, mismos origenes, ordenado por RMSE ====

# Conteos esperados calculados dinamicamente a partir del Chunk 1

n_oseg_h1 <- length(n0_3:(T3 - 1))
n_oseg_H  <- length(n0_3:(T3 - H3))

tab3_h1 <- tabla_origen_movil(y_s3, ajust3, n0_3, 1,  "Ingenuo estacional")
tab3_H  <- tabla_origen_movil(y_s3, ajust3, n0_3, H3, "Ingenuo estacional")

stopifnot(all(tab3_h1$n_origenes == n_oseg_h1), all(tab3_H$n_origenes == n_oseg_H))

cat("h = 1 (", n_oseg_h1, "origenes)\n");  print(tab3_h1, digits = 4)
cat("\nh =", H3, "(", n_oseg_H, "origenes)\n"); print(tab3_H, digits = 4)

# Error medio a h = H (signo del sesgo)
print(sapply(names(ajust3), function(nm) mean(origen_movil(y_s3, ajust3[[nm]], n0_3, H3)$error)))

# ==== FIN CHUNK 5 ====


# ==== CHUNK 6: ITEM 6(d) - Errores de origen movil a un paso del ganador de h=1 ====

ganador3_h1 <- tab3_h1$metodo[1]
print(ganador3_h1)

om3_1 <- origen_movil(y_s3, ajust3[[ganador3_h1]], n0_3, 1)
e3_1  <- om3_1$error

ggplot(data.frame(fecha = serie_3$fecha[om3_1$objetivo], error = e3_1), aes(fecha, error)) +
  geom_hline(yintercept = 0) + geom_line() + geom_point() +
  labs(title = paste("Errores de origen movil a un paso:", ganador3_h1),
       x = "Fecha", y = paste("Error (", attr(serie_3, "unidad"), ")"),
       caption = paste0("n = ", length(e3_1), " errores, h = 1")) +
  theme_minimal()

# ==== FIN CHUNK 6 ====

# ==== CHUNK 7: ITEM 6(d) - Correlograma de los errores (banda con n = numero de errores) ====

correlograma(data.frame(y = e3_1))$panel

# ==== FIN CHUNK 7 ====


# ==== CHUNK 8: ITEM 6(d) - t de media cero, Ljung-Box, Jarque-Bera y Durbin-Watson ====

val3_1 <- validar_errores(e3_1)
n_e3   <- length(e3_1); m_e3 <- min(floor(n_e3 / 4), 24)

cat(sprintf("t media cero (%d gl): t = %.3f, critico = %.3f, p = %.3g\n",
            n_e3 - 1, val3_1$t$obs, qt(0.975, n_e3 - 1), val3_1$t$p))
cat(sprintf("Ljung-Box (m = %d, %d gl): Q = %.3f, critico = %.3f, p = %.3g\n",
            m_e3, m_e3, val3_1$lb$obs, qchisq(0.95, m_e3), val3_1$lb$p))
cat(sprintf("Jarque-Bera (2 gl): JB = %.3f, critico = %.3f, p = %.3g\n",
            val3_1$jb$obs, qchisq(0.95, 2), val3_1$jb$p))
cat(sprintf("Durbin-Watson: d = %.3f, r1 = %.3f, 2(1-r1) = %.3f\n",
            val3_1$dw$d, val3_1$dw$r1, val3_1$dw$aprox_2_1_menos_r1))

# ==== FIN CHUNK 8 ====

# ==== CHUNK 9: ITEM 6(e) - Desviacion de errores de origen movil a cada horizonte h=1..H ====

sd3_h <- vapply(1:H3, function(h) sd(origen_movil(y_s3, ajust3[[ganador3_h1]], n0_3, h)$error),
                numeric(1))
print(round(sd3_h, 3))

# ==== FIN CHUNK 9 ====


# ==== CHUNK 10: ITEM 6(e) - Ganador reajustado sobre toda la serie y H pronosticos ====
# NOTA: serie_3 es mensual -> se usa by = "month"

pron3 <- ajust3[[ganador3_h1]](y_s3)$pronosticar(H3)
fut3  <- seq(max(serie_3$fecha), by = "month", length.out = H3 + 1)[-1]

df3_fc  <- data.frame(fecha = fut3, pron = pron3,
                      inf = pron3 - 1.96 * sd3_h, sup = pron3 + 1.96 * sd3_h)
df3_obs <- tail(data.frame(fecha = serie_3$fecha, y = y_s3), 36)   # ultimos 3 anios (ajustable)

print(df3_fc)

p_final3 <- ggplot() +
  geom_line(data = df3_obs, aes(fecha, y)) +
  geom_ribbon(data = df3_fc, aes(fecha, ymin = inf, ymax = sup), alpha = 0.25) +
  geom_line(data = df3_fc, aes(fecha, pron), colour = "red") +
  labs(title = paste("Serie 3: pronostico a", H3, "meses con", ganador3_h1),
       x = "Fecha", y = attr(serie_3, "unidad"),
       caption = "Banda heuristica: ±1.96 veces la desviacion de los errores de origen movil a cada horizonte; no es un intervalo de confianza.") +
  theme_minimal()

print(p_final3)

# ==== FIN CHUNK 10 ====
