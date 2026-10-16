# serie 2


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


#----pronostico----

# ==== CHUNK 1: Lectura y parametros (dado por el usuario) ====

y_s2 <- serie_2$y

T2   <- length(y_s2)
n0_2 <- floor(0.8 * T2)
H2   <- 7

print(c(T = T2, n0 = n0_2, H = H2,
        origenes_h2 = length(n0_2:(T2 - 1)), origenes_H = length(n0_2:(T2 - H2))))
print(serie_2$fecha[c(1, n0_2, T2)])

# ==== FIN CHUNK 1 ====


# ==== CHUNK 2: Optimos (dado por el usuario) ====

opts2_mm   <- optimizar(y_s2, "mm",   data.frame(k = 2:12), n0 = n0_2)
opts2_dmm  <- optimizar(y_s2, "dmm",  data.frame(k = 3:12), n0 = n0_2)
opts2_ses  <- optimizar(y_s2, "ses",  data.frame(alpha = seq(0.02, 0.98, by = 0.02)), n0 = n0_2)
opts2_holt <- optimizar(y_s2, "holt", expand.grid(alpha = seq(0.05, 0.95, by = 0.05),
                                                  beta  = seq(0.05, 0.95, by = 0.05)), n0 = n0_2)

print(opts2_mm$optimo);   print(opts2_dmm$optimo)
print(opts2_ses$optimo);  print(opts2_holt$optimo)
print(c(MM = opts2_mm$inicio_comun, DMM = opts2_dmm$inicio_comun,
        SES = opts2_ses$inicio_comun, Holt = opts2_holt$inicio_comun))

# ==== FIN CHUNK 2 ====


# ==== CHUNK 3: ITEM 6(a) - Curvas y mapa de MSE (dado por el usuario) ====

(graficar_mse_1d(opts2_mm,  "k",     "MSE de un paso: media movil") |
   graficar_mse_1d(opts2_dmm, "k",     "MSE de un paso: doble media movil")) /
  (graficar_mse_1d(opts2_ses, "alpha", "MSE de un paso: SES") |
     graficar_mse_holt(opts2_holt,       "MSE de un paso: Holt"))

# ==== FIN CHUNK 3 ====


# ==== CHUNK 4: ITEM 6(b) - Tendencias por minimos cuadrados (serie completa) ====


tipos_s2 <- c("lineal", "cuadratica", "exponencial")  # metodos de contraste (no aplican por diagnostico)
fits_s2  <- setNames(lapply(tipos_s2, function(tp) ajustar_tendencia(y_s2, tp)), tipos_s2)

resid_reg_s2 <- list(
  lineal      = fits_s2$lineal$residuales,
  cuadratica  = fits_s2$cuadratica$residuales,
  exponencial = log(y_s2) - log(fits_s2$exponencial$ajustados))   # exponencial en escala log

for (tp in tipos_s2) {
  cat("\n--- Tendencia", tp, "(exponencial: coeficientes en escala log) ---\n")
  print(fits_s2[[tp]]$tabla, digits = 4)
  cat("t critico (5%, gl = n - k):",
      round(qt(0.975, T2 - nrow(fits_s2[[tp]]$tabla)), 3), "\n")   
  cat("R2:", round(fits_s2[[tp]]$r2, 4), "\n")
}

# ==== FIN CHUNK 4 ====

# ==== CHUNK 5: Correlogramas de tendencias (dado por el usuario) ====
# ITEM 6(b) - Residuos: correlograma, Ljung-Box (chi2_m), Jarque-Bera, Durbin-Watson

for (tp in tipos_s2) {
  e <- resid_reg_s2[[tp]]
  v <- validar_errores(e, m = 24)
  cat("\n--- Residuos de la tendencia", tp, "---\n")
  print(correlograma(data.frame(y = e))$panel)
  cat(sprintf("Ljung-Box (m = 24, 24 gl): Q = %.2f, critico = %.2f, p = %.3g\n",
              v$lb$obs, qchisq(0.95, 24), v$lb$p))
  cat(sprintf("Jarque-Bera (2 gl): JB = %.2f, critico = %.2f, p = %.3g\n",
              v$jb$obs, qchisq(0.95, 2), v$jb$p))
  cat(sprintf("Durbin-Watson: d = %.3f, r1 = %.3f, 2(1-r1) = %.3f\n",
              v$dw$d, v$dw$r1, v$dw$aprox_2_1_menos_r1))
}

# ==== FIN CHUNK 5 ====


# ==== CHUNK 6: Calculo rapido de los dos referentes "ingenuos" (inspeccion individual) ====

res_media   <- ajustar_media(y_s2)
res_ingenuo <- ajustar_ingenuo(y_s2)

cat("Nivel final Media simple:", res_media$Extra_muestral(1), "\n")
cat("Nivel final Ingenuo:     ", res_ingenuo$Extra_muestral(1), "\n")

# ==== FIN CHUNK 6 ====


# ==== CHUNK 7: ITEM 6(c) - Metodos aplicados a la serie 2 ====
# Referente tecnico: Ingenuo (coincide con el diagnostico: Y_t = Y_{t-1} + e_t).
# Media simple se incluye como referente alterno/contraste: asume nivel
# constante (estacionariedad), lo cual contradice el diagnostico de la serie.

ajust2 <- list(
  "Ingenuo (referente)"           = ajustar_ingenuo,
  "Media simple" = ajustar_media,
  "Media movil"       = function(v) ajustar_mm(v,  opts2_mm$optimo$k),
  "SES "               = function(v) ajustar_ses(v, opts2_ses$optimo$alpha),
  "Doble media movil (contraste)" = function(v) ajustar_dmm(v, opts2_dmm$optimo$k),
  "Holt (contraste)"              = function(v) ajustar_holt(v, opts2_holt$optimo$alpha,
                                                             opts2_holt$optimo$beta),
  "Tendencia lineal (contraste)"      = function(v) ajustar_tendencia(v, "lineal",      calcular_yhat = FALSE),
  "Tendencia cuadratica (contraste)"  = function(v) ajustar_tendencia(v, "cuadratica",  calcular_yhat = FALSE),
  "Tendencia exponencial (contraste)" = function(v) ajustar_tendencia(v, "exponencial", calcular_yhat = FALSE)
)

# ==== FIN CHUNK 7 ====


# ==== CHUNK 8: ITEM 6(c) - Origen movil h=1 y h=H, mismos origenes, ordenado por RMSE ====
# Conteo esperado de origenes (ver Chunk 1): h=1 -> 44 ; h=H2 -> 38

tab2_h1 <- tabla_origen_movil(y_s2, ajust2, n0_2, 1,   "Ingenuo (referente)")
tab2_H  <- tabla_origen_movil(y_s2, ajust2, n0_2, H2,  "Ingenuo (referente)")

stopifnot(all(tab2_h1$n_origenes == 44), all(tab2_H$n_origenes == 38))

cat("h = 1 (44 origenes)\n");  print(tab2_h1, digits = 4)
cat("\nh =", H2, "(38 origenes)\n"); print(tab2_H, digits = 4)

# Error medio a h = H (signo del sesgo)
print(sapply(names(ajust2), function(nm) mean(origen_movil(y_s2, ajust2[[nm]], n0_2, H2)$error)))

# ==== FIN CHUNK 8 ====


# ==== CHUNK 9: ITEM 6(d) - Errores de origen movil a un paso del ganador de h=1 ====

ganador2_h1 <- tab2_h1$metodo[1]
print(ganador2_h1)

om2_1 <- origen_movil(y_s2, ajust2[[ganador2_h1]], n0_2, 1)
e2_1  <- om2_1$error

ggplot(data.frame(fecha = serie_2$fecha[om2_1$objetivo], error = e2_1), aes(fecha, error)) +
  geom_hline(yintercept = 0) + geom_line() + geom_point() +
  labs(title = paste("Errores de origen movil a un paso:", ganador2_h1),
       x = "Fecha", y = paste("Error (", attr(serie_2, "unidad"), ")"),
       caption = paste0("n = ", length(e2_1), " errores, h = 1")) +
  theme_minimal()

# ==== FIN CHUNK 9 ====

# ==== CHUNK 10: ITEM 6(d) - Correlograma de los errores (banda con n = numero de errores) ====

correlograma(data.frame(y = e2_1))$panel

# ==== FIN CHUNK 10 ====

# ==== CHUNK 11: ITEM 6(d) - t de media cero, Ljung-Box, Jarque-Bera y Durbin-Watson ====

val2_1 <- validar_errores(e2_1)
n_e2   <- length(e2_1); m_e2 <- min(floor(n_e2 / 4), 24)

cat(sprintf("t media cero (%d gl): t = %.3f, critico = %.3f, p = %.3g\n",
            n_e2 - 1, val2_1$t$obs, qt(0.975, n_e2 - 1), val2_1$t$p))
cat(sprintf("Ljung-Box (m = %d, %d gl): Q = %.3f, critico = %.3f, p = %.3g\n",
            m_e2, m_e2, val2_1$lb$obs, qchisq(0.95, m_e2), val2_1$lb$p))
cat(sprintf("Jarque-Bera (2 gl): JB = %.3f, critico = %.3f, p = %.3g\n",
            val2_1$jb$obs, qchisq(0.95, 2), val2_1$jb$p))
cat(sprintf("Durbin-Watson: d = %.3f, r1 = %.3f, 2(1-r1) = %.3f\n",
            val2_1$dw$d, val2_1$dw$r1, val2_1$dw$aprox_2_1_menos_r1))

# ==== FIN CHUNK 11 ====

# ==== CHUNK 12: ITEM 6(e) - Desviacion de errores de origen movil a cada horizonte h=1..H ====

sd2_h <- vapply(1:H2, function(h) sd(origen_movil(y_s2, ajust2[[ganador2_h1]], n0_2, h)$error),
                numeric(1))
print(round(sd2_h, 3))

# ==== FIN CHUNK 12 ====

# ==== CHUNK 13: ITEM 6(e) - Ganador reajustado sobre toda la serie y H pronosticos ====
# NOTA: serie_2 es diaria -> se usa by = "day" (serie_1 era mensual, usaba by = "month")

pron2 <- ajust2[[ganador2_h1]](y_s2)$pronosticar(H2)
fut2  <- seq(max(serie_2$fecha), by = "day", length.out = H2 + 1)[-1]

df2_fc  <- data.frame(fecha = fut2, pron = pron2,
                      inf = pron2 - 1.96 * sd2_h, sup = pron2 + 1.96 * sd2_h)
df2_obs <- tail(data.frame(fecha = serie_2$fecha, y = y_s2), 30)   # ultimo tramo (ajustable)

print(df2_fc)

# Dejar en NA si no se cuenta con un valor de referencia regulatorio para esta variable.
 # <- reemplazar por el valor correspondiente si aplica

p_final <- ggplot() +
  geom_line(data = df2_obs, aes(fecha, y)) +
  geom_ribbon(data = df2_fc, aes(fecha, ymin = inf, ymax = sup), alpha = 0.25) +
  geom_line(data = df2_fc, aes(fecha, pron), colour = "red") +
  labs(title = paste("Serie 2: pronostico a", H2, "dias con", ganador2_h1),
       x = "Fecha", y = attr(serie_2, "unidad"),
       caption = "Banda heuristica: ±1.96 veces la desviacion de los errores de origen movil a cada horizonte; no es un intervalo de confianza.") +
  theme_minimal()


print(p_final)

# ==== FIN CHUNK 13 ====
