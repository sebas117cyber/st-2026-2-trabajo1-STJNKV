# serie 2

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")
source("R/04-metodos.R")
source("R/05-evaluacion.R")

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


#----pronostico----


y_s2 <- serie_2$y 

## ---- Metodos sin parametro ----
res_media   <- ajustar_media(y_s2)
res_ingenuo <- ajustar_ingenuo(y_s2)

## ---- Media movil (k = 2,...,12) ----
rejilla_mm <- data.frame(k = 2:12)
res_mm     <- optimizar(y_s2, "mm", rejilla_mm)
mm_final   <- ajustar_mm(y_s2, k = res_mm$optimo$k)
g_mm <- graficar_mse_1d(res_mm, "k", "MSE - Media Movil (serie_2)")

## ---- SES (alpha = 0.02,...,0.98) ----
rejilla_ses <- data.frame(alpha = seq(0.02, 0.98, by = 0.02))
res_ses     <- optimizar(y_s2, "ses", rejilla_ses)
ses_final   <- ajustar_ses(y_s2, alpha = res_ses$optimo$alpha)
g_ses <- graficar_mse_1d(res_ses, "alpha", "MSE - SES (serie_2)")

## ---- Doble media movil (k = 3,...,12) ----
rejilla_dmm <- data.frame(k = 3:12)
res_dmm     <- optimizar(y_s2, "dmm", rejilla_dmm)
dmm_final   <- ajustar_dmm(y_s2, k = res_dmm$optimo$k)
g_dmm <- graficar_mse_1d(res_dmm, "k", "MSE - Doble Media Movil (serie_2)")

## ---- Tendencias (contraste) ----
tend_lineal <- ajustar_tendencia(y_s2, tipo = "lineal")
tend_cuad   <- ajustar_tendencia(y_s2, tipo = "cuadratica")
tend_exp    <- ajustar_tendencia(y_s2, tipo = "exponencial")

## ---- Holt (contraste, rejilla alpha,beta paso 0.05) ----
alpha_seq    <- seq(0.05, 0.95, by = 0.05)
rejilla_holt <- expand.grid(alpha = alpha_seq, beta = alpha_seq)
res_holt     <- optimizar(y_s2, "holt", rejilla_holt)
holt_final   <- ajustar_holt(y_s2, alpha = res_holt$optimo$alpha,
                             beta  = res_holt$optimo$beta)
g_holt <- graficar_mse_holt(res_holt, "MSE - Holt (serie_2)")

(g_mm | g_ses) / (g_dmm | g_holt)

resumen_optimos <- tibble(
  metodo    = c("Media movil", "SES", "Doble media movil", "Holt"),
  parametro = c(paste("k =", res_mm$optimo$k),
                paste("alpha =", res_ses$optimo$alpha),
                paste("k =", res_dmm$optimo$k),
                paste("alpha =", res_holt$optimo$alpha,
                      ", beta =", res_holt$optimo$beta)),
  mse_min   = c(res_mm$optimo$mse, res_ses$optimo$mse,
                res_dmm$optimo$mse, res_holt$optimo$mse)
)
print(resumen_optimos)

cat("Beta final de Holt:", holt_final$parametros$beta,
    "-> si es cercano a 0, confirma ausencia de tendencia local\n")


m_lb <- 24   # rezagos para Ljung-Box (regla practica para datos diarios)

diagnosticar_tendencia <- function(fit, nombre) {
  
  e <- fit$residuales
  n <- length(e)
  
  # Correlograma (usa acf_muestral_m y acf()/pacf() solo de verificacion internos)
  corr <- correlograma(tibble(y = e), m = m_lb)
  print(corr$panel)
  cat("Diferencia maxima ACF manual vs R (verificacion):",
      corr$diferencia_maxima, "\n")
  
  # Ljung-Box
  r_lb <- acf_muestral_m(e, m_lb)
  lb   <- ljung_box(r_lb, T_obs = n, m = m_lb, p = 0)
  
  # Jarque-Bera
  jb <- jarque_bera(e)
  
  # Durbin-Watson: propio vs aproximacion vs el calculado dentro de la tendencia
  dw_propio <- durbin_watson(e)
  r1        <- acf_muestral_m(e, 1)
  dw_aprox  <- 2 * (1 - r1)
  
  cat("\n----", nombre, "----\n")
  cat("DW (ajustar_tendencia): ", round(fit$dw, 4), "\n")
  cat("DW (funcion propia):    ", dw_propio$estadistico, "\n")
  cat("DW aproximado 2(1-r1):  ", round(dw_aprox, 4), "\n")
  print(lb)
  print(jb)
  
  list(ljung_box = lb, jarque_bera = jb, dw = dw_propio, panel = corr$panel)
}

diag_lineal <- diagnosticar_tendencia(tend_lineal, "Tendencia lineal")
diag_cuad   <- diagnosticar_tendencia(tend_cuad,   "Tendencia cuadratica")
diag_exp    <- diagnosticar_tendencia(tend_exp,    "Tendencia exponencial")


H          <- 7    # horizonte evaluado (1 semana, serie diaria)
n_origenes <- 44   # cantidad de origenes moviles

origenes <- origen_movil(y_s2, n_origenes)

k_mm       <- res_mm$optimo$k
alpha_ses  <- res_ses$optimo$alpha
k_dmm      <- res_dmm$optimo$k
alpha_holt <- res_holt$optimo$alpha
beta_holt  <- res_holt$optimo$beta

# Lista de metodos: cada uno devuelve la funcion pronosticar(h)
metodos_eval <- list(
  "Ingenuo (referente)"   = function(ent) ajustar_ingenuo(ent)$pronosticar,
  "Media simple"          = function(ent) ajustar_media(ent)$pronosticar,
  "Media movil"           = function(ent) ajustar_mm(ent, k_mm)$pronosticar,
  "SES"                   = function(ent) ajustar_ses(ent, alpha_ses)$pronosticar,
  "Doble media movil"     = function(ent) ajustar_dmm(ent, k_dmm)$pronosticar,
  "Tendencia lineal"      = function(ent) ajustar_tendencia(ent, "lineal")$pronosticar,
  "Tendencia cuadratica"  = function(ent) ajustar_tendencia(ent, "cuadratica")$pronosticar,
  "Tendencia exponencial" = function(ent) ajustar_tendencia(ent, "exponencial")$pronosticar,
  "Holt"                  = function(ent) ajustar_holt(ent, alpha_holt, beta_holt)$pronosticar
)

errores_tabla <- purrr::map_dfr(origenes, function(org) {
  
  origen_actual <- org$origen
  entrenamiento <- org$entrenamiento
  
  if (origen_actual + H > length(y_s2)) return(NULL)   # sin datos suficientes
  
  reales <- y_s2[(origen_actual + 1):(origen_actual + H)]
  
  purrr::map_dfr(names(metodos_eval), function(nombre_metodo) {
    
    pron_fn <- metodos_eval[[nombre_metodo]]
    
    # ajustar_tendencia imprime tabla en consola -> se silencia
    pronosticar <- NULL
    invisible(capture.output(pronosticar <- pron_fn(entrenamiento)))
    pronosticos <- pronosticar(H)
    
    tibble(origen = origen_actual, metodo = nombre_metodo,
           h = 1:H, real = reales, pronostico = pronosticos)
  })
})

errores_tabla <- errores_tabla |> 
  mutate(error     = real - pronostico,
         error_abs = abs(error),
         error_pct = abs(error / real) * 100)

tabla_resumen <- errores_tabla |> 
  filter(h %in% c(1, H)) |> 
  group_by(metodo, h) |> 
  summarise(RMSE = sqrt(mean(error^2, na.rm = TRUE)),
            MAD  = mean(error_abs, na.rm = TRUE),
            MAPE = mean(error_pct, na.rm = TRUE),
            .groups = "drop") |> 
  arrange(h, RMSE)

print(tabla_resumen, n = Inf)


ganador_nombre <- tabla_resumen |>  filter(h == 1) |>  slice(1) |>  pull(metodo)
cat("Metodo ganador en h=1:", ganador_nombre, "\n")

errores_ganador_h1 <- errores_tabla |> 
  filter(metodo == ganador_nombre, h == 1)

e <- errores_ganador_h1$error
n_e <- length(e)

# 1) Grafico de errores en el tiempo
g_errores <- ggplot(tibble(origen = errores_ganador_h1$origen, error = e),
                    aes(x = origen, y = error)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste("Errores de origen movil h=1 -", ganador_nombre),
       x = "Origen", y = "Error") +
  theme_minimal()
print(g_errores)

# 2) Correlograma de los errores (banda calculada dentro de la funcion)
m_d <- min(floor(n_e / 4), 20)
corr_ganador <- correlograma(tibble(y = e), m = m_d)
print(corr_ganador$panel)

# 3) Prueba t de media cero (manual, sin usar t.test de forma directa como metodo)
t_stat <- mean(e) / (sd(e) / sqrt(n_e))
df_t   <- n_e - 1
p_t    <- 2 * pt(-abs(t_stat), df = df_t)
cat(sprintf("Prueba t media cero: t = %.4f, df = %d, p-valor = %.4f\n",
            t_stat, df_t, p_t))

# 4) Ljung-Box (p segun parametros del ganador: 0 para Ingenuo, 1 para SES, etc.)
p_ganador <- switch(ganador_nombre,
                    "Ingenuo (referente)" = 0,
                    "SES"                 = 1,
                    "Holt"                = 2,
                    0)
r_d  <- acf_muestral_m(e, m_d)
lb_d <- ljung_box(r_d, T_obs = n_e, m = m_d, p = p_ganador)
print(lb_d)

# 5) Jarque-Bera
jb_d <- jarque_bera(e)
print(jb_d)

# 6) Durbin-Watson
dw_d <- durbin_watson(e)
print(dw_d)



H_final <- 7   # mismo horizonte evaluado en (c)

# Reajuste del ganador con toda la serie
ganador_fit <- switch(ganador_nombre,
                      "Ingenuo (referente)"   = ajustar_ingenuo(y_s2),
                      "Media simple"          = ajusta_media(y_s2),
                      "Media movil"           = ajustar_mm(y_s2, k_mm),
                      "SES"                   = ajustar_ses(y_s2, alpha_ses),
                      "Doble media movil"     = ajustar_dmm(y_s2, k_dmm),
                      "Tendencia lineal"      = ajustar_tendencia(y_s2, "lineal"),
                      "Tendencia cuadratica"  = ajustar_tendencia(y_s2, "cuadratica"),
                      "Tendencia exponencial" = ajustar_tendencia(y_s2, "exponencial"),
                      "Holt"                  = ajustar_holt(y_s2, alpha_holt, beta_holt)
)

pron_final <- ganador_fit$pronosticar(H_final)

# Banda heuristica: +-1.96 * sd(error de origen movil) por cada horizonte
sd_por_horizonte <- errores_tabla |> 
  filter(metodo == ganador_nombre) |> 
  group_by(h) |> 
  summarise(sd_error = sd(error, na.rm = TRUE), .groups = "drop")

banda <- sd_por_horizonte$sd_error[1:H_final] * 1.96

fechas_futuras <- seq(max(serie_2$fecha) + 1, by = "day", length.out = H_final)

df_pronostico <- tibble(
  fecha   = fechas_futuras,
  y       = pron_final,
  lim_inf = pron_final - banda,
  lim_sup = pron_final + banda
)

# Ultimo tramo de la serie para contexto visual
ultimo_tramo <- serie_2 |>  slice_tail(n = 30) |>  select(fecha, y)

g_pronostico <- ggplot() +
  geom_line(data = ultimo_tramo, aes(x = fecha, y = y), color = "black") +
  geom_line(data = df_pronostico, aes(x = fecha, y = y), color = "blue") +
  geom_point(data = df_pronostico, aes(x = fecha, y = y), color = "blue") +
  geom_ribbon(data = df_pronostico,
              aes(x = fecha, ymin = lim_inf, ymax = lim_sup),
              fill = "blue", alpha = 0.2) +
  labs(title = paste("Pronostico final -", ganador_nombre),
       subtitle = "Banda heuristica ±1.96 x sd(errores de origen movil)",
       x = "Fecha", y = attr(serie_2, "unidad")) +
  theme_minimal()

print(g_pronostico)
print(df_pronostico)
