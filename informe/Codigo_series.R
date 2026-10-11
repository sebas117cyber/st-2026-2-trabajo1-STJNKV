# serie 1

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")

library(readr)


#--------Vamos a ver la serie 1 ---------------------#

#Lectura
STJNKV_serie_1 <- leer_serie("datos/STJNKV-serie-1.csv","Serie inventada Corporacion Ambiental de Las Palmas","miles de m³")

grafico_serie1 <- graficar_serie(STJNKV_serie_1,"Consumo mensual de agua potable de una ciudad intermedia")


# Guardar gráfico
ggsave(
  "figs/grafico_serie1.png",
  plot = grafico_serie1,
  width = 8,
  height = 5,
  dpi = 300
)

corr_serie1 <- correlograma(STJNKV_serie_1,NULL)

# Guardar gráfico
ggsave(
  "figs/correlograma_serie1.png",
  plot = corr_serie1$panel,
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
  STJNKV_serie_1$y,
  c(1, 12, 24),
  unidad = "miles de m³",
  nombre = "Gráfico de rezagos - Serie 1"
)

dev.off()

# Frecuencia de la serie 1
frecuencia_serie1 <- attr(STJNKV_serie_1, "frecuencia")
y1 <- STJNKV_serie_1$y

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

#--------------------------------Serie 2---------------------------------------------#
STJNKV_serie_2 <- leer_serie("datos/STJNKV-serie-2.csv","Serie inventada Corporacion Ambiental de Las Palmas","µg/m³")

grafico_serie2 <- graficar_serie(STJNKV_serie_2,"Concentración diaria de PM2.5 en una estación de monitoreo")

# Guardar gráfico
ggsave(
  "figs/grafico_serie2.png",
  plot = grafico_serie2,
  width = 8,
  height = 5,
  dpi = 300
)

corr_serie2 <- correlograma(STJNKV_serie_2,NULL)

# Guardar gráfico
ggsave(
  "figs/correlograma_serie2.png",
  plot = corr_serie2$panel,
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
  STJNKV_serie_2$y,
  c(1, 2),
  unidad = "µg/m³",
  nombre = "Concentración diaria de PM2.5 en una estación de monitoreo"
)

dev.off()

#Item b
y2 <- STJNKV_serie_2$y

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

#--------------------------------------Serie 3----------------------------------------------#

#Lectura
STJNKV_serie_3<- leer_serie("datos/STJNKV-serie-3.csv","Serie inventada Corporacion Ambiental de Las Palmas","m³/s")

grafico_serie3 <- graficar_serie(STJNKV_serie_3,"Medición del caudal medio mensual")

# Guardar gráfico
ggsave(
  "figs/grafico_serie3.png",
  plot = grafico_serie3,
  width = 8,
  height = 5,
  dpi = 300
)

corr_serie3 <- correlograma(STJNKV_serie_3,NULL)

# Guardar gráfico
ggsave(
  "figs/correlograma_serie3.png",
  plot = corr_serie3$panel,
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
  STJNKV_serie_3$y,
  c(1, 2, 3, 4, 5, 6, 8, 9, 12 ),
  unidad = "m³/s",
  nombre = "Medición del caudal medio mensual"
)

dev.off()

#Item b 

#Extraemos los valores de la serie y la frecuencia 
y3 <- STJNKV_serie_3$y
s3 <- attr(STJNKV_serie_3, "frecuencia")  

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


#--------------------------------------------------------#
#Probarlos 
# Media móvil: con s = 4 y y = 1..10, la tendencia de una recta es la misma recta
#y <- 1:10
#media_movil_centrada(y, 4)
# esperado: NA NA 3 4 5 6 7 8 NA NA   (una recta queda igual donde se puede calcular)

# F: dos estaciones con medias claramente distintas debe dar F grande
#x   <- c(1, 2, 1, 2, 10, 11, 10, 11)
#pos <- c(1, 1, 1, 1, 2, 2, 2, 2)
#f_estacional(x, pos, 2)$F   # esperado: 1458 (SSB = 162, SSW = 1.. ver nota)

