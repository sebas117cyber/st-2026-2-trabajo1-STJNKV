<https://github.com/sebas117cyber/st-2026-2-trabajo1-STJNKV.git>

# st-2026-2-trabajo1-STJNKV

## Qué contiene el repositorio

| Archivo | Contenido | Dependencias |
|:-----------------------|:-----------------------|:-----------------------|
| `R/00-lectura.R` | `leer_serie()`: lee un objeto `ts` o un csv y devuelve un tibble con `t`, `fecha`, `y` y los atributos `frecuencia`, `fuente` y `unidad` | tibble |
| `R/01-graficos.R` | `graficar_serie()`, `correlograma()` y `graficar_optimizacion()` | ggplot2 |
| `R/02-operadores.R` | `diferenciar`, `polinomio_rezago` | tibble |
| `R/03-dependencias.R` |   |   |
| `R/04-metodos` | Los ocho métodos (`ajustar_media()`, `ajustar_mm()`, `ajustar_ses()`, `ajustar_dmm()`, `ajustar_tendencia()`, `ajustar_holt()`) y `optimizar()` | R base |
| `informe/informe.qmd` | | Quarto, knitr |
| `figs/` | Figuras generadas por `| | |`sesion-info.txt`| Salida de`sessionInfo()\` de la máquina en la que se cerró la entrega |  |
| `datos/`|   |   |
## Cómo se corre

``` r
# paquetes
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(kableExtra)

```

# tiempos de ejecucion

```         
quarto render informe/informe.qmd
 tardo 55 segundos en renderizar en R 4.51

```
## Propiedad de funciones 

Las funciones de 
`R/00-lectura.R` `R/01-graficos` `R/03-dependencias.R` `R/04-metodos.R` `R/05-evaluaciones.R`
son Propiedad del compañero Daniel Garavito mientras que las funciones de`R/02-operadores.R` fueron creadas por sebastian valencia 


## Cómo se usan las funciones

``` r
# Media simple

y <- c(18.43, 19.98, 19.51, 20.63, 19.78, 21.25, 21.18, 22.14)

media_p <- ajusta_media(y)

# Media Movil con ventana de k = 3

mm_p <- ajustar_mm(y, 3)
mm_p$pronosticar(1)

# Suavizamiento exponencial con alpha= 0.4

ses_p <- ajustar_ses(y, 0.4)
ses_p$pronosticar(1)
# Se utilizo el mismo vector (y) como ejemplo

# Holt con alpha = 0.3 y Beta = 0.4

x <- c(133, 155, 165, 171, 194, 231, 274, 312, 313, 333, 343)

holt_p <- ajustar_holt(x, 0.3, 0.4)


ajustar_holt(x, 0.3, 0.4)

# Doble media movil con k = 4

dmm_p <- ajustar_dmm(x, 4)
dmm_p$pronosticar(1)

#Ajustar tendencia (Cuadratica, lineal y exponencial)

ajustar_tendencia(x, tipo = c("lineal"), corregir_sesgo = FALSE)
ajustar_tendencia(x, tipo = c("cuadratica"), corregir_sesgo = FALSE)
ajustar_tendencia(x, tipo = c("exponencial"), corregir_sesgo = FALSE)
# Se utilizo como vector de ejemplo x

# Para las funciones que requieren parametros primero se utiliza la funcion optimizar() para que busque el mejor parametro y despues se usan las funciones metodos de pronosticos (ej: ajustar_holt(y, alpha, beta)
ajustar_hw(y, s, alpha, beta,
gamma)
ajustar_hw_mult(y, s, alpha, beta,
gamma)) <- funciones que requieren utilizar la función optimizar

# cuando dice que requiere la serie solo se le coloca los valores, (no con fecha) si no solo los valores.
```

## Resumen de resultados
| Serie | Transformación elegida | Modelo ganador | Parámetros | RMSE (h=1) | RMSE (H) |
|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| Serie 1 | operador de rezago orden 1 | Holt | alpha=0.2, beta=0.05 | 5.134 | 6.751 |
| Serie 2 | ninguna | Ingenuo | No aplica | 3.344 | 3.724 |
| Serie 3 | Diferencia estacional | Tendencia con ficticias | No aplica | 11.38 | 10.89 |


## Declaración de uso de IA

Se usó un asistente de IA como apoyo:

- **Lo que se pidió:**
Se pidió apoyo en la creación de tablas latex para la renderizacion del qmd, apoyo en la adaptación de código para crear las funciones  `ajustar_hw` y `ajustar_hw_multi`. también se pidió asistencia en código para la organización de resultados de pronósticos.

- **Lo que se recibió:**
Se recibió código de tablas en látex para qmd, código de R con las funciones creadas y funciones nuevas para la organización de resultados.

- **Lo que se verificó por cuenta propia:**.
Se verificó los valores de las tablas y se organizaron en orden para el informe.El código de las funciones nuevas se verificaron su funcionamiento y se modificaron deacuerdo a lo necesitado, se usaron especialmente en el punto 6 para organizar los resultados de los pronósticos. 
