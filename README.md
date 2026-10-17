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

# codigo de ejemplo con una serie
```

## Resumen de resultados

## Declaración de uso de IA

Se usó un asistente de IA como apoyo:

- **Lo que se pidió:**
Se pidió apoyo en la creación de tablas latex para la renderizacion del qmd, apoyo en la adaptación de código para crear las funciones  `ajustar_hw` y `ajustar_hw_multi`. también se pidió asistencia en código para la organización de resultados de pronósticos.

- **Lo que se recibió:**
Se recibió código de tablas en látex para qmd, código de R con las funciones creadas y funciones nuevas para la organización de resultados.

- **Lo que se verificó por cuenta propia:**.
Se verificó los valores de las tablas y se organizaron en orden para el informe.El código de las funciones nuevas se verificaron su funcionamiento y se modificaron deacuerdo a lo necesitado, se usaron especialmente en el punto 6 para organizar los resultados de los pronósticos. 