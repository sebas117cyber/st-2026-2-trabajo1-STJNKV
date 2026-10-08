<https://github.com/sebas117cyber/st-2026-2-trabajo1-STJNKV.git>

# st-2026-2-trabajo1-STJNKV

## Qué contiene el repositorio

| Archivo | Contenido | Dependencias |
|:-----------------------|:-----------------------|:-----------------------|
| `R/00-lectura.R` | `leer_serie()`: lee un objeto `ts` o un csv y devuelve un tibble con `t`, `fecha`, `y` y los atributos `frecuencia`, `fuente` y `unidad` | tibble |
| `R/01-graficos.R` | `graficar_serie()`, `acf_muestral()`, `banda_ruido_blanco()`, `correlograma()` y `graficar_optimizacion()` | ggplot2, patchwork |
| `R/02-operadores.R` | `diferenciar`, `polinomio_rezago` | tibble |
| `R/03-dependencias.R` |   |   |
| `R/04-metodos` | Los ocho métodos (`ajustar_media()`, `ajustar_mm()`, `ajustar_ses()`, `ajustar_dmm()`, `ajustar_tendencia()`, `ajustar_holt()`) y `optimizar()` | R base |
| `informe/informe.qmd` | | Quarto, knitr |
| `figs/` | Figuras generadas por `| | |`sesion-info.txt`| Salida de`sessionInfo()\` de la máquina en la que se cerró la entrega |  |
| `datos/`|   |   |
## Cómo se corre

``` r
# paquetes
```

# tiempos de ejecucion

```         
quarto render informe/informe.qmd
```

## Cómo se usan las funciones

``` r

# codigo de ejemplo con una serie
```

## Resumen de resultados

## Declaración de uso de IA

Se usó un asistente de IA como apoyo:

- **Lo que se pidió:**
- **Lo que se recibió:**
- **Lo que se verificó por cuenta propia:**.
