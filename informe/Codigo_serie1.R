# serie 1

serie_1 <- leer_serie("datos/STJNKV-serie-1.csv", "CORPALMAS acueducto", "miles m³")

crear_ficha <- function(serie, nombre = "Serie 1", unidad = attr(serie, "unidad")) {
  
  freq <- attr(serie, "frecuencia")
  if (is.null(freq)) freq <- 1
  
  frecuencia_label <- switch(as.character(freq),
                             "12"  = "Mensual",
                             "4"   = "Trimestral",
                             "1"   = "Anual",
                             "52"  = "Semanal",
                             "365" = "Diaria",
                             as.character(freq))
  
  fecha_ini <- as.character(min(serie$fecha, na.rm = TRUE))
  fecha_fin <- as.character(max(serie$fecha, na.rm = TRUE))
  val       <- serie$y
  
  tibble(
    Característica = c("Nombre", "Unidad", "Frecuencia",
                       "Fecha inicial", "Fecha final",
                       "T (observaciones)", "Mínimo",
                       "Máximo", "Media"),
    Valor = c(as.character(nombre),
              as.character(if (is.null(unidad)) "sin unidad" else unidad),
              frecuencia_label,
              fecha_ini,
              fecha_fin,
              as.character(length(val)),
              as.character(round(min(val, na.rm = TRUE), 2)),
              as.character(round(max(val, na.rm = TRUE), 2)),
              as.character(round(mean(val, na.rm = TRUE), 2)))
  )
}

ficha_st1 <- crear_ficha(serie_1, "consumo mensual de agua potable 2011-2025", "m³")

tabla_ficha1 <- kable(ficha_st1, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 1") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha1, file = "figs/ficha_serie1.png")

remove(tabla_ficha1, ficha_st1)


# grafico

serie1_graf <- Graficar_serie(serie_1, "consumo mensual de agua potable 2011-2025")

corre_serie1 <- correlograma(serie_1)

reza_serie1 <- corre_serie1$acf



# rezagos


# parece cuadratica, aplicar log?
# serie con los valores en log
serie_1trans <- serie_1 |> 
  mutate(y = log(y))


Graficar_serie(serie_1trans, "consumo mensual de agua ")

# operaciones de rezago

elegir_d <- function(y, d_max = 2) {
  vars <- purrr::map_dbl(0:d_max, ~ var(diferenciar(y, d = .x, D = 0, s = 1)))
  tibble::tibble(d = 0:d_max, varianza = vars) |>
    dplyr::mutate(optimo = varianza == min(varianza))
}

elegir_D <- function(y, s, D_max = 1) {
  vars <- purrr::map_dbl(0:D_max, ~ var(diferenciar(y, d = 0, D = .x, s = s)))
  tibble::tibble(D = 0:D_max, varianza = vars) |>
    dplyr::mutate(optimo = varianza == min(varianza))
}


elegir_d(serie_1trans$y)

elegir_D(serie_1trans$y, s= 12, D_max = 5)

# (1-B) (1-B^12)
op_rezago_serie1 <- diferenciar(serie_1trans$y, 1, 1, 12)

serie_1trans <- serie_1trans |> 
  append(op_rezago_serie1)




