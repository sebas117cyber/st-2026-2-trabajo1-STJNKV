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
