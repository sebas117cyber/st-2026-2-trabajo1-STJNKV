# serie 2

serie_2 <- leer_serie("datos/STJNKV-serie-2.csv", "CORPALMAS acueducto", "µg/m³")

# ficha

ficha_st2 <- crear_ficha(serie_2, "concentracion de PM2.5  2025-02-03 a 2025-09-10")

tabla_ficha2 <- kable(ficha_st2, booktabs = TRUE, align = c("l", "l"),
      caption = "Ficha técnica - Serie 2") |> 
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE)

save_kable(tabla_ficha2, file = "figs/ficha_serie2.png")

remove(ficha_st2, tabla_ficha2)


