# Trabajo 1 · Código del equipo: STJNKV

Series de Tiempo · Pregrado en Estadística · UNAL Medellín · 2026-II

Este código identifica al equipo en todo el trabajo: en el nombre del repositorio, en el `README.md`, en el encabezado del informe, en la nota para los interesados y en la portada de las diapositivas.

Tres series simuladas. Su contexto, que está al final de este archivo, es ficticio. Cada archivo tiene dos columnas: `fecha` (AAAA-MM-DD, el primer día del período) y `valor`. Los archivos se usan tal como están: ninguna observación se modifica, se elimina ni se imputa.

| Serie | Archivo | Nombre | Unidad | Frecuencia | Rango | T |
|---|---|---|---|---|---|---|
| 1 | `STJNKV-serie-1.csv` | Consumo mensual de agua potable de una ciudad intermedia | miles de m³ | mensual | 2011-11-01 a 2025-12-01 | 170 |
| 2 | `STJNKV-serie-2.csv` | Concentración diaria de PM2.5 en una estación de monitoreo | µg/m³ | diaria | 2025-02-03 a 2025-09-10 | 220 |
| 3 | `STJNKV-serie-3.csv` | Caudal medio mensual de un río andino | m³/s | mensual | 2012-01-01 a 2025-12-01 | 168 |

## El contexto

Las organizaciones, los lugares, las personas y las cifras de este contexto son ficticios.

La Corporación Ambiental de Las Palmas (CORPALMAS) es una autoridad ambiental ficticia que vigila el aire y el agua de una cuenca andina ficticia, en la que está la ciudad intermedia de Villa Clara con su empresa de acueducto. Las tres series salen de las redes de monitoreo de la corporación y de la facturación del acueducto.

### Serie 1: Consumo mensual de agua potable de una ciudad intermedia

Volumen de agua potable facturado cada mes por la empresa de acueducto de Villa Clara, en miles de m³. La planta de potabilización trata como máximo 650 mil m³ al mes; una ampliación cuesta 42.000 millones de pesos y tarda 18 meses entre la decisión y la puesta en marcha, y si la demanda supera la capacidad hay que racionar el servicio por sectores. Interesa a la junta de la empresa, al concejo municipal, que aprueba el crédito, y a los usuarios. La pregunta de la junta: ¿hay que decidir la ampliación de la planta en los próximos meses?

### Serie 2: Concentración diaria de PM2.5 en una estación de monitoreo

Concentración promedio de 24 horas de material particulado fino PM2.5, en µg/m³, en la estación de monitoreo del centro de Villa Clara. El protocolo municipal de calidad del aire obliga a declarar alerta para el día siguiente cuando la concentración pronosticada supera 37 µg/m³; la alerta restringe el tránsito de vehículos de carga y le cuesta al comercio cerca de 400 millones de pesos por día, mientras que un día por encima del umbral sin alerta expone a la población vulnerable. Interesa a la alcaldía, a la corporación, a los transportadores y a las personas con enfermedades respiratorias. La pregunta de la alcaldía: ¿se debe declarar alerta para mañana y para los días siguientes?

### Serie 3: Caudal medio mensual de un río andino

Caudal medio mensual, en m³/s, del río Sinuoso en la bocatoma del acueducto, medido por la estación limnigráfica de la corporación. La concesión permite captar el volumen completo mientras el caudal se mantenga por encima de 110 m³/s; por debajo de ese valor la corporación ordena reducir la captación a la mitad, y la empresa debe llenar con anticipación los tanques de reserva, lo que cuesta 150 millones de pesos por mes. Interesa a la corporación, a la empresa de acueducto y a los usuarios de la cuenca aguas abajo. La pregunta de la empresa de acueducto: ¿en cuáles de los próximos doce meses conviene preparar los tanques de reserva?

