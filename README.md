# `01 Data` — bases de datos

Datos de entrada y salida del proyecto (Chile, cohorte 2010–2020 salvo que se indique).

## Input

| Archivo | Contenido (resumen) |
|--------|----------------------|
| **`births_2010_2020_weeks_long.RData`** | Nacimientos en formato largo: una fila por `id` y semana de gestación (`week_gest_num`), con fechas de ventana semanal, comuna, covariables sociodemográficas y resultados de parto. Base principal para construir analíticas por embarazo y por semana. |
| **`hw_data_1980_2021.RData`** | Panel de olas de calor y métricas relacionadas por comuna y día (umbrales percentiles, EHF, TAD/Tmax, duraciones, etc.). Se cruza con la última semana de gestación por intervalos de fechas. |
| **`pollution_2010_2020.RData`** | Panel comunal diario: contaminantes (O₃, PM₂.₅, NO₂) con métodos IDW/kriging, además de variables meteorológicas/agrícolas en la grilla (`TAD`, `tmax`, `tmin`, NDVI, etc.). |
| **`sovi_datasets.RData`** | Índice de vulnerabilidad social (SOVI) u homólogo por comuna; se une a los datos de nacimiento por `cod_com` / `com` para categorías de vulnerabilidad y puntaje. |

## Output

| Archivo | Contenido (resumen) |
|--------|----------------------|
| **`births_2010_2020_last_week_hw_o3.RData`** | Una fila por nacimiento (última semana de gestación): mismas unidades que el análisis principal, con indicadores de olas de calor en esa semana, medias semanales de O₃ (kriging/IDW), unión SOVI y variables de ajuste. |
| **`births_2010_2020_weekly_gest_tad.RData`** | Formato largo por `id` y `week_gest_num`: media diaria de **TAD** (°C) en `[date_start_week, date_end_week]` por comuna (ventana semanal a lo largo de la gestación). |
| **`Codebook_births_hw_o3.xlsx`** | Libro de códigos (`variable`, `description` en inglés) para la base `births_2010_2020_last_week_hw_o3`. |

Los objetos exactos dentro de cada `.RData` dependen del script de exportación (`02 Code/1.0 Data_process.R` y similares).
