# Competencia 1 - DMEyF 2026

Pipeline en R para predecir la baja de clientes (BAJA+2) en 202108 y armar la lista de envíos.

## Estructura

```
.
├── descargar-datos.R           # 1. descarga y clase ternaria --> Se ejecuta 1ro
├── optimizacion-hiperparam.R   # 2. preprocesamiento + optimización bayesiana --> Se ejecuta 2do
├── prediccion-agosto-v2.R      # 3. entrenamiento final y entrega --> Se ejecuta 3ro
├── datasets/                   # lo crea el paso 1 (no se versiona)
└── exp/                        # salidas de cada experimento (no se versiona)
```

Todos los scripts calculan las rutas desde la raíz del proyecto con `here::here()`, así que conviene abrir el proyecto desde el `.Rproj` (o correrlos con la raíz como directorio de trabajo).

## Limitaciones

- `optimizacion-hiperparam.R`: No se completaron las 30 iteraciones configuradas. Por tiempo, la corrida se cortó con 25 iteraciones registradas en el log y se eligió la iteración 20: 
`num_iterations = 3849`, `num_leaves = 806`, `min_sum_hessian_in_leaf = 0.486`. 
La sección final del script (entrenamiento con 03–04 y ganancia en 202106) no llegó a ejecutarse.

- `prediccion-agosto-v2.R`: Lee la iteración 20 del `BO_log.txt`, pero el `BO_log.txt` lo perdí por configurar mal la VM.
No se corrieron las 20 semillas, solo se completaron las dos primeras (285937 y 474619). 
La entrega final promedia las probabilidades de esas dos y toma los 11750 clientes con mayor probabilidad. Si se corren las 20, ese bloque filtra explícitamente esas dos semillas, así que la entrega no cambia.

- Reproducibilidad: dudosa en este momento: Hay semillas fijas en la partición (413353), en la bayesiana y en cada modelo (`deterministic = TRUE`, `data_random_seed` fijo), pero no está garantizado que se reproduzca exactamente la entrega porque la bayesiana se cortó a mano en la iteración 25. 
