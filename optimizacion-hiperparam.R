# Paquetes

library(data.table)
library(R.utils)
library(rlist)
library(bbotk)
library(mlr3)
library(mlr3mbo)
library(mlr3learners)
library(DiceKriging)
library(lightgbm)

# Definición de parámetros

PARAM <- list()
PARAM$experimento <- "competencia1-EXP2"
PARAM$semilla_primigenia <-  413353   # Mi primera semilla

# training y future
PARAM$training_pct <- 70
PARAM$train <- c(202103, 202104)
PARAM$train_final <- c(202103, 202104)
PARAM$future <- c(202106)
PARAM$cortes <- seq(9000, 15000, by= 500)


# Por el momento, no hago undersampling
PARAM$trainingstrategy$undersampling <- 1

# Iteraciones bayesianas
PARAM$hyperparametertuning$iteraciones <- 30 # nunca llegué a correr esto completo

# Parametros LightGBM

# parametros fijos del LightGBM que se pisaran con la parte variable de la BO
PARAM$lgbm$param_fijos <-  list(
  boosting= "gbdt", 
  objective= "binary",
  metric= "auc",
  feature_pre_filter= FALSE,
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  force_row_wise= TRUE,
  deterministic= TRUE,
  verbosity= -100,
  
  seed= PARAM$semilla_primigenia,
  
  max_depth= -1L,
  min_gain_to_split= 0,
  lambda_l1= 0.0,
  lambda_l2= 0.0,
  max_bin= 31L,
  
  bagging_fraction= 1.0,
  pos_bagging_fraction= 1.0,
  neg_bagging_fraction= 1.0,
  is_unbalance= FALSE,
  scale_pos_weight= 1.0,
  
  drop_rate= 0.1,
  max_drop= 50,
  skip_drop= 0.5,
  
  extra_trees= FALSE,
  
  early_stopping= 0,
  min_data_in_leaf= 0,     # fijo, esto es FUNDAMENTAL (revisar explicación que dió profesor en 
  # clase sobre su relación con min_sum_hessian_in_leaf)
  feature_fraction= 0.50,  # fijo
  feature_fraction_bynode= 0.20,  # fijo
  learning_rate= 0.005,    # fijo
  
  num_iterations= 20000,     # se modificara
  num_leaves= 1024,          # se modificara
  min_sum_hessian_in_leaf= 1e-06  # se modificara
)

# bordes de los hiperparametros de la BO

PARAM$hypeparametertuning$hs <- ps(
  num_iterations= p_int(lower= 64L,  upper= 4096L),
  num_leaves= p_int(lower= 16L,  upper= 2048L),
  min_sum_hessian_in_leaf= p_dbl( lower=1e-06, upper=0.1)
)

# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa= "", campo= "fold", start= 1, seed= NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  
  bloque <- unlist(mapply(
    function(x, y) {rep(y, x)},division, seq(from= start, length.out= length(division))))
  
  data[, (campo) := sample(rep(bloque,ceiling(.N / length(bloque))))[1:.N],by= agrupa]
}

# logueo al archivo BO_log.txt
loguear  <- function( preg, arch=NA, verbose=TRUE )
{
  t0 <- Sys.time()
  reg <- copy( preg )
  if( "feature_contri" %in%  names(reg) ) {
    reg$feature_contri <- reg$feature_contri[1]
  }
  archivo <- arch
  if( is.na(arch) ) archivo <- paste0( folder, substitute( reg), ext )
  
  
  if( !file.exists( archivo ) )
  {
    # Escribo los titulos
    linea  <- paste0( "fecha\t",
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  # escribo el registro
  linea  <- paste0( format(t0, "%Y%m%d.%H%M%S"),  "\t",     # la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  # grabo al archivo
  
  if( verbose )  cat( linea )   # imprimo por pantalla
}

# Preprocesamiento
BASE <- here::here()   
dir.create(file.path(BASE, "exp", PARAM$experimento),
           recursive = TRUE, showWarnings = FALSE)
setwd(file.path(BASE, "exp", PARAM$experimento))

# lectura del dataset
dataset <- fread(file.path(here::here(), "datasets", "competencia_01.csv.gz"),
                 stringsAsFactors = TRUE)


# Meses que se usan en todo el experimento ----------------------
meses_usados <- c(202103, 202104, 202106)
dataset <- dataset[foto_mes %in% meses_usados]

gc()

# Lags y delta lags de orden 1 y 2 ------------------------------
# Se cruza por mes calendario: el lag k de un mes es el valor del
# cliente exactamente k meses antes. Si ese mes no esta en el dataset
# (o el cliente no aparece ese mes), el lag queda NA.

no_lag <- c("numero_de_cliente", "foto_mes", "clase_ternaria", "clase01")
cols_lag <- setdiff(names(dataset), no_lag)
cols_lag <- cols_lag[sapply(dataset[, ..cols_lag], is.numeric)]

dataset[, periodo0 := as.integer(foto_mes %/% 100) * 12L + foto_mes %% 100L]

for (k in 1:2) {
  nombres_lag <- paste0(cols_lag, "_lag", k)
  
  # tabla con los valores "corridos" k meses hacia adelante
  previo <- dataset[, c("numero_de_cliente", "periodo0", cols_lag), with = FALSE]
  previo[, periodo0 := periodo0 + k]
  setnames(previo, cols_lag, nombres_lag)
  
  # update join: trae el valor de k meses atras, NA si no existe
  dataset[previo, (nombres_lag) := mget(paste0("i.", nombres_lag)),
          on = c("numero_de_cliente", "periodo0")]
  rm(previo); gc()
  
  # delta lag: valor actual menos valor k meses atras
  for (col in cols_lag) {
    set(dataset, j = paste0(col, "_delta", k),
        value = as.numeric(dataset[[col]]) - as.numeric(dataset[[paste0(col, "_lag", k)]]))
  }
}

dataset[, periodo0 := NULL]

# Tendencia y promedio de los ultimos 3 meses ----------------------
source(file.path(BASE, "tendencia.R"))
setorderv(dataset, c("numero_de_cliente", "foto_mes"))
TendenciaYmuchomas(dataset, cols = cols_lag, ventana = 3,
                   tendencia = TRUE, minimo = FALSE, maximo = FALSE,
                   promedio = TRUE, ratioavg = FALSE, ratiomax = FALSE)
setorder(dataset, foto_mes, numero_de_cliente)

# Variables _zero: 1 si vale 0, NA si es NA, 0 si no (no se rankean) ---
# Muy posiblemente en este punto rompi algo
cols_zero <- cols_lag[sapply(dataset[, ..cols_lag], function(x) any(x == 0, na.rm = TRUE))]
dataset[, paste0(cols_zero, "_zero") := lapply(.SD, function(x) as.integer(x == 0)), .SDcols = cols_zero]

# Rank con cero fijo, por mes -----------------------------------
# NA -> NA ; 0 -> 0
# positivos -> equidistantes en (0, 1]   segun valores distintos
# negativos -> equidistantes en [-1, 0)  en espejo
rank_cero_fijo <- function(x) {
  res <- rep(NA_real_, length(x))
  res[!is.na(x) & x == 0] <- 0
  
  pos <- which(x > 0)
  if (length(pos) > 0) {
    r <- frank(x[pos], ties.method = "dense")
    res[pos] <- r / max(r)
  }
  
  neg <- which(x < 0)
  if (length(neg) > 0) { # variables solo positivas: no entra
    r <- frank(-x[neg], ties.method = "dense")
    res[neg] <- -r / max(r)
  }
  res
}

no_rank <- c("numero_de_cliente", "foto_mes", "clase_ternaria", "clase01")
cols_rank <- setdiff(names(dataset), no_rank)
cols_rank <- cols_rank[sapply(dataset[, ..cols_rank], is.numeric)]
cols_rank <- setdiff(cols_rank, paste0(cols_zero, "_zero"))

# paso a double: si una columna es entera, data.table truncaria el rank (0.5 -> 0)
dataset[, (cols_rank) := lapply(.SD, as.numeric), .SDcols = cols_rank]

# reemplaza cada variable por su version rankeada (las originales desaparecen)
dataset[, (cols_rank) := lapply(.SD, rank_cero_fijo),
        by = foto_mes, .SDcols = cols_rank]

# División del dataset ------------------

dataset_train <- dataset[foto_mes %in% PARAM$train]

particionar(dataset_train,
            division= c(PARAM$training_pct, 100L -PARAM$training_pct),
            agrupa= c("foto_mes", "clase_ternaria"),
            seed= PARAM$semilla_primigenia # aqui se usa SU semilla
)

# paso la clase a binaria que tome valores {0,1}  enteros
#  BAJA+1 y BAJA+2  son  1,   CONTINUA es 0

dataset_train[,
              clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
]


dataset_train[, training := as.integer(fold == 1)]

# los campos que se van a utilizar

campos_buenos <- setdiff(
  colnames(dataset_train),
  c("clase_ternaria", "clase01", "fold", "azar", "training")
)
# solo para la bayesiana: sin prestamos personales (ni sus lags/deltas)
campos_bo <- campos_buenos[!grepl("prestamos_personales", campos_buenos)]

# Configuracion Bayesian Optimization -----------------------------

# si ya existe un BO_log.txt de una corrida anterior de ESTE experimento,
# sigue numerando desde ahi 
giter <- 0
if (file.exists("BO_log.txt")) {
  tb_BO <- fread("BO_log.txt")
  giter <- tb_BO[, max(iter)]
}

# funcion que evalua la bayesiana: entrena con dtrain, mide en dvalidate
Estimar_lightgbm <- function(xs) {
  flush.console()
  giter <<- giter + 1
  
  # xs pisa (o agrega) a param_fijos
  param_completo <- modifyList(PARAM$lgbm$param_fijos, xs)
  xx <- copy(param_completo)
  
  modelo <- lgb.train(
    data   = dtrain,
    valids = list("val" = dvalidate),
    param  = param_completo
  )
  
  xx$iter    <- giter
  xx$metrica <- modelo$best_score
  
  loguear(xx, "BO_log.txt")
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  
  return(list(metrica = xx$metrica))
}

# training: fold 1 (70% de 202103 + 202104)
dtrain <- lgb.Dataset(
  data  = data.matrix(dataset_train[training == 1L, campos_bo, with = FALSE]),
  label = dataset_train[training == 1L, clase01],
  free_raw_data = FALSE
)

# validacion: fold 2 (30% de 202103 + 202104)
dvalidate <- lgb.Dataset(
  data  = data.matrix(dataset_train[fold == 2L, campos_bo, with = FALSE]),
  label = dataset_train[fold == 2L, clase01],
  free_raw_data = FALSE
)

cat("dtrain:", nrow(dtrain), "filas x", ncol(dtrain), "columnas\n")
cat("dvalidate:", nrow(dvalidate), "filas x", ncol(dvalidate), "columnas\n")

objective <- ObjectiveRFun$new(
  fun      = Estimar_lightgbm,
  domain   = PARAM$hypeparametertuning$hs,
  codomain = ps(metrica = p_dbl(tags = "maximize")),
  id       = "BO_lightgbm"
)

instance <- OptimInstanceBatchSingleCrit$new(
  objective  = objective,
  terminator = trm("evals", n_evals = PARAM$hyperparametertuning$iteraciones)
)

surrogate <- srlrn(lrn("regr.km",
                       optim.method = "BFGS",
                       nugget.estim = TRUE,
                       control      = list(trace = FALSE)))

acq_function  <- acqf("ei")   # expected improvement

acq_optimizer <- acqo(
  optimizer  = opt("random_search", batch_size = 1000),
  terminator = trm("evals", n_evals = 1000)
)

optimizer <- opt("mbo",
                 loop_function = bayesopt_ego,
                 surrogate     = surrogate,
                 acq_function  = acq_function,
                 acq_optimizer = acq_optimizer
)

# Corrida Bayesian Optimization ------------------------------------
t_inicio <- Sys.time()
bayesiana_salida <- optimizer$optimize(instance)
print(Sys.time() - t_inicio)

# libero memoria de la bayesiana antes de produccion
rm(dtrain, dvalidate, dataset_train); gc()


# PRODUCCION

# mejores hiperparametros = los de mayor metrica en el log
tb_bayesiana <- fread("BO_log.txt")
setorder(tb_bayesiana, -metrica)
PARAM$out$lgbm$mejores_hiperparametros <- as.list(tb_bayesiana[1])

param_final <- modifyList(PARAM$lgbm$param_fijos,
                          PARAM$out$lgbm$mejores_hiperparametros)
print(param_final[c("num_iterations", "num_leaves", "min_sum_hessian_in_leaf", "metrica")])

# clase binaria en todo el dataset
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]

# entrenamiento final: 202103 + 202104 completos (sin particion)
dataset_train_final <- dataset[foto_mes %in% PARAM$train_final]
print(dataset_train_final[, .N, list(foto_mes, clase_ternaria)])

dtrain_final <- lgb.Dataset(
  data  = data.matrix(dataset_train_final[, campos_buenos, with = FALSE]),
  label = dataset_train_final[, clase01]
)

modelo_final <- lgb.train(
  data  = dtrain_final,
  param = param_final
)

lgb.save(modelo_final, "modelo.txt")

# aplico el modelo a 202106
dfuture <- dataset[foto_mes %in% PARAM$future]

prediccion <- predict(
  modelo_final,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)

# ganancia: solo BAJA+2 suma, todo lo demas resta
tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
tb_prediccion[, prob := prediccion]
tb_prediccion[, gan := ifelse(clase_ternaria == "BAJA+2", 1072500, -27500)]

setorder(tb_prediccion, -prob)
tb_prediccion[, ganancia_acumulada := cumsum(gan)]
tb_prediccion[, pos := sequence(.N)]

for (envios in PARAM$cortes) {
  ganancia <- tb_prediccion[1:envios, sum(gan)]
  cat(envios, "\t", format(ganancia, big.mark = ".", scientific = FALSE), "\n")
}

fwrite(tb_prediccion, "prediccion_future.csv")

