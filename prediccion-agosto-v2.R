library(data.table)
library(lightgbm)
library(Rcpp)

options(datatable.alloccol = 4096L)

PARAM <- list()
PARAM$experimento    <- "competencia1-PROD3"
PARAM$experimento_bo <- "competencia1-EXP2"
PARAM$iter_bo       <- 20   # iteracion de la bayesiana a usar 
# (no pude terminar de correrla, esta estaba entre las mejores y no diferería mucho 
# de las anteriores)
PARAM$semilla_primigenia <- 413353

PARAM$train_final <- c(202104, 202105, 202106)
PARAM$future  <- c(202108)
PARAM$semillas <- c(
  285937, 474619, 502487, 600673, 666637,
  413353, 824339, 829399, 479701, 485263,
  100019, 100043, 100049, 999961, 999983,
  501367, 504209, 500153, 898543, 798533
)

PARAM$envios  <- 11750

PARAM$ventana_historia <- 3

meses_usados <- c(202103, 202104, 202105, 202106, 202107, 202108)

BASE <- here::here()
dir.create(file.path(BASE, "exp", PARAM$experimento), recursive = TRUE, showWarnings = FALSE)

tb_bo <- fread(file.path(BASE, "exp", PARAM$experimento_bo, "BO_log.txt"))
mejor <- tb_bo[iter == PARAM$iter_bo]
stopifnot(nrow(mejor) == 1)
print(mejor[, .(iter, metrica, learning_rate, num_iterations, num_leaves, min_sum_hessian_in_leaf)])

PARAM$lgbm <- list(
  num_iterations          = as.integer(mejor$num_iterations),
  num_leaves              = as.integer(mejor$num_leaves),
  min_sum_hessian_in_leaf = mejor$min_sum_hessian_in_leaf
)

Sys.setenv(PKG_CXXFLAGS = "-ffp-contract=off")

cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
{
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;

    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}")

TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE) {
  gc(verbose = FALSE)
  ventana_regresion <- ventana

  last <- nrow(dataset)

  vector_ids <- dataset[, numero_de_cliente]

  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )

  vector_desde[1:ventana_regresion] <- 1

  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }

  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
        nueva_col[(0 * last + 1):(1 * last)]]
    }

    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
        nueva_col[(1 * last + 1):(2 * last)]]
    }

    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
        nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
        nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
        get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
        get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}

setwd(file.path(BASE, "exp", PARAM$experimento))

dataset <- fread(file.path(BASE, "datasets", "competencia_01.csv.gz"),
                 stringsAsFactors = TRUE)

dataset <- dataset[foto_mes %in% meses_usados]
gc()

stopifnot(all(c("cprestamos_personales", "mprestamos_personales") %in% names(dataset)))

cols_cajas <- grep("cajas_depositos", names(dataset), value = TRUE)
if (length(cols_cajas) == 0) stop("No encontre ninguna columna con 'cajas_depositos'")

for (col in cols_cajas) {
  antes <- dataset[foto_mes == 202105, .(ceros = sum(get(col) == 0, na.rm = TRUE),
                                         nas = sum(is.na(get(col))))]
  dataset[foto_mes == 202105 & get(col) == 0, (col) := NA]
  despues <- dataset[foto_mes == 202105, .(ceros = sum(get(col) == 0, na.rm = TRUE),
                                           nas = sum(is.na(get(col))))]
  cat(sprintf("%s en 202105: ceros %d -> %d | NA %d -> %d\n",
              col, antes$ceros, despues$ceros, antes$nas, despues$nas))
}

no_feature <- c("numero_de_cliente", "foto_mes", "clase_ternaria", "clase01")
cols_lag <- setdiff(names(dataset), no_feature)
cols_lag <- cols_lag[sapply(dataset[, ..cols_lag], is.numeric)]

dataset[, periodo0 := as.integer(foto_mes %/% 100) * 12L + foto_mes %% 100L]

for (k in 1:2) {
  nombres_lag <- paste0(cols_lag, "_lag", k)

  previo <- dataset[, c("numero_de_cliente", "periodo0", cols_lag), with = FALSE]
  previo[, periodo0 := periodo0 + k]
  setnames(previo, cols_lag, nombres_lag)

  dataset[previo, (nombres_lag) := mget(paste0("i.", nombres_lag)),
          on = c("numero_de_cliente", "periodo0")]
  rm(previo); gc()

  for (col in cols_lag) {
    set(dataset, j = paste0(col, "_delta", k),
        value = as.numeric(dataset[[col]]) - as.numeric(dataset[[paste0(col, "_lag", k)]]))
  }
}
dataset[, periodo0 := NULL]

setorder(dataset, numero_de_cliente, foto_mes)

TendenciaYmuchomas(dataset,
  cols      = cols_lag,
  ventana   = PARAM$ventana_historia,
  tendencia = TRUE,
  minimo    = FALSE,
  maximo    = FALSE,
  promedio  = TRUE,
  ratioavg  = FALSE,
  ratiomax  = FALSE
)
gc()

cols_zero <- cols_lag[sapply(dataset[, ..cols_lag], function(x) any(x == 0, na.rm = TRUE))]
dataset[, paste0(cols_zero, "_zero") := lapply(.SD, function(x) as.integer(x == 0)), .SDcols = cols_zero]

dataset <- dataset[foto_mes %in% c(PARAM$train_final, PARAM$future)]
gc()

rank_cero_fijo <- function(x) {
  res <- rep(NA_real_, length(x))
  res[!is.na(x) & x == 0] <- 0

  pos <- which(x > 0)
  if (length(pos) > 0) {
    r <- frank(x[pos], ties.method = "dense")
    res[pos] <- r / max(r)
  }

  neg <- which(x < 0)
  if (length(neg) > 0) {
    r <- frank(-x[neg], ties.method = "dense")
    res[neg] <- -r / max(r)
  }
  res
}

cols_rank <- setdiff(names(dataset), no_feature)
cols_rank <- cols_rank[sapply(dataset[, ..cols_rank], is.numeric)]
cols_rank <- setdiff(cols_rank, paste0(cols_zero, "_zero"))

na_antes   <- dataset[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = cols_rank]
cero_antes <- dataset[, sapply(.SD, function(x) sum(x == 0, na.rm = TRUE)), .SDcols = cols_rank]

dataset[, (cols_rank) := lapply(.SD, as.numeric), .SDcols = cols_rank]
dataset[, (cols_rank) := lapply(.SD, rank_cero_fijo), by = foto_mes, .SDcols = cols_rank]

na_desp   <- dataset[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = cols_rank]
cero_desp <- dataset[, sapply(.SD, function(x) sum(x == 0, na.rm = TRUE)), .SDcols = cols_rank]
rango     <- dataset[, sapply(.SD, function(x) range(x, na.rm = TRUE)), .SDcols = cols_rank]

if (any(na_antes != na_desp))     stop("El rank cambio la cantidad de NA")
if (any(cero_antes != cero_desp)) stop("El rank cambio la cantidad de ceros")
if (any(rango < -1 | rango > 1, na.rm = TRUE)) stop("Hay valores fuera de [-1, 1]")
cat("Sanity check del rank OK:", length(cols_rank), "variables\n")

dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]

campos_buenos <- setdiff(names(dataset), c("numero_de_cliente", "clase_ternaria", "clase01"))
cat("Campos para entrenar:", length(campos_buenos), "\n")

dataset_train <- dataset[foto_mes %in% PARAM$train_final]
print(dcast(dataset_train[, .N, .(foto_mes, clase_ternaria)],
            foto_mes ~ clase_ternaria, value.var = "N"))

dtrain <- lgb.Dataset(
  data  = data.matrix(dataset_train[, campos_buenos, with = FALSE]),
  label = dataset_train[, clase01],
  free_raw_data = FALSE
)

dfuture  <- dataset[foto_mes %in% PARAM$future]
mfuture  <- data.matrix(dfuture[, campos_buenos, with = FALSE])
clientes <- dfuture$numero_de_cliente
cat("Clientes a predecir en", PARAM$future, ":", length(clientes), "\n")

rm(dataset, dataset_train, dfuture); gc()

param_final <- list(
  boosting = "gbdt", objective = "binary", metric = "auc",
  feature_pre_filter = FALSE, first_metric_only = FALSE,
  boost_from_average = TRUE, force_row_wise = TRUE, deterministic = TRUE,
  verbosity = -100,
  max_depth = -1L, min_gain_to_split = 0, lambda_l1 = 0, lambda_l2 = 0,
  max_bin = 31L,
  bagging_fraction = 1.0, pos_bagging_fraction = 1.0, neg_bagging_fraction = 1.0,
  is_unbalance = FALSE, scale_pos_weight = 1.0,
  extra_trees = FALSE, early_stopping = 0,
  min_data_in_leaf = 0, feature_fraction = 0.50, feature_fraction_bynode = 0.20,
  learning_rate = 0.005,
  data_random_seed = PARAM$semilla_primigenia,

  num_iterations          = PARAM$lgbm$num_iterations,
  num_leaves              = PARAM$lgbm$num_leaves,
  min_sum_hessian_in_leaf = PARAM$lgbm$min_sum_hessian_in_leaf
)

semillas <- PARAM$semillas

grupos <- rep(c("A", "B"), each = length(semillas) / 2)

firma <- list(param = param_final, train_final = PARAM$train_final, campos = campos_buenos)

arch_avance <- "semillerio_avance.rds"
if (file.exists(arch_avance)) {
  avance <- readRDS(arch_avance)
  if (is.null(avance$suma_grupo) ||
      length(avance$suma_grupo$A) != length(clientes) ||
      !identical(avance$semillas, semillas) ||
      !identical(avance$grupos, grupos) ||
      !identical(avance$firma, firma))
    stop("semillerio_avance.rds es de otra corrida: borralo o cambia PARAM$experimento")
  cat("Retomo: ya hay", avance$hechas, "semillas\n")
} else {
  avance <- list(hechas = 0L, semillas = semillas, grupos = grupos, firma = firma,
                 suma_grupo = list(A = rep(0, length(clientes)),
                                   B = rep(0, length(clientes))))
}

arch_por_semilla <- "probabilidades_por_semilla.csv"
if (avance$hechas == 0L && file.exists(arch_por_semilla)) file.remove(arch_por_semilla)

t_inicio <- Sys.time()
for (i in seq_along(semillas)) {
  if (i <= avance$hechas) next
  t0 <- Sys.time()

  param_final$seed <- semillas[i]
  modelo <- lgb.train(data = dtrain, param = param_final)

  prob_i <- predict(modelo, mfuture)
  g <- grupos[i]
  avance$suma_grupo[[g]] <- avance$suma_grupo[[g]] + prob_i
  avance$hechas <- i
  saveRDS(avance, arch_avance)

  fwrite(data.table(semilla = semillas[i], numero_de_cliente = clientes, prob = prob_i),
         arch_por_semilla, append = file.exists(arch_por_semilla))

  rm(modelo); gc(verbose = FALSE)
  cat(sprintf("semilla %3d/%d (%d)  %.1f min\n", i, length(semillas),
              semillas[i], as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
print(Sys.time() - t_inicio)

if (avance$hechas < length(semillas))
  stop("Faltan semillas: hechas ", avance$hechas, " de ", length(semillas))

envios_grupo <- list()
for (g in names(avance$suma_grupo)) {
  n_g <- sum(grupos == g)
  tb_entrega <- data.table(numero_de_cliente = clientes,
                           prob = avance$suma_grupo[[g]] / n_g)
  setorder(tb_entrega, -prob)
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:PARAM$envios, Predicted := 1L]

  fwrite(tb_entrega, paste0("probabilidades_202108_grupo", g, ".csv"))
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         paste0("entrega_", PARAM$experimento, "_grupo", g, "_", PARAM$envios, ".csv"))

  envios_grupo[[g]] <- tb_entrega[Predicted == 1L, numero_de_cliente]
  cat(sprintf("Grupo %s: %d semillas (%s)\n", g, n_g,
              paste(semillas[grupos == g], collapse = ", ")))
}

prob_total <- (avance$suma_grupo$A + avance$suma_grupo$B) / length(semillas)
tb_total <- data.table(numero_de_cliente = clientes, prob = prob_total)
setorder(tb_total, -prob)
fwrite(tb_total, "probabilidades_202108_todas.csv")
arch_total <- paste0("entrega_", PARAM$experimento, "_", length(semillas), "semillas_", PARAM$envios, ".csv")
fwrite(tb_total[1:PARAM$envios, list(numero_de_cliente)], arch_total, col.names = FALSE, scipen = 50L)
cat("Lista de las", length(semillas), "semillas juntas:", arch_total, "\n")

comunes <- length(intersect(envios_grupo$A, envios_grupo$B))
cat(sprintf("Clientes en ambas listas: %d de %d (%.1f%%)\n",
            comunes, PARAM$envios, 100 * comunes / PARAM$envios))

cat("Listo. Envios por lista:", PARAM$envios, "\n")


### Este ultimo script no llegué a ejecutarlo completo por lo
### que promedié las probabilidades predichas por las únicas
### dos semillas que llegué a correr


carpeta <- "~/exp/competencia1-PROD3"
semillas_usadas <- c(285937, 474619)   # las 2 primeras de PARAM$semillas
envios <- 11750

tb <- fread(file.path(carpeta, "probabilidades_por_semilla.csv"))
tb <- unique(tb, by = c("semilla", "numero_de_cliente"))
tb <- tb[semilla %in% semillas_usadas]
stopifnot(uniqueN(tb$semilla) == length(semillas_usadas))
print(tb[, .N, by = semilla])

tb_prom <- tb[, .(prob = mean(prob)), by = numero_de_cliente]
setorder(tb_prom, -prob)

fwrite(tb_prom[1:envios, .(numero_de_cliente)],
       file.path(carpeta, paste0("entrega_competencia1-PROD3_",
                                 length(semillas_usadas), "semillas_", envios, ".csv")),
       col.names = FALSE, scipen = 50L)
