fit_and_reliability_IRT <- function(model_list,
                                    model_names,
                                    modelfit_list,
                                    rename_factors = NULL  # <- nuevo argumento
) {
  library(tidyverse)

  # ─────────────────────────────────────────────────────────────────────────────
  # Función interna 1: Tabla de bondades de ajuste para los modelos IRT
  # ─────────────────────────────────────────────────────────────────────────────
  Table_fit_IRT <- function(model_list, model_names, modelfit_list) {
    # Métricas a extraer
    metricas <- c("logLik", "AIC", "BIC")
    results_df <- data.frame(Modelo = character(),
                             logLik = numeric(),
                             AIC    = numeric(),
                             BIC    = numeric(),
                             stringsAsFactors = FALSE)

    # Iterar sobre los modelos para extraer métricas
    for (i in seq_along(model_list)) {
      metrics_values <- sapply(metricas, function(metrica) {
        extract.mirt(model_list[[i]], metrica)
      })
      temp_df <- data.frame(
        Modelo = model_names[i],
        logLik = metrics_values[1],
        AIC    = metrics_values[2],
        BIC    = metrics_values[3],
        stringsAsFactors = FALSE
      )
      results_df <- rbind(results_df, temp_df)
    }

    row.names(results_df) <- NULL

    # Unir métricas de los modelos con la bondad de ajuste
    fit_df <- bind_rows(modelfit_list) %>%
      bind_cols(results_df) %>%
      relocate(Modelo)

    return(fit_df)
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # Función interna 2: Calcular fiabilidad empírica
  # ─────────────────────────────────────────────────────────────────────────────
  calculate_empirical_rxx <- function(models, model_names, rename_factors) {
    if (length(models) != length(model_names)) {
      stop("La longitud de 'models' debe coincidir con la de 'model_names'.")
    }

    reliability_results <- list()

    for (i in seq_along(models)) {
      # 1) Obtener puntajes EAP para usar en empirical_rxx
      eap_scores <- fscores(
        models[[i]],
        method          = "EAP",
        full.scores.SE  = TRUE,
        QMC             = TRUE
      )

      # 2) Renombrar las columnas de factores (si el usuario lo desea)
      #    rename_factors es algo como c(G="Dependencia", S1="Necesidad", S2="Autonomia")
      if (!is.null(rename_factors)) {
        # Forma 1: usar recode() y un fallback .default
        # Ojo: recode() no siempre funciona bien si las columnas no existen.
        # En su lugar, podemos hacer un pequeño bucle y renombrar manualmente:

        for (old_name in names(rename_factors)) {
          new_name <- rename_factors[[old_name]]
          # Si la columna old_name existe en eap_scores, la renombramos
          if (old_name %in% colnames(eap_scores)) {
            colnames(eap_scores)[colnames(eap_scores) == old_name] <- new_name
          }
        }
      }

      # 3) Calcular fiabilidad empírica (retorna vector nombrado)
      rxx_values <- empirical_rxx(eap_scores)
      rxx_values <- round(rxx_values, 2)

      # 4) Pasar a data frame (1 fila) con las columnas = nombres del vector
      df_temp <- as.data.frame(t(rxx_values)) %>%
        mutate(Modelo = model_names[i]) %>%
        relocate(Modelo)

      reliability_results[[i]] <- df_temp
    }

    # 5) Combinar todos los data.frames
    results_df <- bind_rows(reliability_results)
    return(results_df)
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # 1. Obtener la tabla de bondades de ajuste para los modelos IRT
  # ─────────────────────────────────────────────────────────────────────────────
  combined_results_df <- Table_fit_IRT(model_list, model_names, modelfit_list)

  # ─────────────────────────────────────────────────────────────────────────────
  # 2. Calcular la fiabilidad empírica con la posibilidad de renombrar factores
  # ─────────────────────────────────────────────────────────────────────────────
  reliability_results <- calculate_empirical_rxx(
    model_list,
    model_names,
    rename_factors = rename_factors
  )

  # ─────────────────────────────────────────────────────────────────────────────
  # 3. Unir ambas tablas (bondad de ajuste y fiabilidad)
  # ─────────────────────────────────────────────────────────────────────────────
  final_df <- left_join(combined_results_df, reliability_results, by = "Modelo")

  return(final_df)
}
