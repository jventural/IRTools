fit_and_reliability_IRT <- function(model_list, model_names, modelfit_list) {
  library(tidyverse)

  #Función interna 1
  Table_fit_IRT <- function(model_list, model_names, modelfit_list) {
    # Asegurarte de que los paquetes necesarios están cargados
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("El paquete dplyr es necesario para esta función. Por favor, instálalo utilizando install.packages('dplyr')")
    }

    # Métricas a extraer
    metricas <- c("logLik", "AIC", "BIC")

    # Inicializar un data.frame para almacenar los resultados
    results_df <- data.frame(Modelo = character(), logLik = numeric(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)

    # Bucle para iterar sobre los modelos y extraer métricas
    for (i in seq_along(model_list)) {
      metrics_values <- sapply(metricas, function(metrica) extract.mirt(model_list[[i]], metrica))
      temp_df <- data.frame(Modelo = model_names[i], logLik = metrics_values[1], AIC = metrics_values[2], BIC = metrics_values[3], stringsAsFactors = FALSE)
      results_df <- rbind(results_df, temp_df)
    }

    row.names(results_df) <- NULL

    # Unir las métricas de los modelos con la bondad de ajuste
    fit_df <- bind_rows(modelfit_list) %>%
      bind_cols(results_df) %>%
      relocate(Modelo)

    return(fit_df)
  }

  #Función interna 2
  calculate_empirical_rxx <- function(models, model_names) {
    if (length(models) != length(model_names)) {
      stop("La longitud de 'models' debe coincidir con la de 'model_names'.")
    }

    # Inicializar un vector para almacenar los valores de fiabilidad empírica
    reliability_values <- numeric(length(models))

    # Bucle para procesar cada modelo
    for (i in seq_along(models)) {
      eap_scores <- fscores(models[[i]],
                            method = "EAP",
                            full.scores.SE = TRUE,
                            QMC = TRUE)

      # Calcular la fiabilidad empírica y redondear a dos decimales
      reliability_values[i] <- round(empirical_rxx(eap_scores), 2)
    }

    # Crear un data.frame con los resultados
    results_df <- data.frame(Modelo = model_names, Empirical_Rxx = reliability_values)

    return(results_df)
  }

  # Verificar que los paquetes necesarios están cargados
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("El paquete dplyr es necesario para esta función. Por favor, instálalo utilizando install.packages('dplyr')")
  }

  # Obtener la tabla de bondades de ajuste para los modelos IRT
  combined_results_df <- Table_fit_IRT(model_list, model_names, modelfit_list)

  # Calcular la fiabilidad empírica para los modelos
  reliability_results <- calculate_empirical_rxx(model_list, model_names)

  # Unir las tablas de bondad de ajuste y fiabilidad empírica
  final_df <- inner_join(combined_results_df, reliability_results, by = "Modelo")

  return(final_df)
}
