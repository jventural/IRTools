evaluate_local_independence <- function(gpcm_01) {
  # Cálculo de los scores theta
  Thetas <- fscores(gpcm_01)

  # Cálculo de residuales Q3
  Q3 <- residuals(gpcm_01, type = 'Q3', Theta = Thetas, verbose = FALSE)

  # Función auxiliar para seleccionar todos los elementos excepto la diagonal
  select_all_but_diag <- function(x) {
    matrix(x[lower.tri(x, diag = FALSE) | upper.tri(x, diag = FALSE)],
           nrow = nrow(x) - 1, ncol = ncol(x))
  }

  # Aplicación de la función auxiliar para obtener Q3_new
  Q3_new <- select_all_but_diag(Q3)

  # Cálculo de Q3*
  Q3_star <- round(max(Q3_new) - mean(Q3_new), 2)

  # Evaluación del valor de Q3* contra el umbral de 0.20
  if(Q3_star > 0.20) {
    return(list(Q3_star = Q3_star, message = "No se cumple el principio de independencia local mediante el estadístico Q3*. El valor es superior a 0.20"))
  } else {
    return(list(Q3_star = Q3_star, message = "Se cumple el principio de independencia local mediante el estadístico Q3*. El valor es inferior a 0.20"))
  }
}
