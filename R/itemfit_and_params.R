itemfit_and_params <- function(itemfit_gpcm_03, gpcm_params_03) {
  library(dplyr)
  library(stringr)

  # Combinar los dataframes
  combined_df <- bind_cols(itemfit_gpcm_03, gpcm_params_03) %>%
    as_tibble() %>%
    select(-Zh, -S_X2, -df.S_X2)

  # Identificar el nombre de la última columna que comienza con "b"
  last_b_col <- names(combined_df)[which(str_starts(names(combined_df), "b"))] %>% last()

  # Verificar si existe una columna que comienza con "b"
  if (!is.null(last_b_col)) {
    # Reubicar columnas
    combined_df <- combined_df %>%
      relocate(c(RMSEA.S_X2, p.S_X2), .after = all_of(last_b_col))
  }

  return(combined_df)
}

