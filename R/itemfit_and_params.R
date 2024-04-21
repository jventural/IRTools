itemfit_and_params <- function(itemfit_gpcm_03, gpcm_params_03) {
  library(dplyr)
  library(tibble)
  combined_df <- bind_cols(itemfit_gpcm_03, gpcm_params_03) %>%
    as_tibble() %>%
    select(-Zh, -S_X2, -df.S_X2) %>%
    relocate(c(RMSEA.S_X2, p.S_X2), .after = b4)

  return(combined_df)
}
