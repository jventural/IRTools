value_test_information <- function(gpcm_03) {
  library(ggplot2)
  library(dplyr)
  library(tibble) # Asegúrate de tener todas estas librerías instaladas.
  # Parte 1: Generación de información del test y gráfico
  tI <- testinfo(gpcm_03, Theta = seq(-3, 3, 0.01)) %>% as.data.frame()
  se <- 1 / (sqrt(tI$.))
  tI <- cbind.data.frame(tI, ThetaT = seq(-3, 3, length.out = 601), se)
  colnames(tI) <- c("Test_Info", "ThetaT", "se")
  tI$overall <- "Test"

  plot <- ggplot(tI, aes(x = ThetaT, y = Test_Info)) +
    geom_line(aes(y = Test_Info, color = "Information"), size = 0.3) +
    geom_line(aes(y = se, color = "Standard error"), size = 0.3) +
    scale_x_continuous(breaks = seq(-6, 6, 1)) +
    ylab("Information") +
    ggtitle("") +
    scale_colour_manual(name = "Type", values = c("black", "#CB4335")) +
    labs(title = "", x = "θ", y = "Information") +
    theme_bw() +
    facet_wrap(~overall) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          strip.background = element_rect(fill = "#CB4335"),
          strip.text = element_text(size = 12, face = "bold", color = "white"))

  result <- list(plot = plot, Test_Info = tI$Test_Info)

  # Parte 2: Procesamiento del resultado del gráfico
  max_test_info_value <- result$plot$data %>%
    mutate(se = 1 / (sqrt(Test_Info))) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    select(Test_Info) %>%
    max()

  filtered_data <- result$plot$data %>%
    mutate(se = 1 / (sqrt(Test_Info))) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    filter(Test_Info == max_test_info_value)

  processed_result <- list(max_test_info_value = max_test_info_value, filtered_data = filtered_data)

  return(processed_result)
}
