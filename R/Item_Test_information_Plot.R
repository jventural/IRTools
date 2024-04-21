Item_Test_information_Plot <- function(gpcm_03, num_items, item_names, grayscale = FALSE) {
  library(ggplot2)
  library(dplyr)
  library(mirt)
  library(tibble)
  library(reshape2)
  library(gridExtra)

  # Función interna para el gráfico de información del test
  Plot_test_information <- function(gpcm_03, grayscale) {
    tI <- testinfo(gpcm_03, Theta = seq(-3, 3, 0.01)) %>% as.data.frame()
    se <- 1 / (sqrt(tI$.))
    tI <- cbind(tI, ThetaT = seq(-3, 3, length.out = 601), se)
    colnames(tI) <- c("Test_Info", "ThetaT", "se")
    tI$overall <- "Test"

    color_scale <- if(grayscale) scale_colour_manual(values = c("gray20", "gray50")) else scale_colour_manual(values = c("black", "#CB4335"))

    plot <- ggplot(tI, aes(x = ThetaT)) +
      geom_line(aes(y = Test_Info, color = "Information"), size = 0.3) +  # Información del test
      geom_line(aes(y = se, color = "Standard Error"), linetype = "dotted", size = 0.3) +  # Error estándar
      scale_x_continuous(breaks = seq(-6, 6, 1)) +
      ylab("Information") +
      ggtitle("") +
      color_scale +
      labs(title = "", x = "θ", y = "Information", color = "Type") +
      scale_linetype_manual("Type", values = c("solid", "dotted")) +  # Especificar estilos de línea
      theme_bw() +
      facet_wrap(~overall) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.background = element_rect(fill = if(grayscale) "gray70" else "#CB4335"),
            strip.text = element_text(size = 12, face = "bold", color = if(grayscale) "black" else "white"))

    return(plot)
  }

  # Función interna para el gráfico de información de los ítems
  Plot_item_information <- function(gpcm_03, num_items, item_names, grayscale) {
    item_info_list <- list()
    for (i in 1:num_items) {
      item_info <- as_tibble(testinfo(gpcm_03, Theta = seq(-4, 4, 0.01), which.items = i))
      colnames(item_info) <- paste0("info_", item_names[i])
      item_info_list[[i]] <- item_info
    }
    Theta <- as_tibble(seq(-6, 6, length.out = 801))
    colnames(Theta) <- "Theta"
    itemtI <- bind_cols(item_info_list, Theta)

    Item_Information <- itemtI %>%
      select(Theta, starts_with("info_")) %>%
      melt(id.vars = "Theta") %>%
      group_by(variable) %>%
      mutate(theta = seq(-6, 6, length.out = 801))

    names_to_labels <- setNames(item_names, paste0("info_", item_names))

    plot <- ggplot(data = Item_Information, aes(x = theta, y = value, group = variable)) +
      geom_line(size = 0.3) +
      xlab("θ") +
      ylab("Information") +
      theme_bw() +
      facet_wrap(~variable, labeller = as_labeller(names_to_labels)) +
      theme(strip.background = element_rect(fill = if(grayscale) "gray70" else "#CB4335"),
            strip.text = element_text(size = 10, face = "bold", color = if(grayscale) "black" else "white"))

    return(plot)
  }

  p1 <- Plot_test_information(gpcm_03, grayscale)
  p2 <- Plot_item_information(gpcm_03, num_items, item_names, grayscale)
  combined_plot <- grid.arrange(p2, arrangeGrob(p1), ncol = 2, widths = c(1, 2))
  return(combined_plot)
}
