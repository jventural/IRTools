generate_item_response_plot <- function(Modelo, item_names, greyscale = FALSE, fill = "#CB4335") {
  library(jrt)
  library(ggplot2) # Asegurar que ggplot2 está cargado
  library(mirt)    # Asegurar que mirt está cargado

  # Verificar que item_names sea un vector de caracteres
  if (!is.character(item_names)) {
    stop("item_names debe ser un vector de caracteres.")
  }

  # Renombrar variables basado en item_names proporcionado por el usuario
  Item <- setNames(item_names, paste0("Item ", seq_along(item_names)))

  # Generar la curva característica de los ítems
  Plot <- jcc.plot(Modelo, facet.cols = 2, greyscale = greyscale, overlay.reliability = FALSE,
                   labelled = TRUE, line.width = 0.5, text.size = 2) +
    ggplot2::labs(title = "", x = "θ", y = "Probability of the item response") +
    theme_bw() +
    ggplot2::facet_wrap(~Item, labeller = as_labeller(Item)) +
    ggplot2::theme(strip.background = element_rect(fill = fill),
                   strip.text = ggplot2::element_text(size = 10, face = "bold", color = "white"))

  return(Plot)
}
