plot_item_response_curves <- function(model_group, color_palette = "Set1") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(see)  # Assuming `see` package is installed for styling
  library(RColorBrewer)  # Para las paletas de colores
  # Asegúrate de que la paleta elegida esté disponible
  if (!color_palette %in% rownames(brewer.pal.info)) {
    stop("La paleta especificada no está disponible. Elija otra de RColorBrewer.")
  }

  # Extraer los nombres de los ítems directamente de los datos
  item_names <- colnames(model_group@Data$tabdata)
  # Obtener los nombres de los grupos de model_group@Data$group
  group_names <- levels(model_group@Data$group)

  # Secuencia de theta
  theta <- seq(-4, 4, by = 0.01)

  # Calcular las métricas esperadas para cada grupo basado en los niveles del factor
  e_data <- lapply(seq_along(group_names), function(g) {
    sapply(
      item_names,
      function(item_name) expected.item(
        extract.item(model_group, item_name, group = g),
        Theta = matrix(theta)
      ),
      simplify = FALSE
    ) %>%
      bind_rows(.id = "item") %>%
      mutate(theta = theta, .before = everything()) %>%
      pivot_longer(
        cols = -theta,
        names_to = "item",
        values_to = "expected"
      ) %>%
      mutate(group = group_names[g])
  })

  # Combinar los datos de todos los grupos
  plot_dat <- bind_rows(e_data)

  # Número de colores necesarios
  num_colors <- length(unique(plot_dat$group))

  # Crear el gráfico
  ggplot(plot_dat, aes(x = theta, y = expected, color = group)) +
    geom_line(na.rm = TRUE) +
    facet_wrap(vars(item)) +
    scale_color_brewer(palette = color_palette, direction = 1, name = "Group") +
    theme_modern() +
    labs(color = "Group")
}
