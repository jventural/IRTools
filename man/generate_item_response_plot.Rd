\name{generate_item_response_plot}
\alias{generate_item_response_plot}
\title{Generate Item Response Curves Using mirt and ggplot2}

\description{
This function generates item response curves for specified items from a given IRT model, using the 'mirt' package. It allows for customization of the plot appearance, including greyscale options and the color of facet labels. The function ensures inputs are appropriately checked and provides clear labeling of each item's curve within the generated plot, making it useful for detailed examination of item characteristics across different trait levels.
}

\usage{
generate_item_response_plot(Modelo, item_names, greyscale = FALSE, fill = "#CB4335")
}

\arguments{
  \item{Modelo}{An IRT model object from which the item response plots are generated.}
  \item{item_names}{A character vector specifying the names of the items to be plotted.}
  \item{greyscale}{Boolean flag to specify whether the plots should be in greyscale. Default is FALSE.}
  \item{fill}{The color used for the facet labels. Default is "#CB4335".}
}

\details{
The function begins by checking that `item_names` is a character vector and ensuring required libraries (`ggplot2`, `mirt`) are loaded. It uses the `jcc.plot` function from the `mirt` package to generate the item characteristic curves (ICCs), and then formats the plot using `ggplot2` to enhance readability and visual appeal. This includes adjusting text size, line width, and applying a theme for the background and text elements. The facet wrap displays each item's curve separately for clear comparison.
}

\value{
A `ggplot` object containing the plotted item response curves, ready for display or additional customization.
}

\examples{
# Assume 'model_irt' is a fitted IRT model and you want to plot responses for items 1 to 3
Figura2 <- generate_item_response_plot(Modelo = gpcm_00,
                            item_names = paste0("C", 1:9),
                            greyscale = T,
                            fill = "gray32")
Figura2
}

\author{
Dr. José Ventura-León
}
