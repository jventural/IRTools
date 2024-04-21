\name{Item_Test_information_Plot}
\alias{Item_Test_information_Plot}
\title{Generate and Combine Test and Item Information Plots for GPCM Models}

\description{
This function provides a comprehensive visualization tool for analyzing both test-level and item-level information in Generalized Partial Credit Models (GPCM). It generates two plots: one for the overall test information across a range of theta values, and another set of plots for individual item information. The function supports grayscale rendering for improved visual clarity in publications or presentations.
}

\usage{
Item_Test_information_Plot(gpcm_03, num_items, item_names, grayscale = FALSE)
}

\arguments{
  \item{gpcm_03}{A GPCM model object from which information will be computed.}
  \item{num_items}{An integer indicating the number of items to include in the item information plot.}
  \item{item_names}{A vector of character strings naming each item corresponding to the indices in `num_items`.}
  \item{grayscale}{A logical flag indicating whether the plots should be rendered in grayscale. Default is FALSE.}
}

\details{
The function comprises two main internal functions:
1. `Plot_test_information` which plots the test information curve and its corresponding standard error.
2. `Plot_item_information` which produces a series of plots for specified items showing how information varies with theta.

Both plots use ggplot2 for high-quality graphical output and can be customized for color or grayscale to suit different output needs. These plots are combined using `gridExtra` to provide a side-by-side visual comparison.
}

\value{
A combined graphical object consisting of an item information plot and a test information plot arranged side-by-side. This allows for easy comparison and assessment of model information across different scales and items.
}

\examples{
# Assuming 'gpcm_03' is a fitted GPCM model object
combined_plot <- Item_Test_information_Plot(gpcm_03,
                                            num_items = 5,
                                            item_names = c("C1", "C2", "C3", "C4", "C5"),
                                            grayscale = TRUE)
print(combined_plot)
}

\author{
Dr. José Ventura-León
}
