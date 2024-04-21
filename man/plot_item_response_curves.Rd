\name{plot_item_response_curves}
\alias{plot_item_response_curves}
\title{Plot Expected Item Response Curves for Different Groups}

\description{
This function plots expected item response curves for each item across different groups using a multi-group item response model. It facilitates the comparison of item performances across various groupings in the data, highlighting differences in item functioning that may indicate differential item functioning (DIF) or other group-specific patterns.
}

\usage{
plot_item_response_curves(model_group, color_palette = "Set1")
}

\arguments{
  \item{model_group}{A multi-group item response model object containing data and parameter estimates.}
  \item{color_palette}{The color palette to use for differentiating groups in the plot. Default is "Set1" from RColorBrewer.}
}

\details{
The function extracts item names and group classifications from the model data structure. For each group, it computes expected response values across a range of theta (trait) values. These computations are performed for each item, creating a dataset that merges responses across groups for visual comparison.

The function ensures that the specified color palette is available using the RColorBrewer package, and if not, it stops execution with an error message, prompting the user to select a different palette. It then uses ggplot2 to create a line plot of the expected item responses, faceted by item, with lines colored by group to illustrate differences in item response curves.

Styling is applied using the `see` package's `theme_modern()` for a clean and modern aesthetic. The plot includes a legend that indicates which color corresponds to which group.
}

\value{
A ggplot object depicting the item response curves for each item faceted by group, with lines colored according to the specified RColorBrewer palette. This visual output aids in qualitative comparisons across groups, enhancing understanding of how different groups might respond differently to the same set of items.
}

\examples{
# Assuming 'model_group' is a previously fitted multi-group IRT model
response_curves_plot <- plot_item_response_curves(model_group)
print(response_curves_plot)
}

\author{
Dr. José Ventura-León
}
