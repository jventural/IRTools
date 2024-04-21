\name{evaluate_DRF}
\alias{evaluate_DRF}
\title{Evaluate Differential Response Functioning (DRF) in Item Response Models}

\description{
This function performs Differential Response Functioning (DRF) analysis on specified items within a dataset, applying a multigroup Item Response Theory (IRT) approach. It includes optional data balancing via SMOTE to address class imbalances, fits multigroup IRT models, and assesses Differential Item Functioning (DIF) across specified groups. The function outputs formatted results for directional (dDIF), uniform (uDIF), and non-uniform (sDIF) differential item functioning.
}

\usage{
evaluate_DRF(data, items, balance = TRUE, model_spec, group_col, itemtype = "graded")
}

\arguments{
  \item{data}{The dataset containing item responses and group identifiers.}
  \item{items}{A vector of item names to be included in the DRF analysis.}
  \item{balance}{Logical. If TRUE, applies SMOTE to balance the data based on the outcome variable specified in \code{group_col}. Default is TRUE.}
  \item{model_spec}{A character string specifying the model formula for the IRT model.}
  \item{group_col}{The name of the column in \code{data} that defines the groups for multigroup analysis.}
  \item{itemtype}{The type of IRT model to fit (e.g., "graded", "Rasch"). Default is "graded".}
}

\details{
The function first checks and prepares the data, selecting necessary items and optionally balancing it. It then defines a group variable from the dataset and fits a multigroup IRT model using the \pkg{mirt} package. Following model estimation, DRF analysis is conducted to identify and quantify differential item functioning across groups. Results are presented for three types of DIF: directional (dDIF), uniform (uDIF), and non-uniform (sDIF), each formatted for clarity and ease of interpretation.
}

\value{
A list containing three dataframes, each corresponding to different types of DIF results:
\itemize{
  \item dDIF: Directional DIF results formatted with confidence intervals.
  \item sDIF: Non-uniform DIF results formatted with confidence intervals.
  \item uDIF: Uniform DIF results formatted with confidence intervals.
}
This structured output facilitates detailed assessment of each item's functioning across different group dimensions.
}

\examples{
data <- df  # ensure df is your dataset containing response data and a group column
items <- c("C3", "C4", "C6", "C8", "C9")
group_col <- "Sexo"
model_spec <- "Jealousy = 1-5"

results <- evaluate_DRF(data, items, TRUE, model_spec, group_col)
print(results$dDIF)
print(results$sDIF)
print(results$uDIF)
}

\author{
Dr. José Ventura-León
}

